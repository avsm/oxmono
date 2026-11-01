open Sql

type document = {
  id : string;
  revision : string;
  kind : string;
  title : string;
  text : string;
  url : string;
  date : string;
  facets : (string * string) list;
}

type chunk = { document : document; id : string; ordinal : int; text : string }

type filter = {
  kind : string option;
  since : string option;
  until : string option;
  facets : (string * string) list;
}

let all = { kind = None; since = None; until = None; facets = [] }

type t = { sql : Sqlite3_eio.t; lock : Eio.Mutex.t; snapshot : bool }

exception Conflict

let run t f =
  let result =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        try Ok (Sqlite3_eio.run t.sql f)
        with e -> Error (e, Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok value -> value
  | Error (e, bt) -> Printexc.raise_with_backtrace e bt

let read t f =
  run t (fun db ->
      if t.snapshot then f db
      else transaction ~immediate:false db (fun () -> f db))

let check_version db =
  match rows db "PRAGMA user_version" [] with
  | [ row ] when num row.(0) <= 1 -> ()
  | _ -> failwith "Knowledge index was written by a newer version"

let init sql =
  let t = { sql; lock = Eio.Mutex.create (); snapshot = false } in
  run t (fun db ->
      check (Sqlite3.exec db "PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;");
      transaction db (fun () ->
          check_version db;
          check
            (Sqlite3.exec db
               {|
CREATE TABLE IF NOT EXISTS metadata(name TEXT PRIMARY KEY, value TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS documents(id TEXT PRIMARY KEY, revision TEXT NOT NULL,
 kind TEXT NOT NULL, title TEXT NOT NULL, body TEXT NOT NULL, url TEXT NOT NULL, date TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS facets(document TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
 name TEXT NOT NULL, value TEXT NOT NULL, PRIMARY KEY(document,name,value));
CREATE INDEX IF NOT EXISTS facet_lookup ON facets(name,value,document);
CREATE TABLE IF NOT EXISTS chunks(id TEXT PRIMARY KEY, document TEXT NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
 ordinal INTEGER NOT NULL, body TEXT NOT NULL);
CREATE VIRTUAL TABLE IF NOT EXISTS search USING fts5(id UNINDEXED,title,body);
PRAGMA user_version=1;
|});
          Vector.init db));
  t

let open_db ~sw path =
  Sqlite_vec.register ();
  init (Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path)

let memory ~sw () =
  Sqlite_vec.register ();
  init (Sqlite3_eio.open_memory ~sw ())

let open_snapshot ~sw path =
  Sqlite_vec.register ();
  let sql = Sqlite3_eio.open_path ~sw ~mode:`READONLY ~busy_timeout:5000 path in
  let t = { sql; lock = Eio.Mutex.create (); snapshot = true } in
  run t (fun db ->
      check (Sqlite3.exec db "BEGIN");
      check_version db;
      ignore (rows db "SELECT count(*) FROM sqlite_master" []));
  t

let metadata t name =
  read t (fun db ->
      if rows db "SELECT 1 FROM sqlite_master WHERE name='metadata'" [] = []
      then None
      else
        match
          rows db "SELECT value FROM metadata WHERE name=?" [ txt name ]
        with
        | [ row ] -> Some (str row.(0))
        | _ -> None)

let counts t =
  read t (fun db ->
      match
        rows db
          "SELECT (SELECT count(*) FROM documents), \
           (SELECT count(*) FROM chunks)"
          []
      with
      | [ row ] -> (num row.(0), num row.(1))
      | _ -> assert false)

(* Paragraph boundaries keep headings with neighbouring prose where possible.
   Oversized paragraphs are split at UTF-8 boundaries, never clipped away. *)
let passages text =
  let size = String.length text in
  let rec loop offset acc =
    if offset >= size then List.rev acc
    else
      let stop =
        if size - offset <= 6000 then size
        else begin
          let mutable boundary = offset + 6000 in
          while
            boundary > offset && Char.code text.[boundary] land 0xc0 = 0x80
          do
            boundary <- boundary - 1
          done;
          (* Malformed continuation runs have no UTF-8 boundary. Retain their
             bytes while ensuring that every chunk advances the cursor. *)
          if boundary = offset then boundary <- offset + 6000;
          let halfway = offset + ((boundary - offset) / 2) in
          let mutable newline = boundary - 1 in
          while newline > halfway && text.[newline] <> '\n' do
            newline <- newline - 1
          done;
          if newline > halfway then newline + 1 else boundary
        end
      in
      loop stop (String.sub text offset (stop - offset) :: acc)
  in
  match loop 0 [] with [] -> [ "" ] | xs -> xs

let chunk_id id ordinal =
  Json.encode (Jsont.list Jsont.string) [ id; string_of_int ordinal ]

let chunks (d : document) =
  List.mapi
    (fun ordinal text ->
      { document = d; id = chunk_id d.id ordinal; ordinal; text })
    (passages d.text)

let delete db id =
  List.iter
    (fun r ->
      let key = str r.(0) in
      Vector.invalidate db key;
      execute db "DELETE FROM search WHERE id=?" [ txt key ])
    (rows db "SELECT id FROM chunks WHERE document=?" [ txt id ]);
  execute db "DELETE FROM documents WHERE id=?" [ txt id ]

let replace ?(metadata = []) t documents =
  if t.snapshot then invalid_arg "Cannot replace a read-only index snapshot";
  let ids = List.map (fun (d : document) -> d.id) documents in
  if
    List.exists (( = ) "") ids
    || List.length ids <> List.length (List.sort_uniq String.compare ids)
  then invalid_arg "Corpus IDs must be nonempty and unique";
  let present = Hashtbl.create (List.length ids) in
  List.iter (fun id -> Hashtbl.add present id ()) ids;
  run t (fun db ->
      transaction db (fun () ->
          List.iter
            (fun r ->
              if not (Hashtbl.mem present (str r.(0))) then
                delete db (str r.(0)))
            (rows db "SELECT id FROM documents" []);
          List.iter
            (fun (d : document) ->
              let fs = List.sort_uniq compare d.facets in
              let old =
                rows db
                  "SELECT revision,kind,title,body,url,date FROM documents \
                   WHERE id=?"
                  [ txt d.id ]
              in
              let old_fs =
                rows db
                  "SELECT name,value FROM facets WHERE document=? ORDER BY \
                   name,value"
                  [ txt d.id ]
                |> List.map (fun r -> (str r.(0), str r.(1)))
              in
              let values =
                [
                  txt d.revision;
                  txt d.kind;
                  txt d.title;
                  txt d.text;
                  txt d.url;
                  txt d.date;
                ]
              in
              if old <> [ Array.of_list values ] || old_fs <> fs then (
                delete db d.id;
                execute db "INSERT INTO documents VALUES(?,?,?,?,?,?,?)"
                  (txt d.id :: values);
                List.iter
                  (fun (name, value) ->
                    execute db "INSERT INTO facets VALUES(?,?,?)"
                      [ txt d.id; txt name; txt value ])
                  fs;
                List.iter
                  (fun c ->
                    execute db "INSERT INTO chunks VALUES(?,?,?,?)"
                      [ txt c.id; txt d.id; int c.ordinal; txt c.text ];
                    execute db "INSERT INTO search VALUES(?,?,?)"
                      [ txt c.id; txt d.title; txt c.text ])
                  (chunks d)))
            documents;
          List.iter
            (fun (name, value) ->
              execute db "INSERT OR REPLACE INTO metadata VALUES(?,?)"
                [ txt name; txt value ])
            metadata))

let document db r =
  let id = str r.(0) in
  let facets =
    rows db "SELECT name,value FROM facets WHERE document=? ORDER BY name,value"
      [ txt id ]
    |> List.map (fun r -> (str r.(0), str r.(1)))
  in
  {
    id;
    revision = str r.(1);
    kind = str r.(2);
    title = str r.(3);
    text = str r.(4);
    url = str r.(5);
    date = str r.(6);
    facets;
  }

let get t id =
  read t (fun db ->
      match rows db "SELECT * FROM documents WHERE id=?" [ txt id ] with
      | [ r ] -> Some (document db r)
      | _ -> None)

let where f =
  let clauses = ref [] and args = ref [] in
  let add clause values =
    clauses := !clauses @ [ clause ];
    args := !args @ values
  in
  Option.iter (fun k -> add "d.kind=?" [ txt k ]) f.kind;
  Option.iter (fun d -> add "d.date>=?" [ txt d ]) f.since;
  Option.iter (fun d -> add "d.date<=?" [ txt d ]) f.until;
  List.iter
    (fun (k, v) ->
      add
        "EXISTS (SELECT 1 FROM facets f WHERE f.document=d.id AND f.name=? AND \
         f.value=?)"
        [ txt k; txt v ])
    f.facets;
  ((if !clauses = [] then "1" else String.concat " AND " !clauses), !args)

(* Each result set owns one immutable document per ID. Chunk queries omit
   document bodies, which can be much larger than an individual passage. *)
let chunk_reader db result =
  let documents = Hashtbl.create 16 in
  let seen = Hashtbl.create 16 in
  let ids =
    List.filter_map
      (fun row ->
        let id = str row.(0) in
        if Hashtbl.mem seen id then None
        else (
          Hashtbl.add seen id ();
          Some id))
      result
  in
  let rec split remaining count acc =
    match remaining with
    | [] -> #(List.rev acc, [])
    | _ when count = 0 -> #(List.rev acc, remaining)
    | id :: rest -> split rest (count - 1) (id :: acc)
  in
  let rec load = function
    | [] -> ()
    | ids ->
        let #(batch, rest) = split ids 128 [] in
        let placeholders = String.concat "," (List.map (fun _ -> "?") batch) in
        let args = List.map txt batch in
        let facets = Hashtbl.create 16 in
        rows db
          ("SELECT document,name,value FROM facets WHERE document IN ("
         ^ placeholders ^ ") ORDER BY document,name,value")
          args
        |> List.iter (fun r ->
               let id = str r.(0) in
               let old =
                 Option.value ~default:[] (Hashtbl.find_opt facets id)
               in
               Hashtbl.replace facets id ((str r.(1), str r.(2)) :: old));
        rows db
          ("SELECT * FROM documents WHERE id IN (" ^ placeholders ^ ")")
          args
        |> List.iter (fun r ->
               let id = str r.(0) in
               let facets =
                 Option.value ~default:[] (Hashtbl.find_opt facets id)
                 |> List.rev
               in
               Hashtbl.add documents id
                 {
                   id;
                   revision = str r.(1);
                   kind = str r.(2);
                   title = str r.(3);
                   text = str r.(4);
                   url = str r.(5);
                   date = str r.(6);
                   facets;
                 });
        load rest
  in
  load ids;
  fun r ->
    let document = Hashtbl.find documents (str r.(0)) in
    { document; id = str r.(1); ordinal = num r.(2); text = str r.(3) }

let read_chunks db result = List.map (chunk_reader db result) result

let columns = "c.document,c.id,c.ordinal,c.body"

let selected db filter =
  let cond, args = where filter in
  rows db
    ("SELECT " ^ columns
   ^ " FROM chunks c JOIN documents d ON d.id=c.document WHERE " ^ cond
   ^ " ORDER BY d.id,c.ordinal")
    args
  |> read_chunks db

let lexical t filter query =
  read t (fun db ->
      let cond, args = where filter in
      if String.trim query = "" then
        rows db
          ("SELECT " ^ columns
         ^ " FROM chunks c JOIN documents d ON d.id=c.document WHERE " ^ cond
         ^ " ORDER BY d.date DESC,d.id,c.ordinal LIMIT 100")
          args
        |> read_chunks db
      else
        rows db
          ("SELECT " ^ columns
         ^ " FROM search JOIN chunks c ON c.id=search.id JOIN documents d ON \
            d.id=c.document WHERE search MATCH ? AND " ^ cond
         ^ " ORDER BY bm25(search),d.id,c.ordinal LIMIT 100")
          (txt query :: args)
        |> read_chunks db)

let backend t filter =
  Embedding.
    {
      documents = (fun () -> read t (fun db -> selected db filter));
      key = (fun c -> c.id);
      revision = (fun c -> c.document.revision);
      passage = (fun c -> clip 2000 c.document.title ^ "\n" ^ c.text);
      index =
        (fun ~model ~dimensions ~normalization ->
          if t.snapshot then Vector.identity ~model ~dimensions ~normalization
          else
            run t (fun db ->
                transaction db (fun () ->
                    Vector.index db ~model ~dimensions ~normalization)));
      current =
        (fun ~index ~document:c ~input_hash ->
          run t (fun db ->
              Vector.exists db index
              && Vector.current db ~index ~source:c.id
                ~revision:c.document.revision ~input_hash));
      put =
        (fun ~index ~document:c ~input_hash vector ->
          if t.snapshot then invalid_arg "Cannot index a read-only snapshot";
          run t (fun db ->
              transaction db (fun () ->
                  match
                    rows db
                      "SELECT d.revision,c.body,d.title FROM chunks c JOIN \
                       documents d ON d.id=c.document WHERE c.id=?"
                      [ txt c.id ]
                  with
                  | [ r ]
                    when str r.(0) = c.document.revision
                         && str r.(1) = c.text
                         && str r.(2) = c.document.title ->
                      Vector.put db ~index ~source:c.id
                        ~revision:c.document.revision ~input_hash vector
                  | _ -> raise Conflict)));
      nearest =
        (fun ~index ~limit vector ->
          read t (fun db ->
              if not (Vector.exists db index) then [] else
              let cond, args = where filter in
              (* Exact filtered distance ranking avoids losing eligible neighbours when
       unrelated documents occupy the unfiltered top k. *)
              let result = rows db
                ("SELECT " ^ columns
               ^ ",vec_distance_L2(v.embedding,?) AS distance FROM "
               ^ Vector.name index
               ^ " v JOIN chunks c ON c.id=v.source JOIN documents d ON \
                  d.id=c.document AND d.revision=v.revision WHERE " ^ cond
               ^ " ORDER BY distance,c.id LIMIT ?")
                ((Vector.binding vector :: args) @ [ int limit ])
              in
              let chunk = chunk_reader db result in
              List.map (fun r ->
                     ( chunk r,
                       match r.(4) with
                       | D.FLOAT f -> f
                       | _ -> failwith "Invalid distance" )) result));
      lexical = lexical t filter;
    }

let find_url t url =
  read t (fun db ->
      rows db "SELECT * FROM documents WHERE url=? ORDER BY id" [ txt url ]
      |> List.map (document db))
