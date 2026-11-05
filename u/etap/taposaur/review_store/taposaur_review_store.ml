module R = Taposaur_review
module Q = Sqlite3
type read = Read
type write = Write
type 'access t = {db : Sqlite3_eio.t; lock : Eio.Mutex.t;
  audit : Taposaur_audit.session option; path : string}
exception Not_found
let check = Q.Rc.check
let exec db sql = check (Q.exec db sql)
let stmt db sql f =
  let s = Q.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (Q.finalize s)) (fun () -> f s)
let text s = Q.Data.TEXT s
let int n = Q.Data.INT (Int64.of_int n)
let opt_int = function None -> Q.Data.NULL | Some n -> int n
let bind s args = check (Q.bind_values s args)
let done_ s = match Q.step s with
  | Q.Rc.DONE -> () | rc -> check rc; failwith "Expected SQL completion"
let command db sql args = stmt db sql (fun s -> bind s args; done_ s)
let run t f =
  let result = Eio.Mutex.use_rw ~protect:false t.lock (fun () ->
    try Ok (Sqlite3_eio.run t.db f)
    with ex -> Error (ex, Printexc.get_raw_backtrace ())) in
  match result with Ok v -> v
  | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
let transaction db f =
  exec db "BEGIN IMMEDIATE";
  match (let v = f () in exec db "COMMIT"; v) with
  | v -> v
  | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      (try exec db "ROLLBACK" with _ -> ());
      Printexc.raise_with_backtrace ex bt
let query db sql args decode = stmt db sql (fun s ->
  bind s args;
  let rec loop acc = match Q.step s with
    | Q.Rc.ROW -> loop (decode s :: acc)
    | Q.Rc.DONE -> List.rev acc | rc -> check rc; assert false in
  loop [])
let one = function [v] -> v | _ -> raise Not_found
let require = function Some v -> v | None -> raise Not_found
let from_json codec bytes = match R.decode codec bytes with
  | Ok v -> v | Error e -> failwith ("Invalid review data: " ^ e)
let field_bit = function
  | R.Statistics.Title -> 1 | Abstract -> 2 | Catalogue -> 4
  | Full_text -> 8 | Pdf -> 16 | Xml -> 32 | Tei -> 64 | S2orc -> 128
  | Publisher -> 256 | Stale -> 512 | Error -> 1024
let browse_schema db =
  exec db "CREATE TABLE snapshot_facets (snapshot_id INTEGER NOT NULL \
    REFERENCES snapshots(id), doi TEXT NOT NULL, publisher TEXT NOT NULL, \
    flags INTEGER NOT NULL, PRIMARY KEY(snapshot_id,doi)) WITHOUT ROWID";
  exec db "CREATE INDEX snapshot_publishers ON \
    snapshot_facets(snapshot_id,publisher,doi)";
  List.iter (fun field -> let bit = field_bit field in
    exec db (Printf.sprintf "CREATE INDEX snapshot_flag_%d ON \
      snapshot_facets(snapshot_id,(flags & %d),doi)" bit bit)) R.Statistics.fields;
  exec db "CREATE TABLE snapshot_browse_ready (snapshot_id INTEGER PRIMARY KEY \
    REFERENCES snapshots(id))";
  exec db "PRAGMA user_version=2"
(* Review schema 3: a cached export larger than one SQLite blob can hold
   is stored as ordered parts; the exports row then points at a descriptor
   carrying the whole export's digest and size. *)
let parts_schema db =
  exec db "CREATE TABLE IF NOT EXISTS export_parts (snapshot_id INTEGER NOT NULL, \
    format TEXT NOT NULL, part INTEGER NOT NULL, \
    blob TEXT NOT NULL REFERENCES objects(sha), \
    PRIMARY KEY(snapshot_id,format,part), \
    FOREIGN KEY(snapshot_id,format) REFERENCES exports(snapshot_id,format))";
  exec db "PRAGMA user_version=3"
let schema_version = 3
let open_db ?audit ~sw ~readonly path =
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000
    ?mode:(if readonly then Some `READONLY else None) path in
  let t = {db;lock=Eio.Mutex.create ();audit;path=Eio.Path.native_exn path} in
  run t (fun db ->
    let pragma p = query db ("PRAGMA " ^ p) [] (fun s -> Q.column_int s 0)
      |> one in
    let version = pragma "user_version" in
    let app = pragma "application_id" in
    if version = 0 && app = 0 && not readonly then begin
      let tables = query db
        "SELECT count(*) FROM sqlite_master WHERE name NOT LIKE 'sqlite_%'"
        [] (fun s -> Q.column_int s 0) |> one in
      if tables <> 0 then failwith "Refusing nonempty review database";
      transaction db (fun () -> exec db Schema.sql; browse_schema db;
        parts_schema db)
    end else if version < 1 || version > schema_version || app <> 1414682198 then
      failwith "Unsupported review database";
    if version >= 1 && version < schema_version && not readonly then
      transaction db (fun () ->
        if version < 2 then browse_schema db;
        parts_schema db);
    exec db "PRAGMA foreign_keys=ON";
    if readonly then exec db "PRAGMA query_only=ON"
    else exec db "PRAGMA journal_mode=WAL");
  t
let open_read ~sw path : read t = open_db ~sw ~readonly:true path
let open_write ?audit ~sw path : write t =
  Taposaur_audit.with_operation ?session:audit ~action:"review.store.open"
    ~subject:(Eio.Path.native_exn path)
    (fun _ -> open_db ?audit ~sw ~readonly:false path)
let put db body =
  let sha = R.digest body in
  command db "INSERT OR IGNORE INTO objects(sha,body) VALUES(?,?)"
    [text sha;Q.Data.BLOB body]; sha
let get_blob db sha = query db "SELECT body FROM objects WHERE sha=?"
  [text sha] (fun s -> Q.column_blob s 0) |> one
let put_blob t body = run t (fun db -> put db body)
let blob t sha = run t (fun db -> get_blob db sha)
type review = {
  slug : string; title : string; requester : string; owner : string;
  created : float; revision : int;
}
type revision = {
  review : review; number : int; definition : R.Definition.t;
  actor : string; reason : string; created : float;
}
type input_result = {
  key : string; blob : string; partial : bool;
  format : R.Definition.format;
  occurrences : int; diagnostics : R.diagnostic list;
}
type run = {
  id : int; slug : string; revision : int; actor : string; created : float;
  partial : bool; members : int; inputs : input_result list;
}
type snapshot = {
  id : int; run_id : int; slug : string; revision : int;
  parent : int option; label : string; actor : string; created : float;
  title : string; requester : string; owner : string;
  members : int; titles : int; abstracts : int; held : int;
  partial : bool; manifest : string;
}
let input_codec =
  Jsont.Object.map (fun key blob partial format occurrences diagnostics ->
    {key;blob;partial;format;occurrences;diagnostics})
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun i -> i.key)
  |> Jsont.Object.mem "blob" Jsont.string ~enc:(fun i -> i.blob)
  |> Jsont.Object.mem "partial" Jsont.bool
       ~enc:(fun (i : input_result) -> i.partial)
  |> Jsont.Object.mem "format" (Jsont.enum
       ["doi_lines",R.Definition.Doi_lines;
        "metadata_jsonl",R.Definition.Metadata_jsonl]) ~enc:(fun i -> i.format)
  |> Jsont.Object.mem "occurrences" Jsont.int ~enc:(fun i -> i.occurrences)
  |> Jsont.Object.mem "diagnostics" (Jsont.list R.diagnostic_jsont)
       ~enc:(fun i -> i.diagnostics)
  |> Jsont.Object.finish
let review_row s = {slug=Q.column_text s 0;title=Q.column_text s 1;
  requester=Q.column_text s 2;owner=Q.column_text s 3;
  created=Q.column_double s 4;revision=Q.column_int s 5}
let review_sql = "SELECT slug,title,requester,owner,created,head FROM reviews"
let find_db db slug = query db (review_sql ^ " WHERE slug=?")
  [text slug] review_row |> function [] -> None | xs -> Some (one xs)
let find t slug = run t (fun db -> find_db db slug)
let list t = run t (fun db -> query db (review_sql ^ " ORDER BY slug")
  [] review_row)
let required label s = if String.trim s = "" then
  invalid_arg (label ^ " must not be blank")
let define t ~slug ~title ~requester ~owner ~actor ~reason ~expected
    definition =
  List.iter (fun (name,s) -> required name s)
    ["title",title;"requester",requester;"owner",owner;
     "actor",actor;"reason",reason];
  if String.length slug = 0 || String.length slug > 80 ||
     not (String.for_all (function 'a'..'z' | '0'..'9' | '-' | '_' -> true
       | _ -> false) slug) then invalid_arg "Invalid review slug";
  let encoded = R.Definition.to_string definition in
  run t (fun db -> transaction db (fun () ->
    let current = find_db db slug in
    (match current with
     | None when expected = 0 ->
         command db "INSERT INTO reviews VALUES(?,?,?,?,?,0)"
           [text slug;text title;text requester;text owner;
            Q.Data.FLOAT (Unix.gettimeofday ())]
     | Some r when r.revision = expected ->
         if (r.title,r.requester,r.owner) <> (title,requester,owner) then
           invalid_arg "Review identity differs from the existing review"
     | _ -> invalid_arg "Revision conflict: use the current expected revision");
    List.iter (fun (i : R.Definition.input) ->
      Option.iter (fun sha -> ignore (get_blob db sha)) i.source.blob)
      definition.inputs;
    let sha = put db encoded and number = expected + 1 in
    command db "INSERT INTO revisions VALUES(?,?,?,?,?,?)"
      [text slug;int number;text sha;text actor;text reason;
       Q.Data.FLOAT (Unix.gettimeofday ())];
    command db "UPDATE reviews SET head=? WHERE slug=?"
      [int number;text slug]; number))
let revision_db db ~slug ~number =
  let review = find_db db slug |> require in
  let number = Option.value ~default:review.revision number in
  query db "SELECT definition_sha,actor,reason,created FROM revisions \
    WHERE slug=? AND number=?" [text slug;int number] (fun s ->
    let definition = get_blob db (Q.column_text s 0) |>
      R.Definition.of_string |> function Ok d -> d | Error e -> failwith e in
    {review;number;definition;actor=Q.column_text s 1;
      reason=Q.column_text s 2;created=Q.column_double s 3}) |> one
let revision t ~slug ~number = run t (fun db ->
  revision_db db ~slug ~number)
let revisions t ~slug = run t (fun db ->
  query db "SELECT number FROM revisions WHERE slug=? ORDER BY number DESC"
    [text slug] (fun s -> Q.column_int s 0)
  |> List.map (fun n -> revision_db db ~slug ~number:(Some n)))
let publish_run t (revision : revision) ~actor inputs =
  required "actor" actor;
  let keys = List.map (fun (i : input_result) -> i.key) inputs in
  let expected = List.map (fun (i : R.Definition.input) -> i.key)
    revision.definition.inputs in
  if List.sort String.compare keys <> List.sort String.compare expected then
    invalid_arg "Run inputs do not match the definition";
  run t (fun db -> transaction db (fun () ->
    let stored = revision_db db ~slug:revision.review.slug
      ~number:(Some revision.number) in
    if R.Definition.to_string stored.definition <>
       R.Definition.to_string revision.definition then
      invalid_arg "Revision content mismatch";
    let parsed = List.map (fun (input : R.Definition.input) ->
      let result = List.find (fun (i : input_result) -> i.key = input.key)
        inputs in
      if input.source.kind = R.Definition.List &&
         input.source.blob <> Some result.blob then
        invalid_arg "Fixed list does not match its revision";
      let format = if input.source.kind = R.Definition.Catalogue then
        R.Definition.Metadata_jsonl else input.source.format in
      if format <> result.format then invalid_arg "Input format mismatch";
      let parsed = R.parse format (get_blob db result.blob) in
      if result.occurrences <> List.length parsed.records ||
         result.diagnostics <> parsed.diagnostics ||
         (parsed.diagnostics <> [] && not result.partial) then
        invalid_arg "Run input accounting does not match retained evidence";
      input.key,parsed.records) stored.definition.inputs in
    let members = R.select stored.definition parsed in
    command db "INSERT INTO runs(slug,revision,actor,created,partial,members) \
      VALUES(?,?,?,?,?,?)" [text revision.review.slug;int revision.number;
      text actor;Q.Data.FLOAT (Unix.gettimeofday ());
      int (if List.exists (fun (i : input_result) -> i.partial) inputs
        then 1 else 0);int (List.length members)];
    let id = Int64.to_int (Q.last_insert_rowid db) in
    List.iter (fun (i : input_result) -> command db
      "INSERT INTO run_inputs VALUES(?,?,?,?)"
      [int id;text i.key;text i.blob;text (R.encode input_codec i)]) inputs;
    List.iter (fun (m : R.member) -> command db
      "INSERT INTO run_members VALUES(?,?,?)"
      [int id;text m.doi;text (R.encode R.member_jsont m)]) members;
    id))
let run_db db id =
  let inputs = query db
    "SELECT result FROM run_inputs WHERE run_id=? ORDER BY key" [int id]
    (fun s -> from_json input_codec (Q.column_text s 0)) in
  query db "SELECT slug,revision,actor,created,partial,members FROM runs \
    WHERE id=?" [int id] (fun s ->
    {id;slug=Q.column_text s 0;revision=Q.column_int s 1;
      actor=Q.column_text s 2;created=Q.column_double s 3;
      partial=Q.column_int s 4 <> 0;members=Q.column_int s 5;inputs}) |> one
let access = run
let run t id = access t (fun db -> run_db db id)
let runs t ~slug = access t (fun db ->
  query db "SELECT id FROM runs WHERE slug=? ORDER BY id DESC" [text slug]
    (fun s -> Q.column_int s 0) |> List.map (run_db db))
let members_db db id = query db
  "SELECT payload FROM run_members WHERE run_id=? ORDER BY doi" [int id]
  (fun s -> from_json R.member_jsont (Q.column_text s 0))
let members t id = access t (fun db -> ignore (run_db db id); members_db db id)
let snapshot_sql = "SELECT s.id,s.run_id,r.slug,r.revision,s.parent,s.label,\
  s.actor,s.created,s.title,s.requester,s.owner,s.members,s.titles,\
  s.abstracts,s.held,r.partial,s.manifest FROM snapshots s \
  JOIN runs r ON r.id=s.run_id"
let snapshot_row s =
  let parent = match Q.column s 4 with Q.Data.INT n -> Some (Int64.to_int n)
    | _ -> None in
  {id=Q.column_int s 0;run_id=Q.column_int s 1;slug=Q.column_text s 2;
    revision=Q.column_int s 3;parent;label=Q.column_text s 5;
    actor=Q.column_text s 6;created=Q.column_double s 7;
    title=Q.column_text s 8;requester=Q.column_text s 9;
    owner=Q.column_text s 10;
    members=Q.column_int s 11;titles=Q.column_int s 12;
    abstracts=Q.column_int s 13;held=Q.column_int s 14;
    partial=Q.column_int s 15 <> 0;manifest=Q.column_text s 16}
let snapshot_db db id = query db (snapshot_sql ^ " WHERE s.id=?")
  [int id] snapshot_row |> one
let snapshot t id = access t (fun db -> snapshot_db db id)
let snapshots t ~slug = access t (fun db -> query db
  (snapshot_sql ^ " WHERE r.slug=? ORDER BY s.id DESC")
  [text slug] snapshot_row)
let with_facet_writer db id f =
  stmt db "INSERT OR REPLACE INTO snapshot_facets VALUES(?,?,?,?)" (fun s ->
    f (fun (o : R.observation) ->
      let flags = List.fold_left (fun bits (field,present) ->
        if present then bits lor field_bit field else bits) 0
        (R.Statistics.values o) in
      check (Q.reset s);
      bind s [int id;text o.doi;text (R.Statistics.publisher o);int flags];
      done_ s));
  command db "INSERT OR IGNORE INTO snapshot_browse_ready VALUES(?)" [int id]
let max_budget = 4 * 1024 * 1024 * 1024
let default_chunk = 64 * 1024 * 1024
type descriptor = { sha256 : string; bytes : int; parts : int }
let descriptor_jsont =
  Jsont.Object.map ~kind:"Export parts" (fun sha256 bytes parts ->
    {sha256; bytes; parts})
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun d -> d.sha256)
  |> Jsont.Object.mem "bytes" Jsont.int ~enc:(fun d -> d.bytes)
  |> Jsont.Object.mem "parts" Jsont.int ~enc:(fun d -> d.parts)
  |> Jsont.Object.finish
(* An export at most [chunk_bytes] long is one object; a longer one is
   stored as parts behind a descriptor, and the exports row's blob is the
   descriptor. The manifest always carries the digest of the whole export. *)
let put_export db ~chunk_bytes id format body =
  let n = String.length body in
  if n <= chunk_bytes then begin
    let sha = put db body in
    command db "INSERT INTO exports VALUES(?,?,?)" [int id;text format;text sha]
  end else begin
    let parts = (n + chunk_bytes - 1) / chunk_bytes in
    let descriptor = put db (R.encode descriptor_jsont
      {sha256 = R.digest body; bytes = n; parts}) in
    command db "INSERT INTO exports VALUES(?,?,?)"
      [int id;text format;text descriptor];
    for part = 0 to parts - 1 do
      let start = part * chunk_bytes in
      let sha = put db (String.sub body start (min chunk_bytes (n - start))) in
      command db "INSERT INTO export_parts VALUES(?,?,?,?)"
        [int id;text format;int part;text sha]
    done
  end
let read_export db id format =
  let schema = query db "PRAGMA user_version" [] (fun s -> Q.column_int s 0)
    |> one in
  let body = query db "SELECT o.body FROM exports e JOIN objects o \
    ON o.sha=e.blob WHERE e.snapshot_id=? AND e.format=?"
    [int id;text format] (fun s -> Q.column_blob s 0) |> one in
  let parts = if schema < 3 then [] else query db
    "SELECT o.body FROM export_parts p JOIN objects o ON o.sha=p.blob \
     WHERE p.snapshot_id=? AND p.format=? ORDER BY p.part"
    [int id;text format] (fun s -> Q.column_blob s 0) in
  if parts = [] then body else begin
    let d = from_json descriptor_jsont body in
    let whole = String.concat "" parts in
    if List.length parts <> d.parts || String.length whole <> d.bytes ||
       R.digest whole <> d.sha256 then
      failwith "Cached export parts do not match their descriptor";
    whole
  end
let publish_snapshot ?(max_bytes=256 * 1024 * 1024)
    ?(chunk_bytes=default_chunk) t ~run_id ~parent ~label ~actor observations =
  if max_bytes < 1 || max_bytes > max_budget then
    invalid_arg "Snapshot byte budget must be 1..4 GiB";
  if chunk_bytes < 1 || chunk_bytes > 512 * 1024 * 1024 then
    invalid_arg "Export chunk size must be 1..512 MiB";
  required "label" label; required "actor" actor;
  let observations = List.sort (fun (a : R.observation) b ->
    String.compare a.doi b.doi) observations in
  let present f = List.fold_left (fun n (o : R.observation) ->
    if f o then n+1 else n) 0 observations in
  let titles = present (fun o ->
    (Taposaur.Bibliography.presence o.bibliography).title) in
  let abstracts = present (fun o ->
    (Taposaur.Bibliography.presence o.bibliography).abstract) in
  let held = present (fun o -> o.in_catalogue) in
  let exports = List.map (fun f -> f,R.export f observations)
    [R.Dois;R.Csv;R.Jsonl] in
  let encoded = List.map (fun o -> o.R.doi,
    R.encode R.observation_jsont o) observations in
  if List.fold_left (fun n (_,s) -> n + String.length s) 0 encoded >
     max_bytes then invalid_arg "Snapshot exceeds its byte budget";
  access t (fun db -> transaction db (fun () ->
    let run = run_db db run_id in
    let expected = members_db db run_id in
    if List.map (fun (m : R.member) -> m.doi) expected <>
       List.map (fun (o : R.observation) -> o.doi) observations then
      invalid_arg "Snapshot does not match run membership";
    Option.iter (fun id ->
      if (snapshot_db db id).run_id <> run_id then
        invalid_arg "Parent snapshot belongs to another run") parent;
    List.iter2 (fun (m : R.member) (o : R.observation) ->
      if m.origins <> o.origins || o.bibliography.doi <> Some o.doi then
        invalid_arg "Snapshot provenance does not match run membership")
      expected observations;
    let review = find_db db run.slug |> require in
    let at = Unix.gettimeofday () in
    let id = query db "SELECT coalesce(max(id),0)+1 FROM snapshots" []
      (fun s -> Q.column_int s 0) |> one in
    let statistics = R.Statistics.collect (fun f -> List.iter f observations) in
    let exports = ("statistics-v1",R.encode R.Statistics.jsont statistics)
      :: List.map (fun (f,s) -> R.export_name f,s) exports in
    let encoded = List.map (fun (doi,s) -> doi,put db s) encoded in
    let quote s = R.encode Jsont.string s in
    let definition = query db
      "SELECT definition_sha FROM revisions WHERE slug=? AND number=?"
      [text run.slug;int run.revision] (fun s -> Q.column_text s 0) |> one in
    let rows_hash = R.digest (String.concat "" (List.map (fun (doi,sha) ->
      doi ^ "\000" ^ sha ^ "\n") encoded)) in
    let definition_json = get_blob db definition in
    let object_fields = List.map (fun (name,body) ->
      quote name ^ ":" ^ quote (R.digest body))
      exports |> String.concat "," in
    let manifest = Printf.sprintf
      {|{"version":1,"snapshot":%d,"review":%s,"title":%s,
"requester":%s,"owner":%s,"revision":%d,"definition_sha256":%s,
"definition":%s,"run":%d,"parent":%s,"label":%s,"actor":%s,
"created":%.6f,"partial":%b,"members":%d,"titles":%d,"abstracts":%d,
"in_catalogue":%d,"metadata_policy":"catalogue-then-input/v1",
"export_version":1,"order":"doi","rows_sha256":%s,
"inputs":%s,"exports":{%s}}|}
      id (quote run.slug) (quote review.title) (quote review.requester)
      (quote review.owner) run.revision (quote definition)
      definition_json run_id
      (Option.fold ~none:"null" ~some:string_of_int parent)
      (quote label) (quote actor) at run.partial (List.length observations)
      titles abstracts held (quote rows_hash)
      (R.encode (Jsont.list input_codec) run.inputs) object_fields in
    let manifest = put db manifest in
    command db "INSERT INTO snapshots(id,run_id,parent,label,actor,created,\
      title,requester,owner,members,titles,abstracts,held,manifest) \
      VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      [int id;int run_id;opt_int parent;text label;text actor;Q.Data.FLOAT at;
       text review.title;text review.requester;text review.owner;
       int (List.length observations);int titles;int abstracts;int held;
       text manifest];
    List.iter (fun (doi,sha) -> command db
      "INSERT INTO snapshot_rows VALUES(?,?,?)" [int id;text doi;text sha])
      encoded;
    with_facet_writer db id (fun insert -> List.iter insert observations);
    List.iter (fun (format,body) -> put_export db ~chunk_bytes id format body)
      exports;
    id))
let rows ?(after="") ?(limit=50) t id =
  if limit < 1 || limit > 100 then invalid_arg "Page size must be 1..100";
  access t (fun db -> ignore (snapshot_db db id);
    query db "SELECT o.body FROM snapshot_rows s JOIN objects o \
      ON o.sha=s.observation WHERE s.snapshot_id=? AND s.doi>? \
      ORDER BY s.doi LIMIT ?" [int id;text after;int limit]
      (fun s -> from_json R.observation_jsont (Q.column_blob s 0)))
let export t id format = access t (fun db ->
  read_export db id (R.export_name format))
let statistics t id = access t (fun db ->
  ignore (snapshot_db db id);
  query db "SELECT o.body FROM exports e JOIN objects o ON o.sha=e.blob \
    WHERE e.snapshot_id=? AND e.format='statistics-v1'" [int id]
    (fun s -> from_json R.Statistics.jsont (Q.column_blob s 0))
  |> function [] -> None | xs -> Some (one xs))
let cache_statistics t id = access t (fun db -> transaction db (fun () ->
  ignore (snapshot_db db id);
  let statistics = ref None in
  with_facet_writer db id (fun insert ->
  statistics := Some (R.Statistics.collect (fun consume ->
    stmt db "SELECT o.body FROM snapshot_rows s JOIN objects o \
      ON o.sha=s.observation WHERE s.snapshot_id=? ORDER BY s.doi"
      (fun s -> bind s [int id];
        let rec loop () = match Q.step s with
          | Q.Rc.ROW ->
              let observation = from_json R.observation_jsont (Q.column_blob s 0) in
              insert observation; consume observation;
              loop ()
          | Q.Rc.DONE -> () | rc -> check rc in
        loop ()))));
  let sha = put db (R.encode R.Statistics.jsont (Option.get !statistics)) in
  command db "INSERT OR IGNORE INTO exports VALUES(?,'statistics-v1',?)"
    [int id;text sha]))
let browse ?(after="") ?(limit=50) ?publisher ?filter t id =
  if limit < 1 || limit > 100 then invalid_arg "Page size must be 1..100";
  access t (fun db ->
    ignore (snapshot_db db id);
    let schema = query db "PRAGMA user_version" [] (fun s -> Q.column_int s 0) |> one in
    if schema < 2 || query db
      "SELECT snapshot_id FROM snapshot_browse_ready WHERE snapshot_id=?"
      [int id] (fun s -> Q.column_int s 0) = [] then
      invalid_arg "Paper filters are not cached. Run review cache-statistics for this snapshot";
    let conditions,args = match filter with None -> "",[]
      | Some (field,present) ->
          let bit = field_bit field in
          Printf.sprintf " AND (f.flags & %d)=?" bit,
          [int (if present then bit else 0)] in
    let conditions,args = match publisher with None -> conditions,args
      | Some name -> conditions ^ " AND f.publisher=?",args @ [text name] in
    let index = match publisher,filter with
      | Some _,_ -> " INDEXED BY snapshot_publishers"
      | None,Some (field,_) -> Printf.sprintf " INDEXED BY snapshot_flag_%d" (field_bit field)
      | None,None -> "" in
    query db ("SELECT o.body FROM snapshot_facets f" ^ index ^ " \
      JOIN snapshot_rows s ON s.snapshot_id=f.snapshot_id AND s.doi=f.doi \
      JOIN objects o ON o.sha=s.observation \
      WHERE f.snapshot_id=? AND f.doi>?" ^ conditions ^
      " ORDER BY f.doi LIMIT ?") ([int id;text after] @ args @ [int limit])
      (fun s -> from_json R.observation_jsont (Q.column_blob s 0)))
type difference = {
  added : int; removed : int; changed : int; abstracts_added : int;
}
let compare t ~before ~after = access t (fun db ->
  let a = snapshot_db db before and b = snapshot_db db after in
  if a.slug <> b.slug then invalid_arg "Snapshots belong to different reviews";
  let values id = query db
    "SELECT s.doi,o.body FROM snapshot_rows s JOIN objects o \
     ON o.sha=s.observation WHERE s.snapshot_id=? ORDER BY s.doi"
    [int id] (fun s -> Q.column_text s 0,
      from_json R.observation_jsont (Q.column_blob s 1)) in
  let left = Hashtbl.create a.members and right = Hashtbl.create b.members in
  List.iter (fun (doi,o) -> Hashtbl.add left doi o) (values before);
  List.iter (fun (doi,o) -> Hashtbl.add right doi o) (values after);
  let added = ref 0 and removed = ref 0 and changed = ref 0 in
  let abstracts_added = ref 0 in
  Hashtbl.iter (fun doi (o : R.observation) ->
    match Hashtbl.find_opt left doi with
    | None -> incr added
    | Some old ->
        if old.bibliography <> o.bibliography || old.holdings <> o.holdings ||
           old.in_catalogue <> o.in_catalogue || old.stale <> o.stale ||
           old.error <> o.error || old.diagnostics <> o.diagnostics then
          incr changed;
        if not (Taposaur.Bibliography.presence old.bibliography).abstract &&
           (Taposaur.Bibliography.presence o.bibliography).abstract then
          incr abstracts_added) right;
  Hashtbl.iter (fun doi _ -> if not (Hashtbl.mem right doi) then incr removed)
    left;
  {added= !added;removed= !removed;changed= !changed;
   abstracts_added= !abstracts_added})

let audited t ?actor ?result_details ~action ~subject details f =
  Taposaur_audit.with_operation ?session:t.audit ?actor ?result_details ~action ~subject
    ~details:(("database",t.path)::details) (fun _ -> f ())
let put_blob t body = audited t ~action:"review.input.retain"
  ~subject:("blob:" ^ R.digest body) ["bytes",string_of_int (String.length body)]
  (fun () -> put_blob t body)
let define t ~slug ~title ~requester ~owner ~actor ~reason ~expected definition =
  audited t ~actor ~result_details:(fun n -> ["revision",string_of_int n])
    ~action:"review.definition.publish" ~subject:("review:" ^ slug)
    ["expected_revision",string_of_int expected]
    (fun () -> define t ~slug ~title ~requester ~owner ~actor ~reason ~expected definition)
let publish_run t (revision : revision) ~actor inputs =
  audited t ~actor ~result_details:(fun n -> ["run_id",string_of_int n])
    ~action:"review.run.publish" ~subject:("review:" ^ revision.review.slug)
    ["revision",string_of_int revision.number]
    (fun () -> publish_run t revision ~actor inputs)
let publish_snapshot ?max_bytes ?chunk_bytes t ~run_id ~parent ~label ~actor observations =
  audited t ~actor ~result_details:(fun n -> ["snapshot_id",string_of_int n])
    ~action:"review.snapshot.publish" ~subject:("run:" ^ string_of_int run_id)
    ["members",string_of_int (List.length observations)]
    (fun () -> publish_snapshot ?max_bytes ?chunk_bytes t ~run_id ~parent ~label ~actor observations)
let cache_statistics t id = audited t ~action:"review.statistics.cache"
  ~subject:("snapshot:" ^ string_of_int id) [] (fun () -> cache_statistics t id)
