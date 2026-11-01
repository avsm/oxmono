open Taposaur
module Q = Sqlite3
module S = Taposaur_services
module E = Publisher_evidence
type read = Read
type write = Write
type 'a store = {db:Sqlite3_eio.t; lock:Eio.Mutex.t;
  audit:Taposaur_audit.session option}
type provider = Glutton of string | Datacite of string | Datacite_clickhouse of string
type client = {http:Fetch.plain; providers:provider list;
  clock:float Eio.Time.clock_ty Eio.Resource.t;timeout:float;
  audit:Taposaur_audit.session option}
let get = function Ok v -> v | Error e -> failwith e
let hash body = Digestif.SHA256.(to_hex (digest_string body))
let encode e = get (Jsont_bytesrw.encode_string E.jsont e)
let run t f =
  let result = Eio.Mutex.use_rw ~protect:false t.lock (fun () ->
    try Ok (Sqlite3_eio.run t.db f) with ex -> Error (ex,Printexc.get_raw_backtrace ())) in
  match result with Ok v -> v | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
let exec db sql = Q.Rc.check (Q.exec db sql)
let stmt db sql args f =
  let s = Q.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (Q.finalize s)) (fun () ->
    Q.Rc.check (Q.bind_values s args); f s)
let command db sql args = stmt db sql args (fun s -> Q.Rc.check (Q.step s))
let transaction db f =
  exec db "BEGIN IMMEDIATE";
  try let v=f () in exec db "COMMIT";v with ex ->
    let bt=Printexc.get_raw_backtrace () in
    (try exec db "ROLLBACK" with _ -> ());Printexc.raise_with_backtrace ex bt
let schema = {|
CREATE TABLE records(sha256 TEXT PRIMARY KEY,body BLOB NOT NULL);
CREATE TABLE attempts(id INTEGER PRIMARY KEY,doi TEXT NOT NULL,provider TEXT NOT NULL,
 endpoint TEXT NOT NULL,retrieved REAL NOT NULL,status INTEGER NOT NULL,
 decision TEXT NOT NULL,sha256 TEXT REFERENCES records(sha256));
CREATE INDEX attempts_doi ON attempts(doi,id);
CREATE TABLE publishers(doi TEXT PRIMARY KEY,evidence TEXT NOT NULL);
CREATE TABLE csv_candidates(doi TEXT PRIMARY KEY,publisher TEXT NOT NULL,
 conflict INTEGER NOT NULL,sha256 TEXT NOT NULL,path TEXT NOT NULL);
PRAGMA application_id=1414351186;
PRAGMA user_version=1;
|}
let open_db ?audit ~sw ~readonly path =
  if not readonly then Eio.Path.with_open_out ~follow:false
    ~create:(`If_missing 0o600) path (fun _ -> ());
  let db=Sqlite3_eio.open_path ~sw ~busy_timeout:5000
    ?mode:(if readonly then Some `READONLY else None) path in
  let t={db;lock=Eio.Mutex.create ();audit} in
  run t (fun db ->
    let scalar sql = stmt db sql [] (fun s ->
      if Q.step s <> Q.Rc.ROW then failwith "Missing metadata schema";
      Q.column_int s 0) in
    let check () =
      let v=scalar "PRAGMA user_version" and app=scalar "PRAGMA application_id" in
      if v=0 && app=0 && not readonly then begin
        if scalar "SELECT count(*) FROM sqlite_master WHERE name NOT LIKE 'sqlite_%'" <> 0
        then failwith "Refusing nonempty mirror evidence database";
        exec db schema
      end else if v<>1 || app<>1414351186 then failwith "Unsupported mirror evidence database" in
    if readonly then (check ();exec db "PRAGMA query_only=ON") else begin
      transaction db check;exec db "PRAGMA journal_mode=WAL";
      exec db "PRAGMA synchronous=FULL" end);
  t
let open_read ~sw path : read store = open_db ~sw ~readonly:true path
let open_write ?audit ~sw path : write store =
  Taposaur_audit.with_operation ?session:audit ~action:"mirrors.store.open"
    ~subject:(Eio.Path.native_exn path)
    (fun _ -> open_db ?audit ~sw ~readonly:false path)
let find t doi = run t (fun db -> stmt db
  "SELECT evidence FROM publishers WHERE doi=?" [Q.Data.TEXT (Doi.to_string doi)]
  (fun s -> match Q.step s with
    | Q.Rc.DONE -> None
    | Q.Rc.ROW -> Some (get (Jsont_bytesrw.decode_string E.jsont (Q.column_text s 0)))
    | rc -> Q.Rc.check rc;assert false))
let record t ~sha256 = run t (fun db -> stmt db
  "SELECT body FROM records WHERE sha256=?" [Q.Data.TEXT sha256]
  (fun s -> match Q.step s with Q.Rc.DONE -> None
    | Q.Rc.ROW -> Some (Q.column_blob s 0) | rc -> Q.Rc.check rc;assert false))
let decode ~provider ~endpoint ~doi body =
  match Jsont_bytesrw.decode_string S.Glutton.Record.jsont body with
  | Error _ -> Error "invalid_json"
  | Ok r ->
    let matched=Option.bind r.doi (fun s -> Result.to_option (Doi.of_string s)) in
    if matched <> Some doi then Error "doi_mismatch" else
    match r.publisher with
    | None -> Error "missing_publisher"
    | Some p when String.trim p = "" || String.length p > 4096 -> Error "missing_publisher"
    | Some publisher -> Ok {E.doi=Doi.to_string doi;publisher=String.trim publisher;
        provider;endpoint;sha256=hash body;retrieved=Unix.gettimeofday ()}
let identity = function Glutton endpoint -> "crossref.glutton",endpoint
  | Datacite endpoint -> "datacite.service",endpoint
  | Datacite_clickhouse endpoint -> "datacite.clickhouse",endpoint
let client ?audit ?(timeout=15.) ~clock ~providers http =
  if timeout<=0. || not (Float.is_finite timeout) then invalid_arg "Invalid mirror timeout";
  List.iter (fun p -> let _,endpoint=identity p in
    let uri=Uri.of_string endpoint in
    if not (List.mem (Uri.scheme uri) [Some "http";Some "https"]) ||
      Uri.host uri=None || Uri.userinfo uri<>None || Uri.query uri<>[] || Uri.fragment uri<>None
    then invalid_arg "Mirror endpoint must be HTTP(S), without credentials/query/fragment") providers;
  {http=Fetch.Middleware.of_handler (Fetch.Middleware.handler http);providers;clock=(clock :> float Eio.Time.clock_ty Eio.Resource.t);
   timeout;audit}
let retain ?existing_sha (t : write store) doi provider endpoint status decision body evidence =
  let sha=match existing_sha with Some s -> Some s | None -> Option.map hash body in
  Taposaur_audit.with_operation ?session:t.audit ~action:"mirrors.evidence.retain"
    ~subject:("doi:" ^ Doi.to_string doi)
    ~details:["provider",provider;"http_status",string_of_int status;
      "decision",decision;"sha256",Option.value ~default:"" sha]
    (fun _ -> run t (fun db -> transaction db (fun () ->
      Option.iter (fun body -> command db
        "INSERT OR IGNORE INTO records VALUES(?,?)" [TEXT (hash body);BLOB body]) body;
      command db "INSERT INTO attempts(doi,provider,endpoint,retrieved,status,decision,sha256) VALUES(?,?,?,?,?,?,?)"
        [TEXT (Doi.to_string doi);TEXT provider;TEXT endpoint;FLOAT (Unix.gettimeofday ());
         INT (Int64.of_int status);TEXT decision;
         (match sha with None -> NULL | Some sha -> TEXT sha)];
      Option.iter (fun e -> command db "INSERT OR IGNORE INTO publishers VALUES(?,?)"
        [TEXT e.E.doi;TEXT (encode e)]) evidence)))
let query t p doi =
  Eio.Time.with_timeout_exn t.clock t.timeout (fun () -> Eio.Switch.run (fun sw ->
    let response=match p with
      | Glutton server -> S.Glutton.by_doi ~sw (S.Glutton.client ~server t.http) doi
      | Datacite server -> S.Datacite.by_doi ~sw (S.Datacite.client ~server t.http) doi
      | Datacite_clickhouse endpoint ->
        let sql="SELECT doi AS DOI,publisher FROM datacite.dois WHERE doi IN ({doi:String},upper({doi:String})) ORDER BY updated DESC LIMIT 1 FORMAT JSONEachRow" in
        let url=endpoint ^ "?query=" ^ Uriz.pct_encode ~component:`Unreserved sql ^
          "&param_doi=" ^ Uriz.pct_encode ~component:`Unreserved (Doi.to_string doi) in
        Fetch.get ~sw t.http url in
    Eio.Switch.on_release sw (fun () -> Fetch.close response);
    let body=Eio.Buf_read.(take_all (of_flow ~max_size:(2*1024*1024) (Fetch.body response))) in
    Fetch.status response,body))
let lookup t store doi = match find store doi with
  | Some e -> Some e
  | None ->
    let rec next = function
      | [] -> None
      | p::rest ->
        let provider,endpoint=identity p in
        let result = Taposaur_audit.with_operation ?session:t.audit
          ~action:"mirrors.publisher.lookup" ~subject:("doi:" ^ Doi.to_string doi)
          ~details:["provider",provider;"endpoint",endpoint]
          ~success:Option.is_some
          (fun _ ->
            let response = try Ok (query t p doi) with
              | Eio.Time.Timeout -> Error "timeout"
              | Eio.Io _ -> Error "transport_error"
              | Eio.Buf_read.Buffer_limit_exceeded -> Error "oversized_response" in
            match response with
            | Error reason -> retain store doi provider endpoint 0 reason None None;None
            | Ok (status,body) ->
              let decoded = if status=404 || (status=200 && String.trim body="") then
                  Error "not_found" else if status<>200 then Error "http_error"
                else decode ~provider ~endpoint ~doi body in
              let evidence=Result.to_option decoded in
              let decision=match decoded with Ok _ -> "accepted" | Error e -> e in
              retain store doi provider endpoint status decision (Some body) evidence;
              if evidence=None then None else find store doi) in
        match result with Some _ -> result | None -> next rest in
    next t.providers

let csv_rows text =
  let field=Buffer.create 128 and row=ref [] and rows=ref [] in
  let quoted=ref false and closed=ref false in
  let finish_field () = row:=Buffer.contents field::!row;Buffer.clear field;closed:=false in
  let finish_row () = finish_field ();rows:=List.rev !row::!rows;row:=[] in
  let rec loop i = if i < String.length text then
    let c=text.[i] in
    if !quoted then
      if c='"' then
        if i+1<String.length text && text.[i+1]='"' then
          (Buffer.add_char field '"';loop (i+2))
        else (quoted:=false;closed:=true;loop (i+1))
      else (Buffer.add_char field c;loop (i+1))
    else match c with
      | ',' -> finish_field ();loop (i+1)
      | '\n' -> finish_row ();loop (i+1)
      | '\r' when i+1<String.length text && text.[i+1]='\n' -> finish_row ();loop (i+2)
      | '"' when Buffer.length field=0 && not !closed -> quoted:=true;loop (i+1)
      | _ when !closed || c='"' -> invalid_arg "Malformed CSV quoting"
      | _ -> Buffer.add_char field c;loop (i+1) in
  loop 0;
  if !quoted then invalid_arg "Unterminated CSV field";
  if Buffer.length field>0 || !closed || !row<>[] then finish_row ();
  List.rev !rows
let import_csv (store : write store) ~path body =
  if String.length body>32*1024*1024 then invalid_arg "CSV exceeds 32 MiB";
  let headers,rows=match csv_rows body with [] -> invalid_arg "Empty CSV" | h::r -> h,r in
  let column name =
    let rec loop i = function [] -> invalid_arg ("CSV needs " ^ name)
      | h::_ when String.trim h=name -> i | _::xs -> loop (i+1) xs in loop 0 headers in
  let doi_col=column "doi" and pub_col=column "publisher" in
  let sha=hash body in
  Taposaur_audit.with_operation ?session:store.audit ~action:"mirrors.csv.import"
    ~subject:path ~details:["sha256",sha;"rows",string_of_int (List.length rows)]
    (fun _ -> run store (fun db -> transaction db (fun () ->
      command db "INSERT OR IGNORE INTO records VALUES(?,?)" [TEXT sha;BLOB body];
      List.iter (fun row ->
        if List.length row<>List.length headers then invalid_arg "CSV row width mismatch";
        let publisher=String.trim (List.nth row pub_col) in
        match Doi.of_string (List.nth row doi_col) with
        | Error _ -> ()
        | Ok doi when publisher<>"" && publisher<>"\\N" ->
          command db {|INSERT INTO csv_candidates VALUES(?,?,0,?,?)
ON CONFLICT(doi) DO UPDATE SET conflict=CASE WHEN publisher<>excluded.publisher THEN 1 ELSE conflict END|}
            [TEXT (Doi.to_string doi);TEXT publisher;TEXT sha;TEXT path]
        | _ -> ()) rows)))
let fallback store doi = match find store doi with
  | Some e -> Some e
  | None ->
    let e=run store (fun db -> stmt db
      "SELECT publisher,sha256,path FROM csv_candidates WHERE doi=? AND conflict=0"
      [TEXT (Doi.to_string doi)] (fun s -> match Q.step s with
        | Q.Rc.DONE -> None
        | Q.Rc.ROW -> Some {E.doi=Doi.to_string doi;publisher=Q.column_text s 0;
            sha256=Q.column_text s 1;endpoint=Q.column_text s 2;
            provider="imported.csv";retrieved=Unix.gettimeofday ()}
        | rc -> Q.Rc.check rc;assert false)) in
    Option.iter (fun e ->
      retain ~existing_sha:e.E.sha256 store doi e.provider e.endpoint 200 "accepted_csv" None (Some e)) e;
    e

let default_path () = Option.value ~default:"taposaur-mirrors.db"
  (Sys.getenv_opt "TAPOSAUR_MIRRORS_DB")
let open_configured ~sw ~fs =
  let path=default_path () in
  if Sys.file_exists path then Some (open_read ~sw Eio.Path.(fs / path))
  else if Sys.getenv_opt "TAPOSAUR_MIRRORS_DB" <> None then
    failwith "Configured mirror evidence database does not exist"
  else None
