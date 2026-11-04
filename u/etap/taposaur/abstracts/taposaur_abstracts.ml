open Taposaur
module Q = Sqlite3
module E = Abstract_evidence
type read = Read
type write = Write
type 'a store = {db:Sqlite3_eio.t; lock:Eio.Mutex.t;
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
let application_id = 1414351187
let schema = {|
CREATE TABLE records(sha256 TEXT PRIMARY KEY,body BLOB NOT NULL);
CREATE TABLE attempts(id INTEGER PRIMARY KEY,doi TEXT NOT NULL,provider TEXT NOT NULL,
 endpoint TEXT NOT NULL,retrieved REAL NOT NULL,status INTEGER NOT NULL,
 decision TEXT NOT NULL,sha256 TEXT REFERENCES records(sha256));
CREATE INDEX attempts_doi ON attempts(doi,id);
CREATE TABLE abstracts(doi TEXT PRIMARY KEY,evidence TEXT NOT NULL);
PRAGMA application_id=1414351187;
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
        then failwith "Refusing nonempty abstract evidence database";
        exec db schema
      end else if v<>1 || app<>application_id then
        failwith "Unsupported abstract evidence database" in
    if readonly then (check ();exec db "PRAGMA query_only=ON") else begin
      transaction db check;exec db "PRAGMA journal_mode=WAL";
      exec db "PRAGMA synchronous=FULL" end);
  t
let open_read ~sw path : read store = open_db ~sw ~readonly:true path
let open_write ?audit ~sw path : write store =
  Taposaur_audit.with_operation ?session:audit ~action:"abstracts.store.open"
    ~subject:(Eio.Path.native_exn path)
    (fun _ -> open_db ?audit ~sw ~readonly:false path)
let find t doi = run t (fun db -> stmt db
  "SELECT evidence FROM abstracts WHERE doi=?" [Q.Data.TEXT (Doi.to_string doi)]
  (fun s -> match Q.step s with
    | Q.Rc.DONE -> None
    | Q.Rc.ROW -> Some (get (Jsont_bytesrw.decode_string E.jsont (Q.column_text s 0)))
    | rc -> Q.Rc.check rc;assert false))
let attempted t doi = run t (fun db -> stmt db
  "SELECT 1 FROM attempts WHERE doi=? LIMIT 1" [Q.Data.TEXT (Doi.to_string doi)]
  (fun s -> match Q.step s with
    | Q.Rc.DONE -> false | Q.Rc.ROW -> true | rc -> Q.Rc.check rc;assert false))
let record t ~sha256 = run t (fun db -> stmt db
  "SELECT body FROM records WHERE sha256=?" [Q.Data.TEXT sha256]
  (fun s -> match Q.step s with Q.Rc.DONE -> None
    | Q.Rc.ROW -> Some (Q.column_blob s 0) | rc -> Q.Rc.check rc;assert false))
let count t = run t (fun db -> stmt db "SELECT count(*) FROM abstracts" []
  (fun s -> ignore (Q.step s); Q.column_int s 0))
let iter t f = run t (fun db -> stmt db "SELECT evidence FROM abstracts ORDER BY doi" []
  (fun s -> let rec loop () = match Q.step s with
    | Q.Rc.ROW -> f (get (Jsont_bytesrw.decode_string E.jsont (Q.column_text s 0))); loop ()
    | Q.Rc.DONE -> () | rc -> Q.Rc.check rc in loop ()))

(* OpenAlex works carry abstracts as an inverted index: each word maps to
   its positions. Positions missing from the index are skipped. *)
let max_abstract = 65536
let reconstruct index =
  let last = Jsont.String_map.fold (fun _ positions acc ->
    List.fold_left max acc positions) index (-1) in
  if last < 0 || last > max_abstract then None else begin
    let words = Array.make (last + 1) "" in
    Jsont.String_map.fold (fun word positions () ->
      List.iter (fun i -> if i >= 0 then words.(i) <- word) positions) index ();
    let text = String.concat " " (List.filter ((<>) "") (Array.to_list words)) in
    let text = String.trim text in
    if text = "" || String.length text > max_abstract then None else Some text
  end

type work = {id:string option; doi:string option;
  inverted:int list Jsont.String_map.t option}
let work_jsont =
  Jsont.Object.map (fun id doi inverted -> {id;doi;inverted})
  |> Jsont.Object.mem "id" (Jsont.option Jsont.string) ~dec_absent:(fun () -> None)
       ~enc:(fun w -> w.id)
  |> Jsont.Object.mem "doi" (Jsont.option Jsont.string) ~dec_absent:(fun () -> None)
       ~enc:(fun w -> w.doi)
  |> Jsont.Object.mem "abstract_inverted_index"
       (Jsont.option (Jsont.Object.as_string_map (Jsont.list Jsont.int)))
       ~dec_absent:(fun () -> None) ~enc:(fun w -> w.inverted)
  |> Jsont.Object.finish
let results_jsont =
  Jsont.Object.map (fun results -> results)
  |> Jsont.Object.mem "results" (Jsont.list work_jsont) ~enc:Fun.id
  |> Jsont.Object.finish
let work_doi w = Option.bind w.doi (fun s ->
  let s = String.trim s in
  let prefix = "https://doi.org/" in
  let s = if String.starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
    else s in
  Result.to_option (Doi.of_string s))
let evidence_of_work ~provider ~endpoint ~sha256 ~retrieved doi w =
  match Option.bind w.inverted reconstruct with
  | None -> Error "missing_abstract"
  | Some abstract -> Ok {E.doi=Doi.to_string doi;abstract;provider;endpoint;
      sha256;retrieved}
let decode ~provider ~endpoint ~doi body =
  match Jsont_bytesrw.decode_string work_jsont body with
  | Error _ -> Error "invalid_json"
  | Ok w ->
    if work_doi w <> Some doi then Error "doi_mismatch" else
    evidence_of_work ~provider ~endpoint ~sha256:(hash body)
      ~retrieved:(Unix.gettimeofday ()) doi w
let decode_results body =
  match Jsont_bytesrw.decode_string results_jsont body with
  | Error _ -> Error "invalid_json"
  | Ok works -> Ok (List.filter_map (fun w ->
      Option.map (fun doi -> doi, w) (work_doi w)) works)

type client = {http:Fetch.plain; endpoint:string; api_key:string option;
  clock:float Eio.Time.clock_ty Eio.Resource.t; timeout:float;
  audit:Taposaur_audit.session option}
let provider = "openalex"
let client ?audit ?(timeout=30.) ?api_key ?(endpoint="https://api.openalex.org")
    ~clock http =
  if timeout<=0. || not (Float.is_finite timeout) then invalid_arg "Invalid abstract timeout";
  let uri=Uri.of_string endpoint in
  if not (List.mem (Uri.scheme uri) [Some "http";Some "https"]) ||
    Uri.host uri=None || Uri.userinfo uri<>None || Uri.query uri<>[] || Uri.fragment uri<>None
  then invalid_arg "Abstract endpoint must be HTTP(S), without credentials/query/fragment";
  Option.iter (fun k -> if String.trim k = "" then invalid_arg "Empty API key") api_key;
  {http=Fetch.Middleware.of_handler (Fetch.Middleware.handler http);endpoint;api_key;
   clock=(clock :> float Eio.Time.clock_ty Eio.Resource.t);timeout;audit}
let batch_size = 100
let pct = Uriz.pct_encode ~component:`Unreserved
(* OpenAlex refuses request URLs over 8190 bytes, and a percent-encoded
   DOI with angle brackets and parentheses can run to 120 bytes. *)
let max_filter_bytes = 7000
let filter_value dois = pct (String.concat "|" (List.map Doi.to_string dois))
let batches dois =
  let flush batch acc = if batch = [] then acc else List.rev batch :: acc in
  let rec go batch n bytes acc = function
    | [] -> List.rev (flush batch acc)
    | d :: rest ->
        let size = String.length (pct (Doi.to_string d)) + 3 in
        if batch <> [] && (n >= batch_size || bytes + size > max_filter_bytes)
        then go [d] 1 size (flush batch acc) rest
        else go (d :: batch) (n + 1) (bytes + size) acc rest in
  go [] 0 0 [] dois
let key_query t = match t.api_key with None -> "" | Some k -> "&api_key=" ^ pct k
let fetch t url =
  Eio.Time.with_timeout_exn t.clock t.timeout (fun () -> Eio.Switch.run (fun sw ->
    let response = Fetch.get ~sw t.http url in
    Eio.Switch.on_release sw (fun () -> Fetch.close response);
    let body=Eio.Buf_read.(take_all (of_flow ~max_size:(8*1024*1024) (Fetch.body response))) in
    Fetch.status response,body))
let retain (t : write store) ~body ~retrieved ~status decisions =
  let sha = Option.map hash body in
  run t (fun db -> transaction db (fun () ->
    Option.iter (fun body -> command db
      "INSERT OR IGNORE INTO records VALUES(?,?)" [TEXT (hash body);BLOB body]) body;
    List.iter (fun (doi, decision, evidence) ->
      command db "INSERT INTO attempts(doi,provider,endpoint,retrieved,status,decision,sha256) VALUES(?,?,?,?,?,?,?)"
        [TEXT (Doi.to_string doi);TEXT provider;TEXT "";FLOAT retrieved;INT (Int64.of_int status);
         TEXT decision;Option.fold ~none:Q.Data.NULL ~some:(fun s -> Q.Data.TEXT s) sha];
      Option.iter (fun e -> command db
        "INSERT OR REPLACE INTO abstracts VALUES(?,?)" [TEXT (Doi.to_string doi);TEXT (encode e)])
        evidence) decisions))
type refusal = Rate_limited | Budget | Http of int | Transport of string
let refusal_name = function Rate_limited -> "rate_limited" | Budget -> "budget_exhausted"
  | Http s -> "http_" ^ string_of_int s | Transport e -> e
let lookup_many t store dois =
  let dois = List.filter (fun d -> find store d = None) dois in
  if dois = [] then Ok [] else
  if List.length dois > batch_size then invalid_arg "At most 100 DOIs per lookup" else
  let filter = filter_value dois in
  if String.length filter > max_filter_bytes + 200 then
    invalid_arg "DOI filter exceeds the request URL limit" else
  let url = t.endpoint ^ "/works?filter=doi:" ^ filter ^
    "&per-page=" ^ string_of_int batch_size ^
    "&select=id,doi,abstract_inverted_index" ^ key_query t in
  Taposaur_audit.with_operation ?session:t.audit ~action:"abstracts.lookup"
    ~subject:(provider ^ " batch") ~details:["provider",provider;"endpoint",t.endpoint;
      "dois",string_of_int (List.length dois);"first",Doi.to_string (List.hd dois)]
    ~success:Result.is_ok
    (fun _ ->
      let retrieved = Unix.gettimeofday () in
      let response = try Ok (fetch t url) with
        | Eio.Time.Timeout -> Error (Transport "timeout")
        | Eio.Io _ -> Error (Transport "transport_error")
        | Eio.Buf_read.Buffer_limit_exceeded -> Error (Transport "oversized_response") in
      match response with
      | Error r ->
        retain store ~body:None ~retrieved ~status:0
          (List.map (fun d -> d, refusal_name r, None) dois); Error r
      | Ok (429, _) -> Error Rate_limited
      | Ok ((402 | 403), _) -> Error Budget
      | Ok (status, body) when status <> 200 ->
        (* A refused multi-DOI batch records nothing, so the caller can
           retry its members singly and attribute a bad DOI to itself. *)
        if List.length dois = 1 then
          retain store ~body:(Some body) ~retrieved ~status
            (List.map (fun d -> d, "http_error", None) dois);
        Error (Http status)
      | Ok (status, body) ->
        match decode_results body with
        | Error e ->
          retain store ~body:(Some body) ~retrieved ~status
            (List.map (fun d -> d, e, None) dois); Error (Transport e)
        | Ok works ->
          let sha256 = hash body in
          let decisions = List.map (fun doi ->
            match List.find_opt (fun (d, _) -> Doi.equal d doi) works with
            | None -> doi, "not_found", None
            | Some (_, w) ->
              match evidence_of_work ~provider ~endpoint:t.endpoint ~sha256 ~retrieved doi w with
              | Ok e -> doi, "accepted", Some e
              | Error reason -> doi, reason, None) dois in
          retain store ~body:(Some body) ~retrieved ~status decisions;
          Ok (List.filter_map (fun (d, _, e) -> Option.map (fun e -> d, e) e) decisions))
(* Single-work reads are free of the credit budget, so they carry the
   remainder once the day's list queries are spent. *)
let lookup t store doi =
  match find store doi with
  | Some e -> Ok (Some e)
  | None ->
  let url = t.endpoint ^ "/works/https://doi.org/" ^ pct (Doi.to_string doi) ^
    "?select=id,doi,abstract_inverted_index" ^
    (match t.api_key with None -> "" | Some k -> "&api_key=" ^ pct k) in
  Taposaur_audit.with_operation ?session:t.audit ~action:"abstracts.lookup"
    ~subject:("doi:" ^ Doi.to_string doi) ~details:["provider",provider;"endpoint",t.endpoint]
    ~success:Result.is_ok
    (fun _ ->
      let retrieved = Unix.gettimeofday () in
      let response = try Ok (fetch t url) with
        | Eio.Time.Timeout -> Error (Transport "timeout")
        | Eio.Io _ -> Error (Transport "transport_error")
        | Eio.Buf_read.Buffer_limit_exceeded -> Error (Transport "oversized_response") in
      match response with
      | Error r -> retain store ~body:None ~retrieved ~status:0
          [doi, refusal_name r, None]; Error r
      | Ok (429, _) -> Error Rate_limited
      | Ok ((402 | 403), _) -> Error Budget
      | Ok (404, body) ->
        retain store ~body:(Some body) ~retrieved ~status:404 [doi, "not_found", None];
        Ok None
      | Ok (status, body) when status <> 200 ->
        retain store ~body:(Some body) ~retrieved ~status [doi, "http_error", None];
        Error (Http status)
      | Ok (status, body) ->
        let decoded = decode ~provider ~endpoint:t.endpoint ~doi body in
        let decision = match decoded with Ok _ -> "accepted" | Error e -> e in
        retain store ~body:(Some body) ~retrieved ~status
          [doi, decision, Result.to_option decoded];
        Ok (Result.to_option decoded))

let default_path () = Option.value ~default:"taposaur-abstracts.db"
  (Sys.getenv_opt "TAPOSAUR_ABSTRACTS_DB")
let open_configured ~sw ~fs =
  let path=default_path () in
  if Sys.file_exists path then Some (open_read ~sw Eio.Path.(fs / path))
  else if Sys.getenv_opt "TAPOSAUR_ABSTRACTS_DB" <> None then
    failwith "Configured abstract evidence database does not exist"
  else None
