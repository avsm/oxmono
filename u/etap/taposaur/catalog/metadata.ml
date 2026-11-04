open Taposaur
open Db
module S = Sqlite3

type candidate = {
  row_id : int64;
  row : Index.t; raw : string; generation : int64; revision : int64;
  source_stamp : string option;
}
let index candidate = candidate.row
let row_id candidate = candidate.row_id
let source_stamp candidate = candidate.source_stamp
let optional s column = match S.column s column with
  | S.Data.TEXT value -> Some value | _ -> None
let guard candidate = [text (Uid.to_string candidate.row.uid);
  text candidate.raw; S.Data.INT candidate.generation;
  S.Data.INT candidate.revision]
let condition = "uid=? AND json=? AND generation=? AND metadata_revision=?"

let pending ?(force = false) ?(retry_errors = false) ?(need_details = false)
    ?uid ?shard ?(after_row = 0L) ?(after = "")
    ~limit ~version t =
  if limit < 1 || limit > 1000 then invalid_arg "limit must be 1..1000";
  if version < 1 then invalid_arg "version must be positive";
  if after_row < 0L || (shard = None && after_row <> 0L) ||
     (shard <> None && after <> "") then
    invalid_arg "Row cursors require a shard and cannot use a UID cursor";
  run t (fun db ->
    let range, range_args, order = match shard with
      | None -> "uid>?", [text after], "uid"
      | Some shard -> "shard=? AND id>?",
          [text shard; S.Data.INT after_row], "id" in
    let clauses = range ^
      (if force then "" else " AND (metadata_version<>? OR (metadata_stale=1 AND metadata_error IS NULL)" ^
        (if need_details then
           " OR (detail_json IS NULL AND metadata_error IS NULL)" else "") ^
        (if retry_errors then
           " OR metadata_error IS NOT NULL OR metadata_diagnostics<>'[]'"
         else "") ^ ")") ^
      (if uid = None then "" else " AND uid=?") in
    let args = range_args @
      (if force then [] else [int version]) @
      Option.fold ~none:[] ~some:(fun id -> [text (Uid.to_string id)]) uid @
      [int limit] in
    with_stmt db ("SELECT json,generation,metadata_revision,source_stamp,id \
      FROM papers WHERE " ^ clauses ^ " ORDER BY " ^ order ^ " LIMIT ?") (fun s ->
      bind s args;
      let rec loop acc = match S.step s with
        | S.Rc.ROW ->
            let raw = S.column_text s 0 in
            let row = match Index.of_string raw with
              | Ok e -> e
              | Error e -> failwith ("Invalid catalogue row: " ^ e) in
            loop ({row; raw; row_id = S.column_int64 s 4;
              generation = S.column_int64 s 1;
              revision = S.column_int64 s 2;
              source_stamp = optional s 3} :: acc)
        | S.Rc.DONE -> List.rev acc | rc -> check rc; assert false in
      loop []))

let invalidate t candidate = run t (fun db ->
  with_stmt db ("UPDATE papers SET metadata_stale=1,metadata_version=0, \
    metadata_revision=metadata_revision+1 WHERE " ^ condition) (fun s ->
    bind s (guard candidate); done_ s; S.changes db > 0))

let diagnostics_codec = Jsont.list Jsont.string
let publish ?source_stamp ?detail_cache ?(diagnostics = []) ?text:body_text
    t candidate ~version result =
  if version < 1 then invalid_arg "version must be positive";
  let diagnostics = match
    Jsont_bytesrw.encode_string diagnostics_codec diagnostics with
    | Ok s -> s | Error e -> failwith e in
  run t (fun db -> transaction db (fun () ->
    let version = int version in
    let now = S.Data.FLOAT (Unix.gettimeofday ()) in
    let stamp = match source_stamp with
      | None -> S.Data.NULL | Some s -> text s in
    (match result with
     | Error message -> with_stmt db
         ("UPDATE papers SET metadata_version=?,metadata_error=?,metadata_at=?, \
          metadata_revision=metadata_revision+1 WHERE " ^ condition) (fun s ->
         bind s ([version; text message; now] @ guard candidate); done_ s)
     | Ok (b : Bibliography.t) ->
         let encoded = match Bibliography.to_string b with
           | Ok s -> s | Error e -> failwith e in
         let author (a : Author.t) = String.trim
           (opt_text a.given ^ " " ^ a.family) in
         with_stmt db ({|
UPDATE papers SET title=?,authors=?,journal=?,publisher=?,year=?,abstract=?,
 bibliography_json=?,resolved_json=NULL,detail_json=?,detail_at=?,metadata_version=?,
 metadata_error=NULL,metadata_at=?,source_stamp=?,metadata_diagnostics=?,
 text=?,
 metadata_stale=0,metadata_revision=metadata_revision+1 WHERE |} ^ condition)
           (fun s ->
           bind s ([text (opt_text b.title);
             text (String.concat "; " (List.map author b.authors));
             text (opt_text b.journal); text (opt_text b.publisher);
             (match b.year with None -> S.Data.NULL | Some n -> int n);
             text (opt_text b.abstract); text encoded;
             Option.fold ~none:S.Data.NULL ~some:text detail_cache;
             (if detail_cache = None then S.Data.NULL else now); version; now;
             stamp; text diagnostics;
             (match body_text with None -> S.Data.NULL
              | Some t -> int (if t then 1 else 0))] @ guard candidate);
           done_ s));
    S.changes db > 0))

type status = {
  source_stamp : string option; stale : bool; diagnostics : string list;
  error : string option;
}
let status t uid = run t (fun db -> with_stmt db
  "SELECT source_stamp,metadata_stale,metadata_diagnostics,metadata_error \
   FROM papers WHERE uid=?" (fun s ->
    bind s [text (Uid.to_string uid)];
    match S.step s with
    | S.Rc.DONE -> None
    | S.Rc.ROW ->
        let diagnostics = match Jsont_bytesrw.decode_string diagnostics_codec
          (S.column_text s 2) with Ok xs -> xs | Error e -> failwith e in
        Some {source_stamp = optional s 0; stale = S.column_int s 1 <> 0;
          diagnostics; error = optional s 3}
    | rc -> check rc; assert false))

let errors t = run t (fun db -> with_stmt db
  "SELECT uid,coalesce(metadata_error,metadata_diagnostics) FROM papers \
   WHERE metadata_error IS NOT NULL OR metadata_diagnostics<>'[]' \
   ORDER BY metadata_at DESC LIMIT 100" (fun s ->
    let rec loop acc = match S.step s with
      | S.Rc.ROW ->
          let uid = match Uid.of_string (S.column_text s 0) with
            | Ok uid -> uid | Error e -> failwith e in
          loop ((uid, S.column_text s 1) :: acc)
      | S.Rc.DONE -> List.rev acc | rc -> check rc; assert false in
    loop []))

type detail = {
  index : Index.t; json : string; saved_at : float;
  stale : bool; error : string option; diagnostics : string list;
}

let detail t uid = run t (fun db -> with_stmt db
  "SELECT json,detail_json,detail_at,metadata_stale,metadata_error,\
   metadata_diagnostics FROM papers WHERE uid=? AND detail_json IS NOT NULL"
  (fun s ->
    bind s [text (Uid.to_string uid)];
    match S.step s with
    | S.Rc.DONE -> None
    | S.Rc.ROW ->
        let index = match Index.of_string (S.column_text s 0) with
          | Ok row -> row | Error e -> failwith e in
        let diagnostics = match Jsont_bytesrw.decode_string diagnostics_codec
          (S.column_text s 5) with Ok xs -> xs | Error e -> failwith e in
        Some {index; json = S.column_text s 1; saved_at = S.column_double s 2;
          stale = S.column_int s 3 <> 0; error = optional s 4; diagnostics}
    | rc -> check rc; assert false))

let detail_count t = run t (fun db -> with_stmt db
  "SELECT papers FROM detail_counts WHERE singleton=1" (fun s ->
    ignore (S.step s); S.column_int s 0))

let fill_abstract t ~uid ~abstract ~update_detail =
  if String.trim abstract = "" then invalid_arg "Empty abstract";
  run t (fun db -> transaction db (fun () ->
    match Search.find_db db uid with
    | None -> false
    | Some summary when Option.fold ~none:false
        ~some:(fun a -> String.trim a <> "") summary.bibliography.abstract -> false
    | Some summary ->
      let bibliography = {summary.bibliography with abstract=Some abstract} in
      let encoded = match Bibliography.to_string bibliography with
        | Ok s -> s | Error e -> failwith e in
      let detail = with_stmt db "SELECT json,detail_json FROM papers WHERE uid=?"
        (fun s -> bind s [text (Uid.to_string uid)];
          if S.step s <> S.Rc.ROW then failwith "Missing paper during abstract fill";
          match optional s 1 with None -> None | Some raw ->
            let index = match Index.of_string (S.column_text s 0) with
              | Ok i -> i | Error e -> failwith e in
            Some (update_detail index raw)) in
      with_stmt db "UPDATE papers SET abstract=?,bibliography_json=?,detail_json=?,metadata_revision=metadata_revision+1 WHERE uid=? AND trim(abstract)=''"
        (fun s -> bind s [text abstract;text encoded;
          Option.fold ~none:S.Data.NULL ~some:text detail;text (Uid.to_string uid)];
          done_ s; S.changes db > 0)))

let fill_publisher t ~uid ~publisher ~update_detail =
  if String.trim publisher = "" then invalid_arg "Empty publisher";
  run t (fun db -> transaction db (fun () ->
    match Search.find_db db uid with
    | None -> false
    | Some summary when Option.fold ~none:false
        ~some:(fun p -> String.trim p <> "") summary.bibliography.publisher -> false
    | Some summary ->
      let bibliography = {summary.bibliography with publisher=Some publisher} in
      let encoded = match Bibliography.to_string bibliography with
        | Ok s -> s | Error e -> failwith e in
      let detail = with_stmt db "SELECT json,detail_json FROM papers WHERE uid=?"
        (fun s -> bind s [text (Uid.to_string uid)];
          if S.step s <> S.Rc.ROW then failwith "Missing paper during publisher fill";
          match optional s 1 with None -> None | Some raw ->
            let index = match Index.of_string (S.column_text s 0) with
              | Ok i -> i | Error e -> failwith e in
            Some (update_detail index raw)) in
      with_stmt db "UPDATE papers SET publisher=?,bibliography_json=?,detail_json=?,metadata_revision=metadata_revision+1 WHERE uid=? AND trim(publisher)=''"
        (fun s -> bind s [text publisher;text encoded;
          Option.fold ~none:S.Data.NULL ~some:text detail;text (Uid.to_string uid)];
          done_ s; S.changes db > 0)))
