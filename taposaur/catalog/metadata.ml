open Taposaur
open Db
module S = Sqlite3

type candidate = { row : Index.t; raw : string; generation : int64 }
let index candidate = candidate.row

let pending ?(force = false) ?uid ?(after = "") ~limit ~version t =
  if limit < 1 || limit > 1000 then invalid_arg "limit must be 1..1000";
  if version < 1 then invalid_arg "version must be positive";
  run t (fun db ->
    let clauses = "uid>?" ^
      (if force then "" else " AND metadata_version<>?") ^
      (if uid = None then "" else " AND uid=?") in
    let args = [text after] @
      (if force then [] else [int version]) @
      Option.fold ~none:[] ~some:(fun id -> [text (Uid.to_string id)]) uid @
      [int limit] in
    with_stmt db ("SELECT json,generation FROM papers WHERE " ^ clauses ^
      " ORDER BY uid LIMIT ?") (fun s ->
      bind s args;
      let rec loop acc = match S.step s with
        | S.Rc.ROW ->
            let raw = S.column_text s 0 in
            let row = match Index.of_string raw with
              | Ok e -> e
              | Error e -> failwith ("Invalid catalogue row: " ^ e) in
            loop ({row; raw; generation = S.column_int64 s 1} :: acc)
        | S.Rc.DONE -> List.rev acc | rc -> check rc; assert false in
      loop []))

let publish t candidate ~version result =
  if version < 1 then invalid_arg "version must be positive";
  let identity = Uid.to_string candidate.row.uid in
  let guard = [text identity; text candidate.raw;
    S.Data.INT candidate.generation] in
  run t (fun db -> transaction db (fun () ->
    let version = int version in
    let now = S.Data.FLOAT (Unix.gettimeofday ()) in
    (match result with
     | Error message -> with_stmt db
         "UPDATE papers SET metadata_version=?,metadata_error=?,metadata_at=? \
          WHERE uid=? AND json=? AND generation=?" (fun s ->
         bind s ([version; text message; now] @ guard); done_ s)
     | Ok (b : Bibliography.t) ->
         let encoded = match Bibliography.to_string b with
           | Ok s -> s | Error e -> failwith e in
         let author (a : Author.t) = String.trim
           (opt_text a.given ^ " " ^ a.family) in
         with_stmt db {|
UPDATE papers SET title=?,authors=?,journal=?,publisher=?,year=?,abstract=?,
 bibliography_json=?,resolved_json=NULL,metadata_version=?,
 metadata_error=NULL,metadata_at=? WHERE uid=? AND json=? AND generation=?|}
           (fun s ->
           bind s ([text (opt_text b.title);
             text (String.concat "; " (List.map author b.authors));
             text (opt_text b.journal); text (opt_text b.publisher);
             (match b.year with None -> S.Data.NULL | Some n -> int n);
             text (opt_text b.abstract); text encoded; version; now] @ guard);
           done_ s));
    S.changes db > 0))

let errors t = run t (fun db -> with_stmt db
  "SELECT uid,metadata_error FROM papers WHERE metadata_error IS NOT NULL \
   ORDER BY metadata_at DESC LIMIT 100" (fun s ->
    let rec loop acc = match S.step s with
      | S.Rc.ROW ->
          let uid = match Uid.of_string (S.column_text s 0) with
            | Ok uid -> uid | Error e -> failwith e in
          loop ((uid, S.column_text s 1) :: acc)
      | S.Rc.DONE -> List.rev acc | rc -> check rc; assert false in
    loop []))
