open Db
module S = Sqlite3
type catalog = Db.read Db.t
type t = { db : S.db; mutable active : bool }
type row = {
  summary : Taposaur.Summary.t; stamp : string;
  stale : bool; error : string option; diagnostics : string list;
}
let check_active s = if not s.active then
  invalid_arg "Catalogue snapshot has expired"
let with_read catalog f = run catalog (fun db ->
  exec db "BEGIN";
  let snapshot = {db; active=true} in
  match (let result = f snapshot in
    snapshot.active <- false; exec db "COMMIT"; result) with
  | result -> result
  | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      snapshot.active <- false;
      (try exec db "ROLLBACK" with _ -> ());
      Printexc.raise_with_backtrace ex bt)
let search snapshot query =
  check_active snapshot; Search.search_db snapshot.db query
let find snapshot uid =
  check_active snapshot;
  match Search.find_db snapshot.db uid with
  | None -> None
  | Some summary -> with_stmt snapshot.db
      "SELECT generation,metadata_revision,metadata_version,metadata_stale,\
       metadata_error,metadata_diagnostics,coalesce(metadata_at,0),\
       coalesce(source_stamp,'') FROM papers WHERE uid=?" (fun s ->
      bind s [text (Taposaur.Uid.to_string uid)];
      (match S.step s with
       | S.Rc.ROW -> () | rc -> check rc; failwith "Catalogue row vanished");
      let stamp = Printf.sprintf "v1:%Ld:%Ld:%d:%.6f:%s"
        (S.column_int64 s 0) (S.column_int64 s 1) (S.column_int s 2)
        (S.column_double s 6) (S.column_text s 7) in
      let error = match S.column s 4 with
        | S.Data.TEXT s -> Some s | _ -> None in
      let diagnostics = match Jsont_bytesrw.decode_string
        (Jsont.list Jsont.string) (S.column_text s 5) with
        | Ok ds -> ds | Error e -> failwith e in
      Some {summary; stamp; stale=S.column_int s 3 <> 0;
        error; diagnostics})
