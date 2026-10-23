open Db
module S = Sqlite3
type t = {
  papers : int; pdf : int; xml : int; tei : int; s2orc : int;
  loaded_shards : int; discovered_shards : int option;
  errors : int; last_import : float option;
  titles : int; abstracts : int; publishers : int;
  enriched : int; enrichment_errors : int;
}
let get t = run t (fun db ->
  let titles, abstracts, publishers, enriched, enrichment_errors =
    with_stmt db {|
SELECT count(nullif(title,'')),count(nullif(abstract,'')),
 count(nullif(publisher,'')),
 coalesce(sum(metadata_version>0 AND metadata_error IS NULL),0),
 count(metadata_error) FROM papers|} (fun s ->
      ignore (S.step s);
      S.column_int s 0, S.column_int s 1, S.column_int s 2,
      S.column_int s 3, S.column_int s 4) in
  with_stmt db {|
SELECT coalesce(sum(rows),0),coalesce(sum(pdf),0),coalesce(sum(xml),0),
 coalesce(sum(tei),0),coalesce(sum(s2orc),0),count(*),max(synced),
 (SELECT value FROM settings WHERE key='discovered_shards'),
 (SELECT count(*) FROM import_errors) FROM shards|} (fun s ->
    ignore (S.step s);
    { papers = S.column_int s 0; pdf = S.column_int s 1;
      xml = S.column_int s 2; tei = S.column_int s 3; s2orc = S.column_int s 4;
      loaded_shards = S.column_int s 5;
      last_import = (match S.column s 6 with S.Data.NULL -> None
        | _ -> Some (S.column_double s 6));
      discovered_shards = (match S.column s 7 with S.Data.NULL -> None
        | _ -> Some (S.column_int s 7));
      errors = S.column_int s 8;
      titles; abstracts; publishers; enriched; enrichment_errors }))
