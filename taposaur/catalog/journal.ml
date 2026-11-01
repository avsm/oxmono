open Db
module S = Sqlite3
type t = { name : string; papers : int }

let suggest ?(limit = 12) t ~text:query =
  if limit < 1 || limit > 20 then invalid_arg "limit must be 1..20";
  if String.length query > 256 then invalid_arg "Journal query exceeds 256 bytes";
  let query = String.trim query in
  run t (fun db -> with_stmt db {|
SELECT name,papers FROM journals
 WHERE ?='' OR instr(lower(name),lower(?))>0
 ORDER BY CASE WHEN instr(lower(name),lower(?))=1 THEN 0 ELSE 1 END,
 papers DESC,name COLLATE NOCASE,name LIMIT ?|} (fun s ->
    bind s [text query; text query; text query; int limit];
    let rec loop acc = match S.step s with
      | S.Rc.ROW -> loop
          ({name = S.column_text s 0; papers = S.column_int s 1} :: acc)
      | S.Rc.DONE -> List.rev acc
      | rc -> check rc; assert false in
    loop []))
