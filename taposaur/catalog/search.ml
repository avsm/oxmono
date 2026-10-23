open Taposaur
open Db
open Query
module S = Sqlite3
type page = { papers : Summary.t list; next : string option }
let decode s = match Index.of_string s with
  | Ok e -> e | Error e -> failwith ("Invalid catalogue row: " ^ e)

let summary s =
  let base = Summary.of_index (decode (S.column_text s 0)) in
  let bibliography = match S.column s 2 with
    | S.Data.TEXT json ->
        (match Bibliography.of_string json with Ok b -> b
         | Error e -> failwith ("Invalid bibliography: " ^ e))
    | _ -> match S.column s 1 with
      | S.Data.TEXT json -> Bibliography.of_index (decode json)
      | _ -> base.bibliography in
  { base with bibliography }

let identity s = Result.to_option (Uid.of_input s)

let fts text =
  String.split_on_char ' ' (String.map (function
    | '\n' | '\r' | '\t' -> ' ' | c -> c) text)
  |> List.filter ((<>) "")
  |> List.map (fun s -> "\"" ^
      String.concat "\"\"" (String.split_on_char '"' s) ^ "\"")
  |> String.concat " AND "

let search t (q : Query.t) =
  if q.limit < 1 || q.limit > 100 then invalid_arg "limit must be 1..100";
  if String.length q.text > 512 then invalid_arg "Search exceeds 512 bytes";
  let clauses = ref [] and args = ref [] in
  let add sql value = clauses := sql :: !clauses; args := value :: !args in
  let text_query = String.trim q.text in
  if text_query <> "" then
    (match identity text_query with
     | Some uid -> add "uid=?" (text (Uid.to_string uid))
     | None ->
         add "id IN (SELECT rowid FROM papers_fts WHERE papers_fts MATCH ?)"
           (text (fts text_query)));
  Option.iter (fun s -> add "scheme=?" (text (Uid.Scheme.to_string s)))
    q.scheme;
  if q.publisher <> "" then add "publisher=?" (text q.publisher);
  if q.journal <> "" then add "journal=?" (text q.journal);
  Option.iter (fun n -> add "year>=?" (int n)) q.year_from;
  Option.iter (fun n -> add "year<=?" (int n)) q.year_to;
  Option.iter (fun uid -> add "uid>?" (text uid)) q.after;
  let holding = match q.holdings with
    | Any -> "1" | Pdf -> "pdf=1" | Xml -> "xml=1" | Tei -> "tei=1"
    | S2orc -> "s2orc=1" | Unconverted -> "tei=0 AND (pdf=1 OR xml=1)" in
  List.iter (fun (field, availability) ->
    let column = match field with
      | Title -> "title" | Abstract -> "abstract" | Publisher -> "publisher" in
    clauses := (column ^ (match availability with
      | Present -> "<>''" | Missing -> "=''")) :: !clauses) q.metadata;
  let sql = "SELECT json,resolved_json,bibliography_json FROM papers WHERE " ^
    String.concat " AND " (holding :: List.rev !clauses) ^
    " ORDER BY uid LIMIT ?" in
  let rows = run t (fun db -> with_stmt db sql (fun s ->
    bind s (List.rev !args @ [int (q.limit + 1)]);
    let rec loop acc = match S.step s with
      | S.Rc.ROW -> loop (summary s :: acc)
      | S.Rc.DONE -> List.rev acc
      | rc -> check rc; assert false
    in loop [])) in
  let more = List.length rows > q.limit in
  let papers = List.filteri (fun i _ -> i < q.limit) rows in
  let next = if more then
    Option.map (fun e -> Uid.to_string e.Summary.uid)
      (List.nth_opt papers (q.limit - 1))
    else None in
  { papers; next }

let find_index t uid =
  run t (fun db -> with_stmt db "SELECT json FROM papers WHERE uid=?"
    (fun s -> bind s [text (Uid.to_string uid)];
      match S.step s with S.Rc.ROW -> Some (decode (S.column_text s 0))
      | S.Rc.DONE -> None | rc -> check rc; assert false))

let find t uid = run t (fun db -> with_stmt db
  "SELECT json,resolved_json,bibliography_json FROM papers WHERE uid=?"
  (fun s -> bind s [text (Uid.to_string uid)];
    match S.step s with S.Rc.ROW -> Some (summary s)
    | S.Rc.DONE -> None | rc -> check rc; assert false))
