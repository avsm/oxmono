(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let query ?(text = "") ?(tags = []) ?status ?source ?(include_deleted = false)
    store =
  let notes, errors = Store.scan store in
  let notes =
    List.filter
      (fun n ->
        (include_deleted || not (Doc.deleted n))
        && (match status with None -> true | Some s -> Doc.status n = s)
        && List.for_all
             (fun tag ->
               List.mem (normalize tag) (List.map normalize (Doc.tags n)))
             tags
        &&
        match source with
        | None -> true
        | Some s ->
            List.exists (fun l -> field "type" l = s) (items "links" n.meta))
      notes
  in
  if String.trim text = "" then (notes, errors)
  else
    (* A disposable in-memory FTS index means direct file edits are immediately
     searchable and read commands never need to write to the store. *)
    let db = Sqlite3.db_open ":memory:" in
    let check = function
      | Sqlite3.Rc.OK | DONE -> ()
      | _ -> fail "search index error"
    in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.db_close db))
      (fun () ->
        check
          (Sqlite3.exec db
             "CREATE VIRTUAL TABLE notes USING fts5(id UNINDEXED, content)");
        let insert =
          Sqlite3.prepare db "INSERT INTO notes(id,content) VALUES (?,?)"
        in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize insert))
          (fun () ->
            List.iter
              (fun n ->
                let subjects =
                  List.filter_map
                    (fun l ->
                      Option.bind (find "hints" l) (fun h ->
                          Option.map string (find "subject" h)))
                    (items "links" n.Doc.meta)
                in
                check (Sqlite3.bind_text insert 1 (Doc.id n));
                check
                  (Sqlite3.bind_text insert 2
                     (normalize
                        (String.concat "\n"
                           ((Doc.title n :: n.body :: Doc.tags n) @ subjects))));
                check (Sqlite3.step insert);
                check (Sqlite3.reset insert))
              notes);
        let q =
          String.split_on_char ' ' (normalize text)
          |> List.filter (fun s -> String.trim s <> "")
          |> List.map (fun s ->
              "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\"")
          |> String.concat " AND "
        in
        let stmt =
          Sqlite3.prepare db
            "SELECT id FROM notes WHERE notes MATCH ? ORDER BY rank"
        in
        let ids =
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              check (Sqlite3.bind_text stmt 1 q);
              let rec loop acc =
                match Sqlite3.step stmt with
                | Sqlite3.Rc.ROW -> loop (Sqlite3.column_text stmt 0 :: acc)
                | DONE -> List.rev acc
                | _ -> fail "invalid search query"
              in
              loop [])
        in
        ( List.filter_map
            (fun id -> List.find_opt (fun n -> Doc.id n = id) notes)
            ids,
          errors ))
