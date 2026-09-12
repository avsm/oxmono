(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let cell s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '|' -> Buffer.add_string b "&#124;"
      | '\r' | '\n' -> Buffer.add_char b ' '
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let markdown report =
  let counts = get "upload_counts" report and pull = get "pull" report in
  String.concat "\n"
    ([
       "# CardDAV dry run";
       "";
       "Account: "
       ^ cell (field "account" report)
       ^ ". Collection: "
       ^ cell (field "href" (get "book" report))
       ^ ".";
       "";
       "No contact writes were performed. Source files, server contacts and \
        existing sync journals are unchanged.";
       "";
       "| Upload decision | Contacts |";
       "| --- | ---: |";
     ]
    @ List.map
        (fun (key, label) ->
          Printf.sprintf "| %s | %d |" label (number (get key counts)))
        [
          ("create", "Would create");
          ("unchanged", "Unchanged");
          ("review", "Needs review");
        ]
    @ [
        "";
        "Existing name, email or account matches are held for review. Existing \
         contacts are never automatically merged or overwritten.";
        "";
        "## Pull preview";
        "";
        cell (field "message" pull);
        Printf.sprintf "Would update %d local contact(s)."
          (number (get "local_updates" pull));
        "";
      ]
    @ List.map
        (fun change ->
          "- "
          ^ cell (field "handle" change)
          ^ ": [YAML diff](pull/"
          ^ percent (field "uid" change)
          ^ "/changes.diff)")
        (items "changes" pull)
    @ [
        "";
        "## Upload decisions";
        "";
        "| Sortal ID | Action | Reason / candidates |";
        "| --- | --- | --- |";
      ]
    @ List.map
        (fun row ->
          let details =
            (match find "reason" row with None -> [] | Some s -> [ string s ])
            @ List.map
                (fun s -> "local: " ^ string s)
                (items "source_candidates" row)
            @ List.map string (items "candidates" row)
          in
          "| "
          ^ cell (field "handle" row)
          ^ " | "
          ^ cell (field "action" row)
          ^ " | "
          ^ cell (String.concat "; " details)
          ^ " |")
        (items "upload_plan" report)
    @ [
        "";
        "## Files";
        "";
        "- [Full plan](report.json)";
        "- [Server snapshot](server/report.json)";
        "- [Fresh export manifest](export/manifest.json)";
        "";
        "The fresh export includes current source edits and retains UIDs from \
         the supplied bundle. This preview cannot verify how a server or \
         editor would transform an actual write.";
        "";
      ])

let diff source before after =
  if before = after then ""
  else
    let lines s =
      let xs = String.split_on_char '\n' s in
      if String.ends_with ~suffix:"\n" s then
        List.filteri (fun i _ -> i < List.length xs - 1) xs
      else xs
    in
    let a = lines before and b = lines after in
    (* One full-document hunk, deliberately retaining all surrounding comments. *)
    "--- " ^ source ^ " (current)\n+++ " ^ source ^ " (proposed)\n"
    ^ Printf.sprintf "@@ -%d,%d +%d,%d @@\n"
        (if a = [] then 0 else 1)
        (List.length a)
        (if b = [] then 0 else 1)
        (List.length b)
    ^ String.concat "" (List.map (fun s -> "-" ^ s ^ "\n") a)
    ^ (if before <> "" && not (String.ends_with ~suffix:"\n" before) then
         "\\ No newline at end of file\n"
       else "")
    ^ String.concat "" (List.map (fun s -> "+" ^ s ^ "\n") b)
    ^
    if after <> "" && not (String.ends_with ~suffix:"\n" after) then
      "\\ No newline at end of file\n"
    else ""

let preview ?collection ?previous ?seed ~dav ~source ~bundle ~username ~output
    () =
  if not (Remote.is_readonly dav) then
    fail "sync preview requires a read-only DAV transport";
  let source = absolute source
  and bundle = absolute bundle
  and output = absolute output in
  fresh output;
  separate output
    ([
       source;
       Filename.concat bundle "originals";
       Filename.concat bundle "cards";
     ]
    @ Option.to_list previous);
  let prior =
    Option.map
      (fun p ->
        let journal = load_json (Filename.concat p "report.json") in
        if
          field "account" journal <> username
          || field "status" journal <> "applied"
        then
          fail
            "previous pull must be applied and belong to the selected account";
        journal)
      previous
  in
  ignore (Bundle.verify bundle);
  Unix.mkdir output 0o700;
  let fresh_bundle = Filename.concat output "export" in
  ignore (Bundle.export ~previous:bundle ~source ~output:fresh_bundle ());
  let snapshot = Filename.concat output "server" in
  let remote =
    Remote.inspect ?collection ~apply:false ~dav ~bundle:fresh_bundle ~username
      ~report:snapshot ()
  in
  let counts =
    obj
      (List.map
         (fun action ->
           ( action,
             int
               (List.length
                  (List.filter
                     (fun r -> field "action" r = action)
                     (items "plan" remote))) ))
         [ "create"; "unchanged"; "review" ])
  in
  let seed_dir = Pull.seed_directory ?seed bundle in
  let seed_report =
    if exists (Filename.concat seed_dir "report.json") then
      Some (load_json (Filename.concat seed_dir "report.json"))
    else None
  in
  let matching =
    match seed_report with
    | None -> false
    | Some r ->
        field "account" r = username
        && field "href" (get "book" r) = field "href" (get "book" remote)
  in
  Option.iter
    (fun prior ->
      if
        (not matching)
        || field "href" (get "book" prior) <> field "href" (get "book" remote)
      then fail "previous pull cannot be used for this account and collection")
    prior;
  let pull =
    if not matching then
      obj
        [
          ("status", str "unavailable");
          ("local_updates", int 0);
          ("changes", arr []);
          ( "message",
            str
              "No verified sync baseline for this account and collection. \
               Existing contacts are reviewed for matches; a common baseline \
               is required to preview pull merges." );
        ]
    else
      try
        let pull_dir = Filename.concat output "pull" in
        let journal =
          Pull.prepare ?previous ~seed:seed_dir ~bundle ~snapshot ~source
            ~output:pull_dir ()
        in
        List.iter
          (fun c ->
            let directory = safe_path pull_dir (field "uid" c) in
            write
              (Filename.concat directory "changes.diff")
              (diff (field "source" c)
                 (read (Filename.concat directory "before.yaml"))
                 (read (Filename.concat directory "after.yaml"))))
          (items "changes" journal);
        obj
          [
            ("status", str "planned");
            ( "message",
              str
                "Compared with the saved baseline; proposed YAML diffs are \
                 saved under pull/." );
            ( "local_updates",
              int
                (List.length
                   (List.filter
                      (fun c -> field "status" c = "prepared")
                      (items "changes" journal))) );
            ("changes", get "changes" journal);
          ]
      with Error e ->
        obj
          [
            ("status", str "conflict");
            ("message", str ("Pull requires review: " ^ e));
            ("local_updates", int 0);
            ("changes", arr []);
          ]
  in
  let report =
    obj
      [
        ("version", int 1);
        ("dry_run", `Bool true);
        ("account", str username);
        ("book", get "book" remote);
        ("source", str source);
        ("identity_bundle", str bundle);
        ( "previous_pull",
          match previous with Some p -> str (absolute p) | None -> `Null );
        ("existing_contacts", get "existing_contacts" remote);
        ("upload_counts", counts);
        ("upload_plan", get "plan" remote);
        ("pull", pull);
      ]
  in
  save_json (Filename.concat output "report.json") report;
  write (Filename.concat output "report.md") (markdown report);
  report
