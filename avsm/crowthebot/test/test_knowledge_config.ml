open Crowthebot

let check name b = if not b then failwith name
let admin = "@admin:example.org"

let contains text needle =
  let rec search i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || search (i + 1))
  in
  search 0

let () =
  let root = Filename.temp_file "crow-knowledge-config" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let dir name =
    let path = Filename.concat root name in
    Unix.mkdir path 0o700;
    path
  in
  let config = dir "config" in
  let bushel_config = Filename.concat config "bushel" in
  Unix.mkdir bushel_config 0o700;
  let bushel = dir "bushel"
  and sortal = dir "sortal"
  and data = dir "data"
  and profile = dir "profile" in
  Unix.putenv "XDG_CONFIG_HOME" config;
  Unix.putenv "XDG_DATA_HOME" data;
  Unix.putenv "SORTAL_DATA_DIR" root;
  let config_file = Filename.concat bushel_config "config.toml" in
  let write text =
    Out_channel.with_open_bin config_file (fun oc -> output_string oc text)
  in
  let configure path =
    write (Printf.sprintf "[data]\nlocal_dir = %S\n" path)
  in
  let note directory marker =
    let notes = Filename.concat directory "notes" in
    Unix.mkdir notes 0o700;
    Out_channel.with_open_bin (Filename.concat notes "fixture.md") (fun oc ->
        Printf.fprintf oc
          "---\ntitle: Fixture\ndate: 2026-01-01\nslug: fixture\n---\n%s\n"
          marker)
  in
  note bushel "initialcorpusmarker";
  let knowledge_root = Filename.concat data "bushel/knowledge" in
  let rec indexes path =
    if not (Sys.file_exists path) then []
    else if Sys.is_directory path then
      Array.to_list (Sys.readdir path)
      |> List.concat_map (fun name -> indexes (Filename.concat path name))
    else if Filename.basename path = "knowledge.sqlite3" then [ path ]
    else []
  in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter
        (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () ->
      configure bushel;
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let fs = Eio.Stdenv.fs env in
      let store = Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin in
      Store.add_room store "!room:example.org";
      check "library config discovery"
        (Knowledge.sources ~fs = (bushel, sortal));
      let fetch =
        Fetch_mock.client (fun _ -> failwith "Unexpected embedding request")
      in
      let load store =
        Knowledge.load ~fs ~clock:(Eio.Stdenv.clock env) ~fetch ~store
          ~max_bytes:2048 ()
      in
      let knowledge = Option.get (load store) in
      let other_store =
        Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin
      in
      Store.add_room other_store "!room:example.org";
      let other = Option.get (load other_store) in
      check "availability does not open the shared index"
        (indexes knowledge_root = []);
      let invoke knowledge arguments =
        Knowledge.invoke knowledge ~actor:admin ~room:"!room:example.org"
          ~check:(fun () -> ())
          "knowledge_search" arguments
      in
      let get = function Ok s -> s | Error e -> failwith e in
      check "no extra settings file"
        (not (Sys.file_exists (Filename.concat profile "knowledge.json")));
      check "existing room authorization"
        (Knowledge.available knowledge ~actor:admin ~room:"!room:example.org");
      check "unapproved room rejected"
        (not
           (Knowledge.available knowledge ~actor:admin
              ~room:"!unknown:example.org"));
      Store.add_direct_room store ~room:"!dm:example.org" ~peer:admin;
      check "accepted admin DM"
        (Knowledge.available knowledge ~actor:admin ~room:"!dm:example.org");
      let friend = "@friend:example.org" in
      Store.set_person store ~actor:admin ~user:friend ~role:Friend
        ~allowed:true;
      check "existing allowed friend"
        (Knowledge.available knowledge ~actor:friend ~room:"!room:example.org");
      check "other person's DM rejected"
        (not
           (Knowledge.available knowledge ~actor:friend
              ~room:"!dm:example.org"));
      Store.set_person store ~actor:admin ~user:friend ~role:Friend
        ~allowed:false;
      check "revocation rechecked"
        (not
           (Knowledge.available knowledge ~actor:friend
              ~room:"!room:example.org"));
      let legacy = Filename.concat profile "knowledge.sqlite3" in
      Out_channel.with_open_bin legacy (fun oc -> output_string oc "legacy");
      let first =
        invoke knowledge {|{"query":"initialcorpusmarker"}|} |> get
      in
      let second = invoke other {|{"query":"initialcorpusmarker"}|} |> get in
      check "two Crow consumers read the same shared corpus"
        (first = second && contains first "initialcorpusmarker");
      check "two Crow consumers create one shared index"
        (List.length (indexes knowledge_root) = 1);
      check "legacy Crow index is left alone"
        (In_channel.with_open_bin legacy In_channel.input_all = "legacy");
      check "tools work without knowledge.json"
        (Result.is_ok
           (Knowledge.invoke knowledge ~actor:admin ~room:"!dm:example.org"
              ~check:(fun () -> ())
              "contacts_resolve" {|{"query":"Alex"}|}));
      let changed = dir "changed-bushel" in
      note changed "changedcorpusmarker";
      configure changed;
      let changed_result =
        invoke knowledge {|{"query":"changedcorpusmarker"}|} |> get
      in
      check "loaded consumer discovers source changes on invocation"
        (contains changed_result "changedcorpusmarker"
        && changed_result <> first);
      check "changed corpus gets its own shared index"
        (List.length (indexes knowledge_root) = 2);
      check "other consumer discovers the same new configuration"
        (invoke other {|{"query":"changedcorpusmarker"}|} = Ok changed_result);
      write "[data\ninvalid";
      check "authorization runs before shared configuration is read"
        (Knowledge.invoke knowledge ~actor:friend ~room:"!room:example.org"
           ~check:(fun () -> ())
           "contacts_resolve" {|{"query":"Alex"}|}
        = Error "Knowledge access denied for this actor and room");
      check "cancellation check runs before shared configuration is read"
        (Knowledge.invoke knowledge ~actor:admin ~room:"!room:example.org"
           ~check:(fun () -> failwith "cancel before opening store")
           "contacts_resolve" {|{"query":"Alex"}|}
        = Error "cancel before opening store");
      check "cancellation propagates before opening the shared store"
        (try
           ignore
             (Knowledge.invoke knowledge ~actor:admin
                ~room:"!room:example.org"
                ~check:(fun () -> raise (Eio.Cancel.Cancelled Exit))
                "contacts_resolve" {|{"query":"Alex"}|});
           false
         with Eio.Cancel.Cancelled Exit -> true);
      check "invocation reports changed malformed configuration"
        (Result.is_error (invoke knowledge {|{"query":"anything"}|}));
      configure (Filename.concat root "missing");
      check "source config reread"
        (fst (Knowledge.sources ~fs) = Filename.concat root "missing");
      check "missing installation disables tools" (load store = None);
      write "[data\ninvalid";
      check "malformed source config reported"
        (try
           ignore (Knowledge.sources ~fs);
           false
         with Failure _ -> true));
  print_endline "shared knowledge discovery and existing authorization passed"
