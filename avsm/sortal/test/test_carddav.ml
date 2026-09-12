(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Sortal_carddav
open Common

let check condition message = if not condition then failwith message
let eq a b = check (equal a b) "values differ"

let contains s needle =
  let n = String.length needle in
  let rec loop i =
    i + n <= String.length s && (String.sub s i n = needle || loop (i + 1))
  in
  loop 0

let rejects f =
  match f () with
  | _ -> failwith "expected rejection"
  | exception
      ( Common.Error _ | Invalid_argument _ | Sys_error _ | Unix.Unix_error _
      | Yamlrw.Yamlrw_error _ ) ->
      ()

let photo =
  Base64.decode_exn
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aXioAAAAASUVORK5CYII="

let raw =
  {|# Original comments stay in the recovery archive.
version: 2
kind: person
handle: casey
names: ["Casey Example; Jr.", "C. Example"]
emails: ["test@example.invalid", "other@example.invalid"]
photo: avatar.png
accounts:
  atproto:
    handle: example.invalid
    did: did:plc:example
    apps: [bluesky, tangled]
  github: [example, alternate]
affiliations:
  - {org: Past, until: "2020"}
  - {org: Current, from: "2020", address: "Room 2, Example St"}
  - {org: Future, from: "2099"}
feeds: [{type: atom, url: "https://example.invalid/feed", paused: true}]
links: [{url: "https://example.invalid/", label: "雪, semi; backslash \\ é雪"}]
|}

type fixture = { root : string; source : string; bundle : string; c : value }

let fixture f =
  let root = Filename.temp_file "sortal-carddav-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let source = Filename.concat root "source" in
      Unix.mkdir source 0o700;
      write (Filename.concat source "casey.yaml") raw;
      write (Filename.concat source "avatar.png") photo;
      mkdir (Filename.concat source "feeds");
      write (Filename.concat source "feeds/annotations.json") "{\"read\":true}";
      mkdir (Filename.concat source ".git");
      write (Filename.concat source ".git/HEAD") "ref: refs/heads/main\n";
      write (Filename.concat source "unused.png") "extra asset";
      f { root; source; bundle = Filename.concat root "bundle"; c = yaml raw })

let encode ?version f c =
  fst
    (Mapping.encode ?version ~uid:"uid" ~store_id:"store" ~originals:f.source c)

let roundtrip ?version f c =
  let data = encode ?version f c in
  let decoded, photos = Mapping.decode data in
  eq decoded c;
  List.iter
    (fun (p, bytes) ->
      check (read (safe_path f.source p) = bytes) "photo differs")
    photos;
  data

let replace name fn props =
  List.map
    (fun (p : Mapping.property) -> if p.name = name then fn p else p)
    props

let put_source f c =
  write (Filename.concat f.source "casey.yaml") (Yamlrw.to_string c)

let export f = Bundle.export ~source:f.source ~output:f.bundle ()
let entry m = List.hd (items "contacts" m)
let card f m = read (safe_path f.bundle (field "card" (entry m)))

let book =
  obj
    [
      ("href", str "https://contacts.invalid/dav/Default/");
      ("name", str "Personal");
      ("max_resource_size", `Null);
      ("supported_address_data", `Null);
    ]

let remote data =
  let props = Mapping.parse data in
  {
    Remote.uid = Mapping.untext (Mapping.only props "UID").value;
    href = "https://contacts.invalid/dav/Default/one.vcf";
    etag = Some "\"e1\"";
    data;
    props;
  }

let mock_dav ?(root = "https://contacts.invalid/") ?(readonly = true) f =
  Remote.make ~root ~fetch:(Fetch_mock.client f) ~username:"test"
    ~password:"secret" ~readonly

let answer ?(status = 200) ?(headers = []) data req =
  Fetch_mock.respond ~status ~headers:(Http.Header.of_list headers) data req

let no_network _ = failwith "unexpected network call"

let edited data =
  let props = Mapping.parse data in
  let props =
    List.map
      (fun (p : Mapping.property) ->
        if List.mem p.name [ "FN"; "X-ADDRESSBOOKSERVER-KIND" ] then
          {
            p with
            params = List.remove_assoc "X-SORTAL-PATH" p.params;
            value =
              (if p.name = "X-ADDRESSBOOKSERVER-KIND" then
                 String.uppercase_ascii p.value
               else p.value);
          }
        else if List.mem p.name [ "PHOTO"; "URL" ] then
          {
            p with
            group = "";
            params = p.params @ [ ("PROP-ID", "id-" ^ p.name) ];
          }
        else p)
      props
  in
  let props =
    List.filter
      (fun (p : Mapping.property) -> p.name <> "END" && p.name <> "EMAIL")
      props
  in
  Mapping.render
    (props
    @ List.map Mapping.property
        [
          "EMAIL;PREF=1;TYPE=WORK,PREF;PROP-ID=email-id:casey@example.invalid";
          "NICKNAME;PROP-ID=nick:Caz";
          "REV:20260911T070000Z";
          "END:VCARD";
        ])

let baseline_fixture f k =
  let m = export f in
  let e = entry m in
  let old = card f m in
  let newer = edited old in
  let seed = Filename.concat f.bundle "seed" in
  mkdir (Filename.concat seed "after");
  write (Filename.concat seed ("after/" ^ field "uid" e ^ ".vcf")) old;
  save_json
    (Filename.concat seed "report.json")
    (obj
       [
         ("account", str "test");
         ("book", book);
         ( "results",
           arr
             [
               obj
                 [
                   ("uid", get "uid" e);
                   ("href", str (remote old).href);
                   ("sha256", str (digest old));
                   ("status", str "verified");
                 ];
             ] );
       ]);
  let snapshot = Filename.concat f.root "snapshot" in
  mkdir (Filename.concat snapshot "before");
  write (Filename.concat snapshot "before/one.vcf") newer;
  save_json
    (Filename.concat snapshot "report.json")
    (obj [ ("account", str "test"); ("book", book) ]);
  k m e old newer snapshot

let prepare f snapshot =
  let output = Filename.concat f.root "pull" in
  let r = Pull.prepare ~bundle:f.bundle ~snapshot ~source:f.source ~output () in
  (output, r)

let snapshot_tree root =
  List.map
    (fun p -> (p, digest (read (Filename.concat root p))))
    (inventory root)

let test_export =
  [
    ( "v3 complete field and byte recovery",
      fun f ->
        let data = roundtrip f f.c in
        check (not (contains data "X-SORTAL-META")) "opaque metadata";
        let lines = String.split_on_char '\n' data in
        List.iter
          (fun s -> check (String.length s <= 76) "overlong folded line")
          lines;
        let m = export f in
        ignore (Bundle.verify ~source:f.source f.bundle);
        check (List.length (assoc (get "files" m)) = 5) "archive inventory";
        check
          (read (Filename.concat f.bundle "originals/casey.yaml") = raw)
          "original bytes";
        check ((Unix.stat f.bundle).Unix.st_perm = 0o700) "private bundle" );
    ( "v4 complete field and byte recovery",
      fun f -> ignore (roundtrip ~version:"4.0" f f.c) );
    ( "archive preserves file permissions and timestamps",
      fun f ->
        let path = Filename.concat f.source ".git/HEAD" in
        Unix.chmod path 0o770;
        Unix.utimes path 1_700_000_000. 1_700_000_001.;
        ignore (export f);
        let archived =
          Unix.stat (Filename.concat f.bundle "originals/.git/HEAD")
        in
        check
          (archived.st_perm = 0o770 && archived.st_mtime = 1_700_000_001.)
          "archive lost file metadata" );
    ( "stable IDs and explicit rename",
      fun f ->
        let a = export f in
        let before = field "uid" (entry a) in
        let source = set "handle" (str "renamed") f.c in
        put_source f source;
        let b =
          Bundle.export ~source:f.source
            ~output:(Filename.concat f.root "second")
            ~previous:f.bundle ~renames:[ "casey=renamed" ] ()
        in
        check (field "uid" (entry b) = before) "changed UID" );
    ( "UUIDv5 matches the standard namespace algorithm",
      fun _ ->
        check
          (contact_uuid "6ba7b810-9dad-11d1-80b4-00c04fd430c8" "www.widgets.com"
          = "21f7f8de-8051-5b89-8680-0195ef798b6a")
          "uuid5" );
    ( "duplicate handle removes incomplete output",
      fun f ->
        write (Filename.concat f.source "duplicate.yaml") raw;
        rejects (fun () -> export f);
        check (not (exists f.bundle)) "incomplete export retained" );
    ( "duplicate YAML key",
      fun f ->
        write (Filename.concat f.source "casey.yaml") (raw ^ "version: 2\n");
        rejects (fun () -> export f) );
    ( "missing photo",
      fun f ->
        Unix.unlink (Filename.concat f.source "avatar.png");
        rejects (fun () -> export f) );
    ( "original URL whitespace retained",
      fun f ->
        let c = set "links" (arr [ str "https://example.invalid/\n" ]) f.c in
        let data = roundtrip f c in
        check (contains data "X-SORTAL-ORIGINAL-URL") "missing original URL" );
    ( "existing output never overwritten",
      fun f ->
        ignore (export f);
        let before = snapshot_tree f.bundle in
        rejects (fun () -> export f);
        check (before = snapshot_tree f.bundle) "output changed" );
    ( "snapshot corruption detected",
      fun f ->
        ignore (export f);
        write (Filename.concat f.bundle "originals/casey.yaml") "bad";
        rejects (fun () -> Bundle.verify f.bundle) );
    ( "stripped annotation detected independently of checksums",
      fun f ->
        let data = encode f f.c in
        let props =
          Mapping.parse data
          |> replace "X-SORTAL-ID" (fun p -> { p with params = [] })
        in
        let decoded, _ = Mapping.decode (Mapping.render props) in
        check (not (equal decoded f.c)) "lost identity ignored" );
    ( "unknown top level field rejected",
      fun f -> rejects (fun () -> encode f (set "unknown" (str "keep") f.c)) );
    ( "unknown account rejected",
      fun f ->
        rejects (fun () ->
            encode f (set "accounts" (obj [ ("unknown", str "x") ]) f.c)) );
    ( "reordered properties retain list positions",
      fun f ->
        let props = Mapping.parse (encode f f.c) in
        let head =
          List.filter
            (fun (p : Mapping.property) ->
              List.mem p.name [ "BEGIN"; "VERSION" ])
            props
        in
        let body =
          List.filter
            (fun (p : Mapping.property) ->
              not (List.mem p.name [ "BEGIN"; "VERSION"; "END" ]))
            props
        in
        let decoded, _ =
          Mapping.decode
            (Mapping.render
               (head @ List.rev body @ [ Mapping.property "END:VCARD" ]))
        in
        eq decoded f.c );
    ( "visible field edits win",
      fun f ->
        let props =
          Mapping.parse (encode f f.c)
          |> replace "FN" (fun p -> { p with value = "Changed" })
        in
        let c, _ = Mapping.decode (Mapping.render props) in
        check (List.hd (items "names" c) = str "Changed") "name not read" );
    ( "empty collections and explicit false retained",
      fun f ->
        let c =
          f.c
          |> set "accounts" (obj [])
          |> set "links" (arr [])
          |> set "vcard" (obj [])
          |> set "emails" (arr [])
          |> set "feeds"
               (arr
                  [
                    obj
                      [
                        ("type", str "manual");
                        ("url", str "https://example.invalid/");
                        ("paused", `Bool false);
                      ];
                  ])
        in
        ignore (roundtrip f c) );
    ( "complex free text uses grouped properties",
      fun f ->
        let c =
          f.c
          |> set "accounts" (obj [ ("github", str "quote\"line\n^") ])
          |> set "feeds"
               (arr
                  [
                    obj
                      [
                        ("type", str "rss");
                        ("url", str "https://example.invalid/rss");
                        ("hint", str "line\n\"quote^");
                      ];
                  ])
        in
        ignore (roundtrip f c) );
    ( "partial dates and structured department",
      fun f ->
        let c =
          set "affiliations"
            (arr
               [
                 obj
                   [
                     ("org", str "A;B");
                     ("department", str "C;D");
                     ("from", str "2020-05");
                     ("until", str "2021-01-03");
                   ];
               ])
            f.c
        in
        ignore (roundtrip f c) );
    ( "conflicting social URL rejected",
      fun f ->
        let props =
          Mapping.parse (encode f f.c)
          |> replace "X-SOCIALPROFILE" (fun p ->
              { p with value = "https://other.invalid/" })
        in
        rejects (fun () -> Mapping.decode (Mapping.render props)) );
    ( "feeds remain visible ordinary URLs",
      fun f ->
        List.iter
          (fun version ->
            let data = encode ~version f f.c in
            let props = Mapping.parse data in
            check
              (not
                 (List.exists
                    (fun (p : Mapping.property) -> p.name = "X-FEED")
                    props))
              "X-FEED emitted";
            check
              (List.exists
                 (fun (p : Mapping.property) ->
                   p.name = "URL"
                   && Mapping.param p "X-FEED-TYPE" = Some "atom"
                   && Mapping.param p "MEDIATYPE" <> None = (version = "4.0"))
                 props)
              "feed mapping")
          [ "3.0"; "4.0" ] );
    ( "feed URL edit changes subscription",
      fun f ->
        let props =
          Mapping.parse (encode f f.c)
          |> replace "URL" (fun p ->
              if Mapping.param p "X-FEED-TYPE" <> None then
                { p with value = "https://example.invalid/new" }
              else p)
        in
        let c, _ = Mapping.decode (Mapping.render props) in
        check
          (field "url" (List.hd (items "feeds" c))
          = "https://example.invalid/new")
          "stale feed" );
    ( "derived social link edit rejected",
      fun f ->
        let props =
          Mapping.parse (encode f f.c)
          |> replace "URL" (fun p ->
              if Mapping.param p "X-SORTAL-DERIVED" = Some "profile" then
                { p with value = "https://other.invalid" }
              else p)
        in
        rejects (fun () -> Mapping.decode (Mapping.render props)) );
    ( "ATProto app mirror conflict rejected",
      fun f ->
        let props =
          Mapping.parse (encode f f.c)
          |> replace "URL" (fun p ->
              if contains p.value "bsky.app" then
                { p with value = "https://other.invalid" }
              else p)
        in
        rejects (fun () -> Mapping.decode (Mapping.render props)) );
    ( "bare ATProto has visible fallback",
      fun f ->
        let c =
          set "accounts" (obj [ ("atproto", str "example.invalid") ]) f.c
        in
        let data = roundtrip f c in
        check
          (List.exists
             (fun (p : Mapping.property) ->
               p.name = "URL"
               && p.value = "https://bsky.app/profile/example.invalid")
             (Mapping.parse data))
          "missing fallback" );
    ( "photo path escape rejected",
      fun f ->
        rejects (fun () -> encode f (set "photo" (str "../outside.png") f.c)) );
    ( "symlink archive rejected",
      fun f ->
        Unix.symlink "avatar.png" (Filename.concat f.source "link.png");
        rejects (fun () -> export f) );
    ( "source nested output rejected",
      fun f ->
        rejects (fun () ->
            Bundle.export ~source:f.source
              ~output:(Filename.concat f.source "output")
              ()) );
    ( "invalid passthrough shape rejected",
      fun f ->
        rejects (fun () ->
            encode f (set "vcard" (arr [ arr [ str "TEL"; str "123" ] ]) f.c))
    );
  ]

let test_pull =
  [
    ( "Fastmail edit retains other fields",
      fun f ->
        let old = encode f f.c in
        let merged = Pull.reconcile f.c f.c old (edited old) "uid" "store" in
        List.iter
          (fun k -> eq (get k merged) (get k f.c))
          [
            "names";
            "kind";
            "photo";
            "feeds";
            "accounts";
            "affiliations";
            "links";
          ];
        eq (get "emails" merged) (arr [ str "casey@example.invalid" ]) );
    ( "pulled email exports once with parameters",
      fun f ->
        let old = encode f f.c in
        let merged = Pull.reconcile f.c f.c old (edited old) "uid" "store" in
        List.iter
          (fun version ->
            let data = roundtrip ~version f merged in
            check
              (List.length
                 (List.filter
                    (fun (p : Mapping.property) -> p.name = "EMAIL")
                    (Mapping.parse data))
              = 1)
              "duplicate email")
          [ "3.0"; "4.0" ] );
    ( "repeat merge is idempotent",
      fun f ->
        let old = encode f f.c in
        let newer = edited old in
        let merged = Pull.reconcile f.c f.c old newer "uid" "store" in
        eq merged (Pull.reconcile f.c merged old newer "uid" "store") );
    ( "concurrent email conflict",
      fun f ->
        let old = encode f f.c in
        rejects (fun () ->
            Pull.reconcile f.c
              (set "emails" (arr [ str "local@example.invalid" ]) f.c)
              old (edited old) "uid" "store") );
    ( "unrelated local edits retained",
      fun f ->
        let old = encode f f.c in
        let local = set "links" (arr [ str "https://local.invalid/" ]) f.c in
        let merged = Pull.reconcile f.c local old (edited old) "uid" "store" in
        eq (get "links" merged) (get "links" local) );
    ( "remote photo changes conflict",
      fun f ->
        let old = encode f f.c in
        let newer =
          edited old |> Mapping.parse
          |> replace "PHOTO" (fun p ->
              { p with value = Base64.encode_exn "changed" })
          |> Mapping.render
        in
        rejects (fun () -> Pull.reconcile f.c f.c old newer "uid" "store") );
    ( "remote identity conflict",
      fun f ->
        let old = encode f f.c in
        let newer =
          edited old |> Mapping.parse
          |> replace "X-SORTAL-ID" (fun p -> { p with value = "other" })
          |> Mapping.render
        in
        rejects (fun () -> Pull.reconcile f.c f.c old newer "uid" "store") );
    ( "YAML comments quotes and unknown fields",
      fun _ ->
        let raw =
          "# context\n\
           version: 2\n\
           kind: person\n\
           handle: \"casey\" # stable\n\
           names:\n\
          \  - Casey Example # full name\n\
           custom: keep\n"
        in
        let value =
          set "emails" (arr [ str "casey@example.invalid" ]) (yaml raw)
        in
        let after = Yaml_edit.update raw value in
        eq (yaml after) value;
        List.iter
          (fun s -> check (contains after s) "lost source bytes")
          [
            "# context";
            "handle: \"casey\" # stable";
            "Casey Example # full name";
          ] );
    ( "YAML changed collection values keep comments",
      fun _ ->
        let raw =
          "emails:\n\
          \  - \"old@example.invalid\" # preferred\n\
          \  - keep@example.invalid # retain this\n\
           vcard:\n\
          \  NOTE: \"Keep\" # context\n\
          \  NICKNAME: Old # short name\n"
        in
        let c = yaml raw in
        let c =
          c
          |> set "emails"
               (arr [ str "new@example.invalid"; str "keep@example.invalid" ])
          |> set "vcard" (set "NICKNAME" (str "New") (get "vcard" c))
        in
        let after = Yaml_edit.update raw c in
        eq (yaml after) c;
        List.iter
          (fun s -> check (contains after s) "lost comment")
          [ "# preferred"; "# retain this"; "# context"; "# short name" ] );
    ( "YAML sequence insertion removal and empty collections",
      fun _ ->
        List.iter
          (fun raw ->
            List.iter
              (fun values ->
                let c = set "emails" (arr (List.map str values)) (yaml raw) in
                let after = Yaml_edit.update raw c in
                eq (yaml after) c;
                check (contains after "# keep comment") "lost comment")
              [ []; [ "new" ]; [ "new"; "keep" ]; [ "new"; "keep"; "added" ] ])
          [
            "emails:\n  - old # keep comment\n  - keep\nkind: person\n";
            "kind: person\nemails:\n  - old # keep comment\n  - keep\n";
            "emails: [old, keep] # keep comment\nkind: person\n";
          ] );
    ( "YAML mapping addition and removal preserve neighboring fields",
      fun _ ->
        let raw =
          "vcard:\n\
          \  NOTE: Keep # keep comment\n\
          \  NICKNAME: Old # name comment\n\
           kind: person\n"
        in
        let c = yaml raw in
        List.iter
          (fun fields ->
            let c = set "vcard" (obj fields) c in
            let after = Yaml_edit.update raw c in
            eq (yaml after) c;
            check
              (contains after "# keep comment"
              && contains after "# name comment")
              "lost map comment")
          [
            [ ("NICKNAME", str "New") ];
            [
              ("NOTE", str "Keep"); ("NICKNAME", str "New"); ("TEL", str "123");
            ];
          ] );
    ( "YAML literal scalar and CRLF updates",
      fun _ ->
        let raw =
          "# context\r\n\
           vcard:\r\n\
          \  NOTE: |\r\n\
          \    old text\r\n\
          \    second line\r\n\
          \  NICKNAME: Old # name\r\n\
           kind: person\r\n"
        in
        let c = yaml raw in
        let c = set "vcard" (set "NOTE" (str "New\\ntext") (get "vcard" c)) c in
        let after = Yaml_edit.update raw c in
        eq (yaml after) c;
        check
          (contains after "# context\r\n" && contains after "Old # name\r\n")
          "changed line endings" );
    ( "passthrough grouped phones and labels",
      fun f ->
        let c =
          set "vcard"
            (obj
               [
                 ("itemA.TEL;TYPE=work", str "+123");
                 ("itemA.TEL;TYPE=cell", str "+456");
                 ("itemA.X-ABLabel", str "Office");
               ])
            f.c
        in
        List.iter
          (fun version -> ignore (roundtrip ~version f c))
          [ "3.0"; "4.0" ] );
    ( "passthrough quoted URI header",
      fun f ->
        ignore
          (roundtrip f
             (set "vcard"
                (obj
                   [
                     ( "NOTE;X-SERVICE=\"https://example.invalid/~path\"",
                       str "Line one\\nLine two" );
                   ])
                f.c)) );
    ( "passthrough malformed delimiter rejected",
      fun f ->
        rejects (fun () ->
            encode f
              (set "vcard" (obj [ ("NOTE:unintended", str "actual") ]) f.c)) );
    ( "passthrough reserved identity rejected",
      fun f ->
        rejects (fun () ->
            encode f (set "vcard" (obj [ ("UID", str "override") ]) f.c)) );
    ( "stale email overlay rejected",
      fun f ->
        rejects (fun () ->
            encode f
              (set "vcard"
                 (obj [ ("EMAIL;TYPE=WORK", str "absent@example.invalid") ])
                 f.c)) );
    ( "journal cannot be written inside source",
      fun f ->
        rejects (fun () ->
            Pull.prepare ~bundle:f.bundle
              ~snapshot:(Filename.concat f.root "snapshot")
              ~source:f.source
              ~output:(Filename.concat f.source "journal")
              ()) );
    ( "remote changed since prepare prevents write",
      fun f ->
        baseline_fixture f (fun _ _ _ newer snapshot ->
            let output, _ = prepare f snapshot in
            let before = snapshot_tree f.source in
            let changed =
              newer |> Mapping.parse
              |> replace "FN" (fun p -> { p with value = "Changed again" })
              |> Mapping.render
            in
            let dav =
              mock_dav (answer ~headers:[ ("etag", "\"e2\"") ] changed)
            in
            rejects (fun () ->
                Pull.apply ~dav ~dry_run:false ~username:"test" output);
            check (snapshot_tree f.source = before) "source changed") );
    ( "local changed since prepare prevents write",
      fun f ->
        baseline_fixture f (fun _ _ _ _ snapshot ->
            let output, _ = prepare f snapshot in
            write (Filename.concat f.source "casey.yaml") (raw ^ "# user edit\n");
            rejects (fun () ->
                Pull.apply ~dav:(mock_dav no_network) ~dry_run:false
                  ~username:"test" output)) );
    ( "apply replay and common baseline",
      fun f ->
        baseline_fixture f (fun _ _ _ newer snapshot ->
            let output, _ = prepare f snapshot in
            let dav = mock_dav (answer ~headers:[ ("etag", "\"e2\"") ] newer) in
            ignore (Pull.apply ~dav ~dry_run:false ~username:"test" output);
            let before = snapshot_tree f.source in
            ignore (Pull.apply ~dav ~dry_run:false ~username:"test" output);
            check (before = snapshot_tree f.source) "replay changed source";
            let r =
              Pull.prepare ~previous:output ~bundle:f.bundle ~snapshot
                ~source:f.source
                ~output:(Filename.concat f.root "next-pull")
                ()
            in
            check (items "changes" r = []) "repeat pull") );
    ( "dry apply leaves every file unchanged",
      fun f ->
        baseline_fixture f (fun _ _ _ newer snapshot ->
            let output, _ = prepare f snapshot in
            let before = snapshot_tree f.root in
            ignore
              (Pull.apply
                 ~dav:(mock_dav (answer ~headers:[ ("etag", "\"e2\"") ] newer))
                 ~dry_run:true ~username:"test" output);
            check (before = snapshot_tree f.root) "dry-run changed files") );
    ( "unpushed local edits do not advance common baseline",
      fun f ->
        baseline_fixture f (fun _ e _ newer snapshot ->
            let local =
              set "names" (arr [ str "Local name"; str "C. Example" ]) f.c
            in
            put_source f local;
            let output, _ = prepare f snapshot in
            let common =
              load_yaml
                (Filename.concat output (field "uid" e ^ "/common.yaml"))
            in
            eq (get "names" common) (get "names" f.c);
            ignore
              (Pull.apply
                 ~dav:(mock_dav (answer ~headers:[ ("etag", "\"e2\"") ] newer))
                 ~dry_run:false ~username:"test" output);
            let changed =
              newer |> Mapping.parse
              |> replace "FN" (fun p -> { p with value = "Remote name" })
              |> Mapping.render
            in
            write (Filename.concat snapshot "before/one.vcf") changed;
            rejects (fun () ->
                Pull.prepare ~previous:output ~bundle:f.bundle ~snapshot
                  ~source:f.source
                  ~output:(Filename.concat f.root "next")
                  ())) );
  ]

let xml_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let multistatus body =
  "<d:multistatus xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:carddav\">"
  ^ body ^ "</d:multistatus>"

let response href props =
  "<d:response><d:href>" ^ href ^ "</d:href><d:propstat><d:prop>" ^ props
  ^ "</d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>"

let discovery cards req =
  let target = Fetch.Middleware.Url.path_and_query req.Fetch.Middleware.url in
  let body =
    match (req.meth, target) with
    | `Other "PROPFIND", "/.well-known/carddav" ->
        response "/dav/"
          "<d:current-user-principal><d:href>/principal/</d:href></d:current-user-principal>"
    | `Other "PROPFIND", "/principal/" ->
        response "/principal/"
          "<c:addressbook-home-set><d:href>/dav/</d:href></c:addressbook-home-set>"
    | `Other "PROPFIND", "/dav/" ->
        response "/dav/Default/"
          "<d:resourcetype><c:addressbook/></d:resourcetype><d:displayname>Personal</d:displayname>"
    | `Other "REPORT", "/dav/Default/" ->
        String.concat ""
          (List.mapi
             (fun i data ->
               response
                 ("/dav/Default/" ^ string_of_int i ^ ".vcf")
                 ("<d:getetag>\"e1\"</d:getetag><c:address-data>"
                ^ xml_escape data ^ "</c:address-data>"))
             cards)
    | _ -> failwith ("unexpected HTTP method/path " ^ target)
  in
  answer ~status:207 (multistatus body) req

let test_remote =
  [
    ( "existing name held without losing unknown properties",
      fun f ->
        let m = export f in
        let other =
          "BEGIN:VCARD\r\n\
           VERSION:3.0\r\n\
           UID:existing\r\n\
           FN:Casey Example\\; Jr.\r\n\
           TEL:+123\r\n\
           X-UNKNOWN:keep\r\n\
           END:VCARD\r\n"
        in
        let rows = Remote.plan f.bundle m book [ remote other ] in
        check
          (field "action" (List.hd rows) = "review")
          "duplicate name creation" );
    ( "Unicode normalization and casefold prevent duplicate",
      fun _ ->
        check
          (Remote.normalize_name " ＣＡＳＥＹ　Straße "
          = Remote.normalize_name "casey STRASSE")
          "unicode normalization" );
    ( "matching account held despite different name",
      fun f ->
        let m = export f in
        let other =
          "BEGIN:VCARD\r\n\
           VERSION:3.0\r\n\
           UID:existing\r\n\
           FN:Other\r\n\
           URL:https://github.com/example\r\n\
           END:VCARD\r\n"
        in
        check
          (field "action"
             (List.hd (Remote.plan f.bundle m book [ remote other ]))
          = "review")
          "account duplicate" );
    ( "repeated seed unchanged",
      fun f ->
        let m = export f in
        check
          (field "action"
             (List.hd (Remote.plan f.bundle m book [ remote (card f m) ]))
          = "unchanged")
          "repeat seed" );
    ( "unmapped remote additions retained",
      fun f ->
        let m = export f in
        let props =
          Mapping.parse (card f m)
          |> List.filter (fun (p : Mapping.property) -> p.name <> "END")
        in
        let data =
          Mapping.render
            (props
            @ List.map Mapping.property
                [ "TEL:+123"; "X-REMOTE:keep"; "END:VCARD" ])
        in
        check
          (field "action"
             (List.hd (Remote.plan f.bundle m book [ remote data ]))
          = "unchanged")
          "extra fields caused replacement" );
    ( "missing display fallback held",
      fun f ->
        let m = export f in
        let data =
          card f m |> Mapping.parse
          |> List.filter (fun (p : Mapping.property) -> p.name <> "N")
          |> Mapping.render
        in
        check
          (field "action"
             (List.hd (Remote.plan f.bundle m book [ remote data ]))
          = "review")
          "lost fallback ignored" );
    ( "edited linked contact held",
      fun f ->
        let m = export f in
        check
          (field "action"
             (List.hd
                (Remote.plan f.bundle m book [ remote (edited (card f m)) ]))
          = "review")
          "remote edit overwritten" );
    ( "store identity conflict held",
      fun f ->
        let m = export f in
        let data =
          card f m |> Mapping.parse
          |> replace "X-SORTAL-STORE" (fun p -> { p with value = "different" })
          |> Mapping.render
        in
        check
          (field "action"
             (List.hd (Remote.plan f.bundle m book [ remote data ]))
          = "review")
          "identity conflict" );
    ( "destination size limit holds creation",
      fun f ->
        let m = export f in
        check
          (field "action"
             (List.hd
                (Remote.plan f.bundle m
                   (set "max_resource_size" (int 10) book)
                   []))
          = "review")
          "oversize" );
    ( "incomplete DAV listing rejected",
      fun _ ->
        let data =
          multistatus
            "<d:response><d:href>/dav/Default/</d:href><d:status>HTTP/1.1 507 \
             Insufficient Storage</d:status></d:response>"
        in
        rejects (fun () -> Remote.responses data "https://contacts.invalid/") );
    ( "missing address data rejected",
      fun _ ->
        let dav =
          mock_dav
            (answer ~status:207
               (multistatus
                  (response "/dav/Default/a.vcf" "<d:getetag>\"e1\"</d:getetag>")))
        in
        rejects (fun () -> Remote.fetch_all dav book) );
    ( "duplicate remote UID rejected",
      fun f ->
        let data = encode f f.c in
        let dav = mock_dav (discovery [ data; data ]) in
        rejects (fun () -> Remote.fetch_all dav book) );
    ( "conditional create failure never overwrites",
      fun f ->
        let m = export f in
        let rows = Remote.plan f.bundle m book [] in
        let calls = ref 0 in
        let dav =
          mock_dav ~readonly:false (fun req ->
              incr calls;
              check (req.Fetch.Middleware.meth = `PUT) "unexpected method";
              check
                (Http.Header.get req.headers "if-none-match" = Some "*")
                "missing create condition";
              answer ~status:412 "" req)
        in
        rejects (fun () -> Remote.seed_one dav f.bundle (List.hd rows) f.root);
        check (!calls = 1) "retried with overwrite" );
    ( "all mutations rejected before transport",
      fun _ ->
        let dav = mock_dav no_network in
        List.iter
          (fun meth ->
            rejects (fun () ->
                Remote.request dav meth "https://contacts.invalid/"))
          [
            "PUT";
            "POST";
            "DELETE";
            "PATCH";
            "PROPPATCH";
            "MKCOL";
            "MOVE";
            "COPY";
          ] );
    ( "custom HTTPS port confined",
      fun _ ->
        let dav =
          mock_dav ~root:"https://contacts.invalid:8443/remote.php/dav/"
            no_network
        in
        Remote.validate dav "https://contacts.invalid:8443/other";
        List.iter
          (fun u -> rejects (fun () -> Remote.validate dav u))
          [
            "https://contacts.invalid/";
            "http://contacts.invalid:8443/";
            "https://other.invalid:8443/";
          ] );
    ( "cross-origin redirect refused",
      fun _ ->
        let calls = ref 0 in
        let dav =
          mock_dav (fun req ->
              incr calls;
              answer ~status:302
                ~headers:[ ("location", "https://foreign.invalid/") ]
                "" req)
        in
        rejects (fun () -> Remote.request dav "GET" "https://contacts.invalid/");
        check (!calls = 1) "sent credentials to foreign origin" );
    ( "embedded credentials and fragments rejected",
      fun _ ->
        List.iter
          (fun root -> rejects (fun () -> mock_dav ~root no_network))
          [
            "http://contacts.invalid/";
            "https://user:pass@contacts.invalid/";
            "https://contacts.invalid/#x";
            "https://contacts.invalid/?x";
          ] );
    ( "fresh source exported by sync preview",
      fun f ->
        ignore (export f);
        let baseline = snapshot_tree f.bundle in
        let c = set "emails" (arr [ str "fresh@example.invalid" ]) f.c in
        put_source f c;
        let before = snapshot_tree f.source in
        let output = Filename.concat f.root "preview" in
        let r =
          Sync.preview
            ~dav:(mock_dav (discovery []))
            ~source:f.source ~bundle:f.bundle ~username:"test" ~output ()
        in
        check
          (number (get "create" (get "upload_counts" r)) = 1)
          "creation count";
        let fresh =
          load_yaml (Filename.concat output "export/originals/casey.yaml")
        in
        eq fresh c;
        check
          (before = snapshot_tree f.source && baseline = snapshot_tree f.bundle)
          "preview modified inputs" );
    ( "existing names held in sync preview",
      fun f ->
        ignore (export f);
        let other =
          "BEGIN:VCARD\r\n\
           VERSION:3.0\r\n\
           UID:existing\r\n\
           FN:Casey Example\\; Jr.\r\n\
           TEL:+123\r\n\
           END:VCARD\r\n"
        in
        let r =
          Sync.preview
            ~dav:(mock_dav (discovery [ other ]))
            ~source:f.source ~bundle:f.bundle ~username:"test"
            ~output:(Filename.concat f.root "preview")
            ()
        in
        check (number (get "review" (get "upload_counts" r)) = 1) "not held" );
    ( "preview report cannot be written into source or cards",
      fun f ->
        ignore (export f);
        List.iter
          (fun output ->
            rejects (fun () ->
                Sync.preview ~dav:(mock_dav no_network) ~source:f.source
                  ~bundle:f.bundle ~username:"test" ~output ()))
          [
            Filename.concat f.source "preview";
            Filename.concat f.bundle "cards/preview";
          ] );
    ( "other account's pull journal is refused before network",
      fun f ->
        ignore (export f);
        let previous = Filename.concat f.root "prior" in
        mkdir previous;
        save_json
          (Filename.concat previous "report.json")
          (obj [ ("account", str "other"); ("status", str "applied") ]);
        rejects (fun () ->
            Sync.preview ~previous ~dav:(mock_dav no_network) ~source:f.source
              ~bundle:f.bundle ~username:"test"
              ~output:(Filename.concat f.root "preview")
              ()) );
    ( "combined preview saves pull diff and leaves baselines unchanged",
      fun f ->
        baseline_fixture f (fun _ _ _ newer _ ->
            let source_before = snapshot_tree f.source
            and baseline_before = snapshot_tree f.bundle in
            let output = Filename.concat f.root "preview" in
            let r =
              Sync.preview
                ~dav:(mock_dav (discovery [ newer ]))
                ~source:f.source ~bundle:f.bundle ~username:"test" ~output ()
            in
            let pull = get "pull" r in
            check (field "status" pull = "planned") "pull not planned";
            check (number (get "local_updates" pull) = 1) "missing update";
            let change = List.hd (items "changes" pull) in
            let diff =
              read
                (Filename.concat output
                   ("pull/" ^ field "uid" change ^ "/changes.diff"))
            in
            check
              (contains diff "+\"vcard\":" || contains diff "NICKNAME")
              "missing diff";
            check
              (source_before = snapshot_tree f.source
              && baseline_before = snapshot_tree f.bundle)
              "preview changed source or baseline") );
  ]

let () =
  Eio_main.run (fun _ ->
      let cases xs =
        List.map
          (fun (name, f) ->
            Alcotest.test_case name `Quick (fun () -> fixture f))
          xs
      in
      Alcotest.run "Sortal CardDAV"
        [
          ("export", cases test_export);
          ("pull", cases test_pull);
          ("remote and dry run", cases test_remote);
        ])
