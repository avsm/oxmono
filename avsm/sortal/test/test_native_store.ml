(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Sortal_carddav
open Common
module Contact = Sortal.Contact

let check b message = if not b then failwith message

let rejects f =
  match f () with
  | _ -> failwith "expected rejection"
  | exception (Common.Error _ | Unix.Unix_error _ | Sys_error _) -> ()

let fields =
  json
    {|{"version":2,"kind":"person","handle":"ada","names":["Ada"],
       "emails":["ada@example.test"],"future":{"flag":true},
       "accounts":{"github":"ada","unknown":{"keep":[1,null]}},
       "links":[{"url":"https://a.test","custom":"A"},
                {"url":"https://b.test","custom":"B"}],
       "feeds":[{"type":"future","url":"https://future.test"},
                {"type":"atom","url":"https://feed.test","custom":42}] }|}

let fixture env f =
  let root = Filename.temp_file "sortal-native-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      mkdir (Filename.concat root "cards");
      let store_id = new_uuid () in
      save_json
        (Filename.concat root "store.json")
        (obj [ ("version", int 1); ("store_id", str store_id) ]);
      let raw, _ =
        Mapping.encode ~uid:"stable" ~store_id ~originals:root fields
      in
      let path = Filename.concat root "cards/stable.vcf" in
      let props = Mapping.parse raw in
      let props =
        List.concat_map
          (fun (p : Mapping.property) ->
            if p.name = "EMAIL" then
              [ { p with params = p.params @ [ ("X-CLIENT", "keep") ] } ]
            else if p.name = "END" then
              [
                Mapping.property "external.TEL;TYPE=work:+123";
                Mapping.property "external.X-ABLabel:Office";
                Mapping.property "X-FUTURE;VALUE=text:Unmodelled";
                p;
              ]
            else [ p ])
          props
      in
      write path (Mapping.render props);
      f root path (Sortal.Store.create_at env#fs root))

let loaded store = Option.get (Sortal.Store.lookup store "ada")

let tests =
  [
    ( "account shorthand retains unknown nested fields",
      fun root path _ ->
        let original = Document.value (read path) in
        let account =
          obj
            [
              ("handle", str "ada.test");
              ("custom", int 7);
              ("apps", arr [ str "future-app" ]);
            ]
        in
        let original =
          set "accounts"
            (obj [ ("atproto", account); ("github", arr [ str "ada" ]) ])
            original
        in
        let props = Mapping.parse (read path) in
        let store_id =
          Mapping.untext (Mapping.only props "X-SORTAL-STORE").value
        in
        let raw, _ =
          Mapping.encode ~uid:"stable" ~store_id ~originals:root original
        in
        let before = Document.contact raw |> Document.of_contact in
        let after =
          set "accounts"
            (obj [ ("atproto", str "new.test"); ("github", str "new-name") ])
            before
        in
        let c =
          get_ok
            (Jsont_bytesrw.decode_string Contact.json_t (json_string after))
        in
        let result = Document.edit ~originals:root raw c |> Document.value in
        let account = get "atproto" (get "accounts" result) in
        check
          (get "github" (get "accounts" result) = arr [ str "new-name" ])
          "lost single-account list shape";
        check (field "handle" account = "new.test") "account not changed";
        check (get "custom" account = int 7) "lost account metadata";
        check (items "apps" account = [ str "future-app" ]) "lost future app" );
    ( "no-op keeps original bytes and UID",
      fun _ path store ->
        let before = read path in
        Sortal.Store.save store (loaded store);
        check (read path = before) "no-op re-encoded the card" );
    ( "typed feed edit keeps unknown fields and unsupported feeds",
      fun _ path store ->
        get_ok
          (Sortal.Store.set_feed_paused store "ada" "https://feed.test" true);
        let after = Document.value (read path) in
        check (get "future" fields = get "future" after) "lost future field";
        check (get "accounts" fields = get "accounts" after) "lost account";
        let feeds = items "feeds" after in
        check
          (List.hd feeds = List.hd (items "feeds" fields))
          "lost future feed";
        check
          (get "custom" (List.nth feeds 1) = `Float 42.)
          "lost feed metadata";
        check (get "paused" (List.nth feeds 1) = `Bool true) "pause not saved"
    );
    ( "account edits preserve unrelated properties and parameters",
      fun _ path store ->
        let before = Mapping.parse (read path) in
        get_ok
          (Sortal.Store.set_account store "ada"
             (Sortal_schema.Account.Simple (Github, "new-name")));
        let after = Mapping.parse (read path) in
        List.iter
          (fun p ->
            if
              List.mem p.Mapping.name
                [ "TEL"; "X-ABLABEL"; "X-FUTURE"; "EMAIL" ]
            then check (List.mem p after) "unrelated property changed")
          before );
    ( "renaming retains UID and rejects duplicate handles",
      fun _ path store ->
        get_ok
          (Sortal.Store.update_contact store "ada" (fun c ->
               Contact.make ~handle:"renamed" ~names:(Contact.names c) ()));
        check (Sortal.Store.lookup store "ada" = None) "old handle remains";
        check
          (Sortal.Store.filename store "renamed" = "cards/stable.vcf")
          "rename changed filename";
        Sortal.Store.save store
          (Contact.make ~handle:"ada" ~names:[ "Other" ] ());
        let before = read path in
        rejects (fun () ->
            Sortal.Store.update_contact store "renamed" (fun _ ->
                Contact.make ~handle:"ada" ~names:[ "Collision" ] ()));
        check (read path = before) "collision overwrote card" );
    ( "stale revisions cannot overwrite edits or resurrect deletions",
      fun _ path store ->
        let c = loaded store in
        write path (read path ^ "\r\n");
        let before = read path in
        rejects (fun () -> Sortal.Store.save store c);
        check (read path = before) "stale write overwrote edit";
        Sortal.Store.delete store "ada";
        rejects (fun () -> Sortal.Store.save store c);
        check (not (exists path)) "resurrected deleted card" );
    ( "reordered links retain their individual metadata",
      fun root path store ->
        let c = loaded store in
        let projection = Document.of_contact c in
        let changed =
          set "links" (arr (List.rev (items "links" projection))) projection
        in
        let c =
          get_ok
            (Jsont_bytesrw.decode_string Contact.json_t (json_string changed))
        in
        let after =
          Document.edit ~originals:root (read path) c |> Document.value
        in
        check
          (equal (get "links" after) (arr (List.rev (items "links" fields))))
          "metadata detached from its link" );
    ( "corrupt cards are reported",
      fun root path store ->
        write (Filename.concat root "cards/broken.vcf") "not a vCard";
        rejects (fun () -> Sortal.Store.list store);
        check (exists path) "removed a valid card" );
    ( "contact symlinks are rejected",
      fun root _ store ->
        Unix.symlink "stable.vcf" (Filename.concat root "cards/linked.vcf");
        rejects (fun () -> Sortal.Store.list store) );
    ( "adding and removing link labels preserves future metadata",
      fun root path store ->
        let projection = Document.of_contact (loaded store) in
        let change links raw =
          let c =
            get_ok
              (Jsont_bytesrw.decode_string Contact.json_t
                 (json_string (set "links" (arr links) projection)))
          in
          Document.edit ~originals:root raw c
        in
        let labelled =
          obj [ ("url", str "https://a.test"); ("label", str "Home") ]
        in
        let added = change [ labelled; str "https://b.test" ] (read path) in
        let a = List.hd (items "links" (Document.value added)) in
        check
          (field "custom" a = "A" && field "label" a = "Home")
          "adding label discarded future metadata";
        let removed = change (items "links" projection) added in
        check
          (equal (Document.value removed) fields)
          "removing label discarded future metadata" );
    ( "deleting understood feeds retains unsupported feeds",
      fun root path store ->
        let projection = Document.of_contact (loaded store) |> remove "feeds" in
        let c =
          get_ok
            (Jsont_bytesrw.decode_string Contact.json_t (json_string projection))
        in
        let after =
          Document.edit ~originals:root (read path) c |> Document.value
        in
        check
          (equal (get "feeds" after) (arr [ List.hd (items "feeds" fields) ]))
          "deleted an unsupported feed" );
  ]

let () =
  Eio_main.run (fun env ->
      let git_test () =
        fixture env (fun root path store ->
            let git args =
              Eio.Process.run env#process_mgr ([ "git"; "-C"; root ] @ args)
            in
            git [ "init"; "-q" ];
            git [ "config"; "user.name"; "Sortal tests" ];
            git [ "config"; "user.email"; "sortal@example.test" ];
            git [ "config"; "commit.gpgsign"; "false" ];
            git [ "add"; "store.json"; "cards" ];
            git [ "commit"; "-qm"; "Initial fixture" ];
            let versioned = Sortal.Git_store.create store env in
            get_ok
              (Sortal.Git_store.update_contact versioned "ada"
                 (fun _ -> Contact.make ~handle:"renamed" ~names:[ "Ada" ] ())
                 ~msg:"Rename contact");
            git [ "diff"; "--exit-code"; "HEAD"; "--"; "cards" ];
            check (exists path) "git rename changed the card filename";
            get_ok
              (Sortal.Git_store.save versioned
                 (Option.get (Sortal.Store.lookup store "renamed")));
            get_ok (Sortal.Git_store.delete versioned "renamed");
            git [ "diff"; "--exit-code"; "HEAD"; "--"; "cards" ];
            check (not (exists path)) "git delete left the card")
      in
      Alcotest.run "Native Sortal store"
        [
          ( "storage",
            List.map
              (fun (name, f) ->
                Alcotest.test_case name `Quick (fun () -> fixture env f))
              tests );
          ( "git",
            [ Alcotest.test_case "version the actual store" `Quick git_test ] );
        ])
