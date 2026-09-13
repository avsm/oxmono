(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Dooit
open Common

let check name b = Alcotest.(check bool) name true b
let eq name a b = Alcotest.(check string) name a b

let refuses f =
  match f () with
  | _ -> Alcotest.fail "expected refusal"
  | exception Error _ -> ()
  | exception Invalid_argument _ -> ()

let rec remove_tree path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun n -> remove_tree (Filename.concat path n))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let temp f =
  let path = Filename.temp_file "dooit-test-" "" in
  Unix.unlink path;
  mkdir path;
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

let raw_note =
  {|---
schema: dooit/v1
id: 2d26ec67-139e-4cba-8bc3-6ec0f85b8735
title: 'Original title' # leave this comment
status: open
created_at: "2026-09-12T10:00:00Z"
tags: [ocaml, work]
links: []
org.example.extra:
  payload: 'untouched'
---

Original body.
|}

let original () = Doc.parse raw_note
let set_title title = Doc.change "title" (Some (str title))
let set_tags tags = Doc.change "tags" (Some (arr (List.map str tags)))

let patch operations =
  obj [ ("schema", str "dooit.patch/v1"); ("operations", arr operations) ]

let op kind value = obj [ ("op", str kind); ("value", value) ]
let local root = Store.init (Filename.concat root "local")

type server = {
  files : (string, string * string) Hashtbl.t;
  dirs : (string, unit) Hashtbl.t;
  mutable serial : int;
  mutable requests : (string * string * Http.Header.t) list;
  mutable before_put : string -> unit;
  mutable after_put : string -> unit;
  mutable bad_listing : bool;
  mutable weak : bool;
}

let collection = "https://dav.example.test/files/personal/dooit/"

let server () =
  {
    files = Hashtbl.create 20;
    dirs = Hashtbl.create 5;
    serial = 0;
    requests = [];
    before_put = (fun _ -> ());
    after_put = (fun _ -> ());
    bad_listing = false;
    weak = false;
  }

let set_remote s path raw =
  s.serial <- s.serial + 1;
  Hashtbl.replace s.files path (raw, Printf.sprintf "\"%d\"" s.serial)

let xml_escape s =
  String.concat "&amp;" (String.split_on_char '&' s)
  |> String.split_on_char '<' |> String.concat "&lt;"

let backend s =
  Fetch_mock.client (fun req ->
      let url = Fetch.Middleware.Url.to_string req.url
      and meth = Http.Method.to_string req.meth in
      if not (String.starts_with ~prefix:collection url) then
        fail "request escaped test collection";
      let path =
        String.sub url (String.length collection)
          (String.length url - String.length collection)
      in
      s.requests <- (meth, path, req.headers) :: s.requests;
      let respond ?(status = 200) ?(headers = []) body =
        Fetch_mock.respond ~status
          ~headers:(Http.Header.of_list headers)
          body req
      in
      let tag e = if s.weak then "W/" ^ e else e in
      match meth with
      | "GET" -> (
          match Hashtbl.find_opt s.files path with
          | None -> respond ~status:404 ""
          | Some (raw, e) -> respond ~headers:[ ("ETag", tag e) ] raw)
      | "MKCOL" ->
          if Hashtbl.mem s.dirs path then respond ~status:405 ""
          else (
            Hashtbl.add s.dirs path ();
            respond ~status:201 "")
      | "PROPFIND" ->
          if not (Hashtbl.mem s.dirs path) then respond ~status:404 ""
          else
            let direct n =
              String.starts_with ~prefix:path n
              && n <> path
              &&
              let suffix =
                String.sub n (String.length path)
                  (String.length n - String.length path)
              in
              let suffix =
                if String.ends_with ~suffix:"/" suffix then
                  String.sub suffix 0 (String.length suffix - 1)
                else suffix
              in
              not (String.contains suffix '/')
            in
            let entry name dir etag =
              Printf.sprintf
                "<d:response><d:href>%s</d:href><d:propstat><d:prop><d:resourcetype>%s</d:resourcetype>%s</d:prop><d:status>HTTP/1.1 \
                 200 OK</d:status></d:propstat></d:response>"
                (xml_escape (collection ^ name))
                (if dir then "<d:collection/>" else "")
                (match etag with
                | None -> ""
                | Some e -> "<d:getetag>" ^ tag e ^ "</d:getetag>")
            in
            let entries =
              entry path true None
              :: (Hashtbl.to_seq_keys s.dirs |> List.of_seq
                |> List.filter direct
                 |> List.map (fun name -> entry name true None))
              @ (Hashtbl.to_seq s.files |> List.of_seq
                |> List.filter_map (fun (name, (_, e)) ->
                    if direct name then Some (entry name false (Some e))
                    else None))
            in
            let body =
              if s.bad_listing then "<d:multistatus xmlns:d='DAV:'/>"
              else
                "<d:multistatus xmlns:d='DAV:'>" ^ String.concat "" entries
                ^ "</d:multistatus>"
            in
            respond ~status:207
              ~headers:[ ("Content-Type", "application/xml") ]
              body
      | "PUT" ->
          s.before_put path;
          let existing = Hashtbl.find_opt s.files path in
          let ok =
            match
              ( Http.Header.get req.headers "if-none-match",
                Http.Header.get req.headers "if-match",
                existing )
            with
            | Some "*", None, None -> true
            | None, Some e, Some (_, actual) -> e = actual
            | _ -> false
          in
          if not ok then respond ~status:412 ""
          else
            let raw =
              match req.body with
              | Fetch.String s -> s
              | _ -> fail "unexpected request body"
            in
            set_remote s path raw;
            s.after_put path;
            respond ~status:201 ""
      | _ -> fail "unexpected method %s" meth)

let dav s readonly =
  let config =
    Config.parse ~path:"/tmp/config.toml" ~default_root:"/tmp/unused"
      {|
[webdav]
url = "https://dav.example.test/files/"
subdir = "personal/dooit"
username = "test-user"
password = "test-app-password"
|}
    |> Config.require_webdav
  in
  Remote.make ~fetch:(backend s) ~config ~readonly

let sync ?(dry = false) store s = Sync.run ~dry_run:dry store (dav s dry)

let no_conflict actions =
  check "no conflicts"
    (List.for_all
       (fun a -> a.Sync.kind <> "conflict" && a.kind <> "invalid")
       actions)

let note_path d = "notes/" ^ Doc.id d ^ ".md"
let remote_note s d = fst (Hashtbl.find s.files (note_path d)) |> Doc.parse

let seeded root =
  let store = local root and s = server () in
  let d = Store.add store (original ()) in
  no_conflict (sync store s);
  (store, s, d)

let tree path =
  let rec walk prefix =
    let p = Filename.concat path prefix in
    if (Unix.lstat p).Unix.st_kind = Unix.S_DIR then
      Sys.readdir p |> Array.to_list |> List.sort compare
      |> List.concat_map (fun n -> walk (Filename.concat prefix n))
    else [ (prefix, digest (read p)) ]
  in
  walk ""

let tests environment =
  [
    ( "note preservation",
      fun () ->
        let d = set_title "Changed" (original ()) in
        check "comment" (String.contains d.raw '#');
        eq "body unchanged" (original ()).body d.body;
        check "unknown metadata"
          (find "org.example.extra" d.meta
          = find "org.example.extra" (original ()).meta);
        eq "identity edit is byte stable" d.raw
          (Doc.update d ~meta:d.meta ~body:d.body).raw );
    ( "invalid metadata retained",
      fun () ->
        refuses (fun () -> yaml "a: one\na: two\n");
        refuses (fun () -> yaml "a: &x hello\nb: *x\n");
        refuses (fun () ->
            Doc.change "status" (Some (str "surprise")) (original ()));
        refuses (fun () ->
            Doc.change "due" (Some (str "2026-02-30")) (original ())) );
    ( "merge independent fields",
      fun () ->
        let b = original () in
        let l = set_title "New title" b
        and r = Doc.change "status" (Some (str "done")) b in
        let m = get_ok (Merge.run ~base:b ~local:l ~remote:r) in
        eq "title" "New title" (Doc.title m);
        eq "status" "done" (Doc.status m) );
    ( "merge body conflict",
      fun () ->
        let b = original () in
        let body s = Doc.update b ~meta:b.meta ~body:s in
        check "both retained"
          (Result.is_error
             (Merge.run ~base:b ~local:(body "one") ~remote:(body "two"))) );
    ( "merge deletion conflict",
      fun () ->
        let b = original () in
        check "deletion versus title"
          (Result.is_error
             (Merge.run ~base:b
                ~local:(Doc.change "deleted_at" (Some (str (now ()))) b)
                ~remote:(set_title "other" b))) );
    ( "tag add and remove converge",
      fun () ->
        let b = original () in
        let m =
          get_ok
            (Merge.run ~base:b ~local:(set_tags [ "ocaml" ] b)
               ~remote:(set_tags [ "ocaml"; "work"; "new" ] b))
        in
        check "removed tag stays removed" (Doc.tags m = [ "ocaml"; "new" ]) );
    ( "configuration subdirectory",
      fun () ->
        eq "encoded and scoped"
          "https://dav.example.test/files/work%20notes/tasks/"
          (Config.collection ~url:"https://dav.example.test/files/"
             ~subdir:"work notes/tasks");
        List.iter
          (fun subdir ->
            refuses (fun () -> Config.collection ~url:collection ~subdir))
          [ ""; "/escape"; "../escape"; "x/../../escape"; "x//y"; "x\\y" ] );
    ( "TOML passwords and redaction",
      fun () ->
        temp (fun root ->
            let p = Filename.concat root "password" in
            atomic_write p "secret with spaces\n";
            let raw =
              "[webdav]\n\
               url=\"https://dav.example.test/files/\"\n\
               subdir=\"tasks\"\n\
               username=\"me\"\n\
               password_file=\"password\"\n"
            in
            let c =
              Config.parse
                ~path:(Filename.concat root "config.toml")
                ~default_root:root raw
            in
            eq "password newline only stripped" "secret with spaces"
              (Config.password (Config.require_webdav c));
            Unix.chmod p 0o644;
            refuses (fun () -> Config.password (Config.require_webdav c));
            refuses (fun () ->
                Config.parse ~path:"config.toml" ~default_root:root
                  (raw ^ "password=\"also-set\"\n"));
            match
              Config.parse ~path:"config.toml" ~default_root:root
                "[webdav]\npassword=\"DO_NOT_SHOW\n"
            with
            | _ -> Alcotest.fail "parse succeeded"
            | exception Error s ->
                check "secret not in error" (not (String.contains s 'D'))) );
    ( "XDG no directory creation",
      fun () ->
        temp (fun root ->
            let app = "dooit-readonly-test-" ^ Filename.basename root in
            let xdg = Xdge.create ~create_dirs:false environment#fs app in
            List.iter
              (fun p ->
                check "no XDG directory created"
                  (not (exists (Eio.Path.native_exn p))))
              [
                Xdge.config_dir xdg;
                Xdge.data_dir xdg;
                Xdge.cache_dir xdg;
                Xdge.state_dir xdg;
              ]) );
    ( "agent revision and idempotency",
      fun () ->
        temp (fun root ->
            let store = local root in
            let d = Store.add store (original ()) in
            let operation = new_uuid ()
            and p = patch [ op "set_title" (str "agent") ] in
            let expected = Some (Doc.revision d) in
            let apply () =
              Store.apply store ~id:(Doc.id d) ~expected ~operation
                ~payload:(json_string p) (function
                | Some n -> Doc.patch n p
                | _ -> assert false)
            in
            let first = apply () in
            ignore (Store.modify store (Doc.id d) (set_title "human"));
            eq "retry returns original result" first.raw (apply ()).raw;
            eq "retry does not overwrite newer edit" "human"
              (Doc.title (Store.get store (Doc.id d)));
            refuses (fun () ->
                Store.apply store ~id:(Doc.id d) ~expected
                  ~operation:(new_uuid ()) ~payload:"stale" (fun _ -> d))) );
    ( "operation crash recovery",
      fun () ->
        temp (fun root ->
            let store = local root in
            let d = Store.add store (original ()) in
            let changed = set_title "recovered" d and operation = new_uuid () in
            Store.with_lock store (fun () ->
                let hash = Store.keep store changed.raw in
                let payload = "recover" and before = Doc.revision d in
                let op =
                  obj
                    [
                      ( "request",
                        str (digest (Doc.id d ^ "\n" ^ before ^ "\n" ^ payload))
                      );
                      ("id", str (Doc.id d));
                      ("before", str before);
                      ("after", str hash);
                      ("phase", str "prepared");
                    ]
                in
                Store.save_state store "operations.json"
                  (obj [ (operation, op) ]));
            let got =
              Store.apply store ~id:(Doc.id d)
                ~expected:(Some (Doc.revision d))
                ~operation ~payload:"recover"
                (fun _ -> assert false)
            in
            eq "recovered content" changed.raw got.raw) );
    ( "capture identity and completed task",
      fun () ->
        temp (fun root ->
            let store = local root
            and target =
              Link.email ~service:"https://mail.example.test/jmap/session"
                ~account:"account" ~email:"M1"
            in
            let d = List.hd (Link.capture ~title:"Email task" store target) in
            ignore
              (Store.modify store (Doc.id d)
                 (Doc.change "status" (Some (str "done"))));
            let d2 =
              List.hd (Link.capture ~title:"Do not overwrite" store target)
            in
            eq "same ID" (Doc.id d) (Doc.id d2);
            eq "done stays done" "done" (Doc.status d2);
            let fresh =
              List.hd
                (Link.capture ~new_task:true ~title:"Second task" store target)
            in
            check "explicit new task" (Doc.id fresh <> Doc.id d)) );
    ( "capture scope separation",
      fun () ->
        let store = (original ()).meta |> field "id" in
        let k account =
          Link.key
            (Link.email ~service:"https://mail.example.test/jmap/session"
               ~account ~email:"M1")
        in
        check "accounts separate"
          (uuid5 store (k "one") <> uuid5 store (k "two")) );
    ( "search sees direct edits",
      fun () ->
        temp (fun root ->
            let store = local root in
            let d = Store.add store (original ()) in
            let path = Store.note_path store (Doc.id d) in
            atomic_write path (set_title "Straße prototype" d).raw;
            let got, errors =
              Index.query ~text:"STRASSE" ~tags:[ "OCAML" ] store
            in
            check "Unicode full text and tags"
              (errors = [] && List.length got = 1)) );
    ( "dry run leaves local and remote unchanged",
      fun () ->
        temp (fun root ->
            let store = local root and s = server () in
            ignore (Store.add store (original ()));
            let before = tree store.root in
            let actions = sync ~dry:true store s in
            check "creation planned"
              (List.exists (fun a -> a.Sync.kind = "create") actions);
            check "local bytes unchanged" (tree store.root = before);
            check "no remote changes"
              (Hashtbl.length s.files = 0 && Hashtbl.length s.dirs = 0);
            check "only read methods"
              (List.for_all
                 (fun (m, _, _) -> List.mem m [ "GET"; "PROPFIND" ])
                 s.requests)) );
    ( "preview report outside store",
      fun () ->
        temp (fun root ->
            let store = local root and s = server () in
            ignore (Store.add store (original ()));
            refuses (fun () ->
                Sync.run
                  ~report:(Filename.concat store.root "preview")
                  ~dry_run:true store (dav s true));
            let before = tree store.root
            and report = Filename.concat root "preview" in
            ignore (Sync.run ~report ~dry_run:true store (dav s true));
            check "report exists" (exists (Filename.concat report "report.md"));
            check "store unchanged" (before = tree store.root)) );
    ( "transport prohibits mutations",
      fun () ->
        let s = server () in
        refuses (fun () ->
            Remote.put (dav s true) ~path:"anything" ~previous:None "data");
        refuses (fun () -> Remote.mkdir (dav s true) "");
        check "no requests" (s.requests = []) );
    ( "conditional seed and app password",
      fun () ->
        temp (fun root ->
            let store, s, d = seeded root in
            eq "remote note" d.raw (remote_note s d).raw;
            check "every request authenticated"
              (List.for_all
                 (fun (_, _, h) ->
                   Option.is_some (Http.Header.get h "authorization"))
                 s.requests);
            check "conditional creates"
              (List.for_all
                 (fun (m, _, h) ->
                   m <> "PUT" || Http.Header.get h "if-none-match" = Some "*")
                 s.requests);
            no_conflict (sync store s)) );
    ( "two clients merge independent edits",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            let b =
              Sync.clone ~root:(Filename.concat root "second") (dav s true)
            in
            ignore (Store.modify a (Doc.id d) (set_title "local title"));
            ignore
              (Store.modify b (Doc.id d)
                 (Doc.change "status" (Some (str "done"))));
            no_conflict (sync b s);
            no_conflict (sync a s);
            no_conflict (sync b s);
            let got = Store.get b (Doc.id d) in
            eq "title survives" "local title" (Doc.title got);
            eq "status survives" "done" (Doc.status got);
            eq "clients converge" (Store.get a (Doc.id d)).raw got.raw) );
    ( "same-field conflict retains revisions",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            ignore (Store.modify a (Doc.id d) (set_title "local"));
            let r = set_title "remote" d in
            set_remote s (note_path d) r.raw;
            let actions = sync a s in
            check "conflict"
              (List.exists (fun a -> a.Sync.kind = "conflict") actions);
            eq "local retained" "local" (Doc.title (Store.get a (Doc.id d)));
            eq "remote retained" r.raw (remote_note s d).raw;
            check "conflict saved" (Store.conflicts a = [ Doc.id d ])) );
    ( "lost upload response recovered",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            ignore (Store.modify a (Doc.id d) (set_title "new"));
            s.after_put <- (fun _ -> fail "simulated lost response");
            check "first run pending"
              (List.exists (fun a -> a.Sync.kind = "conflict") (sync a s));
            s.after_put <- (fun _ -> ());
            let puts =
              List.length (List.filter (fun (m, _, _) -> m = "PUT") s.requests)
            in
            no_conflict (sync a s);
            check "no blind replay"
              (puts
              = List.length
                  (List.filter (fun (m, _, _) -> m = "PUT") s.requests));
            check "pending cleared"
              (assoc (get "pending" (Sync.state a (dav s false))) = [])) );
    ( "edit during upload preserves remote additions",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            ignore (Store.modify a (Doc.id d) (set_title "title"));
            set_remote s (note_path d)
              (Doc.change "status" (Some (str "done")) d).raw;
            s.after_put <-
              (fun path ->
                if path = note_path d then
                  ignore
                    (Store.modify a (Doc.id d)
                       (set_tags [ "ocaml"; "work"; "during" ])));
            ignore (sync a s);
            s.after_put <- (fun _ -> ());
            no_conflict (sync a s);
            let got = Store.get a (Doc.id d) in
            eq "remote addition preserved" "done" (Doc.status got);
            check "new local edit preserved" (List.mem "during" (Doc.tags got)))
    );
    ( "weak ETag held in preview",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            ignore (Store.modify a (Doc.id d) (set_title "new"));
            s.weak <- true;
            check "held"
              (List.exists
                 (fun a -> a.Sync.kind = "conflict")
                 (sync ~dry:true a s))) );
    ( "partial listing refuses writes",
      fun () ->
        temp (fun root ->
            let a, s, _ = seeded root in
            s.bad_listing <- true;
            let before = tree a.root in
            refuses (fun () -> sync a s);
            check "unchanged" (tree a.root = before)) );
    ( "soft deletion reaches offline client",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            let b =
              Sync.clone ~root:(Filename.concat root "second") (dav s true)
            in
            ignore
              (Store.modify a (Doc.id d)
                 (Doc.change "deleted_at" (Some (str (now ())))));
            no_conflict (sync a s);
            no_conflict (sync b s);
            check "tombstone retained"
              (Doc.deleted (Store.get b (Doc.id d))
              && Hashtbl.mem s.files (note_path d))) );
    ( "hard removal is not recreated",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            Hashtbl.remove s.files (note_path d);
            check "held"
              (List.exists (fun a -> a.Sync.kind = "conflict") (sync a s));
            check "not recreated" (not (Hashtbl.mem s.files (note_path d)))) );
    ( "store removal is not recreated",
      fun () ->
        temp (fun root ->
            let a, s, _ = seeded root in
            Hashtbl.clear s.files;
            Hashtbl.clear s.dirs;
            refuses (fun () -> sync a s);
            check "not recreated" (Hashtbl.length s.dirs = 0)) );
    ( "resolution rechecks revisions",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            let l = Store.modify a (Doc.id d) (set_title "local") in
            set_remote s (note_path d) (set_title "remote" d).raw;
            ignore (sync a s);
            let resolved = set_title "both reviewed" d in
            let path = Filename.concat root "resolution.md" in
            atomic_write path resolved.raw;
            refuses (fun () ->
                Sync.resolve a (dav s false) ~id:(Doc.id d)
                  ~expected:(Some (Doc.revision d))
                  ~file:path);
            ignore
              (Sync.resolve a (dav s false) ~id:(Doc.id d)
                 ~expected:(Some (Doc.revision l))
                 ~file:path);
            eq "resolution uploaded" resolved.raw (remote_note s d).raw;
            check "conflict cleared" (Store.conflicts a = [])) );
    ( "412 refetch merges a competing update",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            ignore (Store.modify a (Doc.id d) (set_title "local"));
            let first = ref true in
            s.before_put <-
              (fun path ->
                if path = note_path d && !first then (
                  first := false;
                  set_remote s path
                    (Doc.change "status" (Some (str "done")) d).raw));
            no_conflict (sync a s);
            let got = remote_note s d in
            eq "local survives retry" "local" (Doc.title got);
            eq "competing edit retained" "done" (Doc.status got)) );
    ( "server transformation retains candidate",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            let l = Store.modify a (Doc.id d) (set_title "new") in
            s.after_put <-
              (fun path ->
                if path = note_path d then
                  set_remote s path ((remote_note s d).raw ^ "\ntransformed\n"));
            check "readback held"
              (List.exists (fun a -> a.Sync.kind = "conflict") (sync a s));
            eq "candidate remains local" l.raw (Store.get a (Doc.id d)).raw;
            check "pending journal retained"
              (find (Doc.id d) (get "pending" (Sync.state a (dav s false)))
              <> None)) );
    ( "immutable metadata cannot be pushed",
      fun () ->
        temp (fun root ->
            let a, s, d = seeded root in
            let edited =
              Doc.change "created_at" (Some (str "2026-09-13T10:00:00Z")) d
            in
            atomic_write (Store.note_path a (Doc.id d)) edited.raw;
            check "held"
              (List.exists (fun a -> a.Sync.kind = "conflict") (sync a s));
            eq "server original" d.raw (remote_note s d).raw) );
    ( "configured scope rejects foreign URLs",
      fun () ->
        let s = server () in
        let r = dav s false in
        List.iter
          (fun path -> refuses (fun () -> Remote.get r path))
          [
            "../../other";
            "https://evil.example.test/";
            collection ^ "%2e%2e/outside";
          ];
        check "credentials never sent outside scope" (s.requests = []) );
    ( "unknown link target survives unrelated patch",
      fun () ->
        let link =
          obj
            [
              ("id", str "extension");
              ("rel", str "related");
              ("type", str "org.example.issue");
              ( "target",
                obj
                  [
                    ( "nested",
                      arr [ str "untouched"; obj [ ("one", str "two") ] ] );
                  ] );
            ]
        in
        let d = Doc.change "links" (Some (arr [ link ])) (original ()) in
        let changed = Doc.patch d (patch [ op "add_tag" (str "new") ]) in
        check "extension retained" (items "links" changed.meta = [ link ]) );
    ( "indented frontmatter delimiter is content",
      fun () ->
        let d = original () in
        let header = d.header ^ "extra: |\n  ---\n  still YAML content\n" in
        let parsed = Doc.parse (d.prefix ^ header ^ d.separator ^ d.body) in
        eq "literal string" "---\nstill YAML content\n"
          (field "extra" parsed.meta) );
    ( "canonical email key",
      fun () ->
        let target =
          obj
            [
              ("service", str "https://mail.example.test/");
              ("account_id", str "a");
              ("email_id", str "x\n\"é");
            ]
        in
        eq "stable escaping"
          "[\"jmap-email\",\"https://mail.example.test/\",\"a\",\"x\\u000a\\\"é\"]"
          (Link.key target) );
    ( "offline capture on two devices shares identity",
      fun () ->
        temp (fun root ->
            let a = local root and s = server () in
            no_conflict (sync a s);
            let b =
              Sync.clone ~root:(Filename.concat root "second") (dav s true)
            in
            let target =
              Link.email ~service:"https://mail.example.test/jmap/session"
                ~account:"a" ~email:"M1"
            in
            let l = List.hd (Link.capture ~title:"left" a target)
            and r = List.hd (Link.capture ~title:"right" b target) in
            eq "same UUID" (Doc.id l) (Doc.id r);
            no_conflict (sync a s);
            check "differing creation held"
              (List.exists (fun a -> a.Sync.kind = "conflict") (sync b s));
            check "one remote note" (Hashtbl.length s.files = 2);
            eq "second draft retained" "right"
              (Doc.title (Store.get b (Doc.id r)))) );
    ( "JMAP source reads are typed Email/get only",
      fun () ->
        Eio.Switch.run (fun sw ->
            let session =
              {|{
      "capabilities":{"urn:ietf:params:jmap:core":{
        "maxSizeUpload":50000000,"maxConcurrentUpload":4,"maxSizeRequest":10000000,
        "maxConcurrentRequests":4,"maxCallsInRequest":16,"maxObjectsInGet":500,
        "maxObjectsInSet":500,"collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{}},
      "accounts":{"acc":{"name":"Test","isPersonal":true,"isReadOnly":false,
        "accountCapabilities":{"urn:ietf:params:jmap:mail":{
          "maxMailboxesPerEmail":null,"maxMailboxDepth":null,"maxSizeMailboxName":255,
          "maxSizeAttachmentsPerEmail":50000000,"emailQuerySortOptions":["receivedAt"],
          "mayCreateTopLevelMailbox":true}}}},
      "primaryAccounts":{"urn:ietf:params:jmap:mail":"acc"},"username":"test",
      "apiUrl":"https://mail.example.test/api/",
      "downloadUrl":"https://mail.example.test/download/{accountId}/{blobId}/{name}?type={type}",
      "uploadUrl":"https://mail.example.test/upload/{accountId}/",
      "eventSourceUrl":"https://mail.example.test/events/?types={types}&closeafter={closeafter}&ping={ping}",
      "state":"s1"}|}
            in
            let methods = ref [] and wrong = ref false in
            let backend =
              Fetch_mock.client (fun req ->
                  let path = Fetch.Middleware.Url.path_and_query req.url in
                  let body =
                    if path = "/session" then session
                    else (
                      check "JMAP read uses POST"
                        (Http.Method.to_string req.meth = "POST");
                      let raw =
                        match req.body with
                        | Fetch.String s -> s
                        | _ -> fail "request body"
                      in
                      let request = json raw in
                      let call =
                        List.hd (items "methodCalls" request) |> list
                      in
                      let name = string (List.nth call 0)
                      and args = List.nth call 1
                      and cid = List.nth call 2 in
                      methods := name :: !methods;
                      eq "only read method" "Email/get" name;
                      check "explicit message ID"
                        (items "ids" args = [ str "M1" ]);
                      json_string
                        (obj
                           [
                             ("sessionState", str "s1");
                             ( "methodResponses",
                               arr
                                 [
                                   arr
                                     [
                                       str "Email/get";
                                       obj
                                         [
                                           ("accountId", str "acc");
                                           ("state", str "e1");
                                           ( "list",
                                             arr
                                               [
                                                 obj
                                                   [
                                                     ( "id",
                                                       str
                                                         (if !wrong then "M2"
                                                          else "M1") );
                                                     ("subject", str "Subject");
                                                     ("threadId", str "T1");
                                                     ( "messageId",
                                                       arr
                                                         [
                                                           str "m@example.test";
                                                         ] );
                                                   ];
                                               ] );
                                           ("notFound", arr []);
                                         ];
                                       cid;
                                     ];
                                 ] );
                           ]))
                  in
                  Fetch_mock.respond
                    ~headers:
                      (Http.Header.of_list
                         [ ("Content-Type", "application/json") ])
                    body req)
            in
            let client =
              match
                Jmap_eio.Client.connect ~sw
                  ~auth:(Jmap_eio.Auth.bearer "test-token")
                  (Jmap_eio.Transport.of_fetch backend)
                  "https://mail.example.test/session"
              with
              | Ok c -> c
              | Error e -> fail "%s" (Jmap_eio.Client.error_to_string e)
            in
            let message =
              Email.read client ~account:"acc" ~email_id:"M1" ~fetch_body:false
            in
            check "subject retrieved" (message.subject = Some "Subject");
            wrong := true;
            refuses (fun () ->
                Email.read client ~account:"acc" ~email_id:"M1"
                  ~fetch_body:false);
            check "no mail mutations" (!methods = [ "Email/get"; "Email/get" ]))
    );
  ]

let () =
  Eio_main.run (fun environment ->
      Alcotest.run "Dooit"
        [
          ( "native",
            List.map
              (fun (name, f) -> Alcotest.test_case name `Quick f)
              (tests environment) );
        ])
