(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module File_time = Unix
open! Core
module C = Dooit.Common
module M = Termanil_model

let temp f =
  let path = Stdlib.Filename.temp_file "termanil-test-" "" in
  Stdlib.Sys.remove path;
  C.mkdir path;
  Exn.protect
    ~f:(fun () -> f path)
    ~finally:(fun () -> Sortal_carddav.Common.remove_tree path)

let%expect_test
    "task capture is idempotent and completing a stale note preserves edits" =
  temp (fun root ->
      let store = Dooit.Store.init root in
      let m = List.hd_exn Termanil_demo.messages in
      let a = List.hd_exn (Termanil_tasks.capture store m) in
      let b = List.hd_exn (Termanil_tasks.capture store m) in
      printf "same id: %b\n" (String.equal a.id b.id);
      let doc = Dooit.Store.get store a.id in
      let edited =
        Dooit.Doc.change "title" (Some (C.str "Agent edited this")) doc
      in
      C.atomic_write (Dooit.Store.note_path store a.id) edited.raw;
      (try
         ignore (Termanil_tasks.complete store a);
         print_endline "unexpected success"
       with C.Error _ -> print_endline "stale completion refused");
      printf "title: %s\n" (Dooit.Doc.title (Dooit.Store.get store a.id));
      let fresh = Termanil_tasks.task (Dooit.Store.get store a.id) in
      ignore (Termanil_tasks.complete store fresh);
      let again = List.hd_exn (Termanil_tasks.capture store m) in
      printf "recapture: %s\n" again.status);
  [%expect
    {|
    same id: true
    stale completion refused
    title: Agent edited this
    recapture: done
    |}]

let%expect_test
    "contacts with identical names and emails retain separate identities" =
  let c : M.contact =
    {
      key = "sortal:x";
      name = "Alex";
      emails = [ "alex@example.test" ];
      sources = [ "x.vcf" ];
      metadata = [ ("unknown", "retained") ];
    }
  in
  let d = { c with key = "carddav:y" } in
  let contacts = Termanil_contacts.combine [ c ] [ d ] in
  printf "contacts: %d\n" (List.length contacts);
  printf "sender matches: %d\n"
    (List.length
       (Termanil_contacts.for_addresses [ "ALEX@example.test" ] contacts));
  [%expect {|
    contacts: 2
    sender matches: 2
    |}]

let%expect_test "native vCards show metadata and read subsequent edits" =
  temp (fun root ->
      let output = Filename.concat root "store" in
      let directory = Filename.concat output "cards" in
      C.mkdir directory;
      let raw =
        {|{"version":2,"kind":"person","handle":"ada","names":["Ada"],
          "emails":["ada@example.test"],"future":{"colour":"blue","flag":true}}|}
      in
      let data, _ =
        Sortal_carddav.Mapping.encode ~uid:"ada" ~originals:output
          (Sortal_carddav.Common.json raw)
      in
      C.atomic_write (Filename.concat directory "ada.vcf") data;
      C.atomic_write
        (Filename.concat output "store.json")
        {|{"version":1,"store_id":"store"}|};
      C.atomic_write (Filename.concat directory "broken.vcf") "not a vCard";
      let contacts, errors = Termanil_contacts.local output in
      let c = List.hd_exn contacts in
      printf "name: %s\nerrors: %d\n" c.name (List.length errors);
      printf "future metadata: %b\n"
        (List.exists c.metadata ~f:(fun (k, v) ->
             String.equal k "/future" && String.is_substring v ~substring:"blue"));
      let path = List.hd_exn c.sources in
      C.atomic_write path
        (String.substr_replace_all (C.read path)
           ~pattern:"FN;X-SORTAL-PATH=/names/0:Ada"
           ~with_:"FN;X-SORTAL-PATH=/names/0:Ada Lovelace");
      let contacts, _ = Termanil_contacts.local output in
      printf "live edit: %s\n" (List.hd_exn contacts).name);
  [%expect
    {|
    name: Ada
    errors: 1
    future metadata: true
    live edit: Ada Lovelace
    |}]

let%expect_test "CardDAV view exposes UID and unrecognized metadata" =
  let raw =
    "BEGIN:VCARD\r\n\
     VERSION:3.0\r\n\
     UID:abc\r\n\
     FN:Ada\r\n\
     EMAIL:ada@example.test\r\n\
     X-SORTAL-ID:ada\r\n\
     X-EXTRA:untouched\r\n\
     END:VCARD\r\n"
  in
  let r : Sortal_carddav.Remote.card =
    {
      uid = "abc";
      href = "https://dav.example.test/book/abc.vcf";
      etag = Some "\"v1\"";
      data = raw;
      props = Sortal_carddav.Mapping.parse raw;
    }
  in
  let c = Termanil_contacts.card r in
  printf "identity: %s\nextra metadata: %b\n" c.key
    (List.mem c.metadata ("X-EXTRA", "untouched") ~equal:Poly.equal);
  [%expect
    {|
    identity: carddav:https://dav.example.test/book/abc.vcf
    extra metadata: true
    |}]

let%expect_test
    "config uses relative paths and rejects typos, HTTP and foreign origins" =
  let c =
    Termanil_backend.Config.parse ~path:"/tmp/config/config.toml"
      "[contacts]\nvcard_root='contacts'\n[dooit]\nconfig='tasks.toml'\n"
  in
  print_s
    [%sexp (c.vcard_root : string option), (c.dooit_config : string option)];
  List.iter
    [
      "[mail]\nprofiel='x'";
      "[contacts.carddav]\n\
       url='http://example.test/'\n\
       username='x'\n\
       password_file='secret'";
      "[contacts.carddav]\n\
       url='https://example.test/'\n\
       collection='https://elsewhere.test/'\n\
       username='x'\n\
       password_file='secret'";
    ] ~f:(fun raw ->
      try
        ignore (Termanil_backend.Config.parse ~path:"/tmp/config.toml" raw);
        print_endline "unexpected success"
      with C.Error _ -> print_endline "rejected");
  [%expect
    {|
    ((/tmp/config/contacts) (/tmp/config/tasks.toml))
    rejected
    rejected
    rejected
    |}]

let%expect_test "direct edits reorder tasks by filesystem modification time" =
  temp (fun root ->
      let store = Dooit.Store.init root in
      let a =
        List.hd_exn
          (Termanil_tasks.capture store (List.nth_exn Termanil_demo.messages 0))
      in
      let b =
        List.hd_exn
          (Termanil_tasks.capture store (List.nth_exn Termanil_demo.messages 1))
      in
      let touch n time =
        File_time.utimes (Dooit.Store.note_path store n.M.id) time time
      in
      touch a 1000.;
      touch b 2000.;
      let show () =
        let tasks, _ = Termanil_tasks.list store in
        let t = { M.initial with tasks } in
        print_s
          [%sexp
            (List.map (M.visible_tasks t) ~f:(fun n -> n.M.title) : string list)]
      in
      show ();
      let path = Dooit.Store.note_path store a.id in
      C.atomic_write path (C.read path ^ "Agent updated the body\n");
      touch a 3000.;
      show ());
  [%expect
    {|
    ("Lunch on Friday" "Review the garden plan")
    ("Review the garden plan" "Lunch on Friday")
    |}]

let%expect_test
    "signature config supports multiline override and explicit empty value" =
  List.iter
    [ "[mail]\nsignature=\"\"\n"; "[mail]\nsignature=\"\"\"-- \nAnil\"\"\"\n" ]
    ~f:(fun raw ->
      let c = Termanil_backend.Config.parse ~path:"/tmp/config.toml" raw in
      print_s [%sexp (c.signature : string option)]);
  [%expect {|
    ("")
    ( "-- \
     \nAnil")
    |}]
