module C = Matrix_client.Composer_draft
module Id = Matrix_proto.Id

let room s = Result.get_ok (Id.Room_id.of_string s)
let event s = Result.get_ok (Id.Event_id.of_string s)

let option_equal eq a b =
  match (a, b) with None, None -> true | Some a, Some b -> eq a b | _ -> false

let thumbnail_equal (a : C.thumbnail) (b : C.thumbnail) =
  String.equal a.C.filename b.filename
  && String.equal a.data b.data
  && option_equal String.equal a.mimetype b.mimetype
  && option_equal Int64.equal a.width b.width
  && option_equal Int64.equal a.height b.height
  && option_equal Int64.equal a.size b.size

let content_equal a b =
  match (a, b) with
  | C.Image a, C.Image b ->
      String.equal a.data b.data
      && option_equal String.equal a.mimetype b.mimetype
      && option_equal Int64.equal a.size b.size
      && option_equal Int64.equal a.width b.width
      && option_equal Int64.equal a.height b.height
      && option_equal String.equal a.blurhash b.blurhash
      && option_equal thumbnail_equal a.thumbnail b.thumbnail
  | C.Video a, C.Video b ->
      String.equal a.data b.data
      && option_equal String.equal a.mimetype b.mimetype
      && option_equal Int64.equal a.size b.size
      && option_equal Int64.equal a.width b.width
      && option_equal Int64.equal a.height b.height
      && option_equal Int64.equal a.duration_ms b.duration_ms
      && option_equal String.equal a.blurhash b.blurhash
      && option_equal thumbnail_equal a.thumbnail b.thumbnail
  | C.Audio a, C.Audio b ->
      String.equal a.data b.data
      && option_equal String.equal a.mimetype b.mimetype
      && option_equal Int64.equal a.size b.size
      && option_equal Int64.equal a.duration_ms b.duration_ms
  | C.File a, C.File b ->
      String.equal a.data b.data
      && option_equal String.equal a.mimetype b.mimetype
      && option_equal Int64.equal a.size b.size
  | _ -> false

let draft_equal (a : C.t) (b : C.t) =
  let type_equal a b =
    match (a, b) with
    | C.New_message, C.New_message -> true
    | C.Reply a, C.Reply b | C.Edit a, C.Edit b ->
        String.equal (Id.Event_id.to_string a) (Id.Event_id.to_string b)
    | _ -> false
  in
  String.equal a.plain_text b.plain_text
  && option_equal String.equal a.html_text b.html_text
  && type_equal a.draft_type b.draft_type
  && List.length a.attachments = List.length b.attachments
  && List.for_all2
       (fun (a : C.attachment) (b : C.attachment) ->
         String.equal a.filename b.filename && content_equal a.content b.content)
       a.attachments b.attachments

let check_draft name expected actual =
  Alcotest.(check bool) name true (draft_equal expected actual)

let sample () =
  {
    C.plain_text = "hello";
    html_text = Some "<b>hello</b>";
    draft_type = C.Reply (event "$reply:example.org");
    attachments =
      [
        {
          C.filename = "picture.png";
          content =
            C.Image
              {
                data = "\000\255image";
                mimetype = Some "image/png";
                size = Some 8L;
                width = Some 320L;
                height = Some 240L;
                blurhash = Some "LEHV6nWB2yk8pyo0adR*.7kCMdnj";
                thumbnail =
                  Some
                    {
                      C.filename = "thumb.png";
                      data = "thumbnail";
                      mimetype = Some "image/png";
                      width = Some 32L;
                      height = Some 24L;
                      size = Some 9L;
                    };
              };
        };
        {
          C.filename = "clip.mp4";
          content =
            C.Video
              {
                data = "video";
                mimetype = None;
                size = None;
                width = None;
                height = None;
                duration_ms = Some 1500L;
                blurhash = None;
                thumbnail = None;
              };
        };
        {
          C.filename = "sound.ogg";
          content =
            C.Audio
              {
                data = "audio";
                mimetype = None;
                size = None;
                duration_ms = None;
              };
        };
        {
          C.filename = "notes.txt";
          content =
            C.File
              { data = "file"; mimetype = Some "text/plain"; size = Some 4L };
        };
      ];
  }

let test_roundtrip_and_isolation () =
  let store = Matrix_client.Store.memory () in
  let room_id = room "!one:example.org" in
  let thread_root = event "$thread:example.org" in
  let draft = sample () in
  (match C.save store ~room_id draft with
  | Ok () -> ()
  | Error e -> Alcotest.failf "save: %s" (Matrix_client.Error.to_string e));
  (match
     C.save store ~room_id ~thread_root { draft with plain_text = "thread" }
   with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "thread save: %s" (Matrix_client.Error.to_string e));
  let loaded =
    match C.load store ~room_id () with
    | Ok (Some value) -> value
    | Ok None -> Alcotest.fail "room draft missing"
    | Error e -> Alcotest.failf "load: %s" (Matrix_client.Error.to_string e)
  in
  check_draft "binary attachments and metadata round-trip" draft loaded;
  let thread =
    match C.load store ~room_id ~thread_root () with
    | Ok (Some value) -> value
    | Ok None -> Alcotest.fail "thread draft missing"
    | Error e ->
        Alcotest.failf "thread load: %s" (Matrix_client.Error.to_string e)
  in
  Alcotest.(check string) "thread is isolated" "thread" thread.plain_text;
  (match C.clear store ~room_id () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "clear: %s" (Matrix_client.Error.to_string e));
  Alcotest.(check bool)
    "room clear leaves thread" true
    (C.load store ~room_id ~thread_root () = Ok (Some thread));
  Alcotest.(check bool)
    "room draft cleared" true
    (C.load store ~room_id () = Ok None)

let test_restart () =
  Eio_main.run @@ fun env ->
  let path = Filename.temp_file "matrix-composer-draft" ".d" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
  let store = Matrix_client.Store.on_disk ~dir in
  let room_id = room "!restart:example.org" in
  let draft =
    { (sample ()) with draft_type = C.Edit (event "$edit:example.org") }
  in
  (match C.save store ~room_id draft with
  | Ok () -> ()
  | Error e -> Alcotest.failf "save: %s" (Matrix_client.Error.to_string e));
  Alcotest.(check bool)
    "save only dirties" true
    (Matrix_client.Store.dirty store);
  (match Matrix_client.Store.flush store with
  | Ok () -> ()
  | Error e -> Alcotest.failf "flush: %s" (Matrix_client.Error.to_string e));
  let reopened = Matrix_client.Store.on_disk ~dir in
  match C.load reopened ~room_id () with
  | Ok (Some loaded) -> check_draft "draft survives restart" draft loaded
  | Ok None -> Alcotest.fail "draft missing after restart"
  | Error e -> Alcotest.failf "reload: %s" (Matrix_client.Error.to_string e)

let test_missing_attachments_is_legacy_compatible () =
  let store = Matrix_client.Store.memory () in
  let raw_slot =
    Matrix_client.Store.Slot.v ~name:"composer_draft"
      Matrix_proto.Json.Codec.json
  in
  let member name value = Jsont.Json.mem (Jsont.Json.name name) value in
  let obj members = Jsont.Json.object' members in
  let raw =
    Jsont.Json.list
      [
        obj
          [
            member "room_id" (Jsont.Json.string "!legacy:example.org");
            member "draft"
              (obj
                 [
                   member "plain_text" (Jsont.Json.string "old");
                   member "draft_type" (Jsont.Json.string "NewMessage");
                 ]);
          ];
      ]
  in
  (match Matrix_client.Store.Slot.set store raw_slot raw with
  | Ok () -> ()
  | Error e -> Alcotest.failf "raw slot: %s" (Matrix_client.Error.to_string e));
  match C.load store ~room_id:(room "!legacy:example.org") () with
  | Ok (Some draft) ->
      Alcotest.(check string) "legacy plain text" "old" draft.plain_text;
      Alcotest.(check int)
        "legacy attachments default empty" 0
        (List.length draft.attachments)
  | Ok None -> Alcotest.fail "legacy draft missing"
  | Error e ->
      Alcotest.failf "legacy load: %s" (Matrix_client.Error.to_string e)

let () =
  Alcotest.run "composer draft"
    [
      ( "store",
        [
          Alcotest.test_case "round-trip, isolation and clear" `Quick
            test_roundtrip_and_isolation;
          Alcotest.test_case "restart" `Quick test_restart;
          Alcotest.test_case "missing attachments" `Quick
            test_missing_attachments_is_legacy_compatible;
        ] );
    ]
