open Crowthebot
module S = Caldav_store
module R = Caldav_source

let admin = "@admin:example.test"
let check name b = if not b then failwith name

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let reject f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let decode s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let field name = function
  | Jsont.Object (fs, _) ->
      List.assoc name (List.map (fun ((k, _), v) -> (k, v)) fs)
  | _ -> failwith "object"

let int j = Result.get_ok (Jsont.Json.decode Jsont.int j)
let str = function Jsont.String (s, _) -> s | _ -> failwith "string"

let multi body =
  "<d:multistatus xmlns:d='DAV:' xmlns:c='urn:ietf:params:xml:ns:caldav'>"
  ^ body ^ "</d:multistatus>"

let prop href body =
  "<d:response><d:href>" ^ href ^ "</d:href><d:propstat><d:prop>" ^ body
  ^ "</d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>"

let href = "https://example.test/home/calendar/"

let raw =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//Crow test//EN\r\n\
   BEGIN:VEVENT\r\n\
   UID:one\r\n\
   DTSTAMP:20260909T120000Z\r\n\
   DTSTART;TZID=Europe/London:20260910T100000\r\n\
   SUMMARY:Office telescope\r\n\
   RRULE:FREQ=DAILY;COUNT=3\r\n\
   EXDATE;TZID=Europe/London:20260911T100000\r\n\
   X-PRESERVE:9007199254740993\r\n\
   DESCRIPTION:" ^ String.make 3000 'a'
  ^ "🤖\\nFolded\r\n\
    \ continuation\r\n\
     END:VEVENT\r\n\
     BEGIN:VEVENT\r\n\
     UID:one\r\n\
     RECURRENCE-ID;TZID=Europe/London:20260912T100000\r\n\
     DTSTAMP:20260909T120000Z\r\n\
     SUMMARY:Observatory\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"

let () =
  let buffer = Buffer.create 4096 in
  let formatter = Format.formatter_of_buffer buffer in
  Logs.set_reporter (Logs.format_reporter ~app:formatter ~dst:formatter ());
  Diagnostics.configure ~verbose:false;
  Eio_main.run @@ fun env ->
  let filename = Filename.temp_file "crow-caldav" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) @@ fun () ->
  let path = Eio.Path.(Eio.Stdenv.fs env / filename) in
  let failed = ref false and requests = ref [] and delta = ref false in
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        let meth = Http.Method.to_string req.meth in
        requests := (meth, url) :: !requests;
        check "reads only"
          (List.mem meth [ "GET"; "OPTIONS"; "PROPFIND"; "REPORT" ]);
        check "credential stays on origin"
          (String.starts_with ~prefix:"https://example.test/" url);
        let xml ?(status = 207) body =
          Fetch_mock.respond ~status
            ~headers:
              (Http.Header.of_list [ ("Content-Type", "application/xml") ])
            body req
        in
        match (meth, Uri.path (Uri.of_string url)) with
        | "GET", "/home/calendar/one.ics" ->
            if !failed then Fetch_mock.respond ~status:503 "private failure" req
            else
              Fetch_mock.respond
                ~headers:
                  (Http.Header.of_list
                     [ ("Content-Type", "text/calendar"); ("ETag", "\"one\"") ])
                raw req
        | "GET", "/home/calendar/two.ics" ->
            Fetch_mock.respond ~status:404 "gone" req
        | "REPORT", "/home/calendar/" ->
            let body =
              if !delta then
                "<d:response><d:href>/home/calendar/one.ics</d:href><d:status>HTTP/1.1 \
                 404 Not \
                 Found</d:status></d:response><d:sync-token>urn:two</d:sync-token>"
              else
                prop "/home/calendar/one.ics" "<d:getetag>\"one\"</d:getetag>"
                ^ "<d:sync-token>urn:one</d:sync-token>"
            in
            xml (multi body)
        | "PROPFIND", "/home/" ->
            xml
              (multi
                 (prop "/home/calendar/"
                    "<d:resourcetype><d:collection/><c:calendar/></d:resourcetype><d:displayname>Personal</d:displayname><d:supported-report-set><d:supported-report><d:report><d:sync-collection/></d:report></d:supported-report></d:supported-report-set>"))
        | "PROPFIND", "/principal/" ->
            xml
              (multi
                 (prop "/principal/"
                    "<c:calendar-home-set><d:href>/home/</d:href></c:calendar-home-set>"))
        | "PROPFIND", _ ->
            xml
              (multi
                 (prop
                    (Uri.path (Uri.of_string url))
                    "<d:current-user-principal><d:href>/principal/</d:href></d:current-user-principal>"))
        | "OPTIONS", _ ->
            Fetch_mock.respond
              ~headers:
                (Http.Header.of_list [ ("DAV", "1, 3, calendar-access") ])
              "" req
        | _ -> failwith ("unexpected mock request " ^ meth ^ " " ^ url))
  in
  let with_state f =
    Eio.Switch.run @@ fun sw ->
    let db = Sqlite3_eio.open_path ~sw path in
    let store = Store.create ~now:(fun () -> 100000.) db ~admin in
    let source =
      R.initialize ~sw ~fetch ~clock:(Eio.Stdenv.clock env)
        (decode
           {|{"url":"https://example.test/","user":"owner@example.test","password":"synthetic-app-password","max_bytes":1048576}|})
    in
    f store (Store.caldav store) source
  in
  with_state (fun store state source ->
      let tools =
        Caldav_tools.create ~state
          ~sources:[ ("fastmail", source) ]
          ~default:(Some "fastmail")
      in
      let access =
        Caldav_tools.for_request tools ~actor:admin ~room:"!room:example.test"
          ~event:"$source"
      in
      let before = List.length !requests in
      List.iter
        (fun (name, args) ->
          check "mutation-shaped tool arguments rejected"
            (Result.is_error (Caldav_tools.invoke access name args)))
        [
          ("caldav_delete", "{}");
          ("caldav_sync", {|{"connection":"fastmail","method":"DELETE"}|});
          ( "caldav_sync",
            {|{"connection":"fastmail","url":"https://example.test/delete"}|} );
          ( "caldav_read",
            {|{"mirror":1,"version":1,"headers":{"X-HTTP-Method-Override":"DELETE"}}|}
          );
          ("caldav_sync", {|{"connection":"fastmail","connection":"other"}|});
        ];
      check "invalid tool calls perform no HTTP" (List.length !requests = before);
      let round = ref 0 in
      let config =
        Config.default ~admin ~homeserver:"https://matrix.example.test"
      in
      let engine =
        Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
          ~now:(fun () -> 100000.)
          ~complete:(fun _ _ ->
            incr round;
            if !round = 1 then
              ( None,
                [
                  Openrouter.Tool.
                    {
                      id = "delete";
                      name = "caldav_delete";
                      arguments =
                        {|{"url":"https://example.test/home/calendar/one.ics"}|};
                    };
                  Openrouter.Tool.
                    {
                      id = "override";
                      name = "caldav_sync";
                      arguments =
                        {|{"connection":"fastmail","method":"DELETE"}|};
                    };
                ] )
            else (Some "No remote changes.", []))
        |> fun e -> Engine.with_caldav e tools
      in
      Engine.handle engine ~direct:true
        ~send:(fun _ -> ())
        Engine.
          {
            room = "!room:example.test";
            sender = admin;
            id = "$malicious-tool";
            body = "Delete my calendar";
          };
      check "model cannot dispatch invented writes"
        (!round = 2 && List.length !requests = before);
      failed := true;
      let result =
        Caldav_tools.invoke access "caldav_sync" "{}" |> Result.get_ok |> decode
      in
      check "mirror retained on failure" (int (field "mirror" result) = 1);
      let c = List.hd (S.cursors state ~actor:admin 1) in
      check "failed download leaves token unchanged and queue saved"
        (c.token = None && c.staged
        && List.length (S.pending state ~actor:admin c) = 1);
      check "nothing published"
        (S.search state ~actor:admin 1 ~query:"" ~after:0 = []);
      check "one persistent reminder"
        (List.length (Store.reminders store ~actor:admin) = 1));
  with_state (fun store state source ->
      let before_scope = List.length !requests in
      List.iter
        (fun target ->
          check "foreign resource rejected"
            (try
               ignore (R.get source ~collection:href target);
               false
             with _ -> true))
        [
          "https://foreign.test/private.ics";
          "https://example.test/home/calendar/../private.ics";
          "https://example.test/home/calendar/%2e%2e/private.ics";
          "https://example.test/home/calendar/escape%2fprivate.ics";
        ];
      check "scope rejection before network"
        (List.length !requests = before_scope);
      failed := false;
      let before =
        List.length (List.filter (fun (m, _) -> m = "REPORT") !requests)
      in
      let tools =
        Caldav_tools.create ~state
          ~sources:[ ("fastmail", source) ]
          ~default:(Some "fastmail")
      in
      check "resume succeeds" (Caldav_tools.poll tools ~actor:admin 1 = Ok ());
      check "resume uses stored page"
        (List.length (List.filter (fun (m, _) -> m = "REPORT") !requests)
        = before);
      let cursor () = List.hd (S.cursors state ~actor:admin 1) in
      check "cursor advanced after bytes"
        ((cursor ()).token = Some "urn:one" && not (cursor ()).rebuilding);
      let entries =
        S.search state ~actor:admin 1 ~query:"Observatory" ~after:0
      in
      check "recurrence exception searchable" (List.length entries = 1);
      let entry = List.hd entries in
      let access =
        Caldav_tools.for_request tools ~actor:admin ~room:"!room:example.test"
          ~event:"$read"
      in
      List.iter
        (fun mirror ->
          check "CalDAV IDs require exact JSON integers"
            (Result.is_error
               (Caldav_tools.invoke access "caldav_status"
                  (Printf.sprintf {|{"mirror":%s}|} mirror))))
        [ "1.5"; {|"1"|}; "null" ];
      let rec read offset acc =
        let page =
          Caldav_tools.invoke access "caldav_read"
            (Printf.sprintf {|{"mirror":1,"version":%d,"offset":%d}|}
               entry.version offset)
          |> Result.get_ok
        in
        check "bounded encoded pages" (String.length page <= 4096);
        let json = decode page in
        let acc = acc ^ str (field "text" json) in
        match field "next_offset" json with
        | Jsont.Null _ -> acc
        | j ->
            let next = int j in
            check "progress" (next > offset);
            read next acc
      in
      check "exact ICS round trip" (read 0 "" = raw);
      let config =
        Config.default ~admin ~homeserver:"https://matrix.example.test"
      in
      let engine =
        Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
          ~now:(fun () -> 100000.)
          ~complete:(fun _ _ ->
            failwith "CalDAV cron must not invoke the model")
        |> fun e -> Engine.with_caldav e tools
      in
      let job =
        Option.get
          (Store.get_reminder store (S.get state ~actor:admin 1).job_id)
      in
      ignore
        (Engine.fire engine ~run_id:1
           ~send:(fun _ -> failwith "CalDAV cron must not send messages")
           job);
      check "cron uses the same mirror policy"
        ((cursor ()).token = Some "urn:one");
      delta := true;
      check "incremental deletion sync"
        (Caldav_tools.poll tools ~actor:admin 1 = Ok ());
      check "deleted event not searchable"
        (S.search state ~actor:admin 1 ~query:"" ~after:0 = []);
      delta := false;
      check "restore event" (Caldav_tools.poll tools ~actor:admin 1 = Ok ());
      let c = cursor () in
      S.reset state ~actor:admin c;
      check "rebuild keeps complete snapshot"
        (S.search state ~actor:admin 1 ~query:"telescope" ~after:0 <> []);
      check "stale revision rejected"
        (reject (fun () -> S.reset state ~actor:admin c));
      check "nonfriend denied"
        (reject (fun () ->
             S.search state ~actor:"@stranger:example.test" 1 ~query:"" ~after:0));
      let c = cursor () in
      S.stage state ~actor:admin c
        {
          R.token = Some "urn:partial";
          more = true;
          inventory = false;
          changes = [];
        };
      S.commit state ~actor:admin (cursor ()) [];
      check "partial rebuild stays hidden"
        ((cursor ()).token = Some "urn:partial"
        && S.search state ~actor:admin 1 ~query:"" ~after:0 <> []);
      S.stage state ~actor:admin (cursor ())
        {
          R.token = Some "urn:empty";
          more = false;
          inventory = false;
          changes = [];
        };
      S.commit state ~actor:admin (cursor ()) [];
      check "completed rebuild publishes deletions"
        (S.search state ~actor:admin 1 ~query:"" ~after:0 = []);
      check "archived version unavailable to model"
        (reject (fun () ->
             S.read state ~actor:admin 1 ~version:entry.version ~offset:0));
      let c = cursor () in
      let changes =
        List.init 21 (fun i ->
            R.
              {
                href = href ^ string_of_int i ^ ".ics";
                etag = Some "\"same\"";
                removed = false;
              })
      in
      S.stage state ~actor:admin c
        { R.token = None; more = false; inventory = true; changes };
      let c = cursor () in
      let first = S.pending state ~actor:admin c in
      check "bounded inventory downloads" (List.length first = 20);
      let body (position, (change : R.change)) =
        ( position,
          change,
          S.Body
            R.
              {
                href = change.href;
                etag = change.etag;
                raw;
                search = "Office";
                parsed = true;
              } )
      in
      S.commit state ~actor:admin c (List.map body first);
      check "large inventory has pending work"
        ((cursor ()).staged
        && List.length (S.pending state ~actor:admin (cursor ())) = 1);
      check "inventory is hidden until complete"
        (S.search state ~actor:admin 1 ~query:"Office" ~after:0 = []);
      S.commit state ~actor:admin (cursor ())
        (List.map body (S.pending state ~actor:admin (cursor ())));
      check "inventory published"
        (fst (S.counts state ~actor:admin (cursor ())) = 21);
      check "etag reuse"
        (S.held state ~actor:admin (cursor ()) (List.hd changes) <> None);
      Store.set_person store ~actor:admin ~user:"@friend:example.test"
        ~role:Store.Friend ~allowed:true;
      check "friend can read"
        (S.search state ~actor:"@friend:example.test" 1 ~query:"Office" ~after:0
        <> []);
      Store.set_person store ~actor:admin ~user:"@friend:example.test"
        ~role:Store.Friend ~allowed:false;
      check "revoked friend denied"
        (reject (fun () ->
             S.search state ~actor:"@friend:example.test" 1 ~query:"" ~after:0));
      let first_id = (cursor ()).id in
      let first_collection = (cursor ()).collection in
      S.stage state ~actor:admin (cursor ())
        {
          R.token = Some "urn:progress";
          more = true;
          inventory = false;
          changes = [];
        };
      S.commit state ~actor:admin (cursor ()) [];
      S.discover state ~actor:admin 1
        [
          first_collection;
          {
            first_collection with
            href = "https://example.test/home/other/";
            title = "Other";
          };
        ];
      let second = List.hd (S.cursors state ~actor:admin 1) in
      check "large calendar does not starve another calendar"
        (second.id <> first_id);
      S.stage state ~actor:admin second
        {
          R.token = Some "urn:other";
          more = false;
          inventory = false;
          changes = [];
        };
      let second =
        List.find
          (fun (c : S.cursor) -> c.id = second.id)
          (S.cursors state ~actor:admin 1)
      in
      S.commit state ~actor:admin second [];
      let first = List.hd (S.cursors state ~actor:admin 1) in
      check "unfinished work takes priority over fresh calendars"
        (first.id = first_id);
      check "cyclic pagination rejected"
        (reject (fun () ->
             S.stage state ~actor:admin first
               {
                 R.token = Some "urn:progress";
                 more = true;
                 inventory = false;
                 changes = [];
               }));
      check "cancel job"
        (Store.cancel_reminder store ~actor:admin
           (S.get state ~actor:admin 1).job_id);
      check "cancelled job stays cancelled"
        (Result.is_error (Caldav_tools.poll tools ~actor:admin 1)));

  Format.pp_print_flush formatter ();
  let logs = Buffer.contents buffer in
  List.iter
    (fun text ->
      check "calendar progress logged by default" (contains logs text))
    [
      "CalDAV sync started";
      "CalDAV sync discovered";
      "CalDAV sync batch";
      "CalDAV sync committed";
      "downloaded=";
      "reused=";
      "removed=";
      "CalDAV sync finished";
      "CalDAV sync failed";
      "source=\"scheduler\"";
    ];
  List.iter
    (fun text ->
      check "calendar log contents are redacted" (not (contains logs text)))
    [
      "synthetic-app-password";
      "private failure";
      "Office telescope";
      "Observatory";
      "urn:progress";
      "/home/calendar/";
      "BEGIN:VCALENDAR";
    ];
  print_endline "CalDAV mirror tests passed"
