open Crowthebot
module A = Caldav_agenda
module S = Caldav_store
module D = Httpz_dav

let check name b = if not b then failwith name
let admin = "@admin:example.test"
let friend = "@friend:example.test"
let room = "!agenda:example.test"
let decode = Jmap_eio.Codec.decode_exn Jsont.json
let encode = Jmap_eio.Codec.encode_exn Jsont.json

let field k = function
  | Jsont.Object (fs, _) ->
      List.assoc k (List.map (fun ((k, _), v) -> (k, v)) fs)
  | _ -> failwith "object"

let int j = Result.get_ok (Jsont.Json.decode Jsont.int j)
let str = function Jsont.String (s, _) -> s | _ -> failwith "string"
let array = function Jsont.Array (xs, _) -> xs | _ -> failwith "array"
let ok = function Ok x -> x | Error e -> failwith e

let invalid f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let contains text needle =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let raw events =
  "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//test//EN\r\n" ^ events
  ^ "END:VCALENDAR\r\n"

let event props =
  "BEGIN:VEVENT\r\nUID:series\r\nDTSTAMP:20260901T000000Z\r\n" ^ props
  ^ "END:VEVENT\r\n"

let expanded =
  raw
    (event
       "DTSTART:20260910T090000Z\r\n\
        DTEND:20260910T100000Z\r\n\
        RECURRENCE-ID:20260910T090000Z\r\n\
        SUMMARY:Weekly meeting\r\n"
    ^ event
        "DTSTART:20260910T110000Z\r\n\
         DTEND:20260910T120000Z\r\n\
         RECURRENCE-ID:20260910T100000Z\r\n\
         SUMMARY:Moved meeting\r\n"
    ^ event "DTSTART;VALUE=DATE:20260910\r\nSUMMARY:All-day conference\r\n"
    ^ event
        "DTSTART:20260910T140000Z\r\n\
         SUMMARY:Cancelled meeting\r\n\
         STATUS:CANCELLED\r\n"
    ^ String.concat ""
        (List.init 12 (fun i ->
             event
               (Printf.sprintf
                  "DTSTART:20260910T%02d0000Z\r\n\
                   SUMMARY:Meeting %d \"🤖\"\r\n\
                   DESCRIPTION:%s\r\n"
                  (i + 1) i (String.make 500 'x')))))

let original =
  raw
    ("BEGIN:VTIMEZONE\r\n\
      TZID:Europe/London\r\n\
      BEGIN:STANDARD\r\n\
      DTSTART:19591025T020000\r\n\
      TZOFFSETFROM:+0100\r\n\
      TZOFFSETTO:+0000\r\n\
      TZNAME:AncientTimezoneSentinel\r\n\
      END:STANDARD\r\n\
      END:VTIMEZONE\r\n"
    ^ event
        "DTSTART;TZID=Europe/London:20220915T100000\r\n\
         SUMMARY:Weekly meeting\r\n\
         RRULE:FREQ=WEEKLY;BYDAY=TH\r\n\
         EXDATE;TZID=Europe/London:20260917T100000\r\n")

let window =
  A.of_strings ~start:"2026-09-10T00:00:00+01:00"
    ~finish:"2026-09-11T00:00:00+01:00"

let multi children = D.encode_xml (D.element (D.dav "multistatus") children)

let prop href properties =
  D.Element
    (D.element (D.dav "response")
       [
         D.Element (D.element (D.dav "href") [ D.Text href ]);
         D.Element
           (D.element (D.dav "propstat")
              [
                D.Element
                  (D.element (D.dav "prop")
                     (List.map (fun p -> D.Element p) properties));
                D.Element
                  (D.element (D.dav "status") [ D.Text "HTTP/1.1 200 OK" ]);
              ]);
       ])

let collection id =
  Caldav_source.
    {
      href = Printf.sprintf "https://example.test/cal%d/" id;
      title = "Calendar " ^ string_of_int id;
      sync = true;
      properties =
        D.encode_xml
          (D.element (D.dav "prop")
             [
               D.Element
                 (Caldav.Property.timezone_prop
                    (raw
                       "BEGIN:VTIMEZONE\r\n\
                        TZID:Europe/London\r\n\
                        END:VTIMEZONE\r\n"));
             ]);
    }

let () =
  Eio_main.run @@ fun env ->
  check "offset becomes correct UTC day bounds"
    (A.bounds window = ("2026-09-09T23:00:00Z", "2026-09-10T23:00:00Z"));
  check "date-only input cannot silently assume timezone"
    (invalid (fun () -> A.of_strings ~start:"2026-09-10" ~finish:"2026-09-11"));
  check "unbounded range rejected"
    (invalid (fun () ->
         A.of_strings ~start:"2026-01-01T00:00:00Z"
           ~finish:"2027-01-01T00:00:00Z"));
  check "subsecond range cannot silently truncate"
    (invalid (fun () ->
         A.of_strings ~start:"2026-09-10T00:00:00.1Z"
           ~finish:"2026-09-11T00:00:00Z"));
  check "unexpanded series rejected"
    (invalid (fun () -> A.parse ~href:"x" ~etag:None original));
  let occurrences = (A.parse ~href:"x" ~etag:None expanded).occurrences in
  check "cancelled excluded; recurring and override retained"
    (List.length occurrences = 15);
  let all_day =
    List.find
      (fun (o : A.occurrence) ->
        field "all_day" o.summary = Jsont.Json.bool true)
      occurrences
  in
  check "all-day implicit end is exclusive next day"
    (field "end" all_day.summary = Jsont.Json.string "2026-09-11");
  let indexed, parsed = Caldav_text.index original in
  check "semantic index begins with event information"
    (parsed && String.starts_with ~prefix:"DTSTART:" indexed);
  let calls = ref 0
  and mode = ref "normal"
  and now = ref (Ptime.to_float_s window.start +. 43200.) in
  let fetch =
    Fetch_mock.client (fun req ->
        incr calls;
        let href = Fetch.Middleware.Url.to_string req.url in
        let xml body =
          Fetch_mock.respond ~status:207
            ~headers:
              (Http.Header.of_list [ ("content-type", "application/xml") ])
            body req
        in
        check "same configured origin"
          (String.starts_with ~prefix:"https://example.test/" href);
        match Http.Method.to_string req.meth with
        | "PROPFIND" ->
            xml
              (multi
                 [
                   prop href
                     (if href = "https://example.test/principal/" then
                        [
                          D.element Caldav.Property.calendar_home_set
                            [
                              D.Element
                                (D.element (D.dav "href") [ D.Text "/" ]);
                            ];
                        ]
                      else
                        [
                          D.element
                            (D.dav "current-user-principal")
                            [
                              D.Element
                                (D.element (D.dav "href")
                                   [ D.Text "/principal/" ]);
                            ];
                        ]);
                 ])
        | "REPORT" ->
            check "expanded agenda has depth one"
              (Http.Header.get req.headers "depth" = Some "1");
            let body =
              match req.body with Fetch.String s -> s | _ -> assert false
            in
            let q =
              Result.get_ok
                (Caldav.Report.query_of_xml (Result.get_ok (D.parse_xml body)))
            in
            check "exact bounded VEVENT expansion request"
              (Caldav.Report.equal_query q (A.query window));
            if href = "https://example.test/cal2/" then
              if !mode = "partial" then
                Fetch_mock.respond ~status:503 "private failure" req
              else xml (multi [])
            else if !mode = "truncated" then
              xml
                (multi
                   [
                     D.Element
                       (D.element (D.dav "response")
                          [
                            D.Element (D.element (D.dav "href") [ D.Text href ]);
                            D.Element
                              (D.element (D.dav "status")
                                 [ D.Text "HTTP/1.1 507 Insufficient Storage" ]);
                          ]);
                   ])
            else if !mode = "missing" then
              xml
                (multi
                   [
                     prop (href ^ "entry.ics")
                       [ D.element (D.dav "getetag") [ D.Text "etag" ] ];
                   ])
            else
              xml
                (multi
                   [
                     prop (href ^ "entry.ics")
                       [
                         D.element (D.dav "getetag") [ D.Text "etag" ];
                         D.element Caldav.Calendar_data.name
                           [
                             D.Text
                               (if !mode = "unexpanded" then original
                                else expanded);
                           ];
                       ];
                   ])
        | _ -> failwith "unexpected HTTP method")
  in
  let filename = Filename.temp_file "crow-agenda" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) @@ fun () ->
  let path = Eio.Path.(Eio.Stdenv.fs env / filename) in
  let first = ref 0 and source_id = ref 0 in
  let initialize sw =
    let db = Sqlite3_eio.open_path ~sw path in
    let store = Store.create ~now:(fun () -> !now) db ~admin in
    let state = Store.caldav store in
    let source =
      Caldav_source.initialize ~sw ~fetch ~clock:(Eio.Stdenv.clock env)
        (decode
           {|{"url":"https://example.test/","user":"owner","password":"app-fixture","max_bytes":1048576}|})
    in
    let identity = Caldav_source.identity source in
    ignore
      (S.ensure state ~actor:admin ~room ~event:"$origin" ~connection:"fastmail"
         ~identity:identity.key ~principal:identity.principal
         ~cron:"*/15 * * * *" ~next_at:2000.);
    S.discover state ~actor:admin 1 [ collection 1; collection 2 ];
    let tools =
      Caldav_tools.create ~state
        ~sources:[ ("fastmail", source) ]
        ~default:(Some "fastmail")
    in
    let invoke actor name args =
      Caldav_tools.invoke
        (Caldav_tools.for_request tools ~actor ~room ~event:"$agenda")
        name args
    in
    (db, store, state, source, tools, invoke)
  in
  Eio.Switch.run (fun sw ->
      let db, store, state, source, tools, invoke = initialize sw in
      let before = !calls in
      check "unknown actor denied before network"
        (Result.is_error (invoke friend "caldav_agenda" "{}") && !calls = before);
      let args =
        {|{"start":"2026-09-10T00:00:00+01:00","end":"2026-09-11T00:00:00+01:00"}|}
      in
      let answer = invoke admin "caldav_agenda" args |> ok |> decode in
      first := int (field "snapshot" answer);
      check "fresh agenda is complete across both calendars"
        (field "complete" answer = Jsont.Json.bool true
        && int (field "total" answer) = 15
        && !calls = before + 2);
      let count = ref 0 in
      let rec pages p =
        check "escaped page fits engine cap" (String.length (encode p) <= 3800);
        let events = array (field "events" p) in
        count := !count + List.length events;
        if !source_id = 0 then
          source_id := int (field "resource" (List.hd events));
        match field "next_offset" p with
        | Jsont.Null _ -> ()
        | n ->
            invoke admin "caldav_agenda"
              (Printf.sprintf {|{"snapshot":%d,"offset":%d}|} !first (int n))
            |> ok |> decode |> pages
      in
      pages answer;
      check "pagination lossless and network-free"
        (!count = 15 && !calls = before + 2);
      ignore (invoke admin "caldav_agenda" args |> ok);
      check "fresh windows reuse cache" (!calls = before + 2);
      let full = Buffer.create 4096 in
      let rec read offset =
        let p =
          invoke admin "caldav_agenda_read"
            (Printf.sprintf {|{"snapshot":%d,"resource":%d,"offset":%d}|} !first
               !source_id offset)
          |> ok |> decode
        in
        check "raw escaped page fits engine cap"
          (String.length (encode p) <= 3800);
        Buffer.add_string full (str (field "text" p));
        match field "next_offset" p with
        | Jsont.Null _ -> ()
        | n -> read (int n)
      in
      read 0;
      check "full expanded source retained"
        (Ical.equal
           (Result.get_ok (Ical.one_of_string (Buffer.contents full)))
           (Result.get_ok (Ical.one_of_string expanded)));
      let context = Caldav_tools.context tools ~actor:admin in
      check "model receives mirror IDs and timezone"
        (contains context {|"mirror":1|} && contains context "Europe/London");
      let rounds = ref 0 in
      let model_fetch =
        Fetch_mock.client (fun req ->
            incr rounds;
            let body =
              match req.body with
              | Fetch.String s -> decode s
              | _ -> assert false
            in
            let messages = array (field "messages" body) in
            let system = str (field "content" (List.hd messages)) in
            check
              "wire context has current date, mirror, timezone and agenda \
               guidance"
              (List.for_all (contains system)
                 [
                   "Current UTC time: 2026-09-10";
                   context;
                   "call caldav_agenda directly";
                 ]);
            let names =
              array (field "tools" body)
              |> List.map (fun t -> str (field "name" (field "function" t)))
            in
            check "agenda tools reach the model"
              (List.mem "caldav_agenda" names
              && List.mem "caldav_agenda_read" names);
            let message =
              if !rounds = 1 then
                {|{"role":"assistant","content":"","tool_calls":[{"id":"agenda","type":"function","function":{"name":"caldav_agenda","arguments":"{\"start\":\"2026-09-10T00:00:00+01:00\",\"end\":\"2026-09-11T00:00:00+01:00\"}"}}]}|}
              else
                let result =
                  List.find
                    (fun m -> field "role" m = Jsont.Json.string "tool")
                    messages
                in
                let result = decode (str (field "content" result)) in
                check "expanded cached data reaches the model intact"
                  (field "complete" result = Jsont.Json.bool true
                  && int (field "total" result) = 15
                  && field "next_offset" result <> Jsont.Json.null ());
                {|{"role":"assistant","content":"Agenda page received."}|}
            in
            Fetch_mock.respond
              ~headers:
                (Http.Header.of_list [ ("content-type", "application/json") ])
              (Printf.sprintf
                 {|{"id":"agenda","model":"test/model","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"%s","message":%s}]}|}
                 (if !rounds = 1 then "tool_calls" else "stop")
                 message)
              req)
      in
      let config =
        Config.default ~admin ~homeserver:"https://matrix.example.test"
      in
      let engine =
        Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
          ~now:(fun () -> !now)
          ~complete:
            (App.complete env config
               (Openrouter.of_fetch ~base_url:"https://model.example.test/v1"
                  model_fetch))
        |> fun engine -> Engine.with_caldav engine tools
      in
      let delivered = ref false in
      Engine.handle engine ~direct:true
        ~send:(fun _ -> delivered := true)
        Engine.
          {
            room;
            sender = admin;
            id = "$wire-agenda";
            body = "What's on my calendar today in Europe/London?";
          };
      check "engine delivers agenda tool result without extra calendar requests"
        (!delivered && !rounds = 2 && !calls = before + 2);
      List.iter
        (fun failure ->
          now := !now +. 301.;
          mode := failure;
          let p = invoke admin "caldav_agenda" args |> ok |> decode in
          check
            "failed, missing, truncated or unexpanded results never claim \
             completeness"
            (field "complete" p = Jsont.Json.bool false))
        [ "partial"; "missing"; "truncated"; "unexpanded" ];
      mode := "normal";
      Store.observe store friend;
      Store.set_person store ~actor:admin ~user:friend ~role:Store.Friend
        ~allowed:true;
      check "friends share cached agenda"
        (Result.is_ok
           (invoke friend "caldav_agenda"
              (Printf.sprintf {|{"snapshot":%d}|} !first)));
      Store.set_person store ~actor:admin ~user:friend ~role:Store.Friend
        ~allowed:false;
      check "revocation blocks cached reads"
        (Result.is_error
           (invoke friend "caldav_agenda_read"
              (Printf.sprintf {|{"snapshot":%d,"resource":%d}|} !first
                 !source_id)));
      let restricted =
        Caldav_http.read_only ~url:"https://example.test/" fetch
      in
      let send xml =
        let r =
          Fetch.fetch ~sw
            ~headers:Fetch.Header.[ raw "depth" "1" ]
            ~body:(Fetch.String xml) restricted
            (Http.Method.of_string "REPORT")
            "https://example.test/cal1/"
        in
        Fetch.close r
      in
      let denied f =
        try
          f ();
          false
        with Eio.Io (Fetch.E (Denied _ | Invalid_request _), _) -> true
      in
      let before = !calls in
      let wrong =
        Caldav.Report.query
          ~data:
            (Caldav.Calendar_data.v
               ~expand:
                 (Caldav.Filter.time_range
                    ~start:(Ical.Date.of_ptime window.start)
                    ~finish:(Ical.Date.of_ptime window.finish)
                    ())
               ())
          (Caldav.Filter.components "VTODO")
      in
      check "arbitrary REPORT still blocked"
        (denied (fun () ->
             send (D.encode_xml (Caldav.Report.query_to_xml wrong)))
        && !calls = before);
      let extended = Caldav.Report.query_to_xml (A.query window) in
      let extended =
        {
          extended with
          D.children =
            extended.children @ [ D.Element (D.element (D.dav "delete") []) ];
        }
      in
      check "unknown query extensions blocked"
        (denied (fun () -> send (D.encode_xml extended)) && !calls = before);
      let date value = Result.get_ok (Ical.Date.date_time_of_string value) in
      let range start finish =
        Caldav.Filter.time_range ~start:(date start) ~finish:(date finish) ()
      in
      let allowed_range = range "20260909T230000Z" "20260910T230000Z" in
      let make expand filter =
        Caldav.Report.query
          ~data:(Caldav.Calendar_data.v ~expand ())
          (Caldav.Filter.components ~time_range:filter "VEVENT")
        |> Caldav.Report.query_to_xml |> D.encode_xml
      in
      let huge = range "20260101T000000Z" "20270101T000000Z" in
      check "oversized expansion blocked"
        (denied (fun () -> send (make huge huge)) && !calls = before);
      check "filter cannot exceed expansion window"
        (denied (fun () -> send (make allowed_range huge)) && !calls = before);
      let unbounded =
        Caldav.Filter.time_range ~start:(date "20260909T230000Z") ()
      in
      check "open-ended expansion blocked"
        (denied (fun () -> send (make unbounded allowed_range))
        && !calls = before);
      let local = range "20260909T230000" "20260910T230000" in
      check "floating request bounds blocked"
        (denied (fun () -> send (make local local)) && !calls = before);
      ignore source;
      (* Populate an old-style index, then require the migration to remove zone
       definitions without altering source bytes or object IDs. *)
      let c = List.hd (S.cursors state ~actor:admin 1) in
      let item =
        Caldav_source.
          {
            href = c.collection.href ^ "original.ics";
            etag = Some "original";
            raw = original;
            search = "AncientTimezoneSentinel Weekly meeting";
            parsed = true;
          }
      in
      let change =
        Caldav_source.{ href = item.href; etag = item.etag; removed = false }
      in
      S.stage state ~actor:admin c
        {
          Caldav_source.token = Some "s1";
          more = false;
          inventory = false;
          changes = [ change ];
        };
      let c =
        List.find
          (fun (v : S.cursor) -> v.id = c.id)
          (S.cursors state ~actor:admin 1)
      in
      S.commit state ~actor:admin c [ (0, change, S.Body item) ];
      Sqlite3.Rc.check
        (Sqlite3_eio.exec db
           "UPDATE tool_schemas SET version=1 WHERE name='caldav'"));
  Eio.Switch.run (fun sw ->
      let db, _, state, _, _, invoke = initialize sw in
      let before = !calls in
      check "snapshot survives restart"
        (Result.is_ok
           (invoke admin "caldav_agenda"
              (Printf.sprintf {|{"snapshot":%d}|} !first)));
      check "persisted pages need no calendar HTTP" (!calls = before);
      check "zone terms removed by migration"
        (S.search state ~actor:admin 1 ~query:"AncientTimezoneSentinel" ~after:0
        = []);
      let entries = S.search state ~actor:admin 1 ~query:"meeting" ~after:0 in
      check "event text still indexed" (entries <> []);
      let body, size =
        S.read state ~actor:admin 1 ~version:(List.hd entries).version ~offset:0
      in
      check "migration preserves source bytes"
        (body = original && size = String.length original);
      for _ = 1 to 21 do
        ignore
          (Caldav_agenda_store.save (S.agendas state) ~actor:admin ~room
             ~event:"$eviction" ~mirror:1 ~collection:0 ~window
             ~metadata:(decode "{}") ~complete:true [])
      done;
      check "expired snapshot fails explicitly"
        (Result.is_error
           (invoke admin "caldav_agenda"
              (Printf.sprintf {|{"snapshot":%d}|} !first)));
      let scalar sql =
        let stmt = Sqlite3_eio.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3_eio.finalize db stmt))
          (fun () ->
            check "scalar query has one row"
              (Sqlite3_eio.step db stmt = Sqlite3.Rc.ROW);
            Sqlite3.column_int stmt 0)
      in
      check "cache keeps at most twenty snapshots"
        (scalar "SELECT count(*) FROM caldav_agendas" = 20);
      check "eviction removes expanded source and occurrences"
        (scalar "SELECT count(*) FROM caldav_agenda_resources" = 0
        && scalar "SELECT count(*) FROM caldav_occurrences" = 0));
  print_endline
    "CalDAV agenda expansion, paging, cache, partial results, indexing and \
     read-only policy passed."
