open Crowthebot

let check name value = if not value then failwith name

let contains text needle =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let multi body =
  "<d:multistatus xmlns:d='DAV:' xmlns:c='urn:ietf:params:xml:ns:caldav'>"
  ^ body ^ "</d:multistatus>"

let prop href body =
  "<d:response><d:href>" ^ href ^ "</d:href><d:propstat><d:prop>" ^ body
  ^ "</d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response>"

let raw =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//Crow probe test//EN\r\n\
   BEGIN:VEVENT\r\n\
   UID:private-event-uid\r\n\
   DTSTAMP:20260909T120000Z\r\n\
   DTSTART:20260910T100000Z\r\n\
   SUMMARY:private-event-title\r\n\
   END:VEVENT\r\n\
   END:VCALENDAR\r\n"

let settings =
  Result.get_ok
    (Jsont_bytesrw.decode_string Jsont.json
       {|{"url":"https://example.test/","user":"owner@example.test","password":"synthetic-app-password","max_bytes":1048576}|})

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fixture ?(sync = true) ?(empty = false) ?(no_calendars = false)
      ?(sample = raw) ?(root_discovery = false) ?failure () =
    let requests = ref [] in
    let fetch =
      Fetch_mock.client (fun req ->
          let meth = Http.Method.to_string req.meth in
          let path =
            Uri.path (Uri.of_string (Fetch.Middleware.Url.to_string req.url))
          in
          requests := (meth, path) :: !requests;
          check "probe uses read-only HTTP methods"
            (List.mem meth [ "GET"; "PROPFIND"; "REPORT" ]);
          let xml body =
            Fetch_mock.respond ~status:207
              ~headers:
                (Http.Header.of_list [ ("Content-Type", "application/xml") ])
              (multi body) req
          in
          match failure with
          | Some (failed_meth, failed_path, status)
            when meth = failed_meth && path = failed_path ->
              Fetch_mock.respond ~status
                "private-server-error synthetic-app-password" req
          | _ -> (
              match (meth, path) with
              | "PROPFIND", "/" when root_discovery ->
                  Fetch_mock.respond ~status:404 "private-root-error" req
              | "GET", "/.well-known/caldav" ->
                  Fetch_mock.respond ~status:301
                    ~headers:(Http.Header.of_list [ ("Location", "/dav/") ])
                    "" req
              | "PROPFIND", (("/" | "/dav/") as path) ->
                  xml
                    (prop path
                       "<d:current-user-principal><d:href>/principal/</d:href></d:current-user-principal>")
              | "PROPFIND", "/principal/" ->
                  xml
                    (prop "/principal/"
                       "<c:calendar-home-set><d:href>/home/</d:href></c:calendar-home-set>")
              | "PROPFIND", "/home/" ->
                  if no_calendars then xml ""
                  else
                    xml
                      (prop "/home/calendar/"
                         ("<d:resourcetype><d:collection/><c:calendar/></d:resourcetype><d:displayname>private-calendar-title</d:displayname>"
                         ^
                         if sync then
                           "<d:supported-report-set><d:supported-report><d:report><d:sync-collection/></d:report></d:supported-report></d:supported-report-set>"
                         else ""))
              | ("REPORT" | "PROPFIND"), "/home/calendar/" ->
                  xml
                    ((if empty then ""
                      else
                        prop "/home/calendar/one.ics"
                          "<d:getetag>\"one\"</d:getetag>"
                        ^ prop "/home/calendar/two.ics"
                            "<d:getetag>\"two\"</d:getetag>")
                    ^
                    if sync then
                      "<d:sync-token>private-sync-token</d:sync-token>"
                    else "")
              | "GET", "/home/calendar/one.ics" ->
                  Fetch_mock.respond
                    ~headers:
                      (Http.Header.of_list
                         [ ("Content-Type", "text/calendar") ])
                    sample req
              | _ -> failwith "Unexpected probe request"))
    in
    let source () =
      Caldav_source.initialize ~sw ~fetch ~clock:(Eio.Stdenv.clock env) settings
    in
    (source, requests)
  in
  let probe sources =
    let lines = ref [] in
    let ok =
      Caldav_probe.run ~emit:(fun line -> lines := line :: !lines) sources
    in
    let text = String.concat "\n" (List.rev !lines) in
    List.iter
      (fun secret ->
        check "diagnostics redact private data" (not (contains text secret)))
      [
        "private-";
        "synthetic-app-password";
        "owner@example.test";
        "example.test/";
      ];
    (ok, text)
  in
  let source, requests = fixture () in
  let ok, text = probe [ ("fastmail", source) ] in
  check "sync probe succeeds" ok;
  check "sync counts"
    (contains text "calendars=1 incremental=1 inventory=0 samples_read=1");
  check "one sample despite multiple objects"
    (List.rev !requests
    = [
        ("PROPFIND", "/");
        ("PROPFIND", "/principal/");
        ("PROPFIND", "/home/");
        ("REPORT", "/home/calendar/");
        ("GET", "/home/calendar/one.ics");
      ]);
  let source, requests = fixture ~root_discovery:true () in
  let ok, text = probe [ ("fastmail", source) ] in
  check "Fastmail root discovery works through the read-only policy"
    (ok
    && contains text "samples_read=1"
    && List.mem ("GET", "/.well-known/caldav") !requests);
  let source, requests = fixture ~sync:false () in
  let ok, text = probe [ ("legacy", source) ] in
  check "inventory fallback succeeds"
    (ok && contains text "calendars=1 incremental=0 inventory=1 samples_read=1");
  check "inventory fallback makes no REPORT"
    (List.for_all (fun (meth, _) -> meth <> "REPORT") !requests);
  List.iter
    (fun no_calendars ->
      let source, requests = fixture ~empty:true ~no_calendars () in
      let ok, text = probe [ ("empty", source) ] in
      check "empty accounts and calendars are valid"
        (ok && contains text "samples_read=0");
      check "empty listing needs no GET"
        (List.for_all (fun (meth, _) -> meth <> "GET") !requests))
    [ false; true ];
  List.iter
    (fun (meth, path, status, stage) ->
      let failed, _ = fixture ~failure:(meth, path, status) () in
      let good, _ = fixture () in
      let ok, text = probe [ ("broken", failed); ("working", good) ] in
      check "failure affects exit status" (not ok);
      check "failure identifies stage and HTTP status"
        (contains text
           (Printf.sprintf "FAILED (%s: CalDAV HTTP %d." stage status));
      check "failure does not prevent next connection"
        (contains text "CalDAV \"working\": OK"))
    [
      ("PROPFIND", "/", 401, "authentication");
      ("PROPFIND", "/home/", 403, "discovery");
      ("PROPFIND", "/home/", 404, "discovery");
      ("PROPFIND", "/home/", 412, "discovery");
      ("REPORT", "/home/calendar/", 403, "calendar 1 sync report");
      ("GET", "/home/calendar/one.ics", 403, "calendar 1 sample read");
    ];
  let source, _ = fixture ~sample:"private-invalid-calendar" () in
  let ok, text = probe [ ("invalid", source) ] in
  check "malformed sample has useful safe diagnostics"
    ((not ok)
    && contains text "sample read: The sample is not valid iCalendar data.");
  let source, _ = fixture ~failure:("GET", "/home/calendar/one.ics", 404) () in
  let ok, text = probe [ ("raced", source) ] in
  check "sample disappearing during probe is harmless"
    (ok && contains text "samples_read=0");
  let ok, text =
    probe [ ("broken", fun () -> failwith "private-configuration") ]
  in
  check "configuration errors are contained"
    ((not ok) && contains text "FAILED (configuration:");
  let ok, text = probe [] in
  check "unconfigured is reported as skipped" (ok && contains text "skipped");
  let cancelled = Eio.Cancel.Cancelled (Failure "test cancellation") in
  let propagated =
    try
      ignore (probe [ ("cancel", fun () -> raise cancelled) ]);
      false
    with Eio.Cancel.Cancelled _ -> true
  in
  check "cancellation propagates" propagated;
  print_endline
    "CalDAV probe checks discovery, sync and sample reads with private \
     diagnostics."
