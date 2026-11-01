(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The CalDAV client against a scripted server, so that discovery, the
   bodies each request carries and the free-busy answer are checked without
   a network. *)

module Client = Caldav_eio.Client
module Data = Caldav.Data

module Xml = struct
  include Httpz_dav

  let of_string s = parse_xml s
  let is n (e : element) = e.name = n
end

let xml_headers =
  Http.Header.of_list [ ("content-type", "text/xml; charset=utf-8") ]

let base = "https://dav.example.com"

let path (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query req.url

let body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> ""
  | Fetch.String b -> b
  | Fetch.Stream _ -> Alcotest.fail "unexpected streaming request"

let meth (req : Fetch.Middleware.request) = Http.Method.to_string req.meth

let respond ?(status = 207) xml req =
  Fetch_mock.respond ~status ~headers:xml_headers xml req

let principal_body =
  {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:"><D:response><D:href>/dav/</D:href><D:propstat><D:prop><D:current-user-principal><D:href>/principals/alice/</D:href></D:current-user-principal></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}

let home_body =
  {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:response><D:href>/principals/alice/</D:href><D:propstat><D:prop><C:calendar-home-set><D:href>/calendars/alice/</D:href></C:calendar-home-set></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}

let cals_body =
  {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:response><D:href>/calendars/alice/</D:href><D:propstat><D:prop><D:resourcetype><D:collection/></D:resourcetype></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response><D:response><D:href>/calendars/alice/work/</D:href><D:propstat><D:prop><D:resourcetype><D:collection/><C:calendar/></D:resourcetype><D:displayname>Work</D:displayname><C:supported-calendar-component-set><C:comp name="VEVENT"/></C:supported-calendar-component-set></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}

let ics =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//idk//EN\r\n\
   BEGIN:VEVENT\r\n\
   UID:e1\r\n\
   DTSTAMP:20260901T120000Z\r\n\
   DTSTART:20260910T090000Z\r\n\
   DTEND:20260910T100000Z\r\n\
   SUMMARY:Meeting\r\n\
   END:VEVENT\r\n\
   END:VCALENDAR\r\n"

let query_body =
  Printf.sprintf
    {|<?xml version="1.0"?><D:multistatus xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:response><D:href>/calendars/alice/work/e1.ics</D:href><D:propstat><D:prop><D:getetag>"e1"</D:getetag><C:calendar-data>%s</C:calendar-data></D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response></D:multistatus>|}
    ics

let freebusy =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//idk//EN\r\n\
   BEGIN:VFREEBUSY\r\n\
   UID:fb\r\n\
   DTSTAMP:20260901T120000Z\r\n\
   DTSTART:20260901T000000Z\r\n\
   DTEND:20261001T000000Z\r\n\
   FREEBUSY:20260910T090000Z/20260910T100000Z\r\n\
   END:VFREEBUSY\r\n\
   END:VCALENDAR\r\n"

let server seen req =
  seen := (meth req, path req, body req) :: !seen;
  match (meth req, path req) with
  | "GET", "/.well-known/caldav" ->
      Fetch_mock.respond ~status:301
        ~headers:(Http.Header.of_list [ ("location", "/dav/") ])
        "" req
  | "PROPFIND", "/dav/" -> respond principal_body req
  | "PROPFIND", "/principals/alice/" -> respond home_body req
  | "PROPFIND", "/calendars/alice/" -> respond cals_body req
  | "MKCALENDAR", "/calendars/alice/new/" ->
      Fetch_mock.respond ~status:201 "" req
  | "REPORT", "/calendars/alice/work/" ->
      if
        String.length (body req) > 0
        && Option.is_some (String.index_opt (body req) 'f')
        &&
          try
            ignore
              (Str.search_forward
                 (Str.regexp_string "free-busy-query")
                 (body req) 0);
            true
          with Not_found -> false
      then
        Fetch_mock.respond ~status:200
          ~headers:(Http.Header.of_list [ ("content-type", "text/calendar") ])
          freebusy req
      else respond query_body req
  | _ -> Fetch_mock.respond ~status:500 "unexpected" req

let with_client f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let transport = Fetch_mock.client (server seen) in
  match
    Client.connect ~sw
      ~credentials:[ Fetch.Credential.basic ~user:"alice" ~password:"x" ]
      transport
      (base ^ "/.well-known/caldav")
  with
  | Error e -> Alcotest.failf "connect: %s" (Client.error_to_string e)
  | Ok client -> f client seen

let ok what = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %s" what (Client.error_to_string e)

let sent seen m p =
  List.find_map
    (fun (m', p', b) -> if m = m' && p = p' then Some b else None)
    !seen

let test_discovery () =
  with_client @@ fun client _ ->
  Alcotest.(check string)
    "principal"
    (base ^ "/principals/alice/")
    (Client.principal client);
  Alcotest.(check (list string))
    "home"
    [ base ^ "/calendars/alice/" ]
    (Client.home_sets client)

let test_calendars () =
  with_client @@ fun client _ ->
  let cals = ok "calendars" (Client.calendars client) in
  Alcotest.(check int) "one" 1 (List.length cals);
  let c = List.hd cals in
  Alcotest.(check string) "href" (base ^ "/calendars/alice/work/") c.href;
  Alcotest.(check (option string)) "name" (Some "Work") c.display_name;
  Alcotest.(check (list string)) "components" [ "VEVENT" ] c.components

let test_mkcalendar () =
  with_client @@ fun client seen ->
  ok "create"
    (Client.create_calendar client ~display_name:"New"
       ~components:[ "VEVENT"; "VTODO" ]
       (base ^ "/calendars/alice/new/"));
  match sent seen "MKCALENDAR" "/calendars/alice/new/" with
  | None -> Alcotest.fail "no MKCALENDAR"
  | Some b -> (
      let x = Result.get_ok (Xml.of_string b) in
      Alcotest.(check bool)
        "mkcalendar root" true
        (Xml.is (Caldav.Property.caldav "mkcalendar") x);
      let prop =
        Option.get
          (Xml.find (Xml.dav "prop") (Option.get (Xml.find (Xml.dav "set") x)))
      in
      Alcotest.(check (option string))
        "displayname" (Some "New")
        (Option.map Xml.content (Xml.find Httpz_dav.Prop.displayname prop));
      match Xml.find Caldav.Property.supported_calendar_component_set prop with
      | Some set ->
          Alcotest.(check (list string))
            "components" [ "VEVENT"; "VTODO" ]
            (Caldav.Property.component_set set)
      | None -> Alcotest.fail "no component set")

let test_query () =
  with_client @@ fun client seen ->
  let start =
    {
      Ical.Date.date = { year = 2026; month = 9; day = 1 };
      time = { hour = 0; minute = 0; second = 0; utc = true };
    }
  in
  let finish = { start with date = { year = 2026; month = 10; day = 1 } } in
  let es =
    ok "events"
      (Client.events Data.ical client ~start ~finish ~expand:true
         (base ^ "/calendars/alice/work/"))
  in
  Alcotest.(check int) "one" 1 (List.length es);
  let e = List.hd es in
  Alcotest.(check (option string)) "etag" (Some "\"e1\"") e.etag;
  Alcotest.(check (option string)) "uid" (Some "e1") (Ical.uid e.value);
  match sent seen "REPORT" "/calendars/alice/work/" with
  | None -> Alcotest.fail "no REPORT"
  | Some b -> (
      let x = Result.get_ok (Xml.of_string b) in
      Alcotest.(check bool)
        "calendar-query" true
        (Xml.is (Caldav.Property.caldav "calendar-query") x);
      let q = Result.get_ok (Caldav.Report.query_of_xml x) in
      (match q.filter.comp_condition with
      | `Matches (None, [ ev ], []) -> (
          Alcotest.(check string) "VEVENT" "VEVENT" ev.comp;
          match ev.comp_condition with
          | `Matches (Some tr, [], []) ->
              Alcotest.(check bool) "start" true (tr.start = Some start);
              Alcotest.(check bool) "end" true (tr.finish = Some finish)
          | _ -> Alcotest.fail "time range expected")
      | _ -> Alcotest.fail "VCALENDAR/VEVENT expected");
      match q.props with
      | Caldav.Report.Prop (_, Some d) ->
          Alcotest.(check bool) "expand" true (d.expand <> None)
      | _ -> Alcotest.fail "calendar-data expected")

let test_free_busy () =
  with_client @@ fun client _ ->
  let start =
    {
      Ical.Date.date = { year = 2026; month = 9; day = 1 };
      time = { hour = 0; minute = 0; second = 0; utc = true };
    }
  in
  let fb =
    ok "free-busy"
      (Client.free_busy client ~start (base ^ "/calendars/alice/work/"))
  in
  match Ical.free_busy fb with
  | [ c ] -> (
      match Ical.Component.find c "FREEBUSY" with
      | Some p -> (
          match Ical.Property.periods p with
          | Ok [ per ] -> Alcotest.(check int) "busy hour" 9 per.start.time.hour
          | Ok _ -> Alcotest.fail "one period expected"
          | Error e -> Alcotest.fail e)
      | None -> Alcotest.fail "no FREEBUSY")
  | _ -> Alcotest.fail "one VFREEBUSY expected"

let () =
  Alcotest.run "caldav-eio"
    [
      ( "client",
        [
          Alcotest.test_case "discovery" `Quick test_discovery;
          Alcotest.test_case "calendars" `Quick test_calendars;
          Alcotest.test_case "mkcalendar" `Quick test_mkcalendar;
          Alcotest.test_case "query" `Quick test_query;
          Alcotest.test_case "free-busy" `Quick test_free_busy;
        ] );
    ]
