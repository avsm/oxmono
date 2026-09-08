(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ok name = function
  | Ok v -> v
  | Error msg -> Alcotest.failf "%s: %s" name msg

let check_bool name b = Alcotest.(check bool) name true b

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  nl = 0
  ||
  let rec go i =
    i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
  in
  go 0

let dt s = ok ("date_time_of_string " ^ s) (Ical.Date.date_time_of_string s)

(* {1 RFC 4791 Section 7.8 calendar-query request examples} *)

let request_7_8_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop>
    <D:getetag/>
    <C:calendar-data>
      <C:comp name="VCALENDAR">
        <C:prop name="VERSION"/>
        <C:comp name="VEVENT">
          <C:prop name="SUMMARY"/>
          <C:prop name="UID"/>
          <C:prop name="DTSTART"/>
          <C:prop name="DTEND"/>
          <C:prop name="DURATION"/>
          <C:prop name="RRULE"/>
          <C:prop name="RDATE"/>
          <C:prop name="EXRULE"/>
          <C:prop name="EXDATE"/>
          <C:prop name="RECURRENCE-ID"/>
        </C:comp>
        <C:comp name="VTIMEZONE"/>
      </C:comp>
    </C:calendar-data>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT">
        <C:time-range start="20060104T000000Z"
                      end="20060105T000000Z"/>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_2 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop>
    <C:calendar-data>
      <C:limit-recurrence-set start="20060103T000000Z"
                              end="20060105T000000Z"/>
    </C:calendar-data>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT">
        <C:time-range start="20060103T000000Z"
                      end="20060105T000000Z"/>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_3 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop>
    <C:calendar-data>
      <C:expand start="20060103T000000Z"
                end="20060105T000000Z"/>
    </C:calendar-data>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT">
        <C:time-range start="20060103T000000Z"
                      end="20060105T000000Z"/>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_4 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop>
    <C:calendar-data>
      <C:limit-freebusy-set start="20060102T000000Z"
                              end="20060103T000000Z"/>
    </C:calendar-data>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VFREEBUSY">
        <C:time-range start="20060102T000000Z"
                        end="20060103T000000Z"/>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_5 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VTODO">
        <C:comp-filter name="VALARM">
          <C:time-range start="20060106T100000Z"
                          end="20060107T100000Z"/>
        </C:comp-filter>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_6 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT">
        <C:prop-filter name="UID">
          <C:text-match collation="i;octet"
          >DC6C50A017428C5216A2F1CD@example.com</C:text-match>
        </C:prop-filter>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_7 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT">
        <C:prop-filter name="ATTENDEE">
          <C:text-match collation="i;ascii-casemap"
           >mailto:lisa@example.com</C:text-match>
          <C:param-filter name="PARTSTAT">
            <C:text-match collation="i;ascii-casemap"
             >NEEDS-ACTION</C:text-match>
          </C:param-filter>
        </C:prop-filter>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_8 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT"/>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_9 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VTODO">
        <C:prop-filter name="COMPLETED">
          <C:is-not-defined/>
        </C:prop-filter>
        <C:prop-filter name="STATUS">
          <C:text-match
             negate-condition="yes">CANCELLED</C:text-match>
        </C:prop-filter>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_8_10 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VEVENT">
        <C:prop-filter name="X-ABC-GUID">
          <C:text-match>ABC</C:text-match>
        </C:prop-filter>
      </C:comp-filter>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>|}

let request_7_9_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-multiget xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop>
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <D:href>/bernard/work/abcd1.ics</D:href>
  <D:href>/bernard/work/mtg1.ics</D:href>
</C:calendar-multiget>|}

let request_7_10_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:free-busy-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <C:time-range start="20060104T140000Z"
                  end="20060105T220000Z"/>
</C:free-busy-query>|}

let mkcalendar_request_5_3_1_2 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:mkcalendar xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:set>
    <D:prop>
      <D:displayname>Lisa's Events</D:displayname>
      <C:calendar-description xml:lang="en"
>Calendar restricted to events.</C:calendar-description>
      <C:supported-calendar-component-set>
        <C:comp name="VEVENT"/>
      </C:supported-calendar-component-set>
      <C:calendar-timezone><![CDATA[BEGIN:VCALENDAR
PRODID:-//Example Corp.//CalDAV Client//EN
VERSION:2.0
BEGIN:VTIMEZONE
TZID:US-Eastern
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:Eastern Standard Time (US &amp; Canada)
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:Eastern Daylight Time (US &amp; Canada)
END:DAYLIGHT
END:VTIMEZONE
END:VCALENDAR
]]></C:calendar-timezone>
    </D:prop>
  </D:set>
</C:mkcalendar>|}

let query_of_string name s =
  let x = ok name (Httpz_dav.parse_xml s) in
  ok name (Caldav.Report.query_of_xml x)

let multiget_of_string name s =
  let x = ok name (Httpz_dav.parse_xml s) in
  ok name (Caldav.Report.multiget_of_xml x)

let free_busy_of_string name s =
  let x = ok name (Httpz_dav.parse_xml s) in
  ok name (Caldav.Report.free_busy_of_xml x)

(* to_xml omits an attribute at its default value, and a hand-written RFC
   example may still spell the default out. So a request that round-trips
   is not one equal to the very first parse, but one whose second
   encode-decode pass is a fixed point of its first. *)
let query_roundtrips name q =
  let q2 =
    ok name (Caldav.Report.query_of_xml (Caldav.Report.query_to_xml q))
  in
  let q3 =
    ok name (Caldav.Report.query_of_xml (Caldav.Report.query_to_xml q2))
  in
  check_bool (name ^ " roundtrips") (q2 = q3)

let multiget_roundtrips name m =
  let m2 =
    ok name (Caldav.Report.multiget_of_xml (Caldav.Report.multiget_to_xml m))
  in
  let m3 =
    ok name (Caldav.Report.multiget_of_xml (Caldav.Report.multiget_to_xml m2))
  in
  check_bool (name ^ " roundtrips") (m2 = m3)

let free_busy_roundtrips name tr =
  let tr2 =
    ok name (Caldav.Report.free_busy_of_xml (Caldav.Report.free_busy_to_xml tr))
  in
  check_bool (name ^ " roundtrips") (tr2 = tr)

let calendar_data_comp d =
  match d.Caldav.Calendar_data.comp with
  | Some c -> c
  | None -> Alcotest.fail "expected a comp element"

let test_request_7_8_1 () =
  let q = query_of_string "7.8.1" request_7_8_1 in
  query_roundtrips "7.8.1" q;
  (match q.props with
  | Caldav.Report.Prop (names, Some d) -> (
      check_bool "7.8.1 getetag" (names = [ Httpz_dav.Prop.getetag ]);
      let vcal = calendar_data_comp d in
      check_bool "7.8.1 vcal name" (vcal.comp_name = "VCALENDAR");
      check_bool "7.8.1 vcal props" (vcal.props = `Props [ ("VERSION", false) ]);
      match vcal.comps with
      | `Comps [ vevent; vtimezone ] ->
          check_bool "7.8.1 vevent name" (vevent.comp_name = "VEVENT");
          check_bool "7.8.1 vevent props"
            (vevent.props
            = `Props
                [
                  ("SUMMARY", false);
                  ("UID", false);
                  ("DTSTART", false);
                  ("DTEND", false);
                  ("DURATION", false);
                  ("RRULE", false);
                  ("RDATE", false);
                  ("EXRULE", false);
                  ("EXDATE", false);
                  ("RECURRENCE-ID", false);
                ]);
          check_bool "7.8.1 vevent comps" (vevent.comps = `Comps []);
          check_bool "7.8.1 vtimezone name" (vtimezone.comp_name = "VTIMEZONE");
          check_bool "7.8.1 vtimezone props" (vtimezone.props = `Props []);
          check_bool "7.8.1 vtimezone comps" (vtimezone.comps = `Comps [])
      | _ -> Alcotest.fail "7.8.1 vcal comps")
  | _ -> Alcotest.fail "7.8.1 props");
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vevent ], []) }
    -> (
      check_bool "7.8.1 filter comp" (vevent.comp = "VEVENT");
      match vevent.comp_condition with
      | `Matches (Some tr, [], []) ->
          check_bool "7.8.1 filter start"
            (tr.start = Some (dt "20060104T000000Z"));
          check_bool "7.8.1 filter finish"
            (tr.finish = Some (dt "20060105T000000Z"))
      | _ -> Alcotest.fail "7.8.1 filter vevent condition")
  | _ -> Alcotest.fail "7.8.1 filter"

let test_request_7_8_2 () =
  let q = query_of_string "7.8.2" request_7_8_2 in
  query_roundtrips "7.8.2" q;
  (match q.props with
  | Caldav.Report.Prop ([], Some d) ->
      check_bool "7.8.2 comp absent" (d.comp = None);
      (match d.limit_recurrence_set with
      | Some tr ->
          check_bool "7.8.2 lrs start" (tr.start = Some (dt "20060103T000000Z"));
          check_bool "7.8.2 lrs finish"
            (tr.finish = Some (dt "20060105T000000Z"))
      | None -> Alcotest.fail "7.8.2 limit-recurrence-set");
      check_bool "7.8.2 expand" (d.expand = None)
  | _ -> Alcotest.fail "7.8.2 props");
  check_bool "7.8.2 filter comp" (q.filter.comp = "VCALENDAR")

let test_request_7_8_3 () =
  let q = query_of_string "7.8.3" request_7_8_3 in
  query_roundtrips "7.8.3" q;
  match q.props with
  | Caldav.Report.Prop ([], Some d) -> (
      match d.expand with
      | Some tr ->
          check_bool "7.8.3 expand start"
            (tr.start = Some (dt "20060103T000000Z"));
          check_bool "7.8.3 expand finish"
            (tr.finish = Some (dt "20060105T000000Z"))
      | None -> Alcotest.fail "7.8.3 expand")
  | _ -> Alcotest.fail "7.8.3 props"

let test_request_7_8_4 () =
  let q = query_of_string "7.8.4" request_7_8_4 in
  query_roundtrips "7.8.4" q;
  (match q.props with
  | Caldav.Report.Prop ([], Some d) -> (
      match d.limit_freebusy_set with
      | Some tr ->
          check_bool "7.8.4 lfs start" (tr.start = Some (dt "20060102T000000Z"));
          check_bool "7.8.4 lfs finish"
            (tr.finish = Some (dt "20060103T000000Z"))
      | None -> Alcotest.fail "7.8.4 limit-freebusy-set")
  | _ -> Alcotest.fail "7.8.4 props");
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vfb ], []) } ->
      check_bool "7.8.4 filter comp" (vfb.comp = "VFREEBUSY")
  | _ -> Alcotest.fail "7.8.4 filter"

let test_request_7_8_5 () =
  let q = query_of_string "7.8.5" request_7_8_5 in
  query_roundtrips "7.8.5" q;
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vtodo ], []) } -> (
      check_bool "7.8.5 vtodo" (vtodo.comp = "VTODO");
      match vtodo.comp_condition with
      | `Matches (None, [ valarm ], []) -> (
          check_bool "7.8.5 valarm" (valarm.comp = "VALARM");
          match valarm.comp_condition with
          | `Matches (Some tr, [], []) ->
              check_bool "7.8.5 start" (tr.start = Some (dt "20060106T100000Z"));
              check_bool "7.8.5 finish"
                (tr.finish = Some (dt "20060107T100000Z"))
          | _ -> Alcotest.fail "7.8.5 valarm condition")
      | _ -> Alcotest.fail "7.8.5 vtodo condition")
  | _ -> Alcotest.fail "7.8.5 filter"

let test_request_7_8_6 () =
  let q = query_of_string "7.8.6" request_7_8_6 in
  query_roundtrips "7.8.6" q;
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vevent ], []) }
    -> (
      match vevent.comp_condition with
      | `Matches (None, [], [ pf ]) ->
          check_bool "7.8.6 prop" (pf.prop = "UID");
          check_bool "7.8.6 condition"
            (pf.prop_condition
            = `Matches
                ( Some
                    (`Text
                       (Caldav.Filter.text_match ~collation:"i;octet"
                          "DC6C50A017428C5216A2F1CD@example.com")),
                  [] ))
      | _ -> Alcotest.fail "7.8.6 vevent condition")
  | _ -> Alcotest.fail "7.8.6 filter"

let test_request_7_8_7 () =
  let q = query_of_string "7.8.7" request_7_8_7 in
  query_roundtrips "7.8.7" q;
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vevent ], []) }
    -> (
      match vevent.comp_condition with
      | `Matches (None, [], [ pf ]) -> (
          check_bool "7.8.7 prop" (pf.prop = "ATTENDEE");
          match pf.prop_condition with
          | `Matches (Some (`Text tm), [ pmf ]) ->
              check_bool "7.8.7 text"
                (tm
                = Caldav.Filter.text_match ~collation:"i;ascii-casemap"
                    "mailto:lisa@example.com");
              check_bool "7.8.7 param" (pmf.param = "PARTSTAT");
              check_bool "7.8.7 param match"
                (pmf.param_test
                = `Match
                    (Caldav.Filter.text_match ~collation:"i;ascii-casemap"
                       "NEEDS-ACTION"))
          | _ -> Alcotest.fail "7.8.7 prop condition")
      | _ -> Alcotest.fail "7.8.7 vevent condition")
  | _ -> Alcotest.fail "7.8.7 filter"

let test_request_7_8_8 () =
  let q = query_of_string "7.8.8" request_7_8_8 in
  query_roundtrips "7.8.8" q;
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vevent ], []) } ->
      check_bool "7.8.8 comp" (vevent.comp = "VEVENT");
      check_bool "7.8.8 condition" (vevent.comp_condition = `Defined)
  | _ -> Alcotest.fail "7.8.8 filter"

let test_request_7_8_9 () =
  let q = query_of_string "7.8.9" request_7_8_9 in
  query_roundtrips "7.8.9" q;
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vtodo ], []) } -> (
      match vtodo.comp_condition with
      | `Matches (None, [], [ completed; status ]) ->
          check_bool "7.8.9 completed" (completed.prop = "COMPLETED");
          check_bool "7.8.9 completed condition"
            (completed.prop_condition = `Not_defined);
          check_bool "7.8.9 status" (status.prop = "STATUS");
          check_bool "7.8.9 status condition"
            (status.prop_condition
            = `Matches
                ( Some
                    (`Text (Caldav.Filter.text_match ~negate:true "CANCELLED")),
                  [] ))
      | _ -> Alcotest.fail "7.8.9 vtodo condition")
  | _ -> Alcotest.fail "7.8.9 filter"

let test_request_7_8_10 () =
  let q = query_of_string "7.8.10" request_7_8_10 in
  query_roundtrips "7.8.10" q;
  match q.filter with
  | { comp = "VCALENDAR"; comp_condition = `Matches (None, [ vevent ], []) }
    -> (
      match vevent.comp_condition with
      | `Matches (None, [], [ pf ]) ->
          check_bool "7.8.10 prop" (pf.prop = "X-ABC-GUID");
          check_bool "7.8.10 condition"
            (pf.prop_condition
            = `Matches (Some (`Text (Caldav.Filter.text_match "ABC")), []))
      | _ -> Alcotest.fail "7.8.10 vevent condition")
  | _ -> Alcotest.fail "7.8.10 filter"

let test_request_7_9_1 () =
  let m = multiget_of_string "7.9.1" request_7_9_1 in
  multiget_roundtrips "7.9.1" m;
  (match m.Caldav.Report.props with
  | Caldav.Report.Prop (names, Some d) ->
      check_bool "7.9.1 getetag" (names = [ Httpz_dav.Prop.getetag ]);
      check_bool "7.9.1 comp" (d.comp = None)
  | _ -> Alcotest.fail "7.9.1 props");
  check_bool "7.9.1 hrefs"
    (m.hrefs = [ "/bernard/work/abcd1.ics"; "/bernard/work/mtg1.ics" ])

let test_request_7_10_1 () =
  let tr = free_busy_of_string "7.10.1" request_7_10_1 in
  free_busy_roundtrips "7.10.1" tr;
  check_bool "7.10.1 start" (tr.start = Some (dt "20060104T140000Z"));
  check_bool "7.10.1 finish" (tr.finish = Some (dt "20060105T220000Z"))

let test_request_5_3_1_2 () =
  let x = ok "5.3.1.2" (Httpz_dav.parse_xml mkcalendar_request_5_3_1_2) in
  check_bool "5.3.1.2 root"
    ((fun (e : Httpz_dav.element) -> e.name) x
    = ("urn:ietf:params:xml:ns:caldav", "mkcalendar"));
  let set =
    match Httpz_dav.find (Httpz_dav.dav "set") x with
    | Some s -> s
    | None -> Alcotest.fail "5.3.1.2 no D:set"
  in
  let props =
    match Httpz_dav.find (Httpz_dav.dav "prop") set with
    | Some p -> p
    | None -> Alcotest.fail "5.3.1.2 no D:prop"
  in
  let displayname =
    match Httpz_dav.find Httpz_dav.Prop.displayname props with
    | Some p -> Httpz_dav.content p
    | None -> Alcotest.fail "5.3.1.2 no displayname"
  in
  check_bool "5.3.1.2 displayname" (displayname = "Lisa's Events");
  let desc =
    match Httpz_dav.find Caldav.Property.calendar_description props with
    | Some p -> p
    | None -> Alcotest.fail "5.3.1.2 no calendar-description"
  in
  check_bool "5.3.1.2 description"
    (String.trim (Httpz_dav.content desc) = "Calendar restricted to events.");
  check_bool "5.3.1.2 description lang"
    (Httpz_dav.attr ("http://www.w3.org/XML/1998/namespace", "lang") desc
    = Some "en");
  let comps =
    match
      Httpz_dav.find Caldav.Property.supported_calendar_component_set props
    with
    | Some p -> p
    | None -> Alcotest.fail "5.3.1.2 no supported-calendar-component-set"
  in
  check_bool "5.3.1.2 comp" (Caldav.Property.component_set comps = [ "VEVENT" ]);
  let tz =
    match Httpz_dav.find Caldav.Property.calendar_timezone props with
    | Some p -> p
    | None -> Alcotest.fail "5.3.1.2 no calendar-timezone"
  in
  let tztxt =
    ok "5.3.1.2 timezone"
      (Option.to_result ~none:"missing" (Caldav.Property.timezone tz))
  in
  check_bool "5.3.1.2 timezone" (contains ~needle:"TZID:US-Eastern" tztxt)

(* Caldav_mkcalendar has no of_xml reader (RFC 4791 gives no MKCALENDAR
   round trip example either), so the request round trip is checked at the
   encoder: to_xml, to_string then of_string is a fixed point. *)
let mkcalendar_props =
  Httpz_dav.leaf Httpz_dav.Prop.displayname "Lisa's Events"
  :: Caldav.Property.description ~lang:"en" "Calendar restricted to events."
  :: Caldav.Property.supported_components [ "VEVENT" ]
  :: [
       Caldav.Property.timezone_prop
         "BEGIN:VCALENDAR\r\n\
          PRODID:-//Example Corp.//CalDAV Client//EN\r\n\
          VERSION:2.0\r\n\
          BEGIN:VTIMEZONE\r\n\
          TZID:US-Eastern\r\n\
          END:VTIMEZONE\r\n\
          END:VCALENDAR\r\n";
     ]

(* A parsed element carries the namespace declarations it arrived with, so
   the fixed point is compared without attributes. *)
let test_mkcalendar_roundtrip () =
  let rec shape (e : Httpz_dav.element) =
    `S (e.name, List.map shape (Httpz_dav.elements e))
  in
  let x2 =
    ok "mkcalendar roundtrip"
      (Httpz_dav.parse_xml (Httpz_dav.mkcalendar mkcalendar_props))
  in
  let x3 =
    ok "mkcalendar roundtrip" (Httpz_dav.parse_xml (Httpz_dav.encode_xml x2))
  in
  check_bool "mkcalendar roundtrips" (shape x2 = shape x3)

let test_mkcalendar_request () =
  let x =
    ok "mkcalendar body"
      (Httpz_dav.parse_xml (Httpz_dav.mkcalendar mkcalendar_props))
  in
  check_bool "mkcalendar root" (x.name = Caldav.Property.caldav "mkcalendar");
  check_bool "mkcalendar sets props"
    (match Httpz_dav.find (Httpz_dav.dav "set") x with
    | Some set -> Httpz_dav.find (Httpz_dav.dav "prop") set <> None
    | None -> false)

(* {1 RFC 4791 Section 7.8 calendar-query response examples} *)

let response_7_8_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<D:multistatus xmlns:D="DAV:"
           xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:response>
    <D:href>http://cal.example.com/bernard/work/abcd2.ics</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"fffff-abcd2"</D:getetag>
        <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTART;TZID=US/Eastern:20060102T120000
DURATION:PT1H
RRULE:FREQ=DAILY;COUNT=5
SUMMARY:Event #2
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
BEGIN:VEVENT
DTSTART;TZID=US/Eastern:20060104T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060104T120000
SUMMARY:Event #2 bis
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
BEGIN:VEVENT
DTSTART;TZID=US/Eastern:20060106T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060106T120000
SUMMARY:Event #2 bis bis
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
END:VCALENDAR
</C:calendar-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
  <D:response>
    <D:href>http://cal.example.com/bernard/work/abcd3.ics</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"fffff-abcd3"</D:getetag>
        <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTART;TZID=US/Eastern:20060104T100000
DURATION:PT1H
SUMMARY:Event #3
UID:DC6C50A017428C5216A2F1CD@example.com
END:VEVENT
END:VCALENDAR
</C:calendar-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
</D:multistatus>|}

let response_7_9_1 =
  {|<?xml version="1.0" encoding="utf-8" ?>
<D:multistatus xmlns:D="DAV:"
               xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:response>
    <D:href>http://cal.example.com/bernard/work/abcd1.ics</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"fffff-abcd1"</D:getetag>
        <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
DTSTAMP:20060206T001102Z
DTSTART;TZID=US/Eastern:20060102T100000
DURATION:PT1H
SUMMARY:Event #1
UID:74855313FA803DA593CD579A@example.com
END:VEVENT
END:VCALENDAR
</C:calendar-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
  <D:response>
    <D:href>http://cal.example.com/bernard/work/mtg1.ics</D:href>
    <D:status>HTTP/1.1 404 Not Found</D:status>
  </D:response>
</D:multistatus>|}

let response_7_10_1 =
  "BEGIN:VCALENDAR\r\n\
   VERSION:2.0\r\n\
   PRODID:-//Example Corp.//CalDAV Server//EN\r\n\
   BEGIN:VFREEBUSY\r\n\
   DTSTAMP:20050125T090000Z\r\n\
   DTSTART:20060104T140000Z\r\n\
   DTEND:20060105T220000Z\r\n\
   FREEBUSY;FBTYPE=BUSY-TENTATIVE:20060104T150000Z/PT1H\r\n\
   FREEBUSY:20060104T190000Z/PT1H\r\n\
   END:VFREEBUSY\r\n\
   END:VCALENDAR\r\n"

let mkcalendar_response =
  {|<?xml version="1.0" encoding="utf-8" ?>
<C:mkcalendar-response xmlns:D="DAV:"
                        xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:propstat>
    <D:prop>
      <D:displayname/>
    </D:prop>
    <D:status>HTTP/1.1 200 OK</D:status>
  </D:propstat>
  <D:propstat>
    <D:prop>
      <C:calendar-description/>
    </D:prop>
    <D:status>HTTP/1.1 403 Forbidden</D:status>
    <D:error><C:supported-calendar-data/></D:error>
  </D:propstat>
</C:mkcalendar-response>|}

let test_response_7_8_1 () =
  let m =
    ok "7.8.1r"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         response_7_8_1)
  in
  let outcome = Caldav.Report.outcome_of_multistatus ~base:"/bernard/work/" m in
  check_bool "7.8.1r not truncated" (not outcome.truncated);
  match outcome.entries with
  | [ e1; e2 ] ->
      check_bool "7.8.1r href1"
        (e1.href = "http://cal.example.com/bernard/work/abcd2.ics");
      check_bool "7.8.1r etag1" (e1.etag = Some {|"fffff-abcd2"|});
      check_bool "7.8.1r data1"
        (match e1.data with
        | Some d -> contains ~needle:"Event #2 bis bis" d
        | None -> false);
      check_bool "7.8.1r href2"
        (e2.href = "http://cal.example.com/bernard/work/abcd3.ics");
      check_bool "7.8.1r etag2" (e2.etag = Some {|"fffff-abcd3"|})
  | _ -> Alcotest.fail "7.8.1r entries"

let test_response_7_9_1 () =
  let m =
    ok "7.9.1r"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         response_7_9_1)
  in
  let outcome = Caldav.Report.outcome_of_multistatus ~base:"/bernard/work/" m in
  (match outcome.entries with
  | [ e ] ->
      check_bool "7.9.1r href"
        (e.href = "http://cal.example.com/bernard/work/abcd1.ics");
      check_bool "7.9.1r etag" (e.etag = Some {|"fffff-abcd1"|});
      check_bool "7.9.1r data"
        (match e.data with
        | Some d ->
            contains ~needle:"UID:74855313FA803DA593CD579A@example.com" d
        | None -> false)
  | _ -> Alcotest.fail "7.9.1r entries");
  check_bool "7.9.1r missing"
    (Caldav.Report.missing m
    = [ "http://cal.example.com/bernard/work/mtg1.ics" ])

let test_response_7_10_1 () =
  let cal = ok "7.10.1r" (Ical.one_of_string response_7_10_1) in
  match Ical.free_busy cal with
  | [ vfb ] -> (
      let fbs = Ical.Component.find_all vfb "FREEBUSY" in
      check_bool "7.10.1r freebusy count" (List.length fbs = 2);
      match fbs with
      | [ fb1; fb2 ] ->
          check_bool "7.10.1r fb1 type"
            (Ical.Property.find_first fb1 "FBTYPE" = Some "BUSY-TENTATIVE");
          check_bool "7.10.1r fb1 value"
            (Ical.Property.value fb1 = "20060104T150000Z/PT1H");
          check_bool "7.10.1r fb2 type"
            (Ical.Property.find_first fb2 "FBTYPE" = None);
          check_bool "7.10.1r fb2 value"
            (Ical.Property.value fb2 = "20060104T190000Z/PT1H")
      | _ -> Alcotest.fail "7.10.1r freebusy")
  | _ -> Alcotest.fail "7.10.1r vfreebusy"

let test_mkcalendar_response () =
  let ps =
    ok "mkcalendar-response"
      (Result.bind
         (Httpz_dav.parse_xml mkcalendar_response)
         Httpz_dav.mkcol_response)
  in
  match ps with
  | [ p1; p2 ] ->
      check_bool "mkcalendar-response status1" (p1.Httpz_dav.status = 200);
      check_bool "mkcalendar-response status2" (p2.Httpz_dav.status = 403);
      check_bool "mkcalendar-response error"
        (List.exists
           (fun e ->
             (fun (e : Httpz_dav.element) -> e.name) e
             = Caldav.Error.supported_calendar_data)
           p2.Httpz_dav.errors)
  | _ -> Alcotest.fail "mkcalendar-response propstats"

(* {1 Caldav_filter.matches against RFC 4791 Appendix B} *)

let cal_of_string s = ok "appendix b" (Ical.one_of_string s)

let abcd1 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTAMP:20060206T001102Z
DTSTART;TZID=US/Eastern:20060102T100000
DURATION:PT1H
SUMMARY:Event #1
DESCRIPTION:Go Steelers!
UID:74855313FA803DA593CD579A@example.com
END:VEVENT
END:VCALENDAR
|}

let abcd2 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTAMP:20060206T001121Z
DTSTART;TZID=US/Eastern:20060102T120000
DURATION:PT1H
RRULE:FREQ=DAILY;COUNT=5
SUMMARY:Event #2
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
BEGIN:VEVENT
DTSTAMP:20060206T001121Z
DTSTART;TZID=US/Eastern:20060104T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060104T120000
SUMMARY:Event #2 bis
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
END:VCALENDAR
|}

let abcd3 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
ATTENDEE;PARTSTAT=ACCEPTED;ROLE=CHAIR:mailto:cyrus@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:lisa@example.com
DTSTAMP:20060206T001220Z
DTSTART;TZID=US/Eastern:20060104T100000
DURATION:PT1H
LAST-MODIFIED:20060206T001330Z
ORGANIZER:mailto:cyrus@example.com
SEQUENCE:1
STATUS:TENTATIVE
SUMMARY:Event #3
UID:DC6C50A017428C5216A2F1CD@example.com
END:VEVENT
END:VCALENDAR
|}

let abcd4 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTODO
DTSTAMP:20060205T235335Z
DUE;VALUE=DATE:20060104
STATUS:NEEDS-ACTION
SUMMARY:Task #1
UID:DDDEEB7915FA61233B861457@example.com
BEGIN:VALARM
ACTION:AUDIO
TRIGGER;RELATED=START:-PT10M
END:VALARM
END:VTODO
END:VCALENDAR
|}

let abcd5 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTODO
DTSTAMP:20060205T235300Z
DUE;VALUE=DATE:20060106
LAST-MODIFIED:20060205T235308Z
SEQUENCE:1
STATUS:NEEDS-ACTION
SUMMARY:Task #2
UID:E10BA47467C5C69BB74E8720@example.com
BEGIN:VALARM
ACTION:AUDIO
TRIGGER;RELATED=START:-PT10M
END:VALARM
END:VTODO
END:VCALENDAR
|}

let abcd6 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTODO
COMPLETED:20051223T122322Z
DTSTAMP:20060205T235400Z
DUE;VALUE=DATE:20051225
LAST-MODIFIED:20060205T235308Z
SEQUENCE:1
STATUS:COMPLETED
SUMMARY:Task #3
UID:E10BA47467C5C69BB74E8722@example.com
END:VTODO
END:VCALENDAR
|}

let abcd7 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTODO
DTSTAMP:20060205T235600Z
DUE;VALUE=DATE:20060101
LAST-MODIFIED:20060205T235308Z
SEQUENCE:1
STATUS:CANCELLED
SUMMARY:Task #4
UID:E10BA47467C5C69BB74E8725@example.com
END:VTODO
END:VCALENDAR
|}

let abcd8 =
  cal_of_string
    {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VFREEBUSY
ORGANIZER;CN="Bernard Desruisseaux":mailto:bernard@example.com
UID:76ef34-54a3d2@example.com
DTSTAMP:20050530T123421Z
DTSTART:20060101T000000Z
DTEND:20060108T000000Z
FREEBUSY:20050531T230000Z/20050601T010000Z
FREEBUSY;FBTYPE=BUSY-TENTATIVE:20060102T100000Z/20060102T120000Z
FREEBUSY:20060103T100000Z/20060103T120000Z
FREEBUSY:20060104T100000Z/20060104T120000Z
FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:20060105T100000Z/20060105T120000Z
FREEBUSY:20060106T100000Z/20060106T120000Z
END:VFREEBUSY
END:VCALENDAR
|}

let test_filter_7_8_6 () =
  (* Retrieval of Event by UID: matches abcd3, not abcd1. *)
  let open Caldav.Filter in
  let f =
    components "VEVENT"
      ~props:
        [
          prop "UID"
            (Some
               (`Text
                  (text_match ~collation:"i;octet"
                     "DC6C50A017428C5216A2F1CD@example.com")));
        ]
  in
  check_bool "7.8.6 abcd3 matches" (matches f abcd3);
  check_bool "7.8.6 abcd1 does not match" (not (matches f abcd1))

let test_filter_7_8_7 () =
  (* Retrieval of Events by PARTSTAT: matches abcd3, not abcd1. *)
  let open Caldav.Filter in
  let f =
    components "VEVENT"
      ~props:
        [
          prop "ATTENDEE"
            ~params:[ param "PARTSTAT" (Some (text_match "NEEDS-ACTION")) ]
            (Some (`Text (text_match "mailto:lisa@example.com")));
        ]
  in
  check_bool "7.8.7 abcd3 matches" (matches f abcd3);
  check_bool "7.8.7 abcd1 does not match" (not (matches f abcd1))

let test_filter_7_8_8 () =
  (* Retrieval of Events Only. *)
  let f = Caldav.Filter.components "VEVENT" in
  check_bool "7.8.8 abcd1 matches" (Caldav.Filter.matches f abcd1);
  check_bool "7.8.8 abcd3 matches" (Caldav.Filter.matches f abcd3);
  check_bool "7.8.8 abcd4 does not match" (not (Caldav.Filter.matches f abcd4))

let test_filter_7_8_9 () =
  (* Retrieval of All Pending To-Dos: is-not-defined and negate-condition. *)
  let open Caldav.Filter in
  let f =
    components "VTODO"
      ~props:
        [
          prop_not_defined "COMPLETED";
          prop "STATUS" (Some (`Text (text_match ~negate:true "CANCELLED")));
        ]
  in
  check_bool "7.8.9 abcd4 matches" (matches f abcd4);
  check_bool "7.8.9 abcd5 matches" (matches f abcd5);
  check_bool "7.8.9 abcd6 does not match (COMPLETED defined)"
    (not (matches f abcd6));
  check_bool "7.8.9 abcd7 does not match (STATUS is CANCELLED)"
    (not (matches f abcd7))

let test_filter_time_range () =
  (* RFC 4791 Section 7.8.1's query matches abcd3 (a single VEVENT whose
     DTSTART/DTEND overlap the range) directly. RFC 4791 Section 3.2 models
     a recurrence exception as its own component alongside the master, so
     abcd2's stored "Event #2 bis" override (RECURRENCE-ID 4 Jan) is tested
     as its own VEVENT sibling and overlaps the range, even though
     Caldav_filter.matches does not expand the master's RRULE itself: its
     first instance (2 Jan) does not overlap 4-5 Jan. *)
  let open Caldav.Filter in
  let f =
    components "VEVENT"
      ~time_range:
        (time_range ~start:(dt "20060104T000000Z")
           ~finish:(dt "20060105T000000Z") ())
  in
  check_bool "time-range abcd3 matches" (matches f abcd3);
  check_bool "time-range abcd1 does not match" (not (matches f abcd1));
  check_bool "time-range abcd2 matches via its stored override"
    (matches f abcd2)

let test_filter_time_range_freebusy () =
  (* RFC 4791 Section 7.8.4: a VFREEBUSY's own DTSTART/DTEND (not its
     individual FREEBUSY periods) is what Caldav_filter.matches tests. *)
  let open Caldav.Filter in
  let f =
    components "VFREEBUSY"
      ~time_range:
        (time_range ~start:(dt "20060102T000000Z")
           ~finish:(dt "20060103T000000Z") ())
  in
  check_bool "vfreebusy time-range matches" (matches f abcd8);
  let f2 =
    components "VFREEBUSY"
      ~time_range:
        (time_range ~start:(dt "20070101T000000Z")
           ~finish:(dt "20070102T000000Z") ())
  in
  check_bool "vfreebusy time-range outside does not match"
    (not (matches f2 abcd8))

let test_filter_valarm_time_range () =
  (* RFC 4791 Section 7.8.5: the RFC's answer is derived from the VALARM
     TRIGGER relative to the VTODO's DTSTART, which Caldav_filter.matches
     does not compute; a VALARM has no DTSTART of its own, so per the mli
     "a component without DTSTART does not match a time range", the
     comp-filter never matches. *)
  let open Caldav.Filter in
  let f =
    v
      [
        comp "VTODO"
          ~comps:
            [
              comp "VALARM"
                ~time_range:
                  (time_range ~start:(dt "20060106T100000Z")
                     ~finish:(dt "20060107T100000Z") ());
            ];
      ]
  in
  check_bool "valarm time-range never matches (no DTSTART on VALARM)"
    (not (matches f abcd4))

let test_filter_comp_not_defined () =
  let open Caldav.Filter in
  let has_valarm = v [ comp "VTODO" ~comps:[ comp "VALARM" ] ] in
  check_bool "abcd4 has a VALARM" (matches has_valarm abcd4);
  let no_valarm = v [ comp "VTODO" ~comps:[ comp_not_defined "VALARM" ] ] in
  check_bool "abcd4 does not satisfy is-not-defined VALARM"
    (not (matches no_valarm abcd4));
  check_bool "abcd6 has no VALARM" (matches no_valarm abcd6)

let test_filter_param_not_defined () =
  let open Caldav.Filter in
  let f =
    components "VEVENT"
      ~props:[ prop "ORGANIZER" ~params:[ param_not_defined "PARTSTAT" ] None ]
  in
  check_bool "ORGANIZER has no PARTSTAT param" (matches f abcd3)

let test_filter_all () =
  check_bool "all matches everything"
    (Caldav.Filter.matches Caldav.Filter.all abcd1);
  check_bool "all matches a to-do too"
    (Caldav.Filter.matches Caldav.Filter.all abcd4)

(* {1 Caldav_calendar} *)

let radicale_response =
  {|<?xml version='1.0' encoding='utf-8'?>
<multistatus xmlns="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav" xmlns:CS="http://calendarserver.org/ns/"><response><href>/alice/cal/</href><propstat><prop><resourcetype><C:calendar /><collection /></resourcetype><displayname>Cal</displayname><C:calendar-description>desc</C:calendar-description><C:supported-calendar-component-set><C:comp name="VEVENT" /><C:comp name="VTODO" /></C:supported-calendar-component-set><C:max-resource-size>10000000</C:max-resource-size><supported-report-set><supported-report><report><expand-property /></report></supported-report><supported-report><report><sync-collection /></report></supported-report><supported-report><report><C:calendar-multiget /></report></supported-report><supported-report><report><C:calendar-query /></report></supported-report></supported-report-set><sync-token>http://radicale.org/ns/sync/143ad</sync-token><CS:getctag>"4f75e"</CS:getctag></prop><status>HTTP/1.1 200 OK</status></propstat><propstat><prop><C:supported-calendar-data /><C:calendar-timezone /><C:min-date-time /><C:max-date-time /><C:max-instances /><C:max-attendees-per-instance /></prop><status>HTTP/1.1 404 Not Found</status></propstat></response></multistatus>|}

let test_calendar () =
  let m =
    ok "radicale"
      ((fun s -> Result.bind (Httpz_dav.parse_xml s) Httpz_dav.multistatus)
         radicale_response)
  in
  let cals = Caldav.Calendar.of_multistatus m in
  match cals with
  | [ c ] ->
      check_bool "href" (c.href = "/alice/cal/");
      check_bool "display_name" (c.display_name = Some "Cal");
      check_bool "description" (c.description = Some "desc");
      check_bool "components" (c.components = [ "VEVENT"; "VTODO" ]);
      check_bool "max_size" (c.max_size = Some 10000000);
      check_bool "ctag" (c.ctag = Some {|"4f75e"|});
      check_bool "sync_token"
        (c.sync_token = Some "http://radicale.org/ns/sync/143ad");
      check_bool "supports calendar-query"
        (Caldav.Calendar.supports Caldav.Property.calendar_query c);
      check_bool "supports calendar-multiget"
        (Caldav.Calendar.supports Caldav.Property.calendar_multiget c);
      check_bool "supports sync-collection"
        (Caldav.Calendar.supports (Httpz_dav.dav "sync-collection") c);
      check_bool "accepts VEVENT"
        (Caldav.Calendar.accepts ~component:"VEVENT" c);
      check_bool "does not accept VJOURNAL"
        (not (Caldav.Calendar.accepts ~component:"VJOURNAL" c));
      check_bool "timezone absent" (c.timezone = None);
      check_bool "etag absent" (c.etag = None);
      check_bool "min_date_time absent" (c.min_date_time = None);
      check_bool "max_date_time absent" (c.max_date_time = None);
      check_bool "max_instances absent" (c.max_instances = None);
      check_bool "max_attendees absent" (c.max_attendees = None)
  | _ -> Alcotest.failf "expected one calendar, got %d" (List.length cals)

(* {1 Caldav_data} *)

let test_data () =
  let cal =
    Ical.v
      [
        Ical.Component.v "VEVENT"
          ~properties:
            [
              Ical.Property.of_text "SUMMARY" "A B";
              Ical.Property.of_text "UID" "urn:uuid:1";
              Ical.Property.of_date_time "DTSTART"
                (Ical.Date.Date_time (dt "20060102T100000Z"));
            ];
      ]
  in
  let s = Ical.to_string cal in
  let cal2 = ok "ical decode" (Caldav.Data.ical.decode s) in
  check_bool "ical decode roundtrips" (Ical.equal cal cal2);
  Alcotest.(check string)
    "ical encode" s
    (ok "ical encode" (Caldav.Data.ical.encode cal));
  let r = ok "raw decode" (Caldav.Data.raw.decode s) in
  Alcotest.(check string) "raw decode is identity" s r;
  Alcotest.(check string)
    "raw encode is identity" s
    (ok "raw encode" (Caldav.Data.raw.encode s));
  Alcotest.(check (option string))
    "uid" (Some "urn:uuid:1")
    (Caldav.Data.uid Caldav.Data.ical cal);
  let no_uid =
    Ical.v
      [
        Ical.Component.v "VEVENT"
          ~properties:[ Ical.Property.of_text "SUMMARY" "No UID" ];
      ]
  in
  Alcotest.(check (option string))
    "no uid" None
    (Caldav.Data.uid Caldav.Data.ical no_uid);
  let d = Caldav.Data.calendar_data Caldav.Data.ical in
  check_bool "calendar_data content-type" (d.content_type = Some "text/calendar");
  check_bool "calendar_data version" (d.version = Some "2.0");
  check_bool "calendar_data comp" (d.comp = None)

let () =
  Alcotest.run "caldav"
    [
      ( "requests",
        [
          Alcotest.test_case "7.8.1" `Quick test_request_7_8_1;
          Alcotest.test_case "7.8.2" `Quick test_request_7_8_2;
          Alcotest.test_case "7.8.3" `Quick test_request_7_8_3;
          Alcotest.test_case "7.8.4" `Quick test_request_7_8_4;
          Alcotest.test_case "7.8.5" `Quick test_request_7_8_5;
          Alcotest.test_case "7.8.6" `Quick test_request_7_8_6;
          Alcotest.test_case "7.8.7" `Quick test_request_7_8_7;
          Alcotest.test_case "7.8.8" `Quick test_request_7_8_8;
          Alcotest.test_case "7.8.9" `Quick test_request_7_8_9;
          Alcotest.test_case "7.8.10" `Quick test_request_7_8_10;
          Alcotest.test_case "7.9.1" `Quick test_request_7_9_1;
          Alcotest.test_case "7.10.1" `Quick test_request_7_10_1;
          Alcotest.test_case "5.3.1.2" `Quick test_request_5_3_1_2;
          Alcotest.test_case "mkcalendar roundtrip" `Quick
            test_mkcalendar_roundtrip;
          Alcotest.test_case "mkcalendar request" `Quick test_mkcalendar_request;
        ] );
      ( "responses",
        [
          Alcotest.test_case "7.8.1" `Quick test_response_7_8_1;
          Alcotest.test_case "7.9.1" `Quick test_response_7_9_1;
          Alcotest.test_case "7.10.1" `Quick test_response_7_10_1;
          Alcotest.test_case "mkcalendar-response" `Quick
            test_mkcalendar_response;
        ] );
      ( "filter",
        [
          Alcotest.test_case "7.8.6 uid" `Quick test_filter_7_8_6;
          Alcotest.test_case "7.8.7 partstat" `Quick test_filter_7_8_7;
          Alcotest.test_case "7.8.8 events only" `Quick test_filter_7_8_8;
          Alcotest.test_case "7.8.9 pending todos" `Quick test_filter_7_8_9;
          Alcotest.test_case "time-range" `Quick test_filter_time_range;
          Alcotest.test_case "time-range freebusy" `Quick
            test_filter_time_range_freebusy;
          Alcotest.test_case "valarm time-range" `Quick
            test_filter_valarm_time_range;
          Alcotest.test_case "comp not-defined" `Quick
            test_filter_comp_not_defined;
          Alcotest.test_case "param not-defined" `Quick
            test_filter_param_not_defined;
          Alcotest.test_case "all" `Quick test_filter_all;
        ] );
      ("calendar", [ Alcotest.test_case "radicale" `Quick test_calendar ]);
      ("data", [ Alcotest.test_case "ical, raw, uid" `Quick test_data ]);
    ]
