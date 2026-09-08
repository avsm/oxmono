(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ok name = function
  | Ok v -> v
  | Error msg -> Alcotest.failf "%s: %s" name msg

let fails name = function
  | Ok _ -> Alcotest.failf "%s: should fail" name
  | Error _ -> ()

let wrap body =
  "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//test//EN\r\n" ^ body
  ^ "\r\nEND:VCALENDAR\r\n"

let wrap_alarm body =
  wrap
    ("BEGIN:VEVENT\r\n\
      UID:alarm-test@example.com\r\n\
      DTSTAMP:20200101T000000Z\r\n\
      DTSTART:20200101T090000Z\r\n" ^ body ^ "\r\nEND:VEVENT")

(* RFC 5545 Section 4: complete iCalendar object examples. *)
let s4_examples =
  [
    ( "3-day conference",
      {|BEGIN:VCALENDAR
PRODID:-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN
VERSION:2.0
BEGIN:VEVENT
DTSTAMP:19960704T120000Z
UID:uid1@example.com
ORGANIZER:mailto:jsmith@example.com
DTSTART:19960918T143000Z
DTEND:19960920T220000Z
STATUS:CONFIRMED
CATEGORIES:CONFERENCE
SUMMARY:Networld+Interop Conference
DESCRIPTION:Networld+Interop Conference and Exhibit\nAtlanta World Congress Center\nAtlanta\, Georgia
END:VEVENT
END:VCALENDAR
|}
    );
    ( "group-scheduled meeting",
      {|BEGIN:VCALENDAR
PRODID:-//RDU Software//NONSGML HandCal//EN
VERSION:2.0
BEGIN:VTIMEZONE
TZID:America/New_York
BEGIN:STANDARD
DTSTART:19981025T020000
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19990404T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE
BEGIN:VEVENT
DTSTAMP:19980309T231000Z
UID:guid-1.example.com
ORGANIZER:mailto:mrbig@example.com
ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT;CUTYPE=GROUP:mailto:employee-A@example.com
DESCRIPTION:Project XYZ Review Meeting
CATEGORIES:MEETING
CLASS:PUBLIC
CREATED:19980309T130000Z
SUMMARY:XYZ Project Review
DTSTART;TZID=America/New_York:19980312T083000
DTEND;TZID=America/New_York:19980312T093000
LOCATION:1CP Conference Room 4350
END:VEVENT
END:VCALENDAR
|}
    );
    ( "MIME message VEVENT",
      {|BEGIN:VCALENDAR
METHOD:xyz
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VEVENT
DTSTAMP:19970324T120000Z
SEQUENCE:0
UID:uid3@example.com
ORGANIZER:mailto:jdoe@example.com
ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com
DTSTART:19970324T123000Z
DTEND:19970324T210000Z
CATEGORIES:MEETING,PROJECT
CLASS:PUBLIC
SUMMARY:Calendaring Interoperability Planning Meeting
DESCRIPTION:Discuss how we can test c&s interoperability\nusing iCalendar and other IETF standards.
LOCATION:LDB Lobby
ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/conf/bkgrnd.ps
END:VEVENT
END:VCALENDAR
|}
    );
    ( "to-do with alarm",
      {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VTODO
DTSTAMP:19980130T134500Z
SEQUENCE:2
UID:uid4@example.com
ORGANIZER:mailto:unclesam@example.com
ATTENDEE;PARTSTAT=ACCEPTED:mailto:jqpublic@example.com
DUE:19980415T000000
STATUS:NEEDS-ACTION
SUMMARY:Submit Income Taxes
BEGIN:VALARM
ACTION:AUDIO
TRIGGER;VALUE=DATE-TIME:19980403T120000Z
ATTACH;FMTTYPE=audio/basic:http://example.com/pub/audio-files/ssbanner.aud
REPEAT:4
DURATION:PT1H
END:VALARM
END:VTODO
END:VCALENDAR
|}
    );
    ( "journal entry",
      {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VJOURNAL
DTSTAMP:19970324T120000Z
UID:uid5@example.com
ORGANIZER:mailto:jsmith@example.com
STATUS:DRAFT
CLASS:PUBLIC
CATEGORIES:Project Report,XYZ,Weekly Meeting
DESCRIPTION:Project xyz Review Meeting Minutes\nAgenda\n1. Review of project version 1.0 requirements.\n2. Definition of project processes.\n3. Review of project schedule.\nParticipants: John Smith\, Jane Doe\, Jim Dandy\n-It was decided that the requirements need to be signed off by product marketing.\n-Project processes were accepted.\n-Project schedule needs to account for scheduled holidays and employee vacation time. Check with HR for specific dates.\n-New schedule will be distributed by Friday.\n-Next weeks meeting is cancelled. No meeting until 3/23.
END:VJOURNAL
END:VCALENDAR
|}
    );
    ( "published busy time",
      {|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//RDU Software//NONSGML HandCal//EN
BEGIN:VFREEBUSY
ORGANIZER:mailto:jsmith@example.com
DTSTART:19980313T141711Z
DTEND:19980410T141711Z
FREEBUSY:19980314T233000Z/19980315T003000Z
FREEBUSY:19980316T153000Z/19980316T163000Z
FREEBUSY:19980318T030000Z/19980318T040000Z
URL:http://www.example.com/calendar/busytime/jsmith.ifb
END:VFREEBUSY
END:VCALENDAR
|}
    );
  ]

(* RFC 5545 Section 3.6.1: VEVENT examples. *)
let s361_vevent =
  [
    ( "vevent opaque",
      wrap
        {|BEGIN:VEVENT
UID:19970901T130000Z-123401@example.com
DTSTAMP:19970901T130000Z
DTSTART:19970903T163000Z
DTEND:19970903T190000Z
SUMMARY:Annual Employee Review
CLASS:PRIVATE
CATEGORIES:BUSINESS,HUMAN RESOURCES
END:VEVENT|}
    );
    ( "vevent transparent",
      wrap
        {|BEGIN:VEVENT
UID:19970901T130000Z-123402@example.com
DTSTAMP:19970901T130000Z
DTSTART:19970401T163000Z
DTEND:19970402T010000Z
SUMMARY:Laurel is in sensitivity awareness class.
CLASS:PUBLIC
CATEGORIES:BUSINESS,HUMAN RESOURCES
TRANSP:TRANSPARENT
END:VEVENT|}
    );
    ( "vevent anniversary",
      wrap
        {|BEGIN:VEVENT
UID:19970901T130000Z-123403@example.com
DTSTAMP:19970901T130000Z
DTSTART;VALUE=DATE:19971102
SUMMARY:Our Blissful Anniversary
TRANSP:TRANSPARENT
CLASS:CONFIDENTIAL
CATEGORIES:ANNIVERSARY,PERSONAL,SPECIAL OCCASION
RRULE:FREQ=YEARLY
END:VEVENT|}
    );
    ( "vevent multi-day",
      wrap
        {|BEGIN:VEVENT
UID:20070423T123432Z-541111@example.com
DTSTAMP:20070423T123432Z
DTSTART;VALUE=DATE:20070628
DTEND;VALUE=DATE:20070709
SUMMARY:Festival International de Jazz de Montreal
TRANSP:TRANSPARENT
END:VEVENT|}
    );
  ]

(* RFC 5545 Section 3.6.2: VTODO examples. *)
let s362_vtodo =
  [
    ( "vtodo due",
      wrap
        {|BEGIN:VTODO
UID:20070313T123432Z-456553@example.com
DTSTAMP:20070313T123432Z
DUE;VALUE=DATE:20070501
SUMMARY:Submit Quebec Income Tax Return for 2006
CLASS:CONFIDENTIAL
CATEGORIES:FAMILY,FINANCE
STATUS:NEEDS-ACTION
END:VTODO|}
    );
    ( "vtodo completed",
      wrap
        {|BEGIN:VTODO
UID:20070514T103211Z-123404@example.com
DTSTAMP:20070514T103211Z
DTSTART:20070514T110000Z
DUE:20070709T130000Z
COMPLETED:20070707T100000Z
SUMMARY:Submit Revised Internet-Draft
PRIORITY:1
STATUS:NEEDS-ACTION
END:VTODO|}
    );
  ]

(* RFC 5545 Section 3.6.3: VJOURNAL example. *)
let s363_vjournal =
  [
    ( "vjournal",
      wrap
        {|BEGIN:VJOURNAL
UID:19970901T130000Z-123405@example.com
DTSTAMP:19970901T130000Z
DTSTART;VALUE=DATE:19970317
SUMMARY:Staff meeting minutes
DESCRIPTION:1. Staff meeting: Participants include Joe\, Lisa\, and Bob. Aurora project plans were reviewed. There is currently no budget reserves for this project. Lisa will escalate to management. Next meeting on Tuesday.\n2. Telephone Conference: ABC Corp. sales representative called to discuss new printer. Promised to get us a demo by Friday.\n3. Henry Miller (Handsoff Insurance): Car was totaled by tree. Is looking into a loaner car. 555-2323 (tel).
END:VJOURNAL|}
    );
  ]

(* RFC 5545 Section 3.6.4: VFREEBUSY examples. *)
let s364_vfreebusy =
  [
    ( "vfreebusy request",
      wrap
        {|BEGIN:VFREEBUSY
UID:19970901T082949Z-FA43EF@example.com
ORGANIZER:mailto:jane_doe@example.com
ATTENDEE:mailto:john_public@example.com
DTSTART:19971015T050000Z
DTEND:19971016T050000Z
DTSTAMP:19970901T083000Z
END:VFREEBUSY|}
    );
    ( "vfreebusy reply",
      wrap
        {|BEGIN:VFREEBUSY
UID:19970901T095957Z-76A912@example.com
ORGANIZER:mailto:jane_doe@example.com
ATTENDEE:mailto:john_public@example.com
DTSTAMP:19970901T100000Z
FREEBUSY:19971015T050000Z/PT8H30M,19971015T160000Z/PT5H30M,19971015T223000Z/PT6H30M
URL:http://example.com/pub/busy/jpublic-01.ifb
COMMENT:This iCalendar file contains busy time information for the next three months.
END:VFREEBUSY|}
    );
    ( "vfreebusy publish",
      wrap
        {|BEGIN:VFREEBUSY
UID:19970901T115957Z-76A912@example.com
DTSTAMP:19970901T120000Z
ORGANIZER:jsmith@example.com
DTSTART:19980313T141711Z
DTEND:19980410T141711Z
FREEBUSY:19980314T233000Z/19980315T003000Z
FREEBUSY:19980316T153000Z/19980316T163000Z
FREEBUSY:19980318T030000Z/19980318T040000Z
URL:http://www.example.com/calendar/busytime/jsmith.ifb
END:VFREEBUSY|}
    );
  ]

(* RFC 5545 Section 3.6.5: VTIMEZONE examples. *)
let s365_vtimezone =
  [
    ( "vtimezone all rules since 1967",
      wrap
        {|BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:DAYLIGHT
DTSTART:19670430T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19740106T020000
RDATE:19750223T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19760425T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
END:VTIMEZONE|}
    );
    ( "vtimezone dtstart only",
      wrap
        {|BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
BEGIN:STANDARD
DTSTART:20071104T020000
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20070311T020000
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE|}
    );
    ( "vtimezone rrule pattern",
      wrap
        {|BEGIN:VTIMEZONE
TZID:America/New_York
LAST-MODIFIED:20050809T050000Z
TZURL:http://zones.example.com/tz/America-New_York.ics
BEGIN:STANDARD
DTSTART:20071104T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:20070311T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE|}
    );
    ( "vtimezone fictitious end date",
      wrap
        {|BEGIN:VTIMEZONE
TZID:Fictitious
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE|}
    );
    ( "vtimezone fictitious two daylight rules",
      wrap
        {|BEGIN:VTIMEZONE
TZID:Fictitious
LAST-MODIFIED:19870101T000000Z
BEGIN:STANDARD
DTSTART:19671029T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
END:STANDARD
BEGIN:DAYLIGHT
DTSTART:19870405T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
BEGIN:DAYLIGHT
DTSTART:19990424T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=4
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
END:DAYLIGHT
END:VTIMEZONE|}
    );
  ]

(* RFC 5545 Section 3.6.6: VALARM examples, each nested in a minimal VEVENT. *)
let s366_valarm =
  [
    ( "valarm audio",
      wrap_alarm
        {|BEGIN:VALARM
TRIGGER;VALUE=DATE-TIME:19970317T133000Z
REPEAT:4
DURATION:PT15M
ACTION:AUDIO
ATTACH;FMTTYPE=audio/basic:ftp://example.com/pub/sounds/bell-01.aud
END:VALARM|}
    );
    ( "valarm display",
      wrap_alarm
        {|BEGIN:VALARM
TRIGGER:-PT30M
REPEAT:2
DURATION:PT15M
ACTION:DISPLAY
DESCRIPTION:Breakfast meeting with executive\nteam at 8:30 AM EST.
END:VALARM|}
    );
    ( "valarm email",
      wrap_alarm
        {|BEGIN:VALARM
TRIGGER;RELATED=END:-P2D
ACTION:EMAIL
ATTENDEE:mailto:john_doe@example.com
SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***
DESCRIPTION:A draft agenda needs to be sent out to the attendees to the weekly managers meeting (MGR-LIST). Attached is a pointer the document template for the agenda file.
ATTACH;FMTTYPE=application/msword:http://example.com/templates/agenda.doc
END:VALARM|}
    );
  ]

let all_roundtrip_examples =
  s4_examples @ s361_vevent @ s362_vtodo @ s363_vjournal @ s364_vfreebusy
  @ s365_vtimezone @ s366_valarm

let test_roundtrip () =
  List.iter
    (fun (name, s) ->
      let t1 = ok name (Ical.one_of_string s) in
      let s1 = Ical.to_string t1 in
      let t2 = ok (name ^ " reparsed") (Ical.one_of_string s1) in
      Alcotest.(check bool) (name ^ " equal") true (Ical.equal t1 t2);
      let s2 = Ical.to_string t2 in
      Alcotest.(check string) (name ^ " fixed point") s1 s2)
    all_roundtrip_examples

(* RFC 5545 Section 3.8.5.3 and Section 3.3.10: RRULE examples. *)
let rrule_examples =
  [
    "FREQ=DAILY;COUNT=10";
    "FREQ=DAILY;UNTIL=19971224T000000Z";
    "FREQ=DAILY;INTERVAL=2";
    "FREQ=DAILY;INTERVAL=10;COUNT=5";
    "FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA";
    "FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1";
    "FREQ=WEEKLY;COUNT=10";
    "FREQ=WEEKLY;UNTIL=19971224T000000Z";
    "FREQ=WEEKLY;INTERVAL=2;WKST=SU";
    "FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH";
    "FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH";
    "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR";
    "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH";
    "FREQ=MONTHLY;COUNT=10;BYDAY=1FR";
    "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR";
    "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU";
    "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO";
    "FREQ=MONTHLY;BYMONTHDAY=-3";
    "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15";
    "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1";
    "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15";
    "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU";
    "FREQ=YEARLY;COUNT=10;BYMONTH=6,7";
    "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3";
    "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200";
    "FREQ=YEARLY;BYDAY=20MO";
    "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO";
    "FREQ=YEARLY;BYMONTH=3;BYDAY=TH";
    "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8";
    "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13";
    "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13";
    "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8";
    "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3";
    "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2";
    "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z";
    "FREQ=MINUTELY;INTERVAL=15;COUNT=6";
    "FREQ=MINUTELY;INTERVAL=90;COUNT=4";
    "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40";
    "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16";
    "FREQ=DAILY;COUNT=10;INTERVAL=2";
    "FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30";
  ]

let test_recur () =
  List.iter
    (fun s ->
      let r1 = ok s (Ical.Recur.of_string s) in
      let r1 = ok s (Ical.Recur.validate r1) in
      let s2 = Ical.Recur.to_string r1 in
      let r2 = ok (s ^ " reparsed") (Ical.Recur.of_string s2) in
      Alcotest.(check bool) s true (Ical.Recur.equal r1 r2))
    rrule_examples;
  let has s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
    go 0
  in
  let r = ok "byday" (Ical.Recur.of_string "FREQ=MONTHLY;BYDAY=2MO,-1FR") in
  let s = Ical.Recur.to_string r in
  Alcotest.(check bool)
    "positive ordinal has no plus sign" true
    (has s "2MO" && not (has s "+2MO"));
  Alcotest.(check bool) "negative ordinal keeps its sign" true (has s "-1FR")

(* RFC 5545 Section 3.3.6 and 3.3.9: DURATION and PERIOD examples. *)
let test_duration () =
  let module D = Ical.Duration in
  List.iter
    (fun s ->
      let d = ok s (D.of_string s) in
      Alcotest.(check string) s s (D.to_string d))
    [ "P15DT5H0M20S"; "P7W"; "PT0S"; "-PT30M"; "-P2D" ];
  Alcotest.(check string) "zero" "PT0S" (D.to_string D.zero);
  Alcotest.(check int)
    "to_seconds" 1314020
    (D.to_seconds (ok "d" (D.of_string "P15DT5H0M20S")));
  let d = D.of_seconds 1314020 in
  Alcotest.(check string) "of_seconds" "P15DT5H0M20S" (D.to_string d);
  List.iter
    (fun s ->
      let p = ok s (Ical.Period.of_string s) in
      Alcotest.(check string) s s (Ical.Period.to_string p))
    [ "19970101T180000Z/19970102T070000Z"; "19970101T180000Z/PT5H30M" ];
  let p = ok "period" (Ical.Period.of_string "19970101T180000Z/PT5H30M") in
  let e = Ical.Period.finish p in
  Alcotest.(check string)
    "period_end" "19970101T233000Z"
    (Ical.Date.date_time_to_string e)

(* RFC 5545 Section 3.3.5, 3.3.12 and 3.3.14: DATE-TIME, TIME and
   UTC-OFFSET examples. *)
let test_date () =
  let module Dt = Ical.Date in
  List.iter
    (fun s ->
      let d = ok s (Dt.date_time_of_string s) in
      Alcotest.(check string) s s (Dt.date_time_to_string d))
    [ "19980118T230000"; "19980119T070000Z"; "19970714T133000" ];
  List.iter
    (fun s ->
      let t = ok s (Dt.time_of_string s) in
      Alcotest.(check string) s s (Dt.time_to_string t))
    [ "083000"; "133000Z" ];
  List.iter
    (fun s ->
      let n = ok s (Dt.utc_offset_of_string s) in
      Alcotest.(check string) s s (Dt.utc_offset_to_string n))
    [ "-0500"; "+0100" ];
  fails "-0000 rejected" (Dt.utc_offset_of_string "-0000");
  Alcotest.(check string)
    "19970714" "19970714"
    (Dt.to_string (ok "date" (Dt.of_string "19970714")))

(* RFC 5545 Section 3.3.11: TEXT escaping. *)
let test_text () =
  let module P = Ical.Property in
  let p = P.of_text "DESCRIPTION" "a\\b,c;d\ne" in
  Alcotest.(check string) "escaped wire value" "a\\\\b\\,c;d\\ne" (P.value p);
  Alcotest.(check string) "unescaped" "a\\b,c;d\ne" (P.text p);
  Alcotest.(check string) "unescape \\n" "x\ny" (Vcard.Text.unescape "x\\ny");
  Alcotest.(check string) "unescape \\," "x,y" (Vcard.Text.unescape "x\\,y");
  Alcotest.(check string) "unescape \\;" "x;y" (Vcard.Text.unescape "x\\;y");
  Alcotest.(check string) "escape newline" "x\\ny" (Vcard.Text.escape "x\ny");
  Alcotest.(check string) "escape comma" "x\\,y" (Vcard.Text.escape "x,y");
  Alcotest.(check string)
    "escape semicolon kept" "x;y" (Vcard.Text.escape "x;y")

(* RFC 5545 Section 3.1: folding a line longer than 75 octets, splitting
   at a multi-byte character boundary. *)
let test_fold () =
  let s = "SUMMARY:" ^ String.concat "" (List.init 40 (fun _ -> "\xc3\xa9")) in
  let folded = Vcard.fold s in
  let lines = String.split_on_char '\n' folded in
  Alcotest.(check bool) "more than one line" true (List.length lines > 1);
  List.iter
    (fun l ->
      let l =
        if String.length l > 0 && l.[String.length l - 1] = '\r' then
          String.sub l 0 (String.length l - 1)
        else l
      in
      Alcotest.(check bool) "valid utf-8" true (String.is_valid_utf_8 l);
      Alcotest.(check bool) "at most 75 octets" true (String.length l <= 75))
    lines;
  Alcotest.(check (list string)) "unfolds back" [ s ] (Vcard.unfold folded)

(* RFC 5545 Section 3.6: validation. *)
let test_validate () =
  fails "VEVENT without UID"
    (Ical.validate
       (ok "parse"
          (Ical.one_of_string
             (wrap
                "BEGIN:VEVENT\r\n\
                 DTSTAMP:20200101T000000Z\r\n\
                 DTSTART:20200101T090000Z\r\n\
                 END:VEVENT"))));
  fails "VEVENT with DTEND and DURATION"
    (Ical.validate
       (ok "parse"
          (Ical.one_of_string
             (wrap
                "BEGIN:VEVENT\r\n\
                 UID:e1@example.com\r\n\
                 DTSTAMP:20200101T000000Z\r\n\
                 DTSTART:20200101T090000Z\r\n\
                 DTEND:20200101T100000Z\r\n\
                 DURATION:PT1H\r\n\
                 END:VEVENT"))));
  fails "VTIMEZONE without STANDARD or DAYLIGHT"
    (Ical.validate
       (ok "parse"
          (Ical.one_of_string
             (wrap "BEGIN:VTIMEZONE\r\nTZID:X\r\nEND:VTIMEZONE"))));
  fails "RRULE with UNTIL and COUNT"
    (Ical.Recur.validate
       (ok "rrule"
          (Ical.Recur.of_string "FREQ=DAILY;UNTIL=19971224T000000Z;COUNT=5")));
  let good =
    ok "parse"
      (Ical.one_of_string
         (wrap
            "BEGIN:VEVENT\r\n\
             UID:e2@example.com\r\n\
             DTSTAMP:20200101T000000Z\r\n\
             DTSTART:20200101T090000Z\r\n\
             DTEND:20200101T100000Z\r\n\
             END:VEVENT"))
  in
  ignore (ok "validate good" (Ical.validate good))

(* RFC 4791 Section 9.9: Component.dtend. *)
let test_dtend () =
  let module C = Ical.Component in
  let d name props =
    Option.get (ok name (C.dtend (C.v ~properties:props "VEVENT")))
  in
  Alcotest.(check string)
    "DTEND present" "20200101T100000Z"
    (Ical.Date.to_string
       (d "dtend"
          [
            Ical.Property.v "DTSTART" "20200101T090000Z";
            Ical.Property.v "DTEND" "20200101T100000Z";
          ]));
  Alcotest.(check string)
    "DUE present" "20200101T100000Z"
    (Ical.Date.to_string (d "due" [ Ical.Property.v "DUE" "20200101T100000Z" ]));
  Alcotest.(check string)
    "DTSTART plus DURATION" "20200101T103000Z"
    (Ical.Date.to_string
       (d "duration"
          [
            Ical.Property.v "DTSTART" "20200101T090000Z";
            Ical.Property.v "DURATION" "PT1H30M";
          ]));
  Alcotest.(check string)
    "DATE DTSTART next day" "20200102"
    (Ical.Date.to_string
       (d "date"
          [
            Ical.Property.v
              ~params:[ Vcard.Param.v "VALUE" [ "DATE" ] ]
              "DTSTART" "20200101";
          ]));
  Alcotest.(check string)
    "DATE-TIME DTSTART itself" "20200101T090000Z"
    (Ical.Date.to_string
       (d "datetime" [ Ical.Property.v "DTSTART" "20200101T090000Z" ]))

(* Ical.uid. *)
let test_uid () =
  let module C = Ical.Component in
  let ev uid = C.v ~properties:[ Ical.Property.v "UID" uid ] "VEVENT" in
  let tz = C.v ~properties:[ Ical.Property.v "TZID" "X" ] "VTIMEZONE" in
  let matching = Ical.v [ ev "a@example.com"; ev "a@example.com"; tz ] in
  Alcotest.(check (option string))
    "matching" (Some "a@example.com") (Ical.uid matching);
  let differing = Ical.v [ ev "a@example.com"; ev "b@example.com" ] in
  Alcotest.(check (option string)) "differing" None (Ical.uid differing);
  let none = Ical.v [ tz ] in
  Alcotest.(check (option string))
    "no non-timezone components" None (Ical.uid none)

let () =
  Alcotest.run "ical"
    [
      ( "ical",
        [
          Alcotest.test_case "round trips" `Quick test_roundtrip;
          Alcotest.test_case "RRULE examples" `Quick test_recur;
          Alcotest.test_case "DURATION and PERIOD" `Quick test_duration;
          Alcotest.test_case "DATE-TIME, TIME, UTC-OFFSET" `Quick test_date;
          Alcotest.test_case "TEXT escaping" `Quick test_text;
          Alcotest.test_case "folding" `Quick test_fold;
          Alcotest.test_case "validate" `Quick test_validate;
          Alcotest.test_case "Component.dtend" `Quick test_dtend;
          Alcotest.test_case "Ical.uid" `Quick test_uid;
        ] );
    ]
