(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The CalDAV client against a Radicale server, RFC 4791 end to end. *)

open Caldav_oracle
module Client = Caldav_eio.Client
module Data = Caldav.Data
module Filter = Caldav.Filter
module CD = Caldav.Calendar_data

let join collection name =
  if String.ends_with ~suffix:"/" collection then collection ^ name
  else collection ^ "/" ^ name

let paths entries =
  List.sort compare
    (List.map (fun (e : _ Client.entry) -> Httpz_dav.href_path e.href) entries)

let member_paths ms =
  List.sort compare
    (List.map (fun (m : Client.member) -> Httpz_dav.href_path m.href) ms)

let path h = Httpz_dav.href_path h
let sorted l = List.sort compare l
let ical = Data.ical

let fails_with what f =
  match f () with
  | Error (Client.Not_found _) when what = "404" -> ()
  | Error (Client.Precondition_failed _) when what = "412" -> ()
  | Error e ->
      Alcotest.failf "expected %s, got: %s" what (Client.error_to_string e)
  | Ok _ -> Alcotest.failf "expected %s, got success" what

let test_discovery t =
  Alcotest.(check bool)
    "principal names the user" true
    (String.ends_with ~suffix:("/" ^ t.user ^ "/") (Client.principal t.client));
  Alcotest.(check bool) "a home set" true (Client.home_sets t.client <> [])

let test_calendars t =
  let url = fresh_calendar ~components:[ "VEVENT"; "VTODO" ] t in
  let cals = ok "calendars" (Client.calendars t.client) in
  let cal =
    match
      List.find_opt
        (fun (c : Caldav.Calendar.t) -> Httpz_dav.same_href c.href url)
        cals
    with
    | Some c -> c
    | None -> Alcotest.fail "the new calendar is not listed"
  in
  Alcotest.(check (option string))
    "display name" (Some "Oracle") cal.display_name;
  Alcotest.(check (list string))
    "components" [ "VEVENT"; "VTODO" ] cal.components;
  Alcotest.(check bool)
    "accepts VEVENT" true
    (Caldav.Calendar.accepts ~component:"VEVENT" cal);
  Alcotest.(check bool)
    "refuses VJOURNAL" false
    (Caldav.Calendar.accepts ~component:"VJOURNAL" cal);
  Alcotest.(check bool)
    "query report" true
    (Caldav.Calendar.supports Caldav.Property.calendar_query cal);
  Alcotest.(check bool)
    "multiget report" true
    (Caldav.Calendar.supports Caldav.Property.calendar_multiget cal);
  Alcotest.(check bool)
    "sync report" true
    (Caldav.Calendar.supports (Httpz_dav.dav "sync-collection") cal);
  Alcotest.(check bool) "a ctag" true (cal.ctag <> None);
  Alcotest.(check bool) "a sync token" true (cal.sync_token <> None);
  ok "rename"
    (Client.set_props t.client url
       [
         Httpz_dav.Set
           [
             Httpz_dav.leaf Httpz_dav.Prop.displayname "Renamed";
             Caldav.Property.description "Described";
           ];
       ]);
  let cal = ok "calendar" (Client.calendar t.client url) in
  Alcotest.(check (option string)) "renamed" (Some "Renamed") cal.display_name;
  Alcotest.(check (option string))
    "described" (Some "Described") cal.description;
  ok "delete calendar" (Client.delete_calendar t.client url);
  fails_with "404" (fun () -> Client.calendar t.client url)

let test_calendar_with_timezone t =
  let tz =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//idk//EN\r\n\
     BEGIN:VTIMEZONE\r\n\
     TZID:Europe/London\r\n\
     BEGIN:STANDARD\r\n\
     DTSTART:19701025T020000\r\n\
     RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU\r\n\
     TZOFFSETFROM:+0100\r\n\
     TZOFFSETTO:+0000\r\n\
     END:STANDARD\r\n\
     BEGIN:DAYLIGHT\r\n\
     DTSTART:19700329T010000\r\n\
     RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU\r\n\
     TZOFFSETFROM:+0000\r\n\
     TZOFFSETTO:+0100\r\n\
     END:DAYLIGHT\r\n\
     END:VTIMEZONE\r\n\
     END:VCALENDAR\r\n"
  in
  let home = List.hd (Client.home_sets t.client) in
  let url = join home (unique "tzcal") ^ "/" in
  ok "create with timezone"
    (Client.create_calendar t.client ~display_name:"Zoned"
       ~description:"With a zone" ~timezone:tz url);
  let cal = ok "calendar" (Client.calendar t.client url) in
  Alcotest.(check (option string))
    "description" (Some "With a zone") cal.description;
  (match cal.timezone with
  | Some text -> (
      match Ical.one_of_string text with
      | Ok c ->
          Alcotest.(check bool)
            "a VTIMEZONE" true
            (Ical.find_timezone c "Europe/London" <> None)
      | Error e -> Alcotest.failf "timezone does not parse: %s" e)
  | None ->
      Printf.printf "  [note] the server does not store calendar-timezone\n");
  ok "delete" (Client.delete_calendar t.client url)

let test_objects t =
  let url = fresh_calendar t in
  let ev =
    event ~summary:"Standup"
      (utc ~year:2026 ~month:9 ~day:10 ~hour:9 ~minute:0)
      (utc ~year:2026 ~month:9 ~day:10 ~hour:9 ~minute:15)
  in
  let { Client.href; etag; _ } = ok "add" (Client.add ical t.client url ev) in
  Alcotest.(check bool)
    "named by uid" true
    (String.ends_with ~suffix:(uid ev ^ ".ics") href);
  Alcotest.(check bool) "an etag" true (etag <> None);
  let got = ok "get" (Client.get ical t.client href) in
  Alcotest.(check (option string))
    "summary" (Some "Standup") (summary got.value);
  Alcotest.(check (option string)) "uid" (Some (uid ev)) (Ical.uid got.value);
  Alcotest.(check (option string)) "etag agrees" etag got.etag;
  let listed = ok "list" (Client.list t.client url) in
  Alcotest.(check (list string)) "listed" [ path href ] (member_paths listed);
  Alcotest.(check bool)
    "content type" true
    (Option.fold ~none:false
       ~some:(String.starts_with ~prefix:"text/calendar")
       (List.hd listed).content_type);
  fails_with "412" (fun () -> Client.put ical t.client ~create:true href ev);
  fails_with "412" (fun () ->
      Client.put ical t.client ~etag:"\"stale\"" href ev);
  let updated =
    Ical.v
      (List.map
         (fun c ->
           Ical.Component.replace c
             (Ical.Property.of_text "SUMMARY" "Standup (moved)"))
         (Ical.components ev))
  in
  let etag2 = ok "update" (Client.put ical t.client ?etag href updated) in
  Alcotest.(check bool) "etag changed" true (etag2 <> etag);
  let got = ok "get" (Client.get ical t.client href) in
  Alcotest.(check (option string))
    "updated" (Some "Standup (moved)") (summary got.value);
  fails_with "412" (fun () -> Client.delete t.client ~etag:"\"stale\"" href);
  ok "delete" (Client.delete t.client ?etag:etag2 href);
  fails_with "404" (fun () -> Client.get ical t.client href)

let test_component_kinds t =
  let url = fresh_calendar ~components:[ "VEVENT"; "VTODO"; "VJOURNAL" ] t in
  let cal = ok "calendar" (Client.calendar t.client url) in
  let _ =
    ok "event"
      (Client.add ical t.client url
         (event ~summary:"E"
            (utc ~year:2026 ~month:9 ~day:1 ~hour:8 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)))
  in
  (* RFC 4791 Section 5.3.1 lets a server restrict the components a calendar
     stores. With the fixed_components quirk the client refuses a VTODO
     before the server can, naming the component. *)
  if
    not
      (Caldav.Calendar.accepts ~component:"VTODO" cal
      && Caldav.Calendar.accepts ~component:"VJOURNAL" cal)
  then begin
    Printf.printf "  [note] the server stores only %s in a new calendar\n"
      (String.concat ", " cal.components);
    Alcotest.(check bool)
      "quirk names the fixed set" true (Client.quirks t.client).fixed_components;
    match
      Client.add ical t.client url
        (todo ~summary:"T" (utc ~year:2026 ~month:9 ~day:2 ~hour:17 ~minute:0))
    with
    | Error (Client.Data m) ->
        Alcotest.(check bool)
          "refused by the client" true
          (try
             ignore (Str.search_forward (Str.regexp_string "VTODO") m 0);
             true
           with Not_found -> false)
    | Error e -> Alcotest.failf "unexpected: %s" (Client.error_to_string e)
    | Ok _ -> Alcotest.fail "the todo was stored"
  end
  else begin
    let { Client.href = td; _ } =
      ok "todo"
        (Client.add ical t.client url
           (todo ~summary:"T"
              (utc ~year:2026 ~month:9 ~day:2 ~hour:17 ~minute:0)))
    in
    let { Client.href = jn; _ } =
      ok "journal"
        (Client.add ical t.client url
           (journal ~summary:"J" { year = 2026; month = 9; day = 3 }))
    in
    let todos =
      (ok "query todos"
         (Client.query ical t.client url (Filter.components "VTODO")))
        .entries
    in
    Alcotest.(check (list string)) "todos" [ path td ] (paths todos);
    let journals =
      (ok "query journals"
         (Client.query ical t.client url (Filter.components "VJOURNAL")))
        .entries
    in
    Alcotest.(check (list string)) "journals" [ path jn ] (paths journals);
    let all =
      (ok "query all" (Client.query ical t.client url Filter.all)).entries
    in
    Alcotest.(check int) "three" 3 (List.length all);
    let raw = ok "raw" (Client.get Data.raw t.client td) in
    Alcotest.(check bool)
      "raw text" true
      (String.starts_with ~prefix:"BEGIN:VCALENDAR" raw.value)
  end

let test_time_range t =
  let url = fresh_calendar t in
  let { Client.href = a; _ } =
    ok "a"
      (Client.add ical t.client url
         (event ~summary:"Early"
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)))
  in
  let { Client.href = b; _ } =
    ok "b"
      (Client.add ical t.client url
         (event ~summary:"Mid"
            (utc ~year:2026 ~month:9 ~day:15 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:15 ~hour:10 ~minute:0)))
  in
  let { Client.href = c; _ } =
    ok "c"
      (Client.add ical t.client url
         (event ~summary:"Late"
            (utc ~year:2026 ~month:9 ~day:28 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:28 ~hour:10 ~minute:0)))
  in
  let { Client.href = d; _ } =
    ok "d"
      (Client.add ical t.client url
         (event ~summary:"All day"
            ~all_day:{ year = 2026; month = 9; day = 20 }
            (utc ~year:2026 ~month:9 ~day:20 ~hour:0 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:21 ~hour:0 ~minute:0)))
  in
  let { Client.href = sp; _ } =
    ok "e"
      (Client.add ical t.client url
         (event ~summary:"Spanning"
            (utc ~year:2026 ~month:9 ~day:9 ~hour:23 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:11 ~hour:1 ~minute:0)))
  in
  let between s f =
    ok "events" (Client.events ical t.client ~start:s ~finish:f url)
  in
  Alcotest.(check (list string))
    "mid September"
    (sorted [ path b; path d; path sp ])
    (paths
       (between
          (utc ~year:2026 ~month:9 ~day:10 ~hour:0 ~minute:0)
          (utc ~year:2026 ~month:9 ~day:25 ~hour:0 ~minute:0)));
  Alcotest.(check (list string))
    "first day"
    [ path a ]
    (paths
       (between
          (utc ~year:2026 ~month:9 ~day:1 ~hour:0 ~minute:0)
          (utc ~year:2026 ~month:9 ~day:2 ~hour:0 ~minute:0)));
  Alcotest.(check (list string))
    "end excluded" []
    (paths
       (between
          (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)
          (utc ~year:2026 ~month:9 ~day:1 ~hour:11 ~minute:0)));
  Alcotest.(check (list string))
    "open start"
    (sorted [ path a; path sp ])
    (paths
       (ok "open"
          (Client.events ical t.client
             ~finish:(utc ~year:2026 ~month:9 ~day:10 ~hour:0 ~minute:0)
             url)));
  Alcotest.(check (list string))
    "open end"
    (sorted [ path c; path d ])
    (paths
       (ok "open"
          (Client.events ical t.client
             ~start:(utc ~year:2026 ~month:9 ~day:20 ~hour:0 ~minute:0)
             url)));
  let all = ok "all" (Client.events ical t.client url) in
  Alcotest.(check int) "five" 5 (List.length all);
  (* The local matcher agrees with the server for non-recurring events. *)
  let filter =
    Filter.components
      ~time_range:
        (Filter.time_range
           ~start:(utc ~year:2026 ~month:9 ~day:10 ~hour:0 ~minute:0)
           ~finish:(utc ~year:2026 ~month:9 ~day:25 ~hour:0 ~minute:0)
           ())
      "VEVENT"
  in
  List.iter
    (fun (e : _ Client.entry) ->
      Alcotest.(check bool)
        ("matches " ^ path e.href)
        (List.mem (path e.href) [ path b; path d; path sp ])
        (Filter.matches filter e.value))
    all

let test_recurrence t =
  let url = fresh_calendar t in
  let weekly =
    event ~summary:"Weekly" ~rrule:"FREQ=WEEKLY;COUNT=4"
      (utc ~year:2026 ~month:9 ~day:7 ~hour:9 ~minute:0)
      (utc ~year:2026 ~month:9 ~day:7 ~hour:10 ~minute:0)
  in
  let { Client.href; _ } = ok "add" (Client.add ical t.client url weekly) in
  let hit =
    ok "instance range"
      (Client.events ical t.client
         ~start:(utc ~year:2026 ~month:9 ~day:20 ~hour:0 ~minute:0)
         ~finish:(utc ~year:2026 ~month:9 ~day:22 ~hour:0 ~minute:0)
         url)
  in
  Alcotest.(check (list string))
    "third instance overlaps"
    [ path href ]
    (paths hit);
  let miss =
    ok "after last"
      (Client.events ical t.client
         ~start:(utc ~year:2026 ~month:10 ~day:5 ~hour:0 ~minute:0)
         ~finish:(utc ~year:2026 ~month:10 ~day:12 ~hour:0 ~minute:0)
         url)
  in
  Alcotest.(check (list string)) "no fifth instance" [] (paths miss);
  let expanded =
    ok "expand"
      (Client.events ical t.client ~expand:true
         ~start:(utc ~year:2026 ~month:9 ~day:1 ~hour:0 ~minute:0)
         ~finish:(utc ~year:2026 ~month:10 ~day:1 ~hour:0 ~minute:0)
         url)
  in
  (match expanded with
  | [ e ] ->
      let instances = Ical.events e.value in
      Alcotest.(check int) "four instances" 4 (List.length instances);
      (* RFC 4791 Section 9.6.5 puts a RECURRENCE-ID on every instance.
         Fastmail leaves it off the first, which is noted. *)
      Alcotest.(check bool)
        "each has RECURRENCE-ID" true
        (List.for_all
           (fun c -> Ical.Component.find c "RECURRENCE-ID" <> None)
           instances);
      Alcotest.(check bool)
        "no RRULE left" true
        (List.for_all (fun c -> Ical.Component.find c "RRULE" = None) instances)
  | _ -> Alcotest.fail "one resource expected");
  let got = ok "get" (Client.get ical t.client href) in
  match Ical.events got.value with
  | [ c ] -> (
      match Ical.Component.find c "RRULE" with
      | Some p -> (
          match Ical.Property.recur p with
          | Ok r -> Alcotest.(check (option int)) "count" (Some 4) r.count
          | Error e -> Alcotest.fail e)
      | None -> Alcotest.fail "RRULE lost")
  | _ -> Alcotest.fail "one event expected"

let test_text_and_params t =
  let url = fresh_calendar t in
  let att =
    Ical.Property.v
      ~params:
        [
          Vcard.Param.v "PARTSTAT" [ "ACCEPTED" ]; Vcard.Param.v "CN" [ "Bob" ];
        ]
      "ATTENDEE" "mailto:bob@example.com"
  in
  let { Client.href = a; _ } =
    ok "a"
      (Client.add ical t.client url
         (event ~summary:"Budget review"
            ~props:[ att; Ical.Property.of_text "LOCATION" "Room 1" ]
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)))
  in
  let { Client.href = b; _ } =
    ok "b"
      (Client.add ical t.client url
         (event ~summary:"Lunch"
            (utc ~year:2026 ~month:9 ~day:2 ~hour:12 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:2 ~hour:13 ~minute:0)))
  in
  let query f = paths (ok "query" (Client.query ical t.client url f)).entries in
  let summary_has ?collation ?negate s =
    Filter.components
      ~props:
        [
          Filter.prop "SUMMARY"
            (Some (`Text (Filter.text_match ?collation ?negate s)));
        ]
      "VEVENT"
  in
  Alcotest.(check (list string))
    "substring, case folded"
    [ path a ]
    (query (summary_has "budget"));
  Alcotest.(check (list string))
    "octet collation is exact"
    [ path a ]
    (query (summary_has ~collation:"i;octet" "Budget"));
  Alcotest.(check (list string))
    "negated"
    [ path b ]
    (query (summary_has ~negate:true "budget"));
  Alcotest.(check (list string))
    "location defined"
    [ path a ]
    (query (Filter.components ~props:[ Filter.prop "LOCATION" None ] "VEVENT"));
  Alcotest.(check (list string))
    "location not defined"
    [ path b ]
    (query
       (Filter.components
          ~props:[ Filter.prop_not_defined "LOCATION" ]
          "VEVENT"));
  (* A param-filter with a text-match on PARTSTAT is RFC 4791 Section 9.7.3.
     A server that cannot evaluate it is worked around by the client, so the
     answer is the same either way. *)
  Alcotest.(check (list string))
    "attendee partstat"
    [ path a ]
    (query
       (Filter.components
          ~props:
            [
              Filter.prop
                ~params:
                  [
                    Filter.param "PARTSTAT"
                      (Some (Filter.text_match "accepted"));
                  ]
                "ATTENDEE" None;
            ]
          "VEVENT"));
  Alcotest.(check (list string))
    "attendee without DELEGATED-FROM"
    [ path a ]
    (query
       (Filter.components
          ~props:
            [
              Filter.prop
                ~params:[ Filter.param_not_defined "DELEGATED-FROM" ]
                "ATTENDEE" None;
            ]
          "VEVENT"));
  Alcotest.(check (list string))
    "alarm not defined"
    (sorted [ path a; path b ])
    (query
       (Filter.v
          [ Filter.comp ~comps:[ Filter.comp_not_defined "VALARM" ] "VEVENT" ]));
  let all = (ok "all" (Client.query ical t.client url Filter.all)).entries in
  List.iter
    (fun (e : _ Client.entry) ->
      Alcotest.(check bool)
        "local matcher agrees"
        (path e.href = path a)
        (Filter.matches (summary_has "budget") e.value))
    all

let test_partial_retrieval t =
  let url = fresh_calendar t in
  let { Client.href; _ } =
    ok "add"
      (Client.add ical t.client url
         (event ~summary:"Secret" ~alarm:true
            ~props:[ Ical.Property.of_text "DESCRIPTION" "Long text" ]
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)))
  in
  let data =
    CD.v
      ~comp:
        (CD.comp
           ~props:[ ("VERSION", false); ("PRODID", false) ]
           ~comps:
             [
               CD.comp
                 ~props:
                   [ ("UID", false); ("SUMMARY", false); ("DTSTART", false) ]
                 ~comps:[] "VEVENT";
             ]
           "VCALENDAR")
      ()
  in
  let got =
    (ok "query" (Client.query ical t.client ~data url Filter.all)).entries
  in
  match got with
  | [ e ] ->
      Alcotest.(check string) "href" (path href) (path e.href);
      let c = List.hd (Ical.events e.value) in
      Alcotest.(check (option string))
        "summary kept" (Some "Secret")
        (Ical.Component.text c "SUMMARY");
      Alcotest.(check (option string))
        "uid kept"
        (Some (uid e.value))
        (Ical.Component.text c "UID");
      (* RFC 4791 Section 9.6.1 narrows the data returned, which Radicale does
         not implement. The narrowing is checked against the RFC examples in
         the unit tests, so a server that returns everything is noted. *)
      if Ical.Component.text c "DESCRIPTION" <> None then
        Printf.printf
          "  [note] the server ignores calendar-data component selection\n"
      else Alcotest.(check int) "alarm dropped" 0 (List.length c.components)
  | _ -> Alcotest.fail "one entry expected"

let test_multiget t =
  let url = fresh_calendar t in
  let { Client.href = a; _ } =
    ok "a"
      (Client.add ical t.client url
         (event ~summary:"A"
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)))
  in
  let { Client.href = b; _ } =
    ok "b"
      (Client.add ical t.client url
         (event ~summary:"B"
            (utc ~year:2026 ~month:9 ~day:2 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:2 ~hour:10 ~minute:0)))
  in
  let got =
    ok "multiget"
      (Client.multiget ical t.client url [ a; b; join url "missing.ics" ])
  in
  Alcotest.(check (list string)) "found" (sorted [ path a; path b ]) (paths got);
  Alcotest.(check (list string))
    "summaries" [ "A"; "B" ]
    (sorted (List.filter_map (fun (e : _ Client.entry) -> summary e.value) got));
  Alcotest.(check bool)
    "etags" true
    (List.for_all (fun (e : _ Client.entry) -> e.etag <> None) got);
  let none = ok "empty" (Client.multiget ical t.client url []) in
  Alcotest.(check int) "empty request" 0 (List.length none)

let test_free_busy t =
  let url = fresh_calendar t in
  let _ =
    ok "a"
      (Client.add ical t.client url
         (event ~summary:"Busy"
            (utc ~year:2026 ~month:9 ~day:10 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:10 ~hour:10 ~minute:0)))
  in
  let _ =
    ok "b"
      (Client.add ical t.client url
         (event ~summary:"Busy too"
            ~props:[ Ical.Property.of_text "TRANSP" "TRANSPARENT" ]
            (utc ~year:2026 ~month:9 ~day:11 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:11 ~hour:10 ~minute:0)))
  in
  match
    Client.free_busy t.client
      ~start:(utc ~year:2026 ~month:9 ~day:1 ~hour:0 ~minute:0)
      ~finish:(utc ~year:2026 ~month:10 ~day:1 ~hour:0 ~minute:0)
      url
  with
  | Ok fb ->
      let periods =
        List.concat_map
          (fun c ->
            List.map
              (fun p -> Ical.Property.value p)
              (Ical.Component.find_all c "FREEBUSY"))
          (Ical.free_busy fb)
      in
      let busy_components = Ical.free_busy fb in
      Alcotest.(check bool) "a VFREEBUSY" true (busy_components <> []);
      Alcotest.(check bool)
        "busy time reported" true
        (periods <> []
        || List.exists
             (fun c -> Ical.Component.find c "DTSTART" <> None)
             busy_components)
  | Error (Client.Http (code, _)) when code = 403 || code = 501 ->
      Printf.printf
        "  [note] the server does not implement free-busy-query (%d)\n" code
  | Error e -> Alcotest.failf "free-busy: %s" (Client.error_to_string e)

let test_sync t =
  let url = fresh_calendar t in
  let first = ok "initial" (Client.sync ical t.client url) in
  Alcotest.(check bool) "a token" true (first.token <> None);
  Alcotest.(check int) "empty" 0 (List.length first.changes);
  let { Client.href = a; _ } =
    ok "a"
      (Client.add ical t.client url
         (event ~summary:"A"
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)))
  in
  let { Client.href = b; _ } =
    ok "b"
      (Client.add ical t.client url
         (event ~summary:"B"
            (utc ~year:2026 ~month:9 ~day:2 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:2 ~hour:10 ~minute:0)))
  in
  let second = ok "sync" (Client.sync ical t.client ?token:first.token url) in
  let changed =
    List.filter_map
      (function
        | Client.Changed e -> Some (path e.href) | Client.Removed _ -> None)
      second.changes
  in
  Alcotest.(check (list string))
    "two added"
    (sorted [ path a; path b ])
    (sorted changed);
  Alcotest.(check bool)
    "values fetched" true
    (List.for_all
       (function
         | Client.Changed e -> summary e.value <> None
         | Client.Removed _ -> true)
       second.changes);
  ok "delete" (Client.delete t.client a);
  let updated =
    event
      ~uid:(Filename.chop_suffix (Httpz_dav.basename b) ".ics")
      ~summary:"B2"
      (utc ~year:2026 ~month:9 ~day:2 ~hour:9 ~minute:0)
      (utc ~year:2026 ~month:9 ~day:2 ~hour:10 ~minute:0)
  in
  let _ = ok "update" (Client.put ical t.client b updated) in
  let third = ok "sync" (Client.sync ical t.client ?token:second.token url) in
  let removed =
    List.filter_map
      (function Client.Removed h -> Some (path h) | Client.Changed _ -> None)
      third.changes
  in
  let changed =
    List.filter_map
      (function
        | Client.Changed e -> Some (path e.href, summary e.value)
        | Client.Removed _ -> None)
      third.changes
  in
  Alcotest.(check (list string)) "one removed" [ path a ] removed;
  Alcotest.(check (list (pair string (option string))))
    "one changed"
    [ (path b, Some "B2") ]
    changed;
  let fourth = ok "sync" (Client.sync ical t.client ?token:third.token url) in
  Alcotest.(check int) "quiet" 0 (List.length fourth.changes);
  let token = ok "sync token" (Client.sync_token t.client url) in
  Alcotest.(check (option string)) "token property agrees" fourth.token token;
  match
    Client.sync ical t.client ~token:"http://radicale.org/ns/sync/bogus" url
  with
  | Error (Client.Dav (_, conditions)) ->
      Alcotest.(check bool)
        "valid-sync-token" true
        (Httpz_dav.Condition.has Httpz_dav.Condition.valid_sync_token conditions)
  | Error (Client.Http _) -> ()
  | Error e -> Alcotest.failf "unexpected: %s" (Client.error_to_string e)
  | Ok s -> Alcotest.(check bool) "a stale token restarts" true (s.token <> None)

let test_fidelity t =
  let url = fresh_calendar t in
  let long = String.make 200 'x' in
  let props =
    [
      Ical.Property.of_text "DESCRIPTION"
        ("Line one\nLine two, with; punctuation\\ " ^ long);
      Ical.Property.v
        ~params:[ Vcard.Param.v "X-ROOM" [ "Board Room" ] ]
        "X-IDK-NOTE" "kept";
      Ical.Property.v "CATEGORIES" "WORK,MEETING";
      Ical.Property.v "GEO" "51.5;-0.12";
      Ical.Property.v "PRIORITY" "5";
    ]
  in
  let ev =
    event ~summary:"Fidelity ✓ ünïcödé" ~props
      (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
      (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)
  in
  let { Client.href; _ } = ok "add" (Client.add ical t.client url ev) in
  let got = ok "get" (Client.get ical t.client href) in
  let c = List.hd (Ical.events got.value) in
  Alcotest.(check (option string))
    "summary" (Some "Fidelity ✓ ünïcödé")
    (Ical.Component.text c "SUMMARY");
  Alcotest.(check (option string))
    "description"
    (Some ("Line one\nLine two, with; punctuation\\ " ^ long))
    (Ical.Component.text c "DESCRIPTION");
  Alcotest.(check (option string))
    "x property" (Some "kept")
    (Ical.Component.text c "X-IDK-NOTE");
  Alcotest.(check (option (list string)))
    "x param" (Some [ "Board Room" ])
    (Option.bind (Ical.Component.find c "X-IDK-NOTE") (fun p ->
         Ical.Property.find_values p "X-ROOM"));
  Alcotest.(check (list string))
    "categories" [ "MEETING"; "WORK" ]
    (List.sort compare
       (List.concat_map Ical.Property.text_list
          (Ical.Component.find_all c "CATEGORIES")));
  Alcotest.(check (option int))
    "priority" (Some 5)
    (Option.bind (Ical.Component.find c "PRIORITY") (fun p ->
         Result.to_option (Ical.Property.integer p)));
  Alcotest.(check bool)
    "validates" true
    (Result.is_ok (Ical.validate got.value))

let test_local_times t =
  let url = fresh_calendar t in
  let tz =
    Ical.Component.v
      ~properties:[ Ical.Property.of_text "TZID" "America/New_York" ]
      ~components:
        [
          Ical.Component.v
            ~properties:
              [
                Ical.Property.v "DTSTART" "19701101T020000";
                Ical.Property.v "RRULE" "FREQ=YEARLY;BYMONTH=11;BYDAY=1SU";
                Ical.Property.v "TZOFFSETFROM" "-0400";
                Ical.Property.v "TZOFFSETTO" "-0500";
              ]
            "STANDARD";
          Ical.Component.v
            ~properties:
              [
                Ical.Property.v "DTSTART" "19700308T020000";
                Ical.Property.v "RRULE" "FREQ=YEARLY;BYMONTH=3;BYDAY=2SU";
                Ical.Property.v "TZOFFSETFROM" "-0500";
                Ical.Property.v "TZOFFSETTO" "-0400";
              ]
            "DAYLIGHT";
        ]
      "VTIMEZONE"
  in
  let local h =
    {
      Ical.Date.date = { year = 2026; month = 9; day = 15 };
      time = { hour = h; minute = 0; second = 0; utc = false };
    }
  in
  let ev =
    Ical.v
      [
        tz;
        Ical.Component.v
          ~properties:
            [
              Ical.Property.of_text "UID" (unique "ny");
              Ical.Property.of_date_time "DTSTAMP"
                (Ical.Date.Date_time
                   (utc ~year:2026 ~month:9 ~day:1 ~hour:12 ~minute:0));
              Ical.Property.of_text "SUMMARY" "New York";
              Ical.Property.of_date_time ~tzid:"America/New_York" "DTSTART"
                (Ical.Date.Date_time (local 9));
              Ical.Property.of_date_time ~tzid:"America/New_York" "DTEND"
                (Ical.Date.Date_time (local 10));
            ]
          "VEVENT";
      ]
  in
  let { Client.href; _ } = ok "add" (Client.add ical t.client url ev) in
  (* 09:00 in New York in September is 13:00Z. *)
  let hit =
    ok "utc range"
      (Client.events ical t.client
         ~start:(utc ~year:2026 ~month:9 ~day:15 ~hour:12 ~minute:30)
         ~finish:(utc ~year:2026 ~month:9 ~day:15 ~hour:13 ~minute:30)
         url)
  in
  Alcotest.(check (list string)) "found in UTC terms" [ path href ] (paths hit);
  let miss =
    ok "utc range"
      (Client.events ical t.client
         ~start:(utc ~year:2026 ~month:9 ~day:15 ~hour:8 ~minute:30)
         ~finish:(utc ~year:2026 ~month:9 ~day:15 ~hour:9 ~minute:30)
         url)
  in
  Alcotest.(check (list string)) "not at the local wall time" [] (paths miss);
  let got = ok "get" (Client.get ical t.client href) in
  Alcotest.(check bool)
    "timezone kept" true
    (Ical.find_timezone got.value "America/New_York" <> None);
  let c = List.hd (Ical.events got.value) in
  Alcotest.(check (option string))
    "tzid kept" (Some "America/New_York")
    (Option.bind (Ical.Component.find c "DTSTART") Ical.Property.tzid)

let test_uid_conflict t =
  let url = fresh_calendar t in
  let ev =
    event ~uid:(unique "dup") ~summary:"First"
      (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
      (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)
  in
  let _ = ok "add" (Client.add ical t.client url ev) in
  match Client.add ical t.client ~name:(unique "other" ^ ".ics") url ev with
  | Error (Client.Dav (code, conditions)) ->
      Alcotest.(check bool)
        "no-uid-conflict" true
        (Httpz_dav.Condition.has Caldav.Error.no_uid_conflict conditions
        || code = 409 || code = 403)
  | Error (Client.Http (code, _)) ->
      Alcotest.(check bool) "refused" true (code = 409 || code = 403)
  | Error e -> Alcotest.failf "unexpected: %s" (Client.error_to_string e)
  | Ok _ ->
      Printf.printf
        "  [note] the server accepts a second resource with the same UID\n"

let test_invalid_data t =
  let url = fresh_calendar t in
  match
    Client.put Data.raw t.client ~create:true (join url "bad.ics")
      "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nEND:VCALENDAR\r\n"
  with
  | Error (Client.Dav (_, conditions)) ->
      Alcotest.(check bool)
        "valid-calendar-data or object-resource" true
        (Httpz_dav.Condition.has Caldav.Error.valid_calendar_data conditions
        || Httpz_dav.Condition.has Caldav.Error.valid_calendar_object_resource
             conditions
        || Httpz_dav.Condition.has Caldav.Error.supported_calendar_component
             conditions)
  | Error (Client.Http (code, _)) ->
      Alcotest.(check bool) "refused" true (code >= 400)
  | Error e -> Alcotest.failf "unexpected: %s" (Client.error_to_string e)
  | Ok _ ->
      Printf.printf
        "  [note] the server stores a calendar object without components\n"

let () =
  Alcotest.run "caldav-oracle"
    [
      ( "radicale",
        [
          test_case "discovery" test_discovery;
          test_case "calendars" test_calendars;
          test_case "calendar with timezone" test_calendar_with_timezone;
          test_case "calendar objects" test_objects;
          test_case "component kinds" test_component_kinds;
          test_case "time ranges" test_time_range;
          test_case "recurrence and expand" test_recurrence;
          test_case "text and parameter filters" test_text_and_params;
          test_case "partial retrieval" test_partial_retrieval;
          test_case "multiget" test_multiget;
          test_case "free-busy" test_free_busy;
          test_case "sync" test_sync;
          test_case "fidelity" test_fidelity;
          test_case "local times" test_local_times;
          test_case "uid conflict" test_uid_conflict;
          test_case "invalid data" test_invalid_data;
        ] );
    ]
