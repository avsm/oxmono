(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  client : Caldav_eio.Client.t;
  user : string;
}

let join collection name =
  if String.ends_with ~suffix:"/" collection then collection ^ name
  else collection ^ "/" ^ name

let getenv name default =
  match Sys.getenv_opt name with Some v when v <> "" -> v | _ -> default

let configured () = Sys.getenv_opt "CARDDAV_ORACLE_URL" <> None

let url () =
  match Sys.getenv_opt "CARDDAV_ORACLE_URL" with
  | Some u when u <> "" -> u
  | _ -> Alcotest.fail "CARDDAV_ORACLE_URL is not set"

let user () = getenv "CARDDAV_ORACLE_USER" "alice"
let password () = getenv "CARDDAV_ORACLE_PASSWORD" "x"

let credentials () =
  [ Fetch.Credential.basic ~user:(user ()) ~password:(password ()) ]

let connect_with ~sw ?credentials:c env =
  let credentials = match c with Some c -> c | None -> credentials () in
  Caldav_eio.Client.connect ~sw ~credentials ~allow_insecure:true
    (Fetch_httpz.std env) (url ())

let ok what = function
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: %s" what (Caldav_eio.Client.error_to_string e)

(* Every collection a test creates is removed when it ends, whatever its
   outcome, so that a shared account is left as it was found. *)
let created : string list ref = ref []

let test_case name f =
  let run () =
    if not (configured ()) then Alcotest.skip ()
    else
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let client = ok "connect" (connect_with ~sw env) in
      created := [];
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun url -> ignore (Caldav_eio.Client.delete_calendar client url))
            !created)
        (fun () -> f { env; sw; client; user = user () })
  in
  Alcotest.test_case name `Quick run

let counter = ref 0

let unique prefix =
  incr counter;
  Printf.sprintf "%s-%d-%d-%d" prefix (Unix.getpid ())
    (int_of_float (Unix.gettimeofday () *. 1000.) mod 1_000_000)
    !counter

let fresh_calendar ?components t =
  match Caldav_eio.Client.home_sets t.client with
  | [] -> Alcotest.fail "the principal has no calendar home"
  | home :: _ ->
      let url = join home (unique "cal") ^ "/" in
      ok "create calendar"
        (Caldav_eio.Client.create_calendar t.client ~display_name:"Oracle"
           ?components url);
      created := url :: !created;
      url

let utc ~year ~month ~day ~hour ~minute =
  {
    Ical.Date.date = { year; month; day };
    time = { hour; minute; second = 0; utc = true };
  }

module P = Ical.Property

let stamp =
  P.of_date_time "DTSTAMP"
    (Ical.Date.Date_time (utc ~year:2026 ~month:9 ~day:1 ~hour:12 ~minute:0))

let event ?uid ?rrule ?all_day ?(props = []) ?(alarm = false) ~summary start
    finish =
  let uid = match uid with Some u -> u | None -> unique "ev" in
  let times =
    match all_day with
    | Some d ->
        let next =
          Ical.Duration.add (Ical.Date.start_of_day d)
            { Ical.Duration.zero with days = 1 }
        in
        [
          P.of_date_time "DTSTART" (Ical.Date.Date d);
          P.of_date_time "DTEND" (Ical.Date.Date next.date);
        ]
    | None ->
        [
          P.of_date_time "DTSTART" (Ical.Date.Date_time start);
          P.of_date_time "DTEND" (Ical.Date.Date_time finish);
        ]
  in
  let rule = match rrule with Some r -> [ P.v "RRULE" r ] | None -> [] in
  let alarms =
    if alarm then
      [
        Ical.Component.v
          ~properties:
            [
              P.of_text "ACTION" "DISPLAY";
              P.of_text "DESCRIPTION" summary;
              P.v "TRIGGER" "-PT15M";
            ]
          "VALARM";
      ]
    else []
  in
  Ical.v
    [
      Ical.Component.v
        ~properties:
          ([ P.of_text "UID" uid; stamp; P.of_text "SUMMARY" summary ]
          @ times @ rule @ props)
        ~components:alarms "VEVENT";
    ]

let todo ?uid ~summary due =
  let uid = match uid with Some u -> u | None -> unique "todo" in
  Ical.v
    [
      Ical.Component.v
        ~properties:
          [
            P.of_text "UID" uid;
            stamp;
            P.of_text "SUMMARY" summary;
            P.of_date_time "DUE" (Ical.Date.Date_time due);
          ]
        "VTODO";
    ]

let journal ?uid ~summary day =
  let uid = match uid with Some u -> u | None -> unique "jnl" in
  Ical.v
    [
      Ical.Component.v
        ~properties:
          [
            P.of_text "UID" uid;
            stamp;
            P.of_text "SUMMARY" summary;
            P.of_date_time "DTSTART" (Ical.Date.Date day);
          ]
        "VJOURNAL";
    ]

let summary cal =
  match Ical.components cal with
  | c :: _ -> Ical.Component.text c "SUMMARY"
  | [] -> None

let uid cal =
  match Ical.uid cal with Some u -> u | None -> Alcotest.fail "no uid"
