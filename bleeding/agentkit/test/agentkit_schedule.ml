(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The schedule: its parser, its firing arithmetic and its rewrite.

   The parser's job is to refuse and say what the right form is, so every
   reject here is checked for the wording a person needs rather than only for
   being an error. A file that does not parse must come back as an error, since
   a daemon that read it as an empty schedule would stop every task at once,
   which is the failure a person mid-edit will meet.

   The arithmetic is checked against a zone the test builds, one that springs
   forward, rather than against whatever zone the machine is in. A wall clock
   time the change skips must still fire exactly once that day.

   Missed fires do not queue: a daemon down over three due times fires once
   under run_once and not at all under skip, and a run_now serial bumped twice
   while it was stopped fires once. *)

module Schedule = Agentkit.Schedule

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

let refuses name r wanted =
  match r with
  | Ok _ -> check name false
  | Error msg -> check (name ^ " (" ^ msg ^ ")") (contains msg wanted)

(* Durations, times and days. *)

let () =
  check "a duration in minutes" (Schedule.duration_of_string "15m" = Ok 900);
  check "a duration in hours" (Schedule.duration_of_string "6h" = Ok 21600);
  check "a duration in days" (Schedule.duration_of_string "1d" = Ok 86400);
  check "a duration in seconds" (Schedule.duration_of_string "30s" = Ok 30);
  let form = "15m, 6h or 1d" in
  refuses "a duration with no unit" (Schedule.duration_of_string "15") form;
  refuses "a duration with no number" (Schedule.duration_of_string "m") form;
  refuses "a duration of zero" (Schedule.duration_of_string "0m") form;
  refuses "a negative duration" (Schedule.duration_of_string "-5m") form;
  refuses "a duration in an unknown unit"
    (Schedule.duration_of_string "15w")
    form;
  refuses "an empty duration" (Schedule.duration_of_string "") form;
  check "a duration is written in the largest unit that divides it"
    (List.map Schedule.duration_to_string [ 900; 21600; 86400; 90 ]
    = [ "15m"; "6h"; "1d"; "90s" ]);
  check "a time of day" (Schedule.time_of_string "07:00" = Ok (7, 0));
  check "the last minute of the day"
    (Schedule.time_of_string "23:59" = Ok (23, 59));
  let form = "HH:MM" in
  refuses "a time with one digit of hour" (Schedule.time_of_string "7:00") form;
  refuses "an hour past the day" (Schedule.time_of_string "24:00") form;
  refuses "a minute past the hour" (Schedule.time_of_string "07:60") form;
  refuses "a time with no colon" (Schedule.time_of_string "0700") form;
  check "a day of the week" (Schedule.day_of_string "mon" = Ok Schedule.Mon);
  refuses "a day that is not one"
    (Schedule.day_of_string "funday")
    "mon, tue, wed, thu, fri, sat or sun";
  check "the weekday of a date"
    (Schedule.weekday { Schedule.year = 2026; month = 8; day = 8 }
    = Schedule.Sat)

(* The file, against the design's own example. *)

let example =
  {|{ "tasks": [
      { "id": "feeds", "every": "15m", "prompt": "Check the feeds." },
      { "id": "digest", "at": "07:00", "days": ["mon","tue","wed","thu","fri"],
        "prompt": "Summarise yesterday's journal." },
      { "id": "probe", "once": true, "prompt": "Fetch and report." } ] }|}

let () =
  match Schedule.of_string example with
  | Error msg -> check ("the example schedule parses (" ^ msg ^ ")") false
  | Ok s ->
      let ids = List.map (fun (t : Schedule.task) -> t.Schedule.id) s.tasks in
      check "the tasks are read in file order"
        (ids = [ "feeds"; "digest"; "probe" ]);
      let triggers =
        List.map (fun (t : Schedule.task) -> t.Schedule.trigger) s.tasks
      in
      check "each form of trigger is read"
        (triggers
        = [
            Schedule.Every 900;
            Schedule.At
              {
                hour = 7;
                minute = 0;
                days = [ Schedule.Mon; Tue; Wed; Thu; Fri ];
              };
            Schedule.Once;
          ]);
      check "what is not written takes its default"
        (List.for_all
           (fun (t : Schedule.task) ->
             (not t.Schedule.disabled) && t.Schedule.run_now = 0
             && t.Schedule.on_missed = Schedule.Run_once)
           s.tasks);
      check "a schedule survives a round trip"
        (Schedule.of_string (Schedule.to_string s) = Ok s)

let () =
  refuses "a task naming no trigger"
    (Schedule.of_string {|{"tasks":[{"id":"a","prompt":"p"}]}|})
    {|task "a" names no trigger|};
  refuses "a task naming two triggers"
    (Schedule.of_string
       {|{"tasks":[{"id":"a","prompt":"p","every":"1d","once":true}]}|})
    "every and once";
  refuses "days without at"
    (Schedule.of_string
       {|{"tasks":[{"id":"a","prompt":"p","every":"1d","days":["mon"]}]}|})
    {|narrows "at"|};
  refuses "once written false"
    (Schedule.of_string {|{"tasks":[{"id":"a","prompt":"p","once":false}]}|})
    {|only written true|};
  refuses "two tasks with one id"
    (Schedule.of_string
       {|{"tasks":[{"id":"a","prompt":"p","once":true},
                   {"id":"a","prompt":"q","once":true}]}|})
    {|two tasks are called "a"|};
  refuses "a duration in the wrong form"
    (Schedule.of_string
       {|{"tasks":[{"id":"a","prompt":"p","every":"fortnight"}]}|})
    "15m, 6h or 1d";
  refuses "a time in the wrong form"
    (Schedule.of_string {|{"tasks":[{"id":"a","prompt":"p","at":"7am"}]}|})
    "HH:MM";
  refuses "a missed fire policy that is not one"
    (Schedule.of_string
       {|{"tasks":[{"id":"a","prompt":"p","once":true,"on_missed":"queue"}]}|})
    "run_once";
  check "what is written is read back"
    (match
       Schedule.of_string
         {|{"tasks":[{"id":"a","prompt":"p","every":"1d","disabled":true,
                      "run_now":3,"on_missed":"skip"}]}|}
     with
    | Ok { tasks = [ t ] } ->
        t.Schedule.disabled && t.Schedule.run_now = 3
        && t.Schedule.on_missed = Schedule.Skip
    | Ok _ | Error _ -> false)

(* Firing. The plain cases run in UTC, which has no changes to trip over. *)

let utc = Schedule.utc

let at_utc y m d hh mm =
  utc.Schedule.instant { year = y; month = m; day = d } ~hour:hh ~minute:mm

let task ?(disabled = false) ?(run_now = 0) ?(on_missed = Schedule.Run_once) id
    trigger =
  { Schedule.id; prompt = "p"; trigger; disabled; run_now; on_missed }

let poll ?(zone = utc) ?last ?(serial = 0) ?since t now =
  Schedule.poll ~zone t ~last ~serial ~since ~now

let fires s =
  match s.Schedule.fire with None -> None | Some f -> Some (f.due, f.why)

let () =
  let now = at_utc 2026 8 8 9 0 in
  let quarter = task "feeds" (Schedule.Every 900) in
  check "a task that has never run starts now"
    (fires (poll quarter now) = Some (now, Schedule.First));
  check "and its next fire is one period on"
    ((poll quarter now).next = Some (now +. 900.));
  check "a task fires when its period is up"
    (fires (poll quarter ~last:(now -. 900.) now) = Some (now, Schedule.Due));
  check "a task does not fire before its period is up"
    (fires (poll quarter ~last:(now -. 600.) now) = None);
  check "and says when it will"
    ((poll quarter ~last:(now -. 600.) now).next = Some (now +. 300.));
  (* Down over three due times. *)
  let last = now -. (3.5 *. 900.) in
  check "a daemon down over three due times fires once"
    (fires (poll quarter ~last now)
    = Some (last +. (3. *. 900.), Schedule.Missed));
  let skipper = task "feeds" ~on_missed:Schedule.Skip (Schedule.Every 900) in
  check "and fires none of them under skip"
    (fires (poll skipper ~last now) = None);
  check "a skipped task waits for the next due time"
    ((poll skipper ~last now).next = Some (last +. (4. *. 900.)));
  check "a skipped task fires a due time the caller was watching for"
    (fires (poll skipper ~last:(now -. 900.) ~since:(now -. 800.) now)
    = Some (now, Schedule.Due));
  check "a disabled task never fires"
    (poll (task "feeds" ~disabled:true (Schedule.Every 900)) ~last now
    = { Schedule.fire = None; next = None })

let () =
  let now = at_utc 2026 8 8 9 0 in
  let probe = task "probe" Schedule.Once in
  check "a once task fires while the journal holds no wake for it"
    (fires (poll probe now) = Some (now, Schedule.First));
  check "and never fires again"
    (poll probe ~last:(now -. 60.) now = { Schedule.fire = None; next = None })

let () =
  let now = at_utc 2026 8 8 9 0 in
  let bumped = task "feeds" ~run_now:2 (Schedule.Every 900) in
  check "a run_now serial above the journalled one fires"
    (match (poll bumped ~last:(now -. 60.) now).fire with
    | Some { due; why = Schedule.Run_now; serial = Some 2 } -> due = now
    | _ -> false);
  check "a serial bumped twice while stopped fires once"
    (fires (poll bumped ~last:(now -. 60.) ~serial:2 now) = None)

let () =
  let now = at_utc 2026 8 8 9 0 in
  let daily = task "digest" (Schedule.At { hour = 7; minute = 0; days = [] }) in
  check "an at task fires for today's wall clock time"
    (fires (poll daily ~last:(at_utc 2026 8 7 7 0) now)
    = Some (at_utc 2026 8 8 7 0, Schedule.Due));
  check "and not twice for the same day"
    (fires (poll daily ~last:(at_utc 2026 8 8 7 0) now) = None);
  check "and says when it next fires"
    ((poll daily ~last:(at_utc 2026 8 8 7 0) now).next
    = Some (at_utc 2026 8 9 7 0));
  check "an at task down over three days fires once"
    (fires (poll daily ~last:(at_utc 2026 8 5 7 0) now)
    = Some (at_utc 2026 8 8 7 0, Schedule.Missed));
  (* 2026-08-08 is a Saturday, so a weekday task's last due time was Friday
     and its next is Monday. *)
  let weekdays =
    task "digest"
      (Schedule.At
         { hour = 7; minute = 0; days = [ Schedule.Mon; Tue; Wed; Thu; Fri ] })
  in
  check "days narrows an at task to the days it names"
    (fires (poll weekdays ~last:(at_utc 2026 8 6 7 0) now)
    = Some (at_utc 2026 8 7 7 0, Schedule.Due));
  check "and skips the days it does not"
    ((poll weekdays ~last:(at_utc 2026 8 7 7 0) now).next
    = Some (at_utc 2026 8 10 7 0))

(* A zone that springs forward one hour at 2026-03-29T01:00Z, so that local
   02:30 does not exist that day. The wall clock time a change skips must fire
   exactly once, which is what one due instant per local day gives. *)

let spring = at_utc 2026 3 29 1 0

let dst =
  let offset t = if t >= spring then 7200. else 3600. in
  {
    Schedule.local = (fun t -> utc.Schedule.local (t +. offset t));
    instant =
      (fun d ~hour ~minute ->
        let reading = utc.Schedule.instant d ~hour ~minute in
        let summer = reading -. 7200. and winter = reading -. 3600. in
        if summer >= spring then summer
        else if winter < spring then winter
        else spring);
  }

let () =
  let early = task "early" (Schedule.At { hour = 2; minute = 30; days = [] }) in
  let yesterday =
    dst.Schedule.instant { year = 2026; month = 3; day = 28 } ~hour:2 ~minute:30
  in
  check "a wall clock time a change skips fires at the jump"
    (fires (poll ~zone:dst early ~last:yesterday (spring +. 60.))
    = Some (spring, Schedule.Due));
  check "and fires only once that day"
    (fires (poll ~zone:dst early ~last:spring (spring +. 3600.)) = None);
  check "the day after the change is an ordinary summer day"
    ((poll ~zone:dst early ~last:spring (spring +. 3600.)).next
    = Some
        (dst.Schedule.instant
           { year = 2026; month = 3; day = 30 }
           ~hour:2 ~minute:30))

(* The file on disk. Reading is the daemon's and writing is the person's. *)

let path_of dir = Eio.Path.(dir / "schedule.json")

let store dir =
  let path = path_of dir in
  check "an absent schedule reads as an empty one"
    (Schedule.read path = Ok { Schedule.tasks = [] });
  Eio.Path.save ~create:(`Or_truncate 0o600) path example;
  check "a schedule on disk reads back"
    (match Schedule.read path with
    | Ok { tasks = [ _; _; _ ] } -> true
    | Ok _ | Error _ -> false);
  (* A file mid-edit is an error rather than an empty schedule, since a caller
     that holds one is expected to keep it. *)
  Eio.Path.save ~create:(`Or_truncate 0o600) path {|{"tasks":[{"id":"a"|};
  refuses "a half written schedule is an error" (Schedule.read path) "schedule";
  let torn = Eio.Path.load path in
  check "an error leaves the file alone" (torn = {|{"tasks":[{"id":"a"|});
  (match
     Schedule.rewrite path (fun s ->
         { Schedule.tasks = task "x" Schedule.Once :: s.Schedule.tasks })
   with
  | Ok _ -> check "a rewrite over a file that does not parse is refused" false
  | Error _ ->
      check "a rewrite over a file that does not parse is refused"
        (Eio.Path.load path = torn));
  (* The ordinary edit. *)
  Eio.Path.save ~create:(`Or_truncate 0o600) path example;
  (match
     Schedule.rewrite path (fun s ->
         {
           Schedule.tasks =
             s.Schedule.tasks @ [ task "probe2" ~run_now:1 Schedule.Once ];
         })
   with
  | Error msg -> check ("a rewrite adds a task (" ^ msg ^ ")") false
  | Ok s ->
      check "a rewrite adds a task" (List.length s.Schedule.tasks = 4);
      check "and what it wrote is what reads back" (Schedule.read path = Ok s));
  check "a rewrite leaves no temporary file beside the one it wrote"
    (List.sort compare (Eio.Path.read_dir dir) = [ "schedule.json" ]);
  let good = Eio.Path.load path in
  (* A rewrite that would produce a schedule this module refuses to read must
     leave the file as it was rather than write it and find out later. *)
  (match
     Schedule.rewrite path (fun s ->
         { Schedule.tasks = task "feeds" Schedule.Once :: s.Schedule.tasks })
   with
  | Ok _ -> check "a rewrite that would repeat an id is refused" false
  | Error msg ->
      check "a rewrite that would repeat an id is refused"
        (contains msg {|two tasks are called "feeds"|}));
  check "and leaves the file byte for byte as it was" (Eio.Path.load path = good)

let run env =
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-schedule" "" in
  Sys.remove tmp;
  let dir = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; tmp ])))
    (fun () -> store dir)

let () =
  Eio_main.run run;
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
