(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The schedule editor behind [numpty task], on a real temporary file. It needs
   no daemon, which is the point of it: the person writes the file and the daemon
   only reads it.

   The properties are the ones a person meets. An id no task has is refused with
   the ids there are rather than silently doing nothing. A file that does not
   parse is left exactly as it was, since a half-edited file is not something to
   overwrite with a guess at what was meant. The run_now serial only ever goes
   up, because the daemon compares it with the last one it journalled and a lower
   one would fire the task again. *)

module Journal = Agentkit.Journal
module Schedule = Agentkit.Schedule
module Task = Numpty_daemon.Task

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
  | Ok _ -> check (name ^ " (it was allowed)") false
  | Error msg -> check (name ^ " (" ^ msg ^ ")") (contains msg wanted)

let find id (s : Schedule.t) =
  List.find_opt (fun (t : Schedule.task) -> t.Schedule.id = id) s.Schedule.tasks

let ids (s : Schedule.t) =
  List.map (fun (t : Schedule.task) -> t.Schedule.id) s.Schedule.tasks

let run env =
  let tmp = Filename.temp_file "ds4-numpty-task" ".json" in
  Sys.remove tmp;
  let path = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect ~finally:(fun () -> if Sys.file_exists tmp then Sys.remove tmp)
  @@ fun () ->
  let add ?(on_missed = Schedule.Run_once) id trigger prompt =
    Task.add path ~id ~trigger ~on_missed ~prompt
  in
  (* Adding, to a file that is not there yet. *)
  check "an absent file is an empty schedule to add to"
    (match add "feeds" (Schedule.Every 900) "check the feeds" with
    | Ok s -> ids s = [ "feeds" ]
    | Error _ -> false);
  check "and the file reads back as what was written"
    (match Schedule.read path with
    | Ok s -> (
        match find "feeds" s with
        | Some t ->
            t.Schedule.trigger = Schedule.Every 900
            && t.Schedule.prompt = "check the feeds"
            && (not t.Schedule.disabled) && t.Schedule.run_now = 0
        | None -> false)
    | Error _ -> false);
  check "a second task joins the first, in file order"
    (match
       add "digest"
         (Schedule.At { hour = 7; minute = 0; days = [ Schedule.Mon ] })
         "summarise yesterday"
     with
    | Ok s -> ids s = [ "feeds"; "digest" ]
    | Error _ -> false);
  check "a once task is written as one"
    (match
       add ~on_missed:Schedule.Skip "probe" Schedule.Once "fetch a thing"
     with
    | Ok s -> (
        match find "probe" s with
        | Some t ->
            t.Schedule.trigger = Schedule.Once
            && t.Schedule.on_missed = Schedule.Skip
        | None -> false)
    | Error _ -> false);
  (* run_now, which is the whole of what a person asking for a task now
     writes. *)
  check "run bumps the serial from nothing to one"
    (Task.run_now path "feeds" = Ok 1);
  check "and again to two" (Task.run_now path "feeds" = Ok 2);
  check "the serial is in the file"
    (match Schedule.read path with
    | Ok s -> (
        match find "feeds" s with
        | Some t -> t.Schedule.run_now = 2
        | None -> false)
    | Error _ -> false);
  (* Replacing a task keeps its serial. Lowering it would fire the task again,
     since the daemon compares it with the last one it journalled. *)
  check "replacing a task keeps its run_now serial"
    (match
       add "feeds" (Schedule.Every 1800) "check the feeds twice as slowly"
     with
    | Ok s -> (
        match find "feeds" s with
        | Some t ->
            t.Schedule.run_now = 2
            && t.Schedule.trigger = Schedule.Every 1800
            && t.Schedule.prompt = "check the feeds twice as slowly"
        | None -> false)
    | Error _ -> false);
  check "and does not add a second task of that id"
    (match Schedule.read path with
    | Ok s -> ids s = [ "feeds"; "digest"; "probe" ]
    | Error _ -> false);
  (* Disabling leaves the task in the file, so what was asked for is still
     readable. *)
  check "disable sets the flag and leaves the task in the file"
    (match Task.disable path "digest" with
    | Ok s -> (
        match find "digest" s with
        | Some t ->
            t.Schedule.disabled && ids s = [ "feeds"; "digest"; "probe" ]
        | None -> false)
    | Error _ -> false);
  check "enable clears it"
    (match Task.enable path "digest" with
    | Ok s -> (
        match find "digest" s with
        | Some t -> not t.Schedule.disabled
        | None -> false)
    | Error _ -> false);
  check "rm takes the task out"
    (match Task.rm path "probe" with
    | Ok s -> ids s = [ "feeds"; "digest" ]
    | Error _ -> false);
  (* An id no task has is refused with the ids there are. A no-op reported as a
     success sends a person looking for why their change did nothing. *)
  let has_ids = "feeds, digest" in
  refuses "rm of an id no task has is refused" (Task.rm path "nosuch") has_ids;
  refuses "disable of an id no task has is refused"
    (Task.disable path "nosuch")
    has_ids;
  refuses "enable of an id no task has is refused"
    (Task.enable path "nosuch")
    has_ids;
  refuses "run of an id no task has is refused"
    (Task.run_now path "nosuch")
    has_ids;
  check "and none of those touched the file"
    (match Schedule.read path with
    | Ok s -> ids s = [ "feeds"; "digest" ]
    | Error _ -> false);
  (* A file mid-edit. Every edit is a rewrite of the whole file, so one that
     cannot read the file must leave it exactly as it was. *)
  let good = Eio.Path.load path in
  Eio.Path.save ~create:(`Or_truncate 0o600) path "{\"tasks\": [ {\"id\":";
  refuses "an edit to a file that does not parse is refused"
    (add "feeds" (Schedule.Every 60) "no")
    "tasks";
  check "and the half-written file is left as it was"
    (Eio.Path.load path = "{\"tasks\": [ {\"id\":");
  Eio.Path.save ~create:(`Or_truncate 0o600) path good;
  (* Printing. *)
  let s = Result.get_ok (Schedule.read path) in
  let listed = Task.list s in
  check "a listing names each task and its trigger"
    (contains listed "feeds"
    && contains listed "every 30m"
    && contains listed "at 07:00 on mon");
  check "and shows the run_now serial that is outstanding"
    (contains listed "run_now 2");
  let now =
    1786180447.
    (* 2026-08-08T09:14:07Z *)
  in
  let checked =
    Task.check ~zone:Schedule.utc ~now
      ~last:(fun id -> if id = "feeds" then Some (now -. 60.) else None)
      s
  in
  check "check says the file parses" (contains checked "The schedule parses");
  check "a task whose due time has not come says when it next fires"
    (contains checked "next 2026-08-08T09:43:07Z");
  check "a task a person asked for says it is due now"
    (contains
       (Task.check ~zone:Schedule.utc ~now ~last:(fun _ -> None) s)
       "due now");
  let disabled = Result.get_ok (Task.disable path "digest") in
  check "a disabled task is said never to fire"
    (contains
       (Task.check ~zone:Schedule.utc ~now ~last:(fun _ -> None) disabled)
       "never fires")

let () =
  Eio_main.run run;
  if !failures > 0 then begin
    Printf.printf "\n%d failure(s)\n" !failures;
    exit 1
  end
