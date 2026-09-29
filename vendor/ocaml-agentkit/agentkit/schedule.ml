(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let days_assoc =
  [
    ("mon", Mon);
    ("tue", Tue);
    ("wed", Wed);
    ("thu", Thu);
    ("fri", Fri);
    ("sat", Sat);
    ("sun", Sun);
  ]

let day_name d = fst (List.find (fun (_, x) -> x = d) days_assoc)

let day_of_string s =
  match List.assoc_opt s days_assoc with
  | Some d -> Ok d
  | None ->
      Error
        (Printf.sprintf
           "%S is not a day, which is one of mon, tue, wed, thu, fri, sat or \
            sun"
           s)

let units = [ ('s', 1); ('m', 60); ('h', 3600); ('d', 86400) ]

let duration_of_string s =
  let bad =
    Error
      (Printf.sprintf
         "%S is not a duration, which is a positive number followed by s, m, h \
          or d, such as 15m, 6h or 1d"
         s)
  in
  let n = String.length s in
  if n < 2 then bad
  else
    match
      ( int_of_string_opt (String.sub s 0 (n - 1)),
        List.assoc_opt s.[n - 1] units )
    with
    | Some count, Some unit when count > 0 -> Ok (count * unit)
    | _ -> bad

let duration_to_string secs =
  let rec largest = function
    | (name, unit) :: rest ->
        if secs mod unit = 0 then Printf.sprintf "%d%c" (secs / unit) name
        else largest rest
    | [] -> Printf.sprintf "%ds" secs
  in
  largest (List.rev units)

let time_of_string s =
  let bad =
    Error
      (Printf.sprintf
         "%S is not a time, which is HH:MM on a 24-hour clock in local time, \
          such as 07:00"
         s)
  in
  if String.length s <> 5 || s.[2] <> ':' then bad
  else
    match
      ( int_of_string_opt (String.sub s 0 2),
        int_of_string_opt (String.sub s 3 2) )
    with
    | Some hour, Some minute
      when hour >= 0 && hour < 24 && minute >= 0 && minute < 60 ->
        Ok (hour, minute)
    | _ -> bad

let time_to_string hour minute = Printf.sprintf "%02d:%02d" hour minute

(* Calendar arithmetic, so that walking from one local day to the next never
   goes through a zone. A day is a day whatever the clocks did in it. *)

type date = { year : int; month : int; day : int }
type local = { date : date; hour : int; minute : int; second : int }

let days_from_civil year month day =
  let y = if month <= 2 then year - 1 else year in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let m = if month > 2 then month - 3 else month + 9 in
  let doy = (((153 * m) + 2) / 5) + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = if mp < 10 then mp + 3 else mp - 9 in
  { year = (if month <= 2 then y + 1 else y); month; day }

(* 1970-01-01 was a Thursday, which is index 3 with Monday at 0. *)
let weekday d =
  let n = days_from_civil d.year d.month d.day in
  let i = (((n + 3) mod 7) + 7) mod 7 in
  List.nth [ Mon; Tue; Wed; Thu; Fri; Sat; Sun ] i

let add_days d n = civil_from_days (days_from_civil d.year d.month d.day + n)

type zone = {
  local : float -> local;
  instant : date -> hour:int -> minute:int -> float;
}

let system =
  let local t =
    let tm = Unix.localtime t in
    {
      date =
        {
          year = tm.Unix.tm_year + 1900;
          month = tm.Unix.tm_mon + 1;
          day = tm.Unix.tm_mday;
        };
      hour = tm.Unix.tm_hour;
      minute = tm.Unix.tm_min;
      second = tm.Unix.tm_sec;
    }
  in
  let instant d ~hour ~minute =
    let tm =
      {
        Unix.tm_year = d.year - 1900;
        tm_mon = d.month - 1;
        tm_mday = d.day;
        tm_hour = hour;
        tm_min = minute;
        tm_sec = 0;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
    in
    fst (Unix.mktime tm)
  in
  { local; instant }

let utc =
  let local t =
    let days = int_of_float (Float.floor (t /. 86400.)) in
    let secs = int_of_float (t -. (float_of_int days *. 86400.)) in
    {
      date = civil_from_days days;
      hour = secs / 3600;
      minute = secs mod 3600 / 60;
      second = secs mod 60;
    }
  in
  let instant d ~hour ~minute =
    float_of_int
      ((days_from_civil d.year d.month d.day * 86400)
      + (hour * 3600) + (minute * 60))
  in
  { local; instant }

(* Tasks. *)

type trigger =
  | Every of int
  | At of { hour : int; minute : int; days : day list }
  | Once

type on_missed = Run_once | Skip

type task = {
  id : string;
  prompt : string;
  trigger : trigger;
  disabled : bool;
  run_now : int;
  on_missed : on_missed;
}

type t = { tasks : task list }

(* The codec. A trigger is named by which of three members is written rather
   than by a tag, since that is what a person writing the file by hand reads
   best, so the three are decoded as optional members and reduced to one
   trigger here. *)

let string = Jsont.map ~dec:Fun.id ~enc:Fun.id Jsont.string

let of_result = function
  | Ok v -> v
  | Error msg -> Jsont.Error.msg Jsont.Meta.none msg

let duration_jsont =
  Jsont.map ~kind:"duration"
    ~dec:(fun s -> of_result (duration_of_string s))
    ~enc:duration_to_string Jsont.string

let time_jsont =
  Jsont.map ~kind:"time of day"
    ~dec:(fun s -> of_result (time_of_string s))
    ~enc:(fun (hour, minute) -> time_to_string hour minute)
    Jsont.string

let day_jsont = Jsont.enum ~kind:"day of the week" days_assoc

let on_missed_jsont =
  Jsont.enum ~kind:"missed fire policy"
    [ ("run_once", Run_once); ("skip", Skip) ]

let trigger_of ~id ~every ~at ~days ~once =
  let named =
    (match every with None -> [] | Some _ -> [ "every" ])
    @ (match at with None -> [] | Some _ -> [ "at" ])
    @ match once with None -> [] | Some _ -> [ "once" ]
  in
  match named with
  | [] ->
      Jsont.Error.msgf Jsont.Meta.none
        "task %S names no trigger, and a task names one of \"every\", \"at\" \
         or \"once\""
        id
  | _ :: _ :: _ ->
      Jsont.Error.msgf Jsont.Meta.none
        "task %S names %s, and a task names one of them" id
        (String.concat " and " named)
  | [ _ ] -> (
      match (every, at, once, days) with
      | Some _, _, _, Some _ | _, _, Some _, Some _ ->
          Jsont.Error.msgf Jsont.Meta.none
            "task %S has \"days\", which narrows \"at\" and goes with it" id
      | Some secs, _, _, None -> Every secs
      | _, Some (hour, minute), _, days ->
          At { hour; minute; days = Option.value days ~default:[] }
      | _, _, Some true, None -> Once
      | _, _, Some false, None ->
          Jsont.Error.msgf Jsont.Meta.none
            "task %S has \"once\": false, and \"once\" is only written true. \
             Remove it, or write \"disabled\": true"
            id
      | None, None, None, _ -> assert false)

let task_jsont =
  Jsont.Object.map ~kind:"task"
    (fun id prompt every at days once disabled run_now on_missed : task ->
      {
        id;
        prompt;
        trigger = trigger_of ~id ~every ~at ~days ~once;
        disabled;
        run_now;
        on_missed;
      })
  |> Jsont.Object.mem "id" string ~enc:(fun (t : task) -> t.id)
  |> Jsont.Object.mem "prompt" string ~enc:(fun (t : task) -> t.prompt)
  |> Jsont.Object.opt_mem "every" duration_jsont ~enc:(fun (t : task) ->
      match t.trigger with Every s -> Some s | At _ | Once -> None)
  |> Jsont.Object.opt_mem "at" time_jsont ~enc:(fun (t : task) ->
      match t.trigger with
      | At { hour; minute; _ } -> Some (hour, minute)
      | Every _ | Once -> None)
  |> Jsont.Object.opt_mem "days" (Jsont.list day_jsont) ~enc:(fun (t : task) ->
      match t.trigger with
      | At { days = _ :: _ as days; _ } -> Some days
      | At _ | Every _ | Once -> None)
  |> Jsont.Object.opt_mem "once" Jsont.bool ~enc:(fun (t : task) ->
      match t.trigger with Once -> Some true | Every _ | At _ -> None)
  |> Jsont.Object.mem "disabled" Jsont.bool ~dec_absent:false
       ~enc:(fun (t : task) -> t.disabled)
       ~enc_omit:(fun v -> v = false)
  |> Jsont.Object.mem "run_now" Jsont.int ~dec_absent:0
       ~enc:(fun (t : task) -> t.run_now)
       ~enc_omit:(fun v -> v = 0)
  |> Jsont.Object.mem "on_missed" on_missed_jsont ~dec_absent:Run_once
       ~enc:(fun (t : task) -> t.on_missed)
       ~enc_omit:(fun v -> v = Run_once)
  |> Jsont.Object.finish

(* An id is what the journal calls a task, so two tasks sharing one would make
   the trace unreadable and a [run_now] ambiguous. *)
let no_repeats tasks =
  let rec go seen = function
    | [] -> ()
    | (t : task) :: rest ->
        if List.mem t.id seen then
          Jsont.Error.msgf Jsont.Meta.none
            "two tasks are called %S, and an id names one task" t.id
        else go (t.id :: seen) rest
  in
  go [] tasks

let jsont =
  Jsont.Object.map ~kind:"schedule" (fun tasks : t ->
      no_repeats tasks;
      { tasks })
  |> Jsont.Object.mem "tasks" (Jsont.list task_jsont) ~dec_absent:[]
       ~enc:(fun (t : t) -> t.tasks)
  |> Jsont.Object.finish

let of_string s = Jsont_bytesrw.decode_string jsont s

let to_string t =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont t with
  | Ok s -> s
  | Error msg -> invalid_arg ("Schedule.to_string: " ^ msg)

(* Reading, which is what the daemon does. *)

let read path =
  if not (Eio.Path.is_file path) then Ok { tasks = [] }
  else
    match of_string (Eio.Path.load path) with
    | Ok t -> Ok t
    | Error msg -> Error (Format.asprintf "%a: %s" Eio.Path.pp path msg)

(* Writing, which is what the person's command does. *)

let rewrite path edit =
  match read path with
  | Error _ as e -> e
  | Ok current -> (
      let edited = edit current in
      (* Encoding and reading back is the whole of the validation, since the
         codec is the schema and a schedule this module cannot read is one the
         daemon would refuse at its next tick. *)
      match of_string (to_string edited) with
      | Error msg -> Error msg
      | Ok checked ->
          let text = to_string checked ^ "\n" in
          let dir, name =
            match Eio.Path.split path with
            | Some (dir, name) -> (dir, name)
            | None -> invalid_arg "Schedule.rewrite: the path names no file"
          in
          let pending = Eio.Path.(dir / (name ^ ".pending")) in
          Eio.Switch.run (fun sw ->
              let file =
                Eio.Path.open_out ~sw ~create:(`Or_truncate 0o600) pending
              in
              Eio.Flow.copy_string text file;
              Eio.File.sync file);
          Eio.Path.rename pending path;
          Ok checked)

(* When a task fires. *)

type why = First | Due | Missed | Run_now

let why_name = function
  | First -> "first"
  | Due -> "due"
  | Missed -> "missed"
  | Run_now -> "run_now"

type firing = { due : float; why : why; serial : int option }
type status = { fire : firing option; next : float option }

(* A set of weekdays repeats every seven days, so eight days either side of
   today reaches the most recent due time and the next one whatever the set. *)
let span = 8

let at_days zone ~hour ~minute ~days date ~step =
  let matches d = days = [] || List.mem (weekday d) days in
  List.filter_map
    (fun k ->
      let d = add_days date (k * step) in
      if matches d then Some (zone.instant d ~hour ~minute) else None)
    (List.init (span + 1) Fun.id)

let poll ~zone task ~last ~serial ~since ~now =
  let today = (zone.local now).date in
  (* The next due time strictly after [now], with [base] as the reference an
     [every] task counts from. *)
  let next_after base =
    match task.trigger with
    | Once -> None
    | Every secs ->
        let d = float_of_int secs in
        let k = Float.max 1. (Float.floor ((now -. base) /. d) +. 1.) in
        Some (base +. (k *. d))
    | At { hour; minute; days } ->
        List.find_opt
          (fun i -> i > now)
          (at_days zone ~hour ~minute ~days today ~step:1)
  in
  (* The window of due times that may fire now. [Run_once] collapses every one
     of them into a single firing, so it looks back to the last wake. [Skip]
     runs none that nobody was there for, so it looks back only as far as the
     caller has been watching. *)
  let lower =
    match task.on_missed with
    | Run_once -> last
    | Skip -> (
        match (last, since) with
        | _, None -> Some now
        | None, Some s -> Some s
        | Some l, Some s -> Some (Float.max l s))
  in
  let fired due why =
    { fire = Some { due; why; serial = None }; next = next_after now }
  in
  if task.disabled then { fire = None; next = None }
  else if task.run_now > serial then
    {
      fire = Some { due = now; why = Run_now; serial = Some task.run_now };
      next = next_after now;
    }
  else
    match task.trigger with
    | Once -> (
        match last with
        | None ->
            {
              fire = Some { due = now; why = First; serial = None };
              next = None;
            }
        | Some _ -> { fire = None; next = None })
    | Every secs -> (
        let d = float_of_int secs in
        match last with
        | None -> (
            (* Nothing has run, so there is no grid yet. Starting one now is
               the firing under [Run_once] and the next due time under
               [Skip]. *)
            match task.on_missed with
            | Run_once -> fired now First
            | Skip -> { fire = None; next = Some (now +. d) })
        | Some l ->
            let k = Float.floor ((now -. l) /. d) in
            let first =
              match lower with
              | None -> 1.
              | Some x -> Float.max 1. (Float.floor ((x -. l) /. d) +. 1.)
            in
            let count = k -. first +. 1. in
            if count < 1. then { fire = None; next = next_after l }
            else
              let why = if count > 1. then Missed else Due in
              fired (l +. (k *. d)) why)
    | At { hour; minute; days } -> (
        let recent =
          List.filter
            (fun i -> i <= now)
            (at_days zone ~hour ~minute ~days today ~step:(-1))
        in
        let window =
          match lower with
          | None -> recent
          | Some x -> List.filter (fun i -> i > x) recent
        in
        match window with
        | [] -> { fire = None; next = next_after now }
        | due :: rest ->
            let why =
              if last = None then First else if rest <> [] then Missed else Due
            in
            fired due why)
