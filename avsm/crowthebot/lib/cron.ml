let time text =
  match Ptime.of_rfc3339 ~strict:true text with
  | Ok (time, Some _, _) -> Ptime.to_float_s time
  | _ ->
      invalid_arg
        "Use an RFC 3339 timestamp with a timezone, e.g. 2026-09-10T09:00:00Z."

type field = { values : int list; wildcard : bool }

let field ~min ~max text =
  let error () =
    invalid_arg "Invalid cron field. Use *, numbers, ranges, lists or steps."
  in
  let decimal s =
    s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
  in
  let number s =
    if not (decimal s) then error ();
    match int_of_string_opt s with
    | Some n when n >= min && n <= max -> n
    | _ -> error ()
  in
  let part text =
    let range, step =
      match String.split_on_char '/' text with
      | [ range ] -> (range, 1)
      | [ range; step ] -> (
          if not (decimal step) then error ();
          match int_of_string_opt step with
          | Some step when step > 0 && step <= max + 1 -> (range, step)
          | _ -> error ())
      | _ -> error ()
    in
    let first, last =
      match String.split_on_char '-' range with
      | [ "*" ] -> (min, max)
      | [ n ] ->
          let n = number n in
          (n, n)
      | [ a; b ] -> (number a, number b)
      | _ -> error ()
    in
    if first > last then error ();
    let rec values n = if n > last then [] else n :: values (n + step) in
    values first
  in
  {
    values =
      String.split_on_char ',' text
      |> List.concat_map part |> List.sort_uniq Int.compare;
    wildcard = String.starts_with ~prefix:"*" text;
  }

type schedule = {
  minute : field;
  hour : field;
  day : field;
  month : field;
  weekday : field;
}

let parse expression =
  let fields =
    expression
    |> String.map (function '\t' -> ' ' | c -> c)
    |> String.split_on_char ' '
    |> List.filter (( <> ) "")
  in
  match fields with
  | [ minute; hour; day; month; weekday ] ->
      let weekday = field ~min:0 ~max:7 weekday in
      {
        minute = field ~min:0 ~max:59 minute;
        hour = field ~min:0 ~max:23 hour;
        day = field ~min:1 ~max:31 day;
        month = field ~min:1 ~max:12 month;
        weekday =
          { weekday with values = List.map (fun n -> n mod 7) weekday.values };
      }
  | _ ->
      invalid_arg "Cron needs five UTC fields: minute hour day month weekday."

let next schedule ~after ~until =
  let matches (tm : Unix.tm) =
    let day = List.mem tm.tm_mday schedule.day.values in
    let weekday = List.mem tm.tm_wday schedule.weekday.values in
    List.mem (tm.tm_mon + 1) schedule.month.values
    &&
    if schedule.day.wildcard || schedule.weekday.wildcard then day && weekday
    else day || weekday
  in
  let rec days day remaining =
    if
      remaining = 0
      || Option.fold ~none:false ~some:(fun stop -> day > stop) until
    then None
    else if not (matches (Unix.gmtime day)) then
      days (day +. 86400.) (remaining - 1)
    else
      let candidate =
        List.find_map
          (fun hour ->
            List.find_map
              (fun minute ->
                let at = day +. float_of_int ((hour * 3600) + (minute * 60)) in
                if
                  at > after
                  && Option.fold ~none:true ~some:(fun stop -> at <= stop) until
                then Some at
                else None)
              schedule.minute.values)
          schedule.hour.values
      in
      match candidate with
      | Some _ -> candidate
      | None -> days (day +. 86400.) (remaining - 1)
  in
  days (floor (after /. 86400.) *. 86400.) (366 * 8)

type request = {
  fact_id : int;
  instruction : string;
  at : string option;
  cron : string option;
  until : string option;
}

let request_jsont =
  let open Jsont.Object in
  map (fun fact_id instruction at cron until ->
      { fact_id; instruction; at; cron; until })
  |> mem "fact_id" Jsont.int ~enc:(fun r -> r.fact_id)
  |> mem "instruction" Jsont.string ~enc:(fun r -> r.instruction)
  |> mem "at"
       (Jsont.option Jsont.string)
       ~enc:(fun r -> r.at)
       ~dec_absent:(fun () -> None)
  |> mem "cron"
       (Jsont.option Jsont.string)
       ~enc:(fun r -> r.cron)
       ~dec_absent:(fun () -> None)
  |> mem "until"
       (Jsont.option Jsont.string)
       ~enc:(fun r -> r.until)
       ~dec_absent:(fun () -> None)
  |> finish

let names = [ "cron_create"; "cron_list"; "cron_cancel" ]
let is_tool name = List.mem name names

let tools =
  let tool name description schema =
    Openrouter.Tool.v ~name ~description
      ~parameters:
        (Result.get_ok (Jsont_bytesrw.decode_string Jsont.json schema))
      ()
  in
  [
    tool "cron_create"
      "Schedule an OpenRouter action linked to a memory fact, replying in this \
       source room. Use at for a one-off timestamp, or five-field UTC cron for \
       recurrence. Optional until ends recurrence; omitted until recurs \
       forever. Store the relevant fact first."
      {|{"type":"object","properties":{"fact_id":{"type":"integer","minimum":1},"instruction":{"type":"string","maxLength":2048},"at":{"type":"string"},"cron":{"type":"string"},"until":{"type":"string"}},"required":["fact_id","instruction"],"additionalProperties":false}|};
    tool "cron_list"
      "List the profile's active reminders with IDs, schedules and sources."
      {|{"type":"object","properties":{},"additionalProperties":false}|};
    tool "cron_cancel" "Cancel a shared reminder by its numeric ID."
      {|{"type":"object","properties":{"id":{"type":"integer","minimum":1}},"required":["id"],"additionalProperties":false}|};
  ]

let system_prompt =
  "\n\
   Use cron_create to schedule requested future actions or reminders, linked \
   to an existing memory fact. One-off timestamps must include a timezone. \
   Cron expressions are UTC; use until for limited recurrence, omit it for \
   forever. Preserve the user's intent in instruction. The source account, \
   room and event are recorded by the application. Use cron_list and \
   cron_cancel to manage jobs."

let target = function
  | Store.Memory id -> Printf.sprintf "fact #%d" id
  | Store.Tool { namespace; key } -> Printf.sprintf "%s #%d" namespace key

let line (r : Store.reminder) =
  Printf.sprintf "#%d %s, next %s, %s%s\n%s\nSource: %s in %s (%s)"
    r.reminder_id (target r.target)
    (Store.timestamp r.next_at)
    (Option.value ~default:"one-off" r.cron)
    (Option.fold ~none:""
       ~some:(fun t -> ", until " ^ Store.timestamp t)
       r.until_at)
    r.instruction r.creator r.room r.event

type access = {
  create : request -> int;
  list : unit -> Store.reminder list;
  cancel : int -> bool;
}

let for_request store ~actor ~room ~event =
  {
    create =
      (fun r ->
        let now = Store.now store in
        let until = Option.map time r.until in
        let next_at =
          match (r.at, r.cron) with
          | Some at, None when until = None ->
              let at = time at in
              if at <= now then
                invalid_arg "Reminder time must be in the future.";
              at
          | None, Some expression -> (
              match next (parse expression) ~after:now ~until with
              | Some at -> at
              | None ->
                  invalid_arg
                    "No cron occurrence within its end time or the next eight \
                     years.")
          | _ -> invalid_arg "Supply either at, or cron with an optional until."
        in
        Store.add_reminder store ~actor ~room ~event ~fact_id:r.fact_id
          ~instruction:r.instruction ~cron:r.cron ~until_at:until ~next_at);
    list = (fun () -> Store.reminders store ~actor);
    cancel = Store.cancel_reminder store ~actor;
  }

let invoke access name arguments =
  let decode codec =
    match Jsont_bytesrw.decode_string codec arguments with
    | Ok r -> r
    | Error _ -> invalid_arg "Invalid cron arguments."
  in
  try
    if String.length arguments > 4096 then
      invalid_arg "Cron arguments too long.";
    Ok
      (match name with
      | "cron_create" ->
          Printf.sprintf "Registered reminder #%d."
            (access.create (decode request_jsont))
      | "cron_list" ->
          let jobs = access.list () in
          if jobs = [] then "No active reminders."
          else String.concat "\n\n" (List.map line jobs)
      | "cron_cancel" ->
          let codec =
            Jsont.Object.map Fun.id
            |> Jsont.Object.mem "id" Jsont.int ~enc:Fun.id
            |> Jsont.Object.finish
          in
          let id = decode codec in
          if id < 1 then invalid_arg "Reminder ID must be positive.";
          if access.cancel id then Printf.sprintf "Cancelled reminder #%d." id
          else "Reminder not found or already cancelled."
      | _ -> invalid_arg "Unknown cron operation.")
  with Invalid_argument message -> Error message

let help = "cron create JSON | cron list | cron cancel ID. Schedules use UTC."

let command input =
  let action, args =
    match String.index_opt input ' ' with
    | None -> (input, "")
    | Some i ->
        ( String.sub input 0 i,
          String.trim (String.sub input (i + 1) (String.length input - i - 1))
        )
  in
  match action with
  | "create" -> Ok ("cron_create", args)
  | "list" when args = "" -> Ok ("cron_list", "{}")
  | "cancel" -> (
      match int_of_string_opt args with
      | Some id when id > 0 ->
          Ok ("cron_cancel", Printf.sprintf {|{"id":%d}|} id)
      | _ -> Error "Reminder ID must be positive.")
  | _ -> Error help

let run_due store ~fire =
  List.iter
    (fun (job : Store.reminder) ->
      let next_at =
        Option.bind job.cron (fun expression ->
            next (parse expression) ~after:(Store.now store) ~until:job.until_at)
      in
      match Store.claim_reminder store job ~next_at with
      | None -> ()
      | Some run_id -> (
          let finish status =
            Eio.Cancel.protect (fun () ->
                Store.finish_reminder store run_id ~status)
          in
          let expired =
            Option.fold ~none:false
              ~some:(fun until -> Store.now store > until)
              job.until_at
          in
          try
            ignore
              (Audit.run store ~actor:job.creator ~room:job.room
                 ~event:job.event ~source:"scheduler"
                 ~call_id:(string_of_int run_id) ~tool:"cron_fire"
                 ~arguments:
                   (Printf.sprintf "reminder=%d %s" job.reminder_id
                      (target job.target))
                 (fun () ->
                   if expired then
                     Error "Reminder expired while Crow was offline."
                   else Ok (fire job ~run_id)));
            finish (if expired then "expired" else "ok")
          with
          | Eio.Cancel.Cancelled _ as exn ->
              finish "cancelled";
              raise exn
          | _ ->
              finish "error";
              Logs.err (fun m ->
                  m "Scheduled action failed for reminder %d" job.reminder_id)))
    (Store.due_reminders store)
