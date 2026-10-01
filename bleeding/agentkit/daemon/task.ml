(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Schedule = Agentkit.Schedule

let find id (s : Schedule.t) =
  List.find_opt (fun (t : Schedule.task) -> t.Schedule.id = id) s.Schedule.tasks

let ids (s : Schedule.t) =
  List.map (fun (t : Schedule.task) -> t.Schedule.id) s.Schedule.tasks

(* A refusal says what the right form is rather than guessing at what was
   meant, so an id no task has is answered with the ids there are. *)
let no_task id (s : Schedule.t) =
  match ids s with
  | [] -> Printf.sprintf "no task %S, and the schedule has none" id
  | ids ->
      Printf.sprintf "no task %S. The schedule has %s" id
        (String.concat ", " ids)

(* Every edit is a rewrite of the whole file, so one that cannot be applied has
   to refuse before the rewrite happens rather than return the schedule
   unchanged. A no-op reported as a success sends a person looking for why their
   change did nothing. *)
let edit path id f =
  let refused = ref None in
  match
    Schedule.rewrite path (fun s ->
        match find id s with
        | None ->
            refused := Some (no_task id s);
            s
        | Some task ->
            {
              Schedule.tasks =
                List.map
                  (fun (t : Schedule.task) ->
                    if t.Schedule.id = id then f task else t)
                  s.Schedule.tasks;
            })
  with
  | Error e -> Error e
  | Ok s -> ( match !refused with Some e -> Error e | None -> Ok s)

let add path ~id ~trigger ~on_missed ~prompt =
  Schedule.rewrite path (fun s ->
      (* The serial is kept. Lowering it would fire the task again, since the
         daemon compares it with the last one it journalled. *)
      let run_now =
        match find id s with Some t -> t.Schedule.run_now | None -> 0
      in
      let task =
        { Schedule.id; prompt; trigger; disabled = false; run_now; on_missed }
      in
      match find id s with
      | None -> { Schedule.tasks = s.Schedule.tasks @ [ task ] }
      | Some _ ->
          {
            Schedule.tasks =
              List.map
                (fun (t : Schedule.task) ->
                  if t.Schedule.id = id then task else t)
                s.Schedule.tasks;
          })

let rm path id =
  let refused = ref None in
  match
    Schedule.rewrite path (fun s ->
        match find id s with
        | None ->
            refused := Some (no_task id s);
            s
        | Some _ ->
            {
              Schedule.tasks =
                List.filter
                  (fun (t : Schedule.task) -> t.Schedule.id <> id)
                  s.Schedule.tasks;
            })
  with
  | Error e -> Error e
  | Ok s -> ( match !refused with Some e -> Error e | None -> Ok s)

let enable path id =
  edit path id (fun t -> { t with Schedule.disabled = false })

let disable path id =
  edit path id (fun t -> { t with Schedule.disabled = true })

let run_now path id =
  let serial = ref 0 in
  match
    edit path id (fun t ->
        serial := t.Schedule.run_now + 1;
        { t with Schedule.run_now = !serial })
  with
  | Error e -> Error e
  | Ok _ -> Ok !serial

(* Printing. *)

let trigger_name = function
  | Schedule.Every secs -> "every " ^ Schedule.duration_to_string secs
  | Schedule.At { hour; minute; days = [] } ->
      Printf.sprintf "at %02d:%02d" hour minute
  | Schedule.At { hour; minute; days } ->
      Printf.sprintf "at %02d:%02d on %s" hour minute
        (String.concat "," (List.map Schedule.day_name days))
  | Schedule.Once -> "once"

let first_line s =
  match String.index_opt s '\n' with
  | None -> s
  | Some i -> String.sub s 0 i ^ " …"

let state (t : Schedule.task) =
  String.concat " "
    (List.filter
       (fun s -> s <> "")
       [
         (if t.Schedule.disabled then "disabled" else "");
         (if t.Schedule.run_now > 0 then
            Printf.sprintf "run_now %d" t.Schedule.run_now
          else "");
         (match t.Schedule.on_missed with
         | Schedule.Skip -> "on_missed skip"
         | Schedule.Run_once -> "");
       ])

(* One task is three lines: what it is, when it fires, and what it asks for.
   The prompt is a paragraph, so only its first line is shown and 'numpty task
   check' is not where a person reads it. *)
let indent = String.make 17 ' '

let rows (s : Schedule.t) extra =
  match s.Schedule.tasks with
  | [] ->
      "The schedule has no tasks. Add one with 'numpty task add ID --every 15m \
       \"…\"'.\n"
  | tasks ->
      String.concat "\n"
        (List.map
           (fun (t : Schedule.task) ->
             Printf.sprintf "%-16s %-28s %s\n%s%s%s\n" t.Schedule.id
               (trigger_name t.Schedule.trigger)
               (state t) (extra t) indent
               (first_line t.Schedule.prompt))
           tasks)

let list s = rows s (fun _ -> "")

let check ~zone ~now ~last s =
  let extra (t : Schedule.task) =
    if t.Schedule.disabled then indent ^ "disabled, so it never fires\n"
    else
      let status =
        Schedule.poll ~zone t ~last:(last t.Schedule.id)
          ~serial:t.Schedule.run_now ~since:None ~now
      in
      match (status.Schedule.fire, status.Schedule.next) with
      | Some f, _ ->
          Printf.sprintf "%sdue now (%s)\n" indent
            (Schedule.why_name f.Schedule.why)
      | None, Some next ->
          Printf.sprintf "%snext %s\n" indent (Journal.rfc3339 next)
      | None, None -> indent ^ "never fires again\n"
  in
  Printf.sprintf "The schedule parses, with %d task%s.\n\n%s"
    (List.length s.Schedule.tasks)
    (if List.length s.Schedule.tasks = 1 then "" else "s")
    (rows s extra)
