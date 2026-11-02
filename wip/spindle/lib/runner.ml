(* SPDX-License-Identifier: ISC *)
open Json

type t = {
  job : Job.t;
  mutable status : string;
  mutable started : string option;
  mutable finished : string option;
  mutable error : string option;
  mutable events : Jsont.json list;
  mutable log_bytes : int;
  mutable log_seq : int64;
  log_lock : Eio.Mutex.t;
  mutable cancel : (unit -> unit) option;
  mutable cancelled : bool;
}

type env = {
  system : Eio_unix.Stdenv.base;
  directory : Eio.Fs.dir_ty Eio.Path.t;
  slots : Eio.Semaphore.t;
  timeout : float;
  executor : Executor.t;
}

type input = {
  id : string;
  repo : string;
  source : string;
  commit : string;
  metadata : Jsont.json;
  variables : (string * string) list;
  secrets : (string * string) list;
}

exception Cancelled

let now () = Ptime.to_rfc3339 ~tz_offset_s:0 (Ptime_clock.now ())
(* A blocked workflow waits for a collaborator's approval. It is live, so
   cancellation and restart keep it, but it does not run until approved. *)
let terminal p =
  p.status <> "pending" && p.status <> "running" && p.status <> "blocked"

let v job =
  {
    job;
    status = "pending";
    started = None;
    finished = None;
    error = None;
    events = [];
    log_bytes = 0;
    log_seq = 0L;
    log_lock = Eio.Mutex.create ();
    cancel = None;
    cancelled = false;
  }

let optional k f = function None -> [] | Some v -> [ (k, f v) ]

let view p =
  obj
    ([
       ("id", str p.job.name); ("name", str p.job.name); ("status", str p.status);
     ]
    @ optional "startedAt" str p.started
    @ optional "finishedAt" str p.finished
    @ optional "error" str p.error)

let pairs l = obj (List.map (fun (k, v) -> (k, str v)) l)
let unpairs j = List.map (fun (k, v) -> (k, string v)) (members j)

let snapshot p =
  obj
    ([
       ("workflow", view p);
       ("events", arr (List.rev p.events));
       ( "steps",
         arr
           (List.map
              (fun (s : Job.step) ->
                obj
                  [
                    ("name", str s.name);
                    ("command", str s.command);
                    ("env", pairs s.env);
                  ])
              p.job.steps) );
       ("env", pairs p.job.env);
       ("clone", bool p.job.clone);
     ]
    @ optional "definition" Fun.id p.job.definition
    @ optional "yaml" str p.job.yaml)

(* Snapshots from before the executor hold argument vectors, shell scripts or
   a metadata step, which restore as the text of a command. *)
let restore_step step =
  let name = Option.map string (field "name" step) in
  match (field "command" step, field "shell" step, field "argv" step) with
  | Some c, _, _ | None, Some c, _ ->
      Job.step
        ~env:(Option.fold ~none:[] ~some:unpairs (field "env" step))
        (Option.value ~default:"" name)
        (string c)
  | None, None, Some argv ->
      let command = String.concat " " (List.map string (list argv)) in
      Job.step (Option.value ~default:command name) command
  | None, None, None ->
      Job.step (Option.value ~default:"spindle request metadata" name) ""

let restore raw =
  let workflow = required "workflow" raw in
  let steps = List.map restore_step (list (required "steps" raw)) in
  let name = get "name" workflow in
  let job =
    match (field "definition" raw, field "yaml" raw) with
    | Some definition, yaml ->
        Job.workflow ~definition
          ~env:(Option.fold ~none:[] ~some:unpairs (field "env" raw))
          ~clone:
            (match field "clone" raw with
            | Some (Jsont.Bool (b, _)) -> b
            | _ -> true)
          ~yaml:(Option.fold ~none:"" ~some:string yaml)
          name steps
    | None, _ -> Job.v name steps
  in
  let p = v job in
  p.status <- get "status" workflow;
  p.started <- Option.map string (field "startedAt" workflow);
  p.finished <- Option.map string (field "finishedAt" workflow);
  p.error <- Option.map string (field "error" workflow);
  p.events <- List.rev (list (required "events" raw));
  p.log_bytes <-
    List.fold_left (fun n e -> n + String.length (encode e)) 0 p.events;
  if p.status = "running" then (
    p.status <- "failed";
    p.error <- Some "spindle restarted during workflow";
    p.finished <- Some (now ()));
  p

let emit record p ~step ~kind fields =
  Lock.protect p.log_lock @@ fun () ->
  let event =
    obj
      ([
         ("type", str kind);
         ("time", str (now ()));
         ("workflow", str p.job.name);
         ("step", int step);
       ]
      @ fields)
  in
  let raw = encode event in
  let size = String.length raw in
  if p.log_bytes + size > 1024 * 1024 then failwith "job log exceeds 1 MiB";
  let seq = record raw in
  p.log_seq <- seq;
  p.log_bytes <- p.log_bytes + size;
  p.events <- event :: p.events

let control record persist p ~step ~kind ?outcome ~status command =
  emit record p ~step ~kind:"control"
    ([
       ("content", str command);
       ("command", str command);
       ("status", str status);
       ("kind", str kind);
     ]
    @ Option.fold ~none:[] ~some:(fun o -> [ ("outcome", str o) ]) outcome);
  persist ()

(* [run_step record persist p ~step ~kind name f] runs [f] between a start
   and an end event. The end event carries the outcome, as the reference spindle
   sends it, even when [f] fails or is cancelled, so that viewers can mark
   the step that failed. *)
let run_step record persist p ~step ~kind name f =
  control record persist p ~step ~kind ~status:"start" name;
  match f () with
  | () ->
      control record persist p ~step ~kind ~outcome:"success" ~status:"end"
        name
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (* A full log budget or a closed store must not hide the failure. *)
      (try
         Eio.Cancel.protect (fun () ->
             control record persist p ~step ~kind ~outcome:"failed"
               ~status:"end" name)
       with _ -> ());
      Printexc.raise_with_backtrace exn bt

let data record p ~step ~stream content =
  emit record p ~step ~kind:"data"
    [ ("content", str content); ("stream", str stream) ]

(* Log frames contain text, so malformed UTF-8 becomes U+FFFD. *)
let utf8 raw =
  if String.is_valid_utf_8 raw then raw
  else
    let output = Buffer.create (String.length raw) in
    let rec loop off =
      if off < String.length raw then (
        let decoded = String.get_utf_8_uchar raw off in
        Buffer.add_utf_8_uchar output (Uchar.utf_decode_uchar decoded);
        loop (off + Uchar.utf_decode_length decoded))
    in
    loop 0;
    Buffer.contents output

let request input (job : Job.t) : Executor.request =
  {
    pipeline = input.id;
    workflow = job.name;
    repo = input.repo;
    source = input.source;
    commit = input.commit;
    clone = job.clone;
    metadata = input.metadata;
    definition = job.definition;
    yaml = job.yaml;
    env = job.env;
    variables = input.variables;
    secrets = input.secrets;
    steps = job.steps;
  }

(* [context record persist p ~secrets] numbers the executor's steps from 0 and
   masks [secrets] in each stream of a step. Text held back by a mask at the
   end of a step is flushed into that step. *)
let context record persist p ~secrets : Executor.context =
  let values = List.map snd secrets in
  let next = ref 0 in
  let current = ref None in
  let write step stream content =
    if content <> "" then data record p ~step ~stream (utf8 content)
  in
  let step name f =
    let step = !next in
    incr next;
    let masks = [ ("stdout", Mask.v values); ("stderr", Mask.v values) ] in
    run_step record persist p ~step ~kind:"user" name (fun () ->
        current := Some (step, masks);
        Fun.protect
          ~finally:(fun () ->
            current := None;
            try
              Eio.Cancel.protect (fun () ->
                  List.iter
                    (fun (stream, mask) -> write step stream (Mask.finish mask))
                    masks)
            with _ -> ())
          f)
  in
  let log stream content =
    match !current with
    | None -> invalid_arg "Executor log outside a step"
    | Some (step, masks) ->
        let stream =
          match stream with `Stdout -> "stdout" | `Stderr -> "stderr"
        in
        write step stream (Mask.feed (List.assoc stream masks) content)
  in
  { step; log }

let execute state input ~record ~persist p =
  Eio.Semaphore.acquire state.slots;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release state.slots)
  @@ fun () ->
  if (not p.cancelled) && p.status = "pending" then (
    p.status <- "running";
    p.started <- Some (now ());
    persist ();
    let result =
      try
        if p.cancelled then raise Cancelled;
        Eio.Time.with_timeout_exn state.system#clock state.timeout @@ fun () ->
        Eio.Switch.run @@ fun sw ->
        p.cancel <- Some (fun () -> Eio.Switch.fail sw Cancelled);
        state.executor
          (context record persist p ~secrets:input.secrets)
          (request input p.job);
        Ok ()
      with exn -> Error exn
    in
    Eio.Cancel.protect (fun () ->
        p.cancel <- None;
        p.finished <- Some (now ());
        (match result with
        | Ok () -> p.status <- "success"
        | Error exn ->
            p.status <-
              (if p.cancelled then "cancelled"
               else
                 match exn with Eio.Time.Timeout -> "timeout" | _ -> "failed");
            p.error <- Some (Printexc.to_string exn));
        persist ()))

let cancel ~persist p =
  if not (terminal p) then (
    p.cancelled <- true;
    match p.cancel with
    | Some cancel -> cancel ()
    | None ->
        p.status <- "cancelled";
        p.finished <- Some (now ());
        persist ())
