(* SPDX-License-Identifier: ISC *)
open Json

type t = {
  job : Job.t;
  parent : string option;
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
  mutable queued : bool;
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
  files : Executor.files;
  labels : unit -> string list;
}

exception Cancelled

let now () = Ptime.to_rfc3339 ~tz_offset_s:0 (Ptime_clock.now ())

(* A blocked workflow waits for a collaborator's approval. It is live, so
   cancellation and restart keep it, but it does not run until approved. *)
let terminal p =
  p.status <> "pending" && p.status <> "running" && p.status <> "blocked"

let v ?parent job =
  {
    job;
    parent;
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
    queued = false;
  }

let view p =
  obj
    ([
       ("id", str p.job.name); ("name", str p.job.name); ("status", str p.status);
     ]
    @ optional "startedAt" str p.started
    @ optional "finishedAt" str p.finished
    @ optional "error" str p.error)

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
    @ (if p.job.spawned then [ ("spawned", bool true) ] else [])
    @ (if p.queued then [ ("queued", bool true) ] else [])
    @ optional "parent" str p.parent
    @ optional "definition" Fun.id p.job.definition
    @ optional "yaml" str p.job.yaml)

(* Snapshots from before the executor hold argument vectors, shell scripts or
   a metadata step, which restore as the text of a command. *)
(* A workflow waiting in a builder's queue shows as pending, but it was
   running. *)
let interrupted raw =
  match get "status" (required "workflow" raw) with
  | "running" -> true
  | "pending" -> field "queued" raw = Some (Jsont.Json.bool true)
  | _ -> false

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

let restore ?(resume = false) raw =
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
    | None, _ when field "spawned" raw = Some (Jsont.Json.bool true) ->
        Job.spawned
          ~env:(Option.fold ~none:[] ~some:unpairs (field "env" raw))
          name steps
    | None, _ -> Job.v name steps
  in
  let p = v ?parent:(Option.map string (field "parent" raw)) job in
  p.queued <- false;
  p.status <- get "status" workflow;
  p.started <- Option.map string (field "startedAt" workflow);
  p.finished <- Option.map string (field "finishedAt" workflow);
  p.error <- Option.map string (field "error" workflow);
  p.events <- List.rev (list (required "events" raw));
  p.log_bytes <-
    List.fold_left (fun n e -> n + String.length (encode e)) 0 p.events;
  if interrupted raw then
    if resume then (
      (* It runs again from the start, so the interrupted log goes. *)
      p.status <- "pending";
      p.started <- None;
      p.events <- [];
      p.log_bytes <- 0)
    else (
      p.status <- "failed";
      p.error <- Some "spindle restarted during workflow";
      p.finished <- Some (now ()));
  p

(* A workflow interrupted in this process, by a lost lease, runs again too. *)
let reset p =
  p.cancel <- None;
  p.queued <- false;
  p.status <- "pending";
  p.started <- None;
  p.finished <- None;
  p.error <- None;
  p.events <- [];
  p.log_bytes <- 0

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

let control record persist p ~step ~kind ?command ?outcome ~status name =
  emit record p ~step ~kind:"control"
    ([ ("content", str name) ]
    @ optional "command" str command
    @ [ ("status", str status); ("kind", str kind) ]
    @ optional "outcome" str outcome);
  persist ()

(* [run_step record persist p ~step ~kind ?command name f] runs [f] between a
   start and an end event. The end event carries the outcome, as the reference
   spindle sends it, even when [f] fails or is cancelled, so that viewers can
   mark the step that failed. *)
let run_step record persist p ~step ~kind ?command name f =
  control record persist p ~step ~kind ?command ~status:"start" name;
  match f () with
  | () ->
      control record persist p ~step ~kind ~outcome:"success" ~status:"end" name
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
    files = input.files;
    labels = input.labels;
  }

let max_line = 16 * 1024

(* [line_end text] is where a frame that must end within [max_line] bytes may
   stop, short of a UTF-8 character it would split. *)
let line_end text =
  let rec back i =
    if i > 0 && Char.code text.[i] land 0xC0 = 0x80 then back (i - 1) else i
  in
  let i = back max_line in
  if i = 0 then max_line else i

(* A stream of one step. Masked text is framed one line at a time, as the
   reference spindle's logger sends it. *)
type stream = { name : string; mask : Mask.t; pending : Buffer.t }

let stream name values =
  { name; mask = Mask.v values; pending = Buffer.create 256 }

(* [frame record p ~step s text] sends each complete line of [s]'s pending
   text and [text], keeping an unterminated remainder. *)
let frame record p ~step s text =
  Buffer.add_string s.pending text;
  let all = Buffer.contents s.pending in
  Buffer.clear s.pending;
  let send line = data record p ~step ~stream:s.name (utf8 line) in
  let rec loop start =
    let rest = String.length all - start in
    match String.index_from_opt all start '\n' with
    | Some i when i - start <= max_line ->
        send (String.sub all start (i - start + 1));
        loop (i + 1)
    | _ when rest > max_line ->
        let n = line_end (String.sub all start (max_line + 1)) in
        send (String.sub all start n);
        loop (start + n)
    | _ -> Buffer.add_substring s.pending all start rest
  in
  loop 0

(* [flush record p ~step s] sends what [s] still holds at the end of a step. *)
let flush record p ~step s =
  frame record p ~step s (Mask.finish s.mask);
  if Buffer.length s.pending > 0 then (
    let rest = Buffer.contents s.pending in
    Buffer.clear s.pending;
    data record p ~step ~stream:s.name (utf8 rest))

(* [context record persist p ~secrets] numbers the executor's steps from 0 and
   masks [secrets] in each stream of a step. Text held back by a mask or an
   unterminated line at the end of a step is flushed into that step. *)
let context record persist p ~secrets ~spawn ~await ~watch ~finished ~seconds
    ~queued_for : Executor.context =
  let values = List.map snd secrets in
  let hide = Mask.hide values in
  let next = ref 0 in
  let current = ref None in
  let step ?(kind = `User) ?command name f =
    let step = !next in
    incr next;
    let streams = [ stream "stdout" values; stream "stderr" values ] in
    let kind = match kind with `User -> "user" | `System -> "system" in
    let name = hide name and command = Option.map hide command in
    (* What a step's streams still hold is part of it, so a log that cannot
       take it fails the step. After a failure it is kept if it fits. *)
    run_step record persist p ~step ~kind ?command name (fun () ->
        current := Some (step, streams);
        match f () with
        | () ->
            current := None;
            List.iter (flush record p ~step) streams
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            current := None;
            (try
               Eio.Cancel.protect (fun () ->
                   List.iter (flush record p ~step) streams)
             with _ -> ());
            Printexc.raise_with_backtrace exn bt)
  in
  let log stream content =
    match !current with
    | None -> invalid_arg "Executor log outside a step"
    | Some (step, streams) ->
        let s =
          match stream with
          | `Stdout -> List.hd streams
          | `Stderr -> List.nth streams 1
        in
        frame record p ~step s (Mask.feed s.mask content)
  in
  let spawn ?(env = []) name steps = spawn ~env name steps in
  let spawned ?since () = watch since in
  (* A workflow waiting in a builder's queue is pending, and starts when it
     leaves it. *)
  let queued : 'a. (unit -> 'a) -> 'a =
   fun f ->
    p.status <- "pending";
    p.started <- None;
    p.queued <- true;
    persist ();
    let began = seconds () in
    let waited () =
      Eio.Cancel.protect (fun () -> queued_for (seconds () -. began))
    in
    match f () with
    | x ->
        waited ();
        Eio.Cancel.protect (fun () ->
            p.queued <- false;
            if p.status = "pending" then (
              p.status <- "running";
              p.started <- Some (now ());
              persist ()));
        x
    | exception exn ->
        (* A stopping spindle leaves it queued, so it runs again. *)
        let bt = Printexc.get_raw_backtrace () in
        waited ();
        Printexc.raise_with_backtrace exn bt
  in
  { step; log; spawn; await_spawned = await; spawned; finished; queued }

(* [text p] is [p]'s log as a person reads it. *)
let text p =
  let b = Buffer.create 4096 in
  List.iter
    (fun event ->
      let field k = Option.map string (Json.field k event) in
      match (field "type", field "content") with
      | Some "control", Some name when field "status" = Some "start" ->
          Buffer.add_string b ("## " ^ name ^ "\n")
      | Some "data", Some content ->
          Buffer.add_string b content;
          if not (String.ends_with ~suffix:"\n" content) then
            Buffer.add_char b '\n'
      | _ -> ())
    (List.rev p.events);
  Buffer.contents b

let ended p =
  match p.status with
  | "success" -> `Success
  | "cancelled" -> `Cancelled
  | "timeout" -> `Timeout
  | _ -> `Failed

let outcome p : Executor.outcome =
  { workflow = p.job.name; status = ended p; error = p.error; log = text p }

(* The newest control event says whether a step is open. *)
let current_step p =
  List.find_map
    (fun event ->
      let field k = Option.map string (Json.field k event) in
      match (field "type", field "status") with
      | Some "control", Some "start" -> Some (field "content")
      | Some "control", _ -> Some None
      | _ -> None)
    p.events
  |> Option.join

let progress p : Executor.progress =
  let status =
    match p.status with
    | "pending" | "blocked" -> `Pending
    | "running" -> `Running
    | _ -> ended p
  in
  {
    workflow = p.job.name;
    status;
    step = (if status = `Running then current_step p else None);
    error = p.error;
  }

(* [message exn] is what a viewer is shown for a failed workflow. *)
let message = function
  | Failure m | Invalid_argument m -> m
  | exn -> Printexc.to_string exn

let execute state input ~record ~persist ~spawn ?(await = fun () -> [])
    ?(watch = fun _ -> []) ?(finished = fun () -> []) ?(queued_for = ignore)
    ?(interruptible = false) p =
  Eio.Semaphore.acquire state.slots;
  (* The slot is given up while the workflow waits for those it spawned, so
     that waiting workflows cannot occupy every slot their children need. *)
  let held = ref true in
  Fun.protect ~finally:(fun () ->
      if !held then Eio.Semaphore.release state.slots)
  @@ fun () ->
  let gate =
    Gate.v
      ~release:(fun () ->
        held := false;
        Eio.Semaphore.release state.slots)
      ~acquire:(fun () ->
        Eio.Semaphore.acquire state.slots;
        held := true)
  in
  let released f = Gate.run gate f in
  let await () = List.map outcome (released await) in
  let watch = function
    | None -> watch None
    | since -> released (fun () -> watch since)
  in
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
          (context record persist p ~secrets:input.secrets ~spawn ~await ~watch
             ~finished:(fun () -> List.map outcome (finished ()))
             ~seconds:(fun () ->
               Int64.to_float
                 (Mtime.to_uint64_ns
                    (Eio.Time.Mono.now state.system#mono_clock))
               /. 1e9)
             ~queued_for)
          (request input p.job);
        Ok ()
      with
      | Eio.Cancel.Cancelled _ as e when interruptible && not p.cancelled ->
          (* The spindle is stopping, or another worker took the job. The
             workflow stays running, to run again, and its switch is gone. *)
          p.cancel <- None;
          raise e
      | exn -> Error (exn, Printexc.get_raw_backtrace ())
    in
    Eio.Cancel.protect (fun () ->
        p.cancel <- None;
        p.finished <- Some (now ());
        (match result with
        | Ok () -> p.status <- "success"
        | Error (exn, _) ->
            let status, error =
              if p.cancelled then
                ("cancelled", Some "User canceled the workflow")
              else
                match exn with
                | Eio.Time.Timeout -> ("timeout", None)
                | Eio.Cancel.Cancelled _ ->
                    ("failed", Some "the spindle stopped during the workflow")
                | exn ->
                    ( "failed",
                      Some
                        (Mask.hide (List.map snd input.secrets) (message exn))
                    )
            in
            p.status <- status;
            p.error <- error);
        persist ());
    (* A workflow stopped from outside is recorded, and the stop goes on. *)
    match result with
    | Error ((Eio.Cancel.Cancelled _ as exn), bt) when not p.cancelled ->
        Printexc.raise_with_backtrace exn bt
    | _ -> ())

let cancel ~persist p =
  if not (terminal p) then (
    p.cancelled <- true;
    match p.cancel with
    | Some cancel -> cancel ()
    | None ->
        p.status <- "cancelled";
        p.finished <- Some (now ());
        persist ())
