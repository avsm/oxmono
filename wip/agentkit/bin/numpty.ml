(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* numpty: a local model agent that runs unattended.

   The subcommands are meant to be read in order.
     once      one wake-up, no daemon, on the same store
     netd      internal, the child that reaches the network

   The daemon and the commands that read its store follow. *)

module V4 = Ds4.V4
module Agent = Ds4.Agent
module Tool = Ds4.Tool
module Toolbox = Ds4.Toolbox
module Cli = Ds4_cli.Cli
module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Model = Ds4_cli.Model
module Schedule = Agentkit.Schedule
module Client = Numpty_net.Client
module Brief = Numpty_daemon.Brief
module Memory_tools = Numpty_daemon.Memory_tools
module Daemon = Numpty_daemon.Daemon
module History = Numpty_daemon.History
module Control = Numpty_daemon.Control
module Show = Numpty_daemon.Show
module Status = Numpty_daemon.Status
module Store = Numpty_daemon.Store
module Task = Numpty_daemon.Task
module Utc = Agentkit.Utc
module Wake = Numpty_daemon.Wake
module Driver = Agentkit.Driver
module Apple_support = Agentkit_apple_support

let version = "0.1"

(* Two XDG layouts. The models and the compiled cache are shared with humpty,
   under ds4, since a machine holds one copy of a hundred-gigabyte model. What
   numpty owns is its own, under numpty: the store is its state directory and
   the schedule is its config directory. *)
type layout = { models : Xdge.t; own : Xdge.t }

let run f =
  Cli.guard @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  f env { models = Xdge.create fs "ds4"; own = Xdge.create fs "numpty" }

let store_root ~fs ~own = function
  | None -> Store.default_root own
  | Some path -> Eio.Path.(fs / path)

let native path = Option.value ~default:"?" (Eio.Path.native path)

(* numptyd is spawned before the engine exists, because a spawn is a fork of
   this process and forking one that has a model mapped into it costs minutes on
   macOS. It is the only process numpty ever makes.

   A numptyd that will not start ends the command rather than leaving an agent
   with no network. The work numpty is woken for reaches the network, and a
   supervisor reading a nonzero exit is what gets a fresh one. *)
let start_netd ~sw ~proc ~clock =
  let trace line = Logs.info (fun m -> m "netd: %s" line) in
  match Client.start ~sw ~proc ~clock ~trace ~argv:[ Cli.self (); "netd" ] with
  | Ok client ->
      (* Stopped before the release of [sw] kills it, release handlers being run
         in the reverse of the order they were added. *)
      Eio.Switch.on_release sw (fun () -> Client.stop client);
      client
  | Error e ->
      failwith
        (Printf.sprintf
           "the network child would not start, so there is nothing to reach \
            the network with: %s"
           e)

(* An unattended agent has nobody to ask, so a directory outside the workspace
   is refused rather than granted. The refusal reaches the model, which can then
   say so and write it down. *)
let approve path =
  Logs.warn (fun m -> m "refused a capability outside the workspace: %s" path);
  false

(* The whole tool list. The file tools hold a capability minted on the store's
   workspace alone, so what the agent accumulates sits beside the journal that
   explains it. There is no bash: a program is run through numptyd's [run], in
   the process that is there to fork. *)
let assemble ~caps ~net ~client ~store =
  [
    Toolbox.open_dir ~caps;
    Toolbox.caps ~caps;
    Toolbox.tree ~caps;
    Toolbox.list ~caps;
    Toolbox.read ~caps;
    Toolbox.read_lines ~caps;
    Toolbox.find ~caps;
    Toolbox.grep ~caps;
    Toolbox.stat ~caps;
    Toolbox.write ~caps;
    Toolbox.append ~caps;
    Toolbox.edit ~caps;
    Toolbox.dns ~net;
    Numpty_net.Tools.fetch ~client;
    Numpty_net.Tools.head ~client;
    Numpty_net.Tools.run ~client;
  ]
  @ Memory_tools.all ~memory:(Store.memory store) ~journal:(Store.journal store)

(* Every run says how it ended, a run that ended in a failure among them, since
   a trace whose last line is the start reads as a run still going. A journal
   that cannot be written is one of the failures this meets, and then there is
   nothing to record with, so the record is attempted and the failure raised
   whatever came of it. *)
let stopping journal f =
  match f () with
  | v -> v
  | exception e ->
      (try
         ignore
           (Journal.append journal (Journal.Run_stop (Printexc.to_string e)))
       with _ -> ());
      raise e

(* ---- a run -------------------------------------------------------------- *)

(* What both commands that load a model take. They differ only in what asks for
   the work: a prompt on the command line, or the schedule file. *)
type opts = {
  model : string;
  store : string option;
  ctx_size : int;
  max_ctx : int;
  think : Dsml.thinking_mode;
  seed : int;
  handover_at : float;
}

(* The whole of a run's assembly, in the order it has to happen in: the model
   resolved before anything is started, the store locked, the start recorded,
   numptyd spawned while this process is still small, and only then the engine.

   A failure anywhere after the run_start is recorded as the reason the run
   stopped, since a trace whose last line is the start reads as a run still
   going. *)
let with_model env layout opts f =
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  if opts.handover_at <= 0. || opts.handover_at > 1. then
    failwith
      (Printf.sprintf
         "--handover-at is a fraction of the largest context, above 0 and at \
          most 1, and %g is not"
         opts.handover_at);
  let chosen =
    let ds4 =
      Driver.v ~name:"ds4"
        ~models:(Agentkit_ds4.models ~dir:(Model.dir layout.models))
        ~create:(fun model ->
          `Ds4 (Agentkit_ds4.model_path ~dir:(Model.dir layout.models) model))
    in
    let apple =
      if Apple_support.available then
        [
          Driver.v ~name:"apple" ~models:Apple_support.models
            ~create:(fun model ->
              if
                opts.think <> Dsml.Chat || opts.seed <> 0
                || opts.ctx_size <> 32768 || opts.max_ctx <> 262144
              then
                failwith
                  "--think, --seed, --ctx and --max-ctx are DS4 options and \
                   cannot be set for apple/default";
              `Apple (model, Apple_support.context_size model));
        ]
      else []
    in
    match Driver.create (Driver.merge (ds4 :: apple)) opts.model with
    | Ok chosen -> chosen
    | Error message -> failwith message
  in
  let model_path, backend, context_size =
    match chosen with
    | `Ds4 path -> (path, Cli.backend_name, opts.ctx_size)
    | `Apple (name, size) -> ("apple/" ^ name, "Apple Foundation Models", size)
  in
  let root = store_root ~fs ~own:layout.own opts.store in
  let store = Store.open_ ~sw ~clock root in
  let journal = Store.journal store in
  ignore
    (Journal.append journal
       (Journal.Run_start
          {
            Journal.pid = Unix.getpid ();
            version;
            backend;
            model = model_path;
            ctx_size = context_size;
          }));
  (* Said however the run ends, a fault included, since the account is most
     wanted when something went wrong. *)
  Fun.protect ~finally:(fun () ->
      Printf.eprintf "The run is recorded at %s. 'numpty log' reads it.\n%!"
        (Option.value ~default:"?" (Eio.Path.native (Store.journal_dir root))))
  @@ fun () ->
  stopping journal @@ fun () ->
  let client = start_netd ~sw ~proc:(Eio.Stdenv.process_mgr env) ~clock in
  let engine =
    match chosen with
    | `Ds4 path ->
        Some
          (V4.create ~sw
             ~domain_mgr:(Eio.Stdenv.domain_mgr env)
             ~cache:(Xdge.cache_dir layout.models)
             ~model:Eio.Path.(fs / path)
             ())
    | `Apple _ -> None
  in
  Eio.Path.with_subtree (Store.workspace store) @@ fun ws ->
  let caps = Toolbox.Caps.create ~sw ~fs ~approve ws in
  let tools = assemble ~caps ~net:(Eio.Stdenv.net env) ~client ~store in
  let config =
    {
      Wake.ctx_size = context_size;
      max_ctx_size =
        (match chosen with `Ds4 _ -> opts.max_ctx | `Apple _ -> context_size);
      handover_at = opts.handover_at;
      max_sessions = Wake.default_config.Wake.max_sessions;
      thinking = opts.think;
      seed = Cli.resolve_seed opts.seed;
      tool_result_limit =
        (match chosen with
        | `Ds4 _ -> Wake.default_config.Wake.tool_result_limit
        | `Apple _ -> 0);
    }
  in
  (* What the control fiber reads. The agent loop publishes a whole job record
     at each state change, the schedule loop publishes the next fire times, and
     the prefill counter is a reader into the running session, which is the one
     live call into the agent that does not wait for the engine. *)
  let job = ref None and tasks = ref [] and prefill = ref (fun () -> (0, 0)) in
  let since = Journal.rfc3339 (Eio.Time.now clock) in
  let snapshot () =
    {
      Status.run = Journal.run journal;
      since;
      model = model_path;
      backend;
      netd =
        (match Client.fault client with None -> "alive" | Some why -> why);
      version = Agentkit.Memory.version (Store.memory store);
      job =
        Option.map
          (fun (j : Status.job) ->
            let prefilled, prefill_total = !prefill () in
            { j with Status.prefilled; prefill_total })
          !job;
      tasks = !tasks;
    }
  in
  let wake ~stop ~task ~prompt =
    let create ~system =
      match engine with
      | Some engine ->
          let agent =
            Agent.create engine ~system ~thinking:config.thinking
              ~ctx_size:config.ctx_size ~max_ctx_size:config.max_ctx_size
              ~tool_result_limit:config.tool_result_limit ~seed:config.seed
              ~now:(fun () -> Eio.Time.now clock)
              ~tools
          in
          Driver.session
            ~prefill_progress:(fun () -> Agent.prefill_progress agent)
            (module Agentkit_ds4.Agent)
            agent
      | None ->
          let name =
            match chosen with `Apple (name, _) -> name | _ -> assert false
          in
          let system =
            system
            ^ "\n\n\
               Before a file tool, call open_dir to get a capability and pass \
               its name as cap. Memory survives this wake-up only through \
               memory_write."
          in
          fst (Apple_support.create ~sw ~model:name ~system tools)
    in
    Wake.run
      ~cancel_on_stop:(match chosen with `Ds4 _ -> true | `Apple _ -> false)
      ~clock ~store ~create ~config
      ~fault:(fun () -> Client.fault client)
      ~stop
      ~publish:(fun j -> job := j)
      ~prefill:(fun reader -> prefill := reader)
      ~task ~prompt
  in
  f ~sw ~clock ~store ~journal ~wake ~snapshot
    ~publish_tasks:(fun t -> tasks := t)
    ~net:(Eio.Stdenv.net env)

(* Nonzero, because there is no respawn: a fresh numptyd has to be made before
   an engine is loaded, which is a supervisor's restart. A numpty run without a
   supervisor stays down until a person returns, which is what the nonzero exit
   is for. *)
let faulted journal why =
  ignore
    (Journal.append journal
       (Journal.Run_stop ("the network child faulted: " ^ why)));
  failwith
    (Printf.sprintf
       "the network child faulted, so the run stopped: %s. The handover was \
        taken and the journal names the fault."
       why)

(* ---- once -------------------------------------------------------------- *)

(* One wake-up on the store, with no daemon and no socket. It is how a change is
   tested, and what the live test drives. *)
let once opts prompt =
  run @@ fun env layout ->
  with_model env layout opts
  @@
  fun ~sw:_
    ~clock
    ~store:_
    ~journal
    ~wake
    ~snapshot:_
    ~publish_tasks:_
    ~net:_
  ->
  ignore
    (Journal.append journal
       (Journal.Wake
          {
            Journal.task = "once";
            due = Journal.rfc3339 (Eio.Time.now clock);
            why = "once";
            serial = None;
          }));
  (* No signal handler is installed here, this being a foreground command a
     person stops by leaving it. *)
  match wake ~stop:(fun () -> false) ~task:"once" ~prompt with
  | Wake.Finished ->
      ignore (Journal.append journal (Journal.Run_stop "the wake-up finished"))
  | Wake.Faulted why -> faulted journal why

(* ---- run --------------------------------------------------------------- *)

(* The daemon. It holds the engine and numptyd for the life of the run and works
   through the tasks the schedule file asks for, one at a time. *)
let daemon opts tick =
  run @@ fun env layout ->
  with_model env layout opts
  @@ fun ~sw ~clock ~store ~journal ~wake ~snapshot ~publish_tasks ~net ->
  let schedule = Store.schedule_file layout.own in
  Logs.info (fun m -> m "reading the schedule from %s" (native schedule));
  (* The socket is bound after the lock is taken, so a socket file a crashed run
     left belongs to nobody and is replaced. A store path too long for a unix
     address is a warning and no socket: an agent that cannot be queried is
     still an agent. *)
  (match
     Control.serve ~sw ~net ~root:(Store.root store)
       (Control.make ~snapshot ~clock ~root:(Store.root store))
   with
  | `Serving path -> Logs.info (fun m -> m "answering on %s" path)
  | `Unbound why -> Logs.warn (fun m -> m "%s" why));
  match
    Daemon.run ~clock ~store ~schedule ~tick ~publish_tasks
      ~wake:(wake ~stop:(fun () -> Daemon.stopping () <> None))
  with
  | Daemon.Signalled why ->
      ignore
        (Journal.append journal
           (Journal.Run_stop
              (why ^ ": the turn in flight finished and the handover was taken")))
  | Daemon.Faulted why -> faulted journal why

(* ---- task -------------------------------------------------------------- *)

(* An ordinary file editor over the schedule. It runs as the person, holds no
   model, and needs no daemon: a change made while numpty is stopped is picked up
   at startup, and one made while it runs is picked up on its next tick. *)
let task_list () =
  run @@ fun _env layout ->
  match Schedule.read (Store.schedule_file layout.own) with
  | Ok s -> print_string (Task.list s)
  | Error e -> failwith e

let task_check store_opt =
  run @@ fun env layout ->
  let fs = Eio.Stdenv.fs env in
  let path = Store.schedule_file layout.own in
  match Schedule.read path with
  | Error e -> failwith e
  | Ok s ->
      (* The next fire times come from the journal, since that is the only state
         and a next fire time is computed rather than stored. *)
      let history =
        History.read
          (Store.journal_dir (store_root ~fs ~own:layout.own store_opt))
      in
      let last id =
        Option.map
          (fun (f : History.fired) -> f.History.due)
          (History.fired history id)
      in
      print_string
        (Task.check ~zone:Schedule.system
           ~now:(Eio.Time.now (Eio.Stdenv.clock env))
           ~last s)

let task_add id every at days once_ on_missed prompt =
  run @@ fun _env layout ->
  let path = Store.schedule_file layout.own in
  let trigger =
    match (every, at, once_) with
    | Some d, None, false ->
        Result.fold
          ~ok:(fun secs -> Schedule.Every secs)
          ~error:failwith
          (Schedule.duration_of_string d)
    | None, Some t, false ->
        let hour, minute =
          Result.fold ~ok:Fun.id ~error:failwith (Schedule.time_of_string t)
        in
        let days =
          List.map
            (fun d ->
              Result.fold ~ok:Fun.id ~error:failwith (Schedule.day_of_string d))
            days
        in
        Schedule.At { hour; minute; days }
    | None, None, true -> Schedule.Once
    | None, None, false ->
        failwith "a task needs one trigger: --every 15m, --at 07:00, or --once"
    | _ ->
        failwith
          "a task names one trigger and this names two. Write one of --every \
           15m, --at 07:00 or --once"
  in
  if days <> [] && at = None then
    failwith "--days narrows --at, so it needs an --at HH:MM to narrow";
  Result.fold
    ~ok:(fun (s : Schedule.t) ->
      Printf.printf "%s: %d task%s in the schedule now.\n" id
        (List.length s.Schedule.tasks)
        (if List.length s.Schedule.tasks = 1 then "" else "s"))
    ~error:failwith
    (Task.add path ~id ~trigger ~on_missed ~prompt)

let task_one f describe id =
  run @@ fun _env layout ->
  Result.fold
    ~ok:(fun _ -> Printf.printf "%s: %s.\n" id describe)
    ~error:failwith
    (f (Store.schedule_file layout.own) id)

let task_run id =
  run @@ fun _env layout ->
  match Task.run_now (Store.schedule_file layout.own) id with
  | Error e -> failwith e
  | Ok serial ->
      Printf.printf
        "%s: asked for as run_now %d. A running numpty fires it on its next \
         tick, and a stopped one at startup.\n"
        id serial

(* ---- log and memory ---------------------------------------------------- *)

(* Both read the store with the codecs that wrote it and print. They work while
   numpty runs, while it is stopped, and on a store copied off the machine, so
   neither takes the lock. *)

(* A kind nobody writes matches nothing, and a filter that matches nothing looks
   exactly like a journal with nothing in it. So a misspelling is refused with
   the kinds there are rather than answered with silence. A journal written by a
   later build may hold a kind this one does not know, which is why the message
   says whose list this is. *)
let check_kinds kinds =
  match List.filter (fun k -> not (List.mem k Journal.kind_names)) kinds with
  | [] -> ()
  | unknown ->
      failwith
        (Printf.sprintf "no record has the kind %s. This build writes %s"
           (String.concat " or " (List.map (Printf.sprintf "%S") unknown))
           (String.concat ", " Journal.kind_names))

let print_log store_opt since kinds task run_opt =
  run @@ fun env layout ->
  let fs = Eio.Stdenv.fs env in
  let since =
    match since with
    | None -> None
    | Some s -> Some (Result.fold ~ok:Fun.id ~error:failwith (Utc.of_since s))
  in
  check_kinds kinds;
  let kinds = match kinds with [] -> None | k -> Some k in
  let root = store_root ~fs ~own:layout.own store_opt in
  let dir = Store.journal_dir root in
  let printed = ref 0 in
  Show.log ?since ?kinds ?task ?run:run_opt dir (fun line ->
      incr printed;
      print_endline line);
  (* Printing nothing says neither which of the two it was, and a person who has
     mistyped a store path is owed the path that was read. *)
  if !printed = 0 then
    if Journal.segments dir = [] then
      Printf.printf
        "The journal at %s holds nothing. No run has written to this store.\n"
        (native dir)
    else
      Printf.printf "No record in the journal at %s matches those filters.\n"
        (native dir)

let with_memory env layout store_opt f =
  let fs = Eio.Stdenv.fs env in
  let root = store_root ~fs ~own:layout.own store_opt in
  print_string
    (f (Memory.create ~clock:(Eio.Stdenv.clock env) (Store.memory_dir root)))

let memory_show store_opt at =
  run @@ fun env layout ->
  with_memory env layout store_opt (fun m -> Show.show m ~at)

let memory_history store_opt =
  run @@ fun env layout -> with_memory env layout store_opt Show.history

let memory_diff store_opt v w =
  run @@ fun env layout ->
  with_memory env layout store_opt (fun m -> Show.diff m v w)

(* ---- status, jobs and follow ------------------------------------------- *)

(* The three that need the socket, since what they ask about is the one thing
   the files cannot say. With no daemon listening they say so and, where the
   journal can answer at all, print what the last run was doing when it stopped
   rather than nothing. *)
let last_run root =
  let out = ref [] in
  Show.log
    ~kinds:[ "run_start"; "wake"; "continued"; "handover"; "error"; "run_stop" ]
    (Store.journal_dir root) (fun l -> out := l :: !out);
  List.rev (List.filteri (fun i _ -> i < 6) !out)

let with_daemon store_opt f =
  run @@ fun env layout ->
  Eio.Switch.run @@ fun sw ->
  let root = store_root ~fs:(Eio.Stdenv.fs env) ~own:layout.own store_opt in
  match f ~sw ~net:(Eio.Stdenv.net env) ~root with
  | Ok () -> ()
  | Error e ->
      (match last_run root with
      | [] -> ()
      | lines ->
          print_endline "The last thing the store's journal says happened:";
          List.iter print_endline lines);
      failwith e

let unexpected = "the numpty on this store answered something else"

let status_cmd store_opt =
  with_daemon store_opt @@ fun ~sw ~net ~root ->
  match Control.ask ~sw ~net ~root Control.Status with
  | Error e -> Error e
  | Ok (Control.Refused what) -> Error what
  | Ok (Control.Running (r : Status.running)) ->
      Printf.printf "run %d, since %s\n" r.Status.r_run r.Status.r_since;
      Printf.printf "model %s, on %s\n" r.Status.r_model r.Status.r_backend;
      Printf.printf "network child %s\n" r.Status.r_netd;
      Printf.printf "memory at version %d\n" r.Status.r_version;
      print_endline
        (match r.Status.r_job with
        | None -> "no job is running"
        | Some task -> "working on " ^ task);
      Ok ()
  | Ok _ -> Error unexpected

let jobs_cmd store_opt =
  with_daemon store_opt @@ fun ~sw ~net ~root ->
  match Control.ask ~sw ~net ~root Control.Jobs with
  | Error e -> Error e
  | Ok (Control.Refused what) -> Error what
  | Ok (Control.Jobs_are j) ->
      (match j.Status.j_job with
      | None -> print_endline "No job is running."
      | Some (job : Status.job) ->
          Printf.printf "%s, session %d, started %s\n" job.Status.task
            job.Status.session job.Status.started;
          Printf.printf "  context %d of %d, %d turn%s, %d tool call%s\n"
            job.Status.ctx_used job.Status.ctx_size job.Status.turns
            (if job.Status.turns = 1 then "" else "s")
            job.Status.tool_calls
            (if job.Status.tool_calls = 1 then "" else "s");
          (match job.Status.tool with
          | Some tool -> Printf.printf "  in a call to %s\n" tool
          | None -> ());
          if job.Status.prefill_total > 0 then
            Printf.printf "  prefilled %d of %d tokens\n" job.Status.prefilled
              job.Status.prefill_total);
      if j.Status.j_tasks = [] then print_endline "The schedule holds no tasks."
      else begin
        print_endline "tasks:";
        List.iter
          (fun (d : Status.due) ->
            Printf.printf "  %-16s %s\n" d.Status.task
              (match (d.Status.waiting, d.Status.next) with
              | true, _ -> "due now, waiting for the engine"
              | false, Some next -> "next " ^ next
              | false, None -> "never fires again"))
          j.Status.j_tasks
      end;
      Ok ()
  | Ok _ -> Error unexpected

let follow_cmd store_opt kinds =
  with_daemon store_opt @@ fun ~sw ~net ~root ->
  check_kinds kinds;
  Control.stream ~sw ~net ~root ~kinds print_endline

(* ---- netd ------------------------------------------------------------- *)

(* The network in a process of its own, spawned by the daemon before it loads a
   model and speaking the protocol over the pipes it was given. It holds every
   program numpty runs, so that a numpty which has loaded a model forks
   nothing. *)
let netd () =
  Cli.guard @@ fun () ->
  Eio_main.run @@ fun env ->
  Numpty_net.Server.run ~stdin:(Eio.Stdenv.stdin env)
    ~stdout:(Eio.Stdenv.stdout env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ()

(* ---- cmdliner ---------------------------------------------------------- *)

open Cmdliner

let with_log t = Cli.with_logs t
let model = Driver.model_term ~default:"ds4/auto" ~short_driver:"ds4" ()

let store =
  Arg.(
    value
    & opt (some string) None
    & info [ "store" ] ~docv:"DIR"
        ~doc:
          "The directory numpty keeps its journal, its memory and its \
           workspace in. It defaults to $(b,\\$XDG_STATE_HOME/numpty). One run \
           owns a store, so a second run on the same one is refused with the \
           first one's process id.")

let ctx =
  Arg.(
    value & opt int 32768
    & info [ "ctx" ] ~docv:"N"
        ~doc:
          "The context window a wake-up starts with, in tokens. It must hold \
           the brief and the tool output the work reads. Memory use grows with \
           it.")

let max_ctx =
  Arg.(
    value & opt int 262144
    & info [ "max-ctx" ] ~docv:"N"
        ~doc:
          "The largest context a session may grow to. Reaching \
           $(b,--handover-at) of it ends the session with a handover rather \
           than growing further.")

let handover_at =
  Arg.(
    value & opt float 0.75
    & info [ "handover-at" ] ~docv:"FRACTION"
        ~doc:
          "How full the context may get before the wake-up stops feeding the \
           agent work, as a fraction of $(b,--max-ctx). At that point it asks \
           the agent what should survive, writes it to memory, closes the \
           session and starts a fresh one on the same task.")

let opts =
  Term.(
    const (fun model store ctx_size max_ctx think seed handover_at ->
        { model; store; ctx_size; max_ctx; think; seed; handover_at })
    $ model $ store $ ctx $ max_ctx $ Cli.think $ Cli.seed $ handover_at)

let once_prompt =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PROMPT" ~doc:"What to ask the agent to do.")

let once_cmd =
  let doc = "Run one wake-up on the store and exit." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "One wake-up, with no daemon and no schedule. It opens the store, \
         builds a brief from the memory in it, gives the agent the prompt as \
         though a task had fired, lets it work, asks it what should survive, \
         and closes the session.";
      `P
        "Everything it does is recorded in the store's journal, and what it \
         writes down survives in the store's memory, so a later $(b,numpty \
         run) on the same store carries on from it. It binds no socket, since \
         it is not there to be asked.";
      `P
        "The exit status is nonzero if the network child faulted. The handover \
         is still taken and the journal names the fault, since a run that \
         stopped is one a supervisor restarts.";
    ]
  in
  let term = with_log Term.(const once $ opts $ once_prompt) in
  Cmd.v (Cmd.info "once" ~doc ~man) term

(* ---- run command ---- *)

let tick =
  Arg.(
    value & opt float 1.0
    & info [ "tick" ] ~docv:"SECS"
        ~doc:
          "How often to look at the schedule file and at which tasks are due. \
           It stats the file rather than reading it, so a short tick costs \
           little.")

let run_cmd =
  let doc = "Run the daemon, waking for each task the schedule asks for." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The daemon. It opens the store, spawns the network child, loads the \
         model once, and then waits. Each time a task in \
         $(b,\\$XDG_CONFIG_HOME/numpty/schedule.json) comes due it builds a \
         brief from memory, works, asks the agent what should survive and \
         closes the session. One task runs at a time, since a process holds \
         one model.";
      `P
        "It reads the schedule and never writes it, so nothing it concludes \
         can change what it was told to do. Ask for work with $(b,numpty \
         task), which needs no daemon. A change is picked up on the next tick, \
         and $(b,SIGHUP) rereads the file at once. A file that does not parse \
         is reported and the schedule in force stays in force.";
      `P
        "$(b,SIGTERM) and $(b,SIGINT) stop it: the turn in flight finishes, \
         the handover is taken, and it exits zero. It exits nonzero if the \
         network child faulted, since there is no respawn and a fresh one has \
         to be made before a model is loaded, which is a supervisor's restart.";
      `P
        "Everything it does is in the store's journal, which $(b,numpty log) \
         prints and which needs no daemon to read.";
    ]
  in
  Cmd.v (Cmd.info "run" ~doc ~man) (with_log Term.(const daemon $ opts $ tick))

(* ---- task commands ---- *)

let task_id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"ID"
        ~doc:"The task's id, which is what the journal calls it.")

let task_prompt =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"PROMPT" ~doc:"What to ask the agent to do when it fires.")

let every =
  Arg.(
    value
    & opt (some string) None
    & info [ "every" ] ~docv:"D"
        ~doc:
          "Fire on this interval, a positive number and one of s, m, h or d, \
           such as $(b,15m), $(b,6h) or $(b,1d).")

let at =
  Arg.(
    value
    & opt (some string) None
    & info [ "at" ] ~docv:"HH:MM"
        ~doc:
          "Fire daily at this time on the local wall clock, narrowed by \
           $(b,--days). A time a daylight saving change skips or repeats still \
           fires exactly once that day.")

let days =
  Arg.(
    value & opt_all string []
    & info [ "days" ] ~docv:"DAY"
        ~doc:
          "Narrow $(b,--at) to these weekdays, one of mon, tue, wed, thu, fri, \
           sat or sun. Repeat the option for each day.")

let once_flag =
  Arg.(
    value & flag
    & info [ "once" ]
        ~doc:
          "Fire at the next opportunity and then never again. That it has \
           fired is read out of the journal, so this file needs no state \
           written back into it.")

let on_missed =
  Arg.(
    value
    & opt
        (enum [ ("run-once", Schedule.Run_once); ("skip", Schedule.Skip) ])
        Schedule.Run_once
    & info [ "on-missed" ] ~docv:"WHAT"
        ~doc:
          "What to do about the due times a stopped daemon was not there for. \
           $(b,run-once) runs one of them, and $(b,skip) runs none and waits \
           for the next. Missed fires never queue.")

let task_cmds =
  [
    Cmd.v
      (Cmd.info "list" ~doc:"Show the tasks in the schedule.")
      (with_log Term.(const task_list $ const ()));
    Cmd.v
      (Cmd.info "check"
         ~doc:
           "Check that the schedule parses and say when each task fires next.")
      (with_log Term.(const task_check $ store));
    Cmd.v
      (Cmd.info "add" ~doc:"Add a task, or replace the one of that id.")
      (with_log
         Term.(
           const task_add $ task_id $ every $ at $ days $ once_flag $ on_missed
           $ task_prompt));
    Cmd.v
      (Cmd.info "rm" ~doc:"Remove a task.")
      (with_log Term.(const (task_one Task.rm "removed") $ task_id));
    Cmd.v
      (Cmd.info "enable" ~doc:"Let a disabled task fire again.")
      (with_log Term.(const (task_one Task.enable "enabled") $ task_id));
    Cmd.v
      (Cmd.info "disable"
         ~doc:"Stop a task firing, leaving it in the file to be read.")
      (with_log Term.(const (task_one Task.disable "disabled") $ task_id));
    Cmd.v
      (Cmd.info "run" ~doc:"Ask for a task to fire now.")
      (with_log Term.(const task_run $ task_id));
  ]

let task_cmd =
  let doc = "Read and write the schedule, which is what asks for work." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The schedule at $(b,\\$XDG_CONFIG_HOME/numpty/schedule.json) is the \
         only place work is asked for. These commands edit it, and the daemon \
         only reads it, so the orders stay a thing you can read, diff and keep \
         in git, and nothing numpty concludes can change what it was told to \
         do.";
      `P
        "No daemon is needed. An edit is written to a sibling temporary file \
         and renamed over the original, so a running numpty never stats a \
         half-written file, and it picks the change up on its next tick. A \
         change made while numpty is stopped is picked up at startup.";
      `P
        "A task has an id, a prompt and one trigger: $(b,--every D), $(b,--at \
         HH:MM) narrowed by $(b,--days), or $(b,--once).";
    ]
  in
  Cmd.group (Cmd.info "task" ~doc ~man) task_cmds

(* ---- log command ---- *)

let since =
  Arg.(
    value
    & opt (some string) None
    & info [ "since" ] ~docv:"T"
        ~doc:
          "Show only what was written at or after $(i,T), which is a timestamp \
           such as $(b,2026-08-08T09:14:07Z), a date such as $(b,2026-08-08), \
           which is midnight UTC on it, or how far back to go, such as $(b,2h) \
           or $(b,7d).")

let kinds =
  Arg.(
    value & opt_all string []
    & info [ "kind" ] ~docv:"K"
        ~doc:
          "Show only records of this kind, such as $(b,tool_call) or \
           $(b,content). Repeat the option for each kind. A name that is not a \
           kind is refused with the kinds there are, since it would otherwise \
           match nothing and read as an empty journal.")

let task_filter =
  Arg.(
    value
    & opt (some string) None
    & info [ "task" ] ~docv:"ID"
        ~doc:
          "Show only the wake-ups of this task, being the records from each of \
           its $(b,wake) records up to the next wake.")

let run_filter =
  Arg.(
    value
    & opt (some int) None
    & info [ "run" ] ~docv:"N" ~doc:"Show only records written by run $(i,N).")

let log_cmd =
  let doc = "Print the journal, which says everything a run did." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "One line per record, oldest first: its sequence number, the time in \
         UTC, the run that wrote it, its kind, and a summary. Long text is cut \
         to one line, and the file itself, one JSON object per line under \
         $(b,journal/) in the store, is where the whole of it is.";
      `P
        "It needs no daemon. Every record was on disk before the thing it \
         describes was allowed to happen, so this works while numpty runs, \
         while it is stopped, and on a store copied off the machine.";
    ]
  in
  Cmd.v (Cmd.info "log" ~doc ~man)
    (with_log
       Term.(const print_log $ store $ since $ kinds $ task_filter $ run_filter))

(* ---- memory command ---- *)

let at_version =
  Arg.(
    value
    & opt (some int) None
    & info [ "at" ] ~docv:"V"
        ~doc:
          "Show memory as it was at version $(i,V). A version is written once \
           and never rewritten, so later versions do not change what it says.")

let version_pos n name =
  Arg.(
    required
    & pos n (some int) None
    & info [] ~docv:name ~doc:("The " ^ name ^ " version."))

let memory_cmd =
  let doc = "Print what the agent knows between one wake-up and the next." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Memory is a set of entries, and a version is a complete snapshot of \
         that set, written once and never rewritten. $(b,show) prints the \
         version in force, or the one $(b,--at) names. $(b,history) prints one \
         line per version. $(b,diff) prints what changed between two.";
      `P
        "Nothing prunes, so every version a run ever wrote is still there. It \
         needs no daemon.";
    ]
  in
  let show = with_log Term.(const memory_show $ store $ at_version) in
  Cmd.group ~default:show
    (Cmd.info "memory" ~doc ~man)
    [
      Cmd.v
        (Cmd.info "show" ~doc:"Print the whole of memory at one version.")
        show;
      Cmd.v
        (Cmd.info "history" ~doc:"Print one line per version, oldest first.")
        (with_log Term.(const memory_history $ store));
      Cmd.v
        (Cmd.info "diff" ~doc:"Print what changed between two versions.")
        (with_log
           Term.(
             const memory_diff $ store $ version_pos 0 "V" $ version_pos 1 "W"));
    ]

(* ---- status, jobs and follow commands ---- *)

let socket_man =
  `P
    "It asks the running daemon over the unix socket at $(b,control) under the \
     store, which answers only what the files cannot: what is running now. \
     Everything else is on disk before the daemon proceeds past it, so \
     $(b,numpty log) and $(b,numpty memory) read the store directly and need \
     no daemon. With nothing listening this says so and prints what the \
     journal says the last run was doing when it stopped."

let status_cmd =
  let doc = "Say what the running daemon is doing." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Which run, since when, which model on which backend, whether the \
         network child is alive or which fault killed it, the memory version \
         in force, and the id of the running job if there is one.";
      socket_man;
    ]
  in
  Cmd.v (Cmd.info "status" ~doc ~man) (with_log Term.(const status_cmd $ store))

let jobs_cmd =
  let doc = "Show the running job and when each task fires next." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "One job runs at a time, since a process holds one engine. It reports \
         its task, when it started, which session it is on if it has handed \
         over and continued, how full its context is, its turn and tool-call \
         counts, the tool call in flight and, while a prefill is running, how \
         far that has got.";
      `P
        "A status is as fresh as the last turn boundary, apart from the \
         prefill counter, which is live. A turn that runs for four minutes \
         reports the tool call it is in and the context it had when the turn \
         began, since reading anything more current would mean entering the \
         engine the turn is inside.";
      socket_man;
    ]
  in
  Cmd.v (Cmd.info "jobs" ~doc ~man) (with_log Term.(const jobs_cmd $ store))

let follow_cmd =
  let doc = "Print journal records as the daemon writes them." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "One line per record, as $(b,numpty log) prints them, streamed as they \
         are appended. Narrow it with $(b,--kind), repeated for each kind you \
         want.";
      socket_man;
    ]
  in
  Cmd.v
    (Cmd.info "follow" ~doc ~man)
    (with_log Term.(const follow_cmd $ store $ kinds))

let netd_cmd =
  let doc = "Reach the network for a numpty over a pipe. Internal." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Internal. It is spawned by the numpty daemon before the model is \
         loaded, and it speaks a protocol over its standard input and output. \
         It holds every program numpty runs, so that a process with a model in \
         it never forks. Not for direct use.";
    ]
  in
  Cmd.v (Cmd.info "netd" ~doc ~man) (with_log Term.(const netd $ const ()))

(* ---- top-level group ---------------------------------------------------- *)

let () =
  let doc = "Run a local model as an agent that works unattended." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "numpty runs a local DS4 or Apple Foundation Models agent without \
         anybody watching. It wakes on a schedule, works, writes what should \
         survive into a durable memory, and records every step in a journal \
         you read afterwards.";
      `P
        (Printf.sprintf
           "DS4 models use the %s compute backend in this build. On a \
            supported Mac, --model apple/default uses Apple Foundation Models."
           Cli.backend_name);
    ]
  in
  let info = Cmd.info "numpty" ~version ~doc ~man in
  exit
    (Cmd.eval_result
       (Cmd.group info
          [
            run_cmd;
            once_cmd;
            status_cmd;
            jobs_cmd;
            follow_cmd;
            task_cmd;
            log_cmd;
            memory_cmd;
            Agentkit_model_support.command ();
            netd_cmd;
          ]))
