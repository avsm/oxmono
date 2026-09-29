(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* humpty: a command line over the DS4 inference engine.

   The subcommands are meant to be read in order.
     download  fetch a model onto this machine
     chat      send one prompt and print the reply
     agent     run that model in a tool-using loop
     expect    run those tools, or that loop, from a script
     list      show which models are available

   Models live under $XDG_DATA_HOME/ds4. *)

module V4 = Ds4.V4
module Agent = Ds4.Agent
module Tool = Ds4.Tool
module Toolbox = Ds4.Toolbox
module Cli = Ds4_cli.Cli
module Journal = Agentkit.Journal
module Model = Ds4_cli.Model
module Schedule = Agentkit.Schedule
module Script = Humpty_cmd.Expect
module Driver = Agentkit.Driver
module Event = Agentkit.Agent
module Apple_support = Agentkit_apple_support

let version = "0.1"
let model_dir = Model.dir
let run = Cli.run

(* ---- agent ------------------------------------------------------------- *)

(* Name the model by the target it came from, which says which build it is,
   rather than by the name inside the file, which does not. A superseded build
   is called out, since the alias now points at a newer one. *)
let describe_build path =
  match Model.of_path path with
  | Some m when m.deprecated -> m.name ^ " [deprecated]"
  | Some m -> m.name
  | None -> Filename.basename path

(* The note the interface shows, whose wording is [Okit.Status]'s: okitd sends
   that same line in its greeting, so the note is shown rather than composed.
   A workspace okit would not serve is reported here alone, since the interface
   takes the terminal and a line written above it goes unread.

   A refusal is flattened onto one line, which a note has and a transcript
   records. Its wrapping is Eio's rather than the message's, so every sentence
   is kept: the one that says a socket cannot be reached is followed by the one
   that says what to do about it. What [Script.one_line] does drop is the last
   output of the dune that would not serve, which is that program speaking
   rather than okit and names a pid. [full] keeps the whole of it, for a reader
   who asked to see what was said. *)
let okit_note ?(full = false) status =
  let flatten = if full then Fun.id else Script.one_line in
  match status with
  | Okit.Toolset.Unstarted e ->
      Okit.Status.note ~flatten (Okit.Status.Refused e)
  | Okit.Toolset.Serving { hello; _ } -> flatten hello.Okit.Proto.status

(* okitd is this same binary under another subcommand, so the argv names humpty
   and the workspace it is to serve. [humpty expect] assembles its tools through
   here too, so that a transcript exercises the list the interface is given
   rather than one written beside it. *)
let assemble ~sw ~proc ~net ~clock ~caps ~trace ~root =
  let ws = Option.value ~default:"." (Eio.Path.native root) in
  Okit.Toolset.assemble ~vision:false ~sw ~proc ~net ~clock ~caps ~trace
    ~argv:[ Cli.self (); "okitd"; "--dir"; ws ]
    ~root

let agent model_choice vision workspace system think seed ctx_size max_ctx =
  run @@ fun env xdg ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let net = Eio.Stdenv.net env in
  let vision = Option.map (fun path -> Eio.Path.(fs / path)) vision in
  let cache = Xdge.cache_dir xdg in
  (* Confine the filesystem tools to [workspace]. They hold only this
     capability, and Eio refuses ".." and symlinks that leave the subtree, so no
     path the model supplies can reach outside it. *)
  Eio.Path.with_subtree Eio.Path.(fs / workspace) @@ fun ws ->
  (* The starting capability, and the only one until the model asks for more.
     Anything outside it has to be granted by [approve]. *)
  (* The queue the interface shows as notes. The terminal belongs to the
     interface from here, so anything worth a person's attention is written as a
     line and queued rather than printed. The decision to allow a directory is
     one such line. Returning true accepts everything for now, and this is where
     a person would be asked. *)
  let notes = Queue.create () in
  let approve path =
    Queue.add (Printf.sprintf "⚿ allowed %s" path) notes;
    true
  in
  (* The step okit is on, which the interface shows and then forgets. A tool
     that hangs is the reason this exists, so the lines are queued from the
     fiber the tool runs in and drained by every message the interface takes,
     the tick among them.

     The session is started before the interface exists, and starting it is
     where the waiting is: ten seconds for a socket, then a handshake with
     nothing bounding it. A queue alone would hold those lines until the wait
     was over and the answer already known, so until the interface takes the
     terminal each line also goes to standard error as it happens, where the
     warning about a session that could not start already goes. *)
  let trace = Queue.create () in
  let on_screen = ref false in
  let trace_line s =
    if not !on_screen then prerr_endline s;
    Queue.add s trace
  in
  (* The tools are assembled before the engine exists, because assembling them
     spawns okitd, and a spawn is a fork of this process. Forking one that has a
     model mapped into it costs minutes on macOS and blocks the domain that
     asked, which is this one. okitd is the only process humpty ever spawns, and
     it is spawned here, while this one is still small. Everything okit runs
     from then on, the dune server and every merlin among it, is forked by okitd
     instead.

     A failure after this point unwinds [sw], whose release stops okitd and the
     dune server it started, so an engine that will not load leaves nothing
     behind. *)
  let argv =
    [
      Cli.self ();
      "okitd";
      "--dir";
      Option.value ~default:"." (Eio.Path.native ws);
    ]
  in
  let ds4 =
    Driver.v ~name:"ds4"
      ~models:(Agentkit_ds4.models ~dir:(model_dir xdg))
      ~create:(fun model ->
        (* Model resolution precedes the okitd spawn. *)
        let path = Agentkit_ds4.model_path ~dir:(model_dir xdg) model in
        let setup =
          Okit.Toolset.create_agent ~sw ~proc ~net ~clock:(Eio.Stdenv.clock env)
            ~domain_mgr:(Eio.Stdenv.domain_mgr env)
            ~fs ~cache
            ~model:Eio.Path.(fs / path)
            ~vision ~root:ws ~approve ~trace:trace_line ~argv ~system
            ~thinking:think ~ctx_size ~max_ctx_size:max_ctx
            ~seed:(Cli.resolve_seed seed)
        in
        let session =
          Driver.session
            ~prefill_progress:(fun () -> Agent.prefill_progress setup.agent)
            (module Agentkit_ds4.Agent)
            setup.agent
        in
        ( session,
          setup.status,
          setup.instructions,
          path,
          Cli.backend_name,
          ctx_size,
          describe_build path ))
  in
  let apple =
    if not Apple_support.available then []
    else
      [
        Driver.v ~name:"apple" ~models:Apple_support.models
          ~create:(fun model ->
            if model <> "default" then
              failwith "Apple model must be apple/default";
            if Option.is_some vision then
              failwith "--vision is supported only with a DS4 model";
            if
              think <> Dsml.Chat || seed <> 0 || ctx_size <> 32768
              || max_ctx <> 262144
            then
              failwith
                "--think, --seed, --ctx and --max-ctx are DS4 options and \
                 cannot be set for apple/default";
            let context_size = Apple_support.context_size model in
            let caps = Toolbox.Caps.create ~sw ~fs ~approve ws in
            let tools, okit, status =
              Okit.Toolset.assemble ~vision:false ~sw ~proc ~net
                ~clock:(Eio.Stdenv.clock env) ~caps ~trace:trace_line ~argv
                ~root:ws
            in
            let instructions = Option.is_some (Agentkit.Instructions.load ws) in
            let system = Okit.Toolset.system_prompt ~base:system ~okit ws in
            let session, _ = Apple_support.create ~sw ~model ~system tools in
            ( session,
              status,
              instructions,
              "apple/" ^ model,
              "Apple Foundation Models",
              context_size,
              "Apple system model" ));
      ]
  in
  let ( session,
        status,
        instructions,
        model_path,
        backend,
        context_size,
        title_model ) =
    match Driver.create (Driver.merge (ds4 :: apple)) model_choice with
    | Ok setup -> setup
    | Error message -> failwith message
  in
  Queue.add (okit_note status) notes;
  (* The conversation is recorded into a journal under the state directory,
     one record per line, append only. The run number and sequence continue
     from whatever the last session left, so a journal read after any number
     of sessions is a complete account of every exchange in order. *)
  let journal_dir = Eio.Path.(Xdge.state_dir xdg / "journal") in
  let journal = Journal.open_ ~sw ~clock:(Eio.Stdenv.clock env) journal_dir in
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
  (* The interface takes over the terminal from here, so the session is
     described in its frame rather than printed above it. *)
  (* A title longer than its box is dropped rather than truncated, so this
     stays short. What is static about the session is said in the greeting. *)
  let title = Printf.sprintf " humpty %s · %s " version title_model in
  (* The header has one line, so it says only what changes between sessions.
     The greeting carries the longer form. *)
  let about =
    Printf.sprintf "%s · %s%s" backend workspace
      (if instructions then " · AGENTS.md" else "")
  in
  (* From here the terminal is the interface's, so a trace line has only the
     queue to reach a person by. *)
  on_screen := true;
  Tui.run ~env ~sw ~title ~about ~max_ctx ~ctx_size:context_size ~notes ~trace
    ~journal session;
  (* The interface has given the terminal back, so this is the one place the
     session can say where its account went. *)
  Printf.printf "This session is recorded at %s. 'humpty log' reads it.\n"
    (Option.value ~default:"?" (Eio.Path.native journal_dir))

(* ---- expect ------------------------------------------------------------ *)

(* The status line, which differs from the note the interface shows in one
   case: a workspace with no dune-project had no session attempted, and a
   transcript says so in the fewest words that still name the reason. That is
   the case [Serving] leaves unrefused while the dune tools are absent. *)
let okit_status_line ~full = function
  | Okit.Toolset.Serving { hello; refused = None }
    when not hello.Okit.Proto.dune ->
      "okit: off (no dune-project)"
  | status -> okit_note ~full status

(* A script is a few lines, so the limit is only there to bound a mistake such
   as a model file on standard input. *)
let max_script = 1_000_000

let script_text env = function
  | None | Some "-" ->
      Eio.Buf_read.(parse_exn take_all)
        ~max_size:max_script (Eio.Stdenv.stdin env)
  | Some path -> (
      try Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
      with Eio.Exn.Io _ as e ->
        (* On one line, since an Eio error is written over several and this one
           is reported beside the rest of the command's diagnostics. *)
        failwith
          (Printf.sprintf "cannot read %s: %s" path
             (Script.one_line (Printexc.to_string e))))

let expect model_choice workspace system think seed ctx_size max_ctx timeout
    prompt_timeout raw script =
  run @@ fun env xdg ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  (* Read the whole script before anything is started, so that a script with a
     bad line costs no dune server. *)
  let items =
    match Script.parse (script_text env script) with
    | Ok items -> items
    | Error e -> failwith e
  in
  let has_prompt =
    List.exists
      (function Script.Prompt _ -> true | Script.Call _ -> false)
      items
  in
  (* A prompt drives the model, so the model is resolved before anything is
     started, as [agent] resolves it. A script with no prompt resolves nothing,
     so it runs on a machine that has no model. A model that is not there is
     refused here rather than by the engine, whose refusal would come after
     okitd and a dune server had already been started. *)
  let plan =
    if not has_prompt then None
    else
      let ds4 =
        Driver.v ~name:"ds4"
          ~models:(Agentkit_ds4.models ~dir:(model_dir xdg))
          ~create:(fun model ->
            let path = Agentkit_ds4.model_path ~dir:(model_dir xdg) model in
            fun tools system ->
              let engine =
                V4.create ~sw
                  ~domain_mgr:(Eio.Stdenv.domain_mgr env)
                  ~cache:(Xdge.cache_dir xdg)
                  ~model:Eio.Path.(fs / path)
                  ()
              in
              let agent =
                Agent.create engine ~system ~thinking:think ~ctx_size
                  ~max_ctx_size:max_ctx ~seed:(Cli.resolve_seed seed)
                  ~now:(fun () -> Eio.Time.now clock)
                  ~tools
              in
              Driver.session (module Agentkit_ds4.Agent) agent)
      in
      let apple =
        if not Apple_support.available then []
        else
          [
            Driver.v ~name:"apple" ~models:Apple_support.models
              ~create:(fun model ->
                if model <> "default" then
                  failwith "Apple model must be apple/default";
                if
                  think <> Dsml.Chat || seed <> 1 || ctx_size <> 32768
                  || max_ctx <> 262144
                then
                  failwith
                    "--think, --seed, --ctx and --max-ctx are DS4 options and \
                     cannot be set for apple/default";
                ignore (Apple_support.context_size model);
                fun tools system ->
                  fst (Apple_support.create ~sw ~model ~system tools));
          ]
      in
      Some
        (match Driver.create (Driver.merge (ds4 :: apple)) model_choice with
        | Ok plan -> plan
        | Error message -> failwith message)
  in
  Eio.Path.with_subtree Eio.Path.(fs / workspace) @@ fun ws ->
  (* A workspace is reached by two names on a machine where /tmp is a link, and
     tools report both: Eio uses the name it was given, and dune and merlin
     resolve it. *)
  let roots =
    Script.roots
      [
        Option.value ~default:"" (Eio.Path.native ws);
        (try Unix.realpath workspace with Unix.Unix_error _ -> "");
      ]
  in
  let scrub s = if raw then s else Script.scrub ~roots s in
  (* Every line of the transcript goes through here, and is flushed, so that a
     run that is later killed has said everything it had done. *)
  let emit s =
    print_string s;
    print_newline ();
    flush stdout
  in
  let line s = emit (scrub s) in
  (* The trace of the session's own start is not shown. It varies with how long
     dune takes to open its socket, which is what the status line reports the
     outcome of, and a transcript that gains a line per second of waiting is no
     use in a cram test. *)
  let running = ref false in
  (* The brackets go on after the scrubber has seen the line, so that a trace
     line ending in a duration still ends in one when it is dropped. *)
  let trace s = if !running then emit ("[" ^ scrub s ^ "]") in
  (* No approval callback, so a directory outside the workspace is granted when
     it is asked for, as it is for the agent. *)
  let caps = Toolbox.Caps.create ~sw ~fs ws in
  let tools, okit_system, status =
    assemble ~sw ~proc ~net ~clock ~caps ~trace ~root:ws
  in
  line (okit_status_line ~full:raw status);
  let find name = List.find_opt (fun t -> Tool.name t = name) tools in
  (* Checked for the whole script before any of it runs, since a name that no
     tool answers to is a fault in the script rather than a result to record. *)
  List.iter
    (function
      | Script.Prompt _ -> ()
      | Script.Call (c : Script.call) ->
          if find c.tool = None then
            failwith
              (Printf.sprintf
                 "line %d: no tool named %s. This workspace has: %s" c.line
                 c.tool
                 (String.concat ", " (List.map Tool.name tools))))
    items;
  (* The engine after [assemble], in the order [agent] keeps, so that okitd is
     spawned while this process is still small. It runs on its own domain, so
     that the fiber bounding a prompt keeps running during generation. The
     system prompt is assembled as [agent] assembles it, so a transcript
     exercises the conversation the interface would hold. *)
  let agent =
    match plan with
    | None -> None
    | Some start ->
        let system =
          Okit.Toolset.system_prompt ~base:system ~okit:okit_system ws
        in
        Some (start tools system)
  in
  running := true;
  (* Verbatim, except that the trailing newlines a block ends with become one,
     so a result is one block whatever the tool does about it. *)
  let chomp s =
    let n = ref (String.length s) in
    while !n > 0 && s.[!n - 1] = '\n' do
      decr n
    done;
    String.sub s 0 !n
  in
  let block s =
    let s = chomp s in
    if s <> "" then line s
  in
  (* One bound for a scripted call and another for a whole prompt exchange.
     Expiry ends the run by leaving the process rather than by returning to the
     script, since unwinding the switch would wait on the work that is already
     too slow. okitd is left with any call it is inside, and reads the end of
     its standard input as soon as that call is answered, so it stops itself and
     the dune server it started. Nothing later in the script could have been
     served anyway, and a run that times out is one to look at by hand. *)
  let bound ~seconds ~what f =
    Eio.Fiber.first f (fun () ->
        Eio.Time.sleep clock (float_of_int seconds);
        (* Not through the scrubber, since the seconds here are the ones that
           were asked for rather than ones that were measured. *)
        emit (Printf.sprintf "= timeout after %ds" seconds);
        prerr_endline
          (Printf.sprintf
             "humpty: %s did not answer within %d seconds, so the rest of the \
              script was not run"
             what seconds);
        exit 1)
  in
  let run_call (c : Script.call) =
    let tool = Option.get (find c.tool) in
    line (Printf.sprintf "> %s %s" c.tool c.arguments);
    block
      (bound ~seconds:timeout
         ~what:(Printf.sprintf "the %s call on line %d" c.tool c.line)
         (fun () ->
           Tool.invoke tool
             { Dsml.name = c.tool; arguments = c.arguments; id = None }))
  in
  (* The exchange prints as it happens: the model's tool calls echo as a
     scripted call does, and the reply is held back and printed whole under a
     [= reply] line when the exchange ends. Reasoning, per-turn stats and
     context growth are dropped unless [--raw], each being either unstable
     between runs or timing coloured.

     A turn that ends in a tool call may say something first. That text is
     printed as a block above the call it accompanies, and the buffer starts
     again. So [= reply] introduces the text of the final turn alone, which is
     the reply, rather than every turn's text run together. *)
  let run_prompt (p : Script.prompt) =
    let agent = Option.get agent in
    line (Printf.sprintf "? %s" p.text);
    let reply = Buffer.create 256 and reasoning = Buffer.create 256 in
    let on_event = function
      | Event.Tool_call tc ->
          if Buffer.length reply > 0 then begin
            block (Buffer.contents reply);
            Buffer.clear reply
          end;
          line (Printf.sprintf "> %s %s" tc.Event.name tc.Event.arguments)
      | Event.Tool_result (_, result) -> block result
      | Event.Content c -> Buffer.add_string reply c
      | Event.Reasoning r -> if raw then Buffer.add_string reasoning r
      | Event.Stats s ->
          if raw then
            emit
              (Printf.sprintf "= stats turns %d tools %d ctx %d/%d"
                 s.Event.turns s.Event.tool_calls s.Event.ctx_used
                 s.Event.ctx_size)
      | Event.Expanded n ->
          if raw then emit (Printf.sprintf "= context expanded to %d" n)
      (* Reported whatever the mode, since the reply that follows is cut off
         and the transcript would otherwise not say why. *)
      | Event.Squeezed n ->
          emit (Printf.sprintf "= context full, %d tokens to reply in" n)
      (* Reported for the same reason: a transcript that shows neither the call
         nor why it is missing reads as a model that chose not to make it. *)
      | Event.Cut_off c ->
          emit
            (Printf.sprintf "= reply cut off at %d tokens%s" c.Event.tokens
               (if c.Event.tool_call then ", tool call discarded" else ""))
      (* Reported for the same reason: the conversation from here on is not
         the one the earlier lines of the transcript show. *)
      | Event.Compacted c ->
          emit
            (Printf.sprintf "= context compacted from %d to %d tokens"
               c.Event.before c.Event.after)
      | Event.Done ->
          if raw && Buffer.length reasoning > 0 then begin
            emit "= reasoning";
            block (Buffer.contents reasoning)
          end;
          emit "= reply";
          block (Buffer.contents reply)
    in
    bound ~seconds:prompt_timeout
      ~what:(Printf.sprintf "the prompt on line %d" p.line) (fun () ->
        Driver.send agent ~on_event p.text)
  in
  List.iter
    (function Script.Call c -> run_call c | Script.Prompt p -> run_prompt p)
    items

(* ---- log --------------------------------------------------------------- *)

(* The rendering and the filters are Agentkit.Show's, so a record reads the
   same here as under 'numpty log' or the agentkit browser. *)
let log since kinds run_opt =
  run @@ fun env xdg ->
  let journal_dir = Eio.Path.(Xdge.state_dir xdg / "journal") in
  let since =
    match since with
    | None -> None
    | Some s ->
        Some (Result.fold ~ok:Fun.id ~error:failwith (Agentkit.Utc.of_since s))
  in
  let kinds = match kinds with [] -> None | k -> Some k in
  let printed = ref 0 in
  Agentkit.Show.log ~agent:"humpty" ?since ?kinds ?run:run_opt journal_dir
    (fun line ->
      incr printed;
      print_endline line);
  if !printed = 0 then
    if Journal.segments journal_dir = [] then
      Printf.printf
        "The journal at %s holds nothing. No session has written to it yet.\n"
        (Option.value ~default:"?" (Eio.Path.native journal_dir))
    else
      Printf.printf "No record in the journal at %s matches those filters.\n"
        (Option.value ~default:"?" (Eio.Path.native journal_dir))

(* ---- okitd ------------------------------------------------------------- *)

(* okit in a process of its own, spawned by [agent] and [expect] and speaking
   the protocol over the pipes they gave it. It holds the dune session, the
   merlin and every process okit runs, so that a humpty which has loaded a
   model forks nothing. *)
let okitd workspace =
  run @@ fun env _xdg ->
  Okit.Server.run ~dir:workspace ~stdin:(Eio.Stdenv.stdin env)
    ~stdout:(Eio.Stdenv.stdout env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
    ~fs:(Eio.Stdenv.fs env) ()

(* ---- cmdliner ---------------------------------------------------------- *)

open Cmdliner

(* Threaded, because the engine runs on a domain of its own and logs from
   there. *)
let with_log t = Cli.with_logs ~threaded:true t
let model = Driver.model_term ~default:"ds4/auto" ~short_driver:"ds4" ()

(* How far the context may grow before the agent gives up rather than moving
   to a larger one. *)
let agent_max_ctx =
  Arg.(
    value & opt int 262144
    & info [ "max-ctx" ] ~docv:"N"
        ~doc:
          "The largest context the conversation may grow to. Reaching it ends \
           the exchange rather than growing further.")

let agent_ctx =
  Arg.(
    value & opt int 32768
    & info [ "ctx" ] ~docv:"N"
        ~doc:
          "The context window, in tokens. It must hold the whole conversation, \
           including tool output, so a long session needs a large one. Memory \
           use grows with it.")

(* ---- agent command ---- *)

let workspace =
  Arg.(
    value & opt string "."
    & info [ "d"; "dir" ] ~docv:"DIR"
        ~doc:"The workspace directory the agent's file tools are confined to.")

let vision =
  Arg.(
    value
    & opt (some string) None
    & info [ "vision" ] ~docv:"FILE"
        ~doc:
          "Load the vision sidecar that matches the model and enable the \
           capability-confined view_image tool. GLM 5.3, DeepSeek V4 Vision \
           Experimental, DeepSeek V4.1 Flash and Qwen3.8 Flash Next each have \
           one of their own.")

let agent_system =
  Arg.(
    value
    & opt string Okit.Toolset.default_system
    & info [ "s"; "system" ] ~docv:"TEXT"
        ~doc:"The system prompt that sets the agent's role and behaviour.")

let agent_cmd =
  let doc =
    "Run an interactive agent that uses tools to act on your requests."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "An agent is a model placed in a loop and given tools. This starts an \
         interactive session in which the model can list, read, search and \
         write files and resolve host names to carry out your requests over \
         several turns.";
      `P
        "The file tools are confined to the directory given by $(b,--dir) and \
         cannot reach outside it.";
      `P
        "Type a request at the prompt and press Enter. Press Ctrl-D to exit. A \
         line after each reply reports how full the context is and how fast \
         the model ran.";
    ]
  in
  let term =
    with_log
      Term.(
        const agent $ model $ vision $ workspace $ agent_system $ Cli.think
        $ Cli.seed $ agent_ctx $ agent_max_ctx)
  in
  Cmd.v (Cmd.info "agent" ~doc ~man) term

(* ---- expect command ---- *)

let expect_timeout =
  Arg.(
    value & opt int 60
    & info [ "timeout" ] ~docv:"SECS"
        ~doc:
          "How long one scripted tool call may take. A call that takes longer \
           ends the run, since a session that has stopped answering makes the \
           rest of the script meaningless. A tool call the model makes is not \
           bounded by this, but by $(b,--prompt-timeout), which covers the \
           whole exchange the call is part of. The run ends by leaving the \
           process, which closes the pipe to okit's server: that server stops \
           itself, and the dune server it started, as soon as the call it is \
           inside is answered. One that never answers is left for you to find \
           and stop.")

let expect_seed =
  Arg.(
    value & opt int 1
    & info [ "seed" ] ~docv:"N"
        ~doc:
          "The seed for the sampler when a prompt loads the model. It defaults \
           to a fixed value rather than to a fresh one, so that two runs of \
           one script sample identically.")

let expect_prompt_timeout =
  Arg.(
    value & opt int 600
    & info [ "prompt-timeout" ] ~docv:"SECS"
        ~doc:
          "How long one prompt's whole exchange may take, generation and tool \
           calls together. A prompt that takes longer ends the run, as a tool \
           call that exceeds $(b,--timeout) does.")

let expect_raw =
  Arg.(
    value & flag
    & info [ "raw" ]
        ~doc:
          "Print what the tools said, with absolute paths and durations left \
           in. For reading by eye rather than for a test.")

let expect_script =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"SCRIPT"
        ~doc:
          "The script to run. Standard input is read when it is absent or is \
           $(b,-).")

let expect_cmd =
  let doc =
    "Run the agent's tools, or the agent itself, from a script and print the \
     transcript."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Assemble the tools $(b,humpty agent) would give a model for this \
         workspace, run the calls and the prompts a script names, and print \
         what each one returned. A model is loaded only when the script has a \
         prompt.";
      `P
        "A script line is blank, a $(b,#) comment, a tool call, or a prompt. A \
         tool call is the tool's name, one space, and its arguments as the \
         JSON object a model would send, as in $(b,build {}). A prompt is \
         $(b,?) followed by the text to send, as in $(b,? describe this \
         repository).";
      `P
        "A script with no prompt loads no model, and a run takes as long as \
         the tools do. A script with a prompt resolves and loads the model \
         first, then runs each prompt through the same agent loop $(b,humpty \
         agent) runs: the model's tool calls print as scripted calls do, and \
         its reply prints under a $(b,= reply) line. All prompts share one \
         conversation. The seed defaults to a fixed value, so two runs of one \
         script sample identically on one machine.";
      `P
        "The transcript opens with a line saying whether the dune tools are \
         there and why. Each call then prints as a $(b,>) line echoing it, \
         okit's trace in brackets as it happens, and the tool's result. A tool \
         error is part of the transcript rather than a failure of the run. \
         Text a turn says before its tool call prints as a bare block above \
         that call's $(b,>) line, so $(b,= reply) introduces the final turn's \
         text alone. A run that expired ends with a $(b,= timeout after Ns) \
         line, and standard error names what did not answer.";
      `P
        "The workspace's path is printed as $(b,\\$WS) and durations are \
         dropped, so that two runs of one script give the same text. \
         $(b,--raw) turns that off.";
      `P
        "The exit status is zero when every item ran, and nonzero for a script \
         that could not be read, a call naming a tool this workspace has not \
         got, a prompt whose model is not on this machine, or a call or prompt \
         that timed out.";
    ]
  in
  let term =
    with_log
      Term.(
        const expect $ model $ workspace $ agent_system $ Cli.think
        $ expect_seed $ agent_ctx $ agent_max_ctx $ expect_timeout
        $ expect_prompt_timeout $ expect_raw $ expect_script)
  in
  Cmd.v (Cmd.info "expect" ~doc ~man) term

(* ---- log command ---- *)

let log_since =
  Arg.(
    value
    & opt (some string) None
    & info [ "since" ] ~docv:"T"
        ~doc:
          "Show only what was written at or after $(i,T), which is a timestamp \
           such as $(b,2026-08-08T09:14:07Z), a date such as $(b,2026-08-08), \
           which is midnight UTC on it, or how far back to go, such as $(b,2h) \
           or $(b,7d).")

let log_kinds =
  Arg.(
    value & opt_all string []
    & info [ "kind" ] ~docv:"K"
        ~doc:
          "Show only records of this kind, such as $(b,tool_call) or \
           $(b,content). Repeat the option for each kind. A name that is not a \
           kind is refused with the kinds there are.")

let log_run =
  Arg.(
    value
    & opt (some int) None
    & info [ "run" ] ~docv:"N" ~doc:"Show only records written by run $(i,N).")

let log_cmd =
  let doc = "Print the journal, which records every conversation." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "One line per record, oldest first: its sequence number, the time in \
         UTC, the run that wrote it, its kind, and a summary. Long text is cut \
         to one line, and the file itself, one JSON object per line under \
         $(b,journal/) in the state directory, is where the whole of it is.";
      `P
        "Every prompt, reasoning, reply, tool call and result of a $(b,humpty \
         agent) session is recorded here, so this is how to read a session \
         back after it has ended.";
    ]
  in
  let term = with_log Term.(const log $ log_since $ log_kinds $ log_run) in
  Cmd.v (Cmd.info "log" ~doc ~man) term

(* ---- okitd command ---- *)

let okitd_cmd =
  let doc = "Serve okit's tools over a pipe. Internal." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Internal. It is spawned by $(b,humpty agent) and $(b,humpty expect) \
         to hold the dune session, merlin and every process okit runs, and it \
         speaks a protocol over its standard input and output. Not for direct \
         use.";
    ]
  in
  Cmd.v (Cmd.info "okitd" ~doc ~man) (with_log Term.(const okitd $ workspace))

let list_models () =
  run @@ fun _env xdg ->
  let ds4 =
    Driver.v ~name:"ds4"
      ~models:(Agentkit_ds4.models ~dir:(model_dir xdg))
      ~create:(fun _ -> ())
  in
  let apple =
    if Apple_support.available then
      [
        Driver.v ~name:"apple" ~models:Apple_support.models ~create:(fun _ ->
            ());
      ]
    else []
  in
  Driver.models (Driver.merge (ds4 :: apple))
  |> List.iter (fun (model : Driver.model) ->
      Printf.printf "%-42s %s\n" model.name model.description)

let list_cmd =
  Cmd.v
    (Cmd.info "list" ~doc:"List models available from linked drivers.")
    (with_log Term.(const list_models $ const ()))

(* ---- top-level group ---- *)

let () =
  let doc = "Run a local model and learn how an agent is built." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "humpty runs a local DS4 or Apple Foundation Models agent and shows \
         how an agent is assembled from simpler parts.";
      `P
        "Work through the subcommands in order. $(b,models list) shows the \
         available models. $(b,models fetch) explicitly downloads a DS4 model \
         onto this machine. $(b,chat) sends it a single prompt and prints the \
         reply. $(b,agent) puts a model in a loop with tools, so that it can \
         act on your requests over several turns. $(b,expect) runs those same \
         tools, and that same loop, from a script rather than from a person, \
         which is how they are tested. $(b,log) prints every conversation \
         $(b,agent) has recorded.";
      `P
        (Printf.sprintf
           "DS4 models use the %s compute backend in this build. On a \
            supported Mac, --model apple/default uses Apple Foundation Models."
           Cli.backend_name);
    ]
  in
  let info = Cmd.info "humpty" ~version ~doc ~man in
  exit
    (Cmd.eval_result
       (Cmd.group info
          [
            Cli.download_cmd;
            Cli.chat_cmd;
            agent_cmd;
            expect_cmd;
            log_cmd;
            list_cmd;
            Agentkit_model_support.command ();
            okitd_cmd;
          ]))
