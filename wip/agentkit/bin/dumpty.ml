(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* dumpty: one prompt through the agent loop, and then out.

   There is one command and one internal subcommand.
     dumpty PROMPT  run the exchange, stream the account, print the result
     okitd          internal, the child that holds the dune session

   Standard output carries one JSON object and nothing else, standard error
   carries the journal of the run as it happens, the same records are appended
   to a store under the XDG state directory so the account outlives the
   stream, and a run that printed nothing exited nonzero. *)

module V4 = Ds4.V4
module Agent = Ds4.Agent
module Event = Agentkit.Agent
module Toolbox = Ds4.Toolbox
module Cli = Ds4_cli.Cli
module Journal = Agentkit.Journal
module Line = Agentkit.Line
module Model = Ds4_cli.Model
module Trace = Agentkit.Trace
module Driver = Agentkit.Driver
module Apple_support = Agentkit_apple_support

let version = "0.1"

let run f =
  Cli.guard @@ fun () ->
  Eio_main.run @@ fun env -> f env (Xdge.create (Eio.Stdenv.fs env) "ds4")

(* ---- what standard output carries -------------------------------------- *)

(* The whole of the result, written once when the exchange ends. A consumer
   reads one line and needs no state, so nothing here is optional and nothing
   is written before the run is over. *)
type result = {
  reply : string;  (** the text of the final turn *)
  turns : int;
  tool_calls : int;
  ctx_used : int;
  ctx_size : int;
  squeezed : bool;  (** the reply ran in a context that could not grow *)
  cut_off : bool;  (** the reply stopped at the token ceiling, unfinished *)
  journal : string;  (** the directory the run's account was appended to *)
}

(* Scrubbed as the journal scrubs its strings. A reply carries whatever the
   model generated, and a JSON parser refuses a string that is not UTF-8, so a
   stray byte would otherwise cost the caller the whole object. *)
let string = Jsont.map ~dec:Fun.id ~enc:Line.utf_8 Jsont.string

let result_jsont =
  Jsont.Object.map ~kind:"result"
    (fun reply turns tool_calls ctx_used ctx_size squeezed cut_off journal ->
      {
        reply;
        turns;
        tool_calls;
        ctx_used;
        ctx_size;
        squeezed;
        cut_off;
        journal;
      })
  |> Jsont.Object.mem "reply" string ~enc:(fun r -> r.reply)
  |> Jsont.Object.mem "turns" Jsont.int ~enc:(fun r -> r.turns)
  |> Jsont.Object.mem "tool_calls" Jsont.int ~enc:(fun r -> r.tool_calls)
  |> Jsont.Object.mem "ctx_used" Jsont.int ~enc:(fun r -> r.ctx_used)
  |> Jsont.Object.mem "ctx_size" Jsont.int ~enc:(fun r -> r.ctx_size)
  |> Jsont.Object.mem "squeezed" Jsont.bool ~enc:(fun r -> r.squeezed)
  |> Jsont.Object.mem "cut_off" Jsont.bool ~enc:(fun r -> r.cut_off)
  |> Jsont.Object.mem "journal" string ~enc:(fun r -> r.journal)
  |> Jsont.Object.finish

let encode r =
  match Jsont_bytesrw.encode_string result_jsont r with
  | Ok s -> s
  | Error e -> failwith ("the result could not be encoded: " ^ e)

(* ---- what standard error carries ---------------------------------------- *)

(* The same records numpty appends to its store, appended to a store of
   dumpty's own under the XDG state directory and streamed as they are written.
   The store is why a run whose standard error nobody kept can still be read
   back, and the stream is why a watcher sees the run as it happens. One
   append stamps both, so the two accounts are the same account.

   The record and its newline go out in one write, since the engine logs from
   a domain of its own and a channel is locked per operation, so a second
   write here is a place a log line could land in the middle of a record. *)
let journal_dir fs =
  Eio.Path.(Xdge.state_dir (Xdge.create fs "dumpty") / "journal")

let stream ~sw ~clock dir =
  let journal = Journal.open_ ~sw ~clock dir in
  fun kind ->
    let r = Journal.append journal kind in
    output_string stderr (Journal.to_string r ^ "\n");
    flush stderr

(* Every run says how it ended, a run that ended in a failure among them, since
   a stream whose last record is the start reads as a run still going. *)
let stopping append f =
  match f () with
  | v -> v
  | exception e ->
      (try append (Journal.Run_stop (Printexc.to_string e)) with _ -> ());
      raise e

(* ---- the run ------------------------------------------------------------ *)

(* A prompt is a few lines, so the limit is only there to bound a mistake such
   as a model file on standard input. *)
let max_prompt = 1_000_000

let prompt_text env = function
  | "-" ->
      Eio.Buf_read.(parse_exn take_all)
        ~max_size:max_prompt (Eio.Stdenv.stdin env)
  | text -> text

(* One bound covers generation and tool calls. Cancelling the session lets an
   expiry unwind through the normal failure path and release every resource. *)
let bound ~clock ~cancel ~seconds f =
  if seconds = 0 then f ()
  else
    Eio.Fiber.first f (fun () ->
        Eio.Time.sleep clock (float_of_int seconds);
        cancel ();
        failwith (Printf.sprintf "timeout after %ds" seconds))

let one_shot model_choice workspace system think seed ctx_size max_ctx timeout
    text =
  run @@ fun env xdg ->
  Eio.Switch.run @@ fun sw ->
  (* Dumpty's standard error is a JSON-lines journal. The command-line setup
     installs engine log forwarding before this function runs, which prevents
     C diagnostics from writing around the runtime lock, but its usual reporter
     would put those diagnostics between journal records. Keep forwarding and
     discard the presentation-only messages for this command. Failures still
     become a [run_stop] through [stopping] below. *)
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter Logs.nop_reporter;
  V4.forward_logs ();
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let prompt = prompt_text env text in
  (* Resolved before anything is started, so that a model that is not there
     costs no dune server and no journal. A model that is not there is refused
     here, since the engine would report it and call [exit] where a run is
     already under way and no [run_stop] can be appended. *)
  let selected =
    let ds4 =
      Driver.v ~name:"ds4"
        ~models:(Agentkit_ds4.models ~dir:(Model.dir xdg))
        ~create:Fun.id
    in
    let apple =
      if Apple_support.available then
        [ Driver.v ~name:"apple" ~models:Apple_support.models ~create:Fun.id ]
      else []
    in
    match Driver.select (Driver.merge (ds4 :: apple)) model_choice with
    | Ok selected -> selected
    | Error message -> failwith message
  in
  let model_path, backend, context_size =
    match Driver.driver_name selected with
    | "ds4" ->
        ( Agentkit_ds4.model_path ~dir:(Model.dir xdg)
            (Driver.model_name selected),
          Cli.backend_name,
          ctx_size )
    | "apple" ->
        if
          think <> Dsml.Chat || seed <> 0 || ctx_size <> 32768
          || max_ctx <> 262144
        then
          failwith
            "--think, --seed, --ctx and --max-ctx are DS4 options and cannot \
             be set for apple/default";
        ( model_choice,
          "Apple Foundation Models",
          Apple_support.context_size (Driver.model_name selected) )
    | _ -> assert false
  in
  let journal_dir = journal_dir fs in
  let append = stream ~sw ~clock journal_dir in
  append
    (Journal.Run_start
       {
         Journal.pid = Unix.getpid ();
         version;
         backend;
         model = model_path;
         ctx_size = context_size;
       });
  let line =
    stopping append @@ fun () ->
    (* Confine the filesystem tools to [workspace]. Eio refuses ".." and
       symlinks that leave the subtree, so no path the model supplies can reach
       outside it. *)
    Eio.Path.with_subtree Eio.Path.(fs / workspace) @@ fun ws ->
    (* A directory outside the workspace is granted rather than refused,
       because a person invoked this run deliberately and the grant is on the
       record: the [open_dir] call and its result are journal records like every
       other call. An unattended numpty refuses for the opposite reason, having
       nobody to ask. *)
    (* okit's progress is ephemeral and is logged rather than journalled. A
       record stream is the account of what was done, and a line saying which
       step a start-up has reached belongs to neither the account nor, at the
       default verbosity, the stream.

       The tools are assembled before the engine exists, because assembling
       them spawns okitd, and a spawn is a fork of this process. Forking one
       that has a model mapped into it costs minutes on macOS. okitd is the only
       process dumpty spawns, and it is spawned here, while this one is still
       small. *)
    let argv =
      [
        Cli.self ();
        "okitd";
        "--dir";
        Option.value ~default:"." (Eio.Path.native ws);
      ]
    in
    let trace l = Logs.info (fun m -> m "okit: %s" l) in
    let session, status =
      let ds4 =
        Driver.v ~name:"ds4"
          ~models:(Agentkit_ds4.models ~dir:(Model.dir xdg))
          ~create:(fun _ ->
            let setup =
              Okit.Toolset.create_agent ~vision:None ~sw
                ~proc:(Eio.Stdenv.process_mgr env)
                ~net:(Eio.Stdenv.net env) ~clock
                ~domain_mgr:(Eio.Stdenv.domain_mgr env)
                ~fs ~cache:(Xdge.cache_dir xdg)
                ~model:Eio.Path.(fs / model_path)
                ~root:ws
                ~approve:(fun _ -> true)
                ~trace ~argv ~system ~thinking:think ~ctx_size
                ~max_ctx_size:max_ctx ~seed:(Cli.resolve_seed seed)
            in
            ( Driver.session (module Agentkit_ds4.Agent) setup.agent,
              setup.status ))
      in
      let apple =
        if not Apple_support.available then []
        else
          [
            Driver.v ~name:"apple" ~models:Apple_support.models
              ~create:(fun model ->
                let caps =
                  Toolbox.Caps.create ~sw ~fs ~approve:(fun _ -> true) ws
                in
                let tools, okit, status =
                  Okit.Toolset.assemble ~vision:false ~sw
                    ~proc:(Eio.Stdenv.process_mgr env)
                    ~net:(Eio.Stdenv.net env) ~clock ~caps ~trace ~argv ~root:ws
                in
                let system = Okit.Toolset.system_prompt ~base:system ~okit ws in
                let session, _ =
                  Apple_support.create ~sw ~model ~system tools
                in
                (session, status));
          ]
      in
      match Driver.create (Driver.merge (ds4 :: apple)) model_choice with
      | Ok value -> value
      | Error message -> failwith message
    in
    (* Journalled rather than logged, so that the account says the agent worked
       without okit's tools and the stream stays records in the common case. An
       okitd that would not start and a workspace whose dune session was refused
       both leave the agent working without those tools, so both are on the
       record. A workspace with no dune-project asked for none and is not a
       refusal. *)
    (match status with
    | Okit.Toolset.Unstarted what
    | Okit.Toolset.Serving { refused = Some what; _ } ->
        append (Journal.Error { Journal.where = "okit"; what })
    | Okit.Toolset.Serving { refused = None; _ } -> ());
    let agent = session in
    append (Journal.Prompt prompt);
    let trace =
      Trace.create ~now:(fun () -> Eio.Time.now clock) ~emit:append ()
    in
    (* The trace makes the account and this makes the result. The reply buffer
       starts again at each tool call, so what is left at the end is the text of
       the final turn, which is the reply the exchange ended on rather than
       every turn's text run together. The squeeze starts again with it, since
       it says whether that reply may be cut off rather than whether any turn of
       the run was. A turn squeezed earlier is in the account as its own
       record. *)
    let reply = Buffer.create 1024 in
    let last = ref None and squeezed = ref false in
    (* The ceiling is reported against the turn it stopped, and a turn ends at
       its statistics, so the flag the result carries is the last turn's. A
       turn cut short earlier is in the account as its own record, as a squeeze
       is. *)
    let cut_off = ref false and turn_cut = ref false in
    let on_event ev =
      Trace.event trace ev;
      match ev with
      | Event.Content c -> Buffer.add_string reply c
      | Event.Tool_call _ ->
          Buffer.clear reply;
          squeezed := false
      | Event.Stats s ->
          last := Some s;
          cut_off := !turn_cut;
          turn_cut := false
      | Event.Squeezed _ -> squeezed := true
      | Event.Cut_off _ -> turn_cut := true
      | Event.Reasoning _ | Event.Tool_result _ | Event.Expanded _
      | Event.Compacted _ | Event.Done ->
          ()
    in
    bound ~clock
      ~cancel:(fun () -> Driver.cancel agent)
      ~seconds:timeout
      (fun () ->
        try Driver.send agent ~on_event prompt with
        | Agent.Tool_call_cut_off { tokens; attempts } ->
            append
              (Journal.Error
                 {
                   Journal.where = "agent";
                   what =
                     Printf.sprintf
                       "%d turns running ended with a tool call past the %d \
                        token ceiling, so nothing they asked for was done"
                       attempts tokens;
                 });
            failwith
              (Printf.sprintf
                 "the model could not write a tool call inside its %d token \
                  reply, on %d turns running, so nothing was done. Ask for the \
                  work in smaller steps"
                 tokens attempts)
        | Agent.Context_exhausted { needed; ctx } ->
            append
              (Journal.Error
                 {
                   Journal.where = "agent";
                   what =
                     Printf.sprintf
                       "the conversation needs %d tokens and the context had \
                        reached %d, which is as large as it may get"
                       needed ctx;
                 });
            failwith
              (Printf.sprintf
                 "the prompt and its work need %d tokens and the context \
                  stopped growing at %d. Retry with a larger --ctx or \
                  --max-ctx, or with a shorter prompt"
                 needed ctx)
        | Agent.Empty_reply { attempts } ->
            append
              (Journal.Error
                 {
                   Journal.where = "agent";
                   what =
                     Printf.sprintf
                       "the model ended %d turns without a reply or a tool call"
                       attempts;
                 });
            failwith
              (Printf.sprintf
                 "the model ended %d turns without a reply or a tool call. \
                  Retry with a smaller prompt or start a new session"
                 attempts)
        | Agent.Malformed_tool_call { message; attempts } ->
            append
              (Journal.Error
                 {
                   Journal.where = "agent";
                   what =
                     Printf.sprintf
                       "the model wrote malformed tool syntax on %d turns: %s"
                       attempts message;
                 });
            failwith
              (Printf.sprintf
                 "the model wrote malformed tool syntax on %d turns. Retry \
                  with a smaller prompt or start a new session: %s"
                 attempts message));
    (* The agent reports its figures after every turn, so the last of them is
       what the exchange cost. Asking the agent is the fallback for an exchange
       that somehow ran no turn, and it waits for the engine, which is why it is
       not the first choice. *)
    let stats = match !last with Some s -> s | None -> Driver.stats agent in
    (* Encoded before the run is declared over, so that a failure to encode is a
       failure of the run rather than a second [run_stop] after the first. *)
    let line =
      encode
        {
          reply = Buffer.contents reply;
          turns = stats.Event.turns;
          tool_calls = stats.Event.tool_calls;
          ctx_used = stats.Event.ctx_used;
          ctx_size = stats.Event.ctx_size;
          squeezed = !squeezed;
          cut_off = !cut_off;
          journal = Option.value ~default:"?" (Eio.Path.native journal_dir);
        }
    in
    append (Journal.Run_stop "the exchange finished");
    line
  in
  (* Written outside the scope that appends a [run_stop] on a failure, so that a
     write which fails after the run was declared over does not declare it over
     a second time. It still leaves standard output without a whole line and the
     status nonzero, which is the failure contract. The line and its newline go
     out in one write, as a record does. *)
  output_string stdout (line ^ "\n");
  flush stdout

(* ---- okitd ------------------------------------------------------------- *)

(* okit in a process of its own, spawned by the run above and speaking the
   protocol over the pipes it was given. It holds the dune session, the merlin
   and every process okit runs, so that a dumpty which has loaded a model forks
   nothing. It is humpty's okitd under another name, both commands spawning
   themselves. *)
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

let workspace =
  Arg.(
    value & opt string "."
    & info [ "d"; "dir" ] ~docv:"DIR"
        ~doc:"The workspace directory the agent's file tools are confined to.")

let system =
  Arg.(
    value
    & opt string Okit.Toolset.default_system
    & info [ "s"; "system" ] ~docv:"TEXT"
        ~doc:"The system prompt that sets the agent's role and behaviour.")

let ctx =
  Arg.(
    value & opt int 32768
    & info [ "ctx" ] ~docv:"N"
        ~doc:
          "The context window, in tokens. It must hold the prompt, the reply \
           and every tool result the work reads. Memory use grows with it.")

let max_ctx =
  Arg.(
    value & opt int 262144
    & info [ "max-ctx" ] ~docv:"N"
        ~doc:
          "The largest context the exchange may grow to. Reaching it ends the \
           run rather than growing further.")

(* A bound is a count of seconds, and 0 is how it is removed. A negative one is
   refused rather than read as another way of removing it, since a caller that
   arrived at one by arithmetic meant something it did not get. *)
let seconds =
  let parse s =
    match Arg.conv_parser Arg.int s with
    | Ok n when n >= 0 -> Ok n
    | Ok _ ->
        Error
          (`Msg
             "expected 0, which removes the bound, or a positive count of \
              seconds")
    | Error _ as e -> e
  in
  Arg.conv ~docv:"SECS" (parse, Arg.conv_printer Arg.int)

let timeout =
  Arg.(
    value & opt seconds 600
    & info [ "timeout" ] ~docv:"SECS"
        ~doc:
          "How long the whole exchange may take, generation and tool calls \
           together. Expiry appends a $(b,run_stop) naming it, writes nothing \
           on standard output and exits nonzero, since a one-shot command left \
           in a script must not hang the script. The run ends by leaving the \
           process, which closes the pipe to okit's server: that server \
           unwinds, stopping itself and the dune server it started, once it is \
           done with the call it was inside. $(b,0) removes the bound, and a \
           negative count is refused.")

let prompt =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PROMPT"
        ~doc:
          "What to ask the agent to do. Standard input is read when it is \
           $(b,-).")

let one_shot_term =
  with_log
    Term.(
      const one_shot
      $ Driver.model_term ~default:"ds4/auto" ~short_driver:"ds4" ()
      $ workspace $ system $ Cli.think $ Cli.seed $ ctx $ max_ctx $ timeout
      $ prompt)

let okitd_cmd =
  let doc = "Serve okit's tools over a pipe. Internal." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Internal. It is spawned by a dumpty run to hold the dune session, \
         merlin and every process okit runs, and it speaks a protocol over its \
         standard input and output. Not for direct use.";
    ]
  in
  Cmd.v (Cmd.info "okitd" ~doc ~man) (with_log Term.(const okitd $ workspace))

(* ---- top-level group ---------------------------------------------------- *)

let () =
  let doc = "Run one prompt through a tool-using agent and print the result." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "dumpty is the third command over a model running on your own machine. \
         $(b,humpty) is the conversation, held with a person at a terminal. \
         $(b,numpty) is the daemon, which wakes on a schedule and runs for \
         weeks. dumpty is the one shot: it does the job it was given and \
         exits, which is what a script or a build step wants.";
      `P
        "It assembles the tools $(b,humpty agent) gives a model for the \
         workspace named by $(b,--dir), sends $(i,PROMPT) through the same \
         agent loop, and stops when the model replies with text alone. An \
         $(b,AGENTS.md) at the root of that workspace is added to the system \
         prompt, as humpty adds it. A $(i,PROMPT) of $(b,-) is read from \
         standard input, for a prompt another program assembled.";
      `P
        "Standard output carries exactly one line, written when the exchange \
         ends and nothing before it. It is a JSON object whose $(b,reply) is \
         the text of the final turn, whose $(b,turns), $(b,tool_calls), \
         $(b,ctx_used) and $(b,ctx_size) are the last figures the agent \
         reported, whose $(b,squeezed) says whether that reply ran in a \
         context which could no longer grow, whose $(b,cut_off) says whether \
         it stopped at the ceiling on a reply rather than where the model \
         meant to stop, and so is unfinished, and whose $(b,journal) names the \
         directory the run's account was appended to.";
      `P
        "Standard error carries the account, one journal record per line in \
         the codec $(b,numpty log) reads: a $(b,run_start) first, then the \
         prompt, each tool call and its result, the model's text, the turn \
         statistics and the context changes, and a $(b,run_stop) last. Each is \
         flushed as it is written, so a watcher sees the run as it happens. \
         The same records are appended to a journal under the XDG state \
         directory as they are streamed, so a run whose standard error nobody \
         kept is still on the record, and $(b,agentkit log dumpty) reads it \
         back.";
      `P
        "The exit status is zero when the exchange finished, squeezed or not. \
         It is nonzero when there was nothing to print: the model was not \
         there, the engine would not load, the prompt did not fit a context \
         that could no longer grow, the model could not write a tool call \
         inside one reply on three turns running, or the exchange outran \
         $(b,--timeout). None of those writes anything on standard output, so \
         an empty standard output and a nonzero status is the whole failure \
         contract. A failure met once the run has begun also appends a \
         $(b,run_stop) naming the reason, and a model refused before it began \
         writes no journal at all.";
      `P
        "Use $(b,models list) to see the available choices and $(b,models \
         fetch ds4/MODEL) to download DS4 weights. A run never downloads \
         weights automatically.";
      `P
        (Printf.sprintf
           "DS4 models use the %s compute backend in this build. On a \
            supported Mac, --model apple/default uses Apple Foundation Models."
           Cli.backend_name);
    ]
  in
  let info = Cmd.info "dumpty" ~version ~doc ~man in
  exit
    (Cmd.eval_result
       (Cmd.group info ~default:one_shot_term
          [ Agentkit_model_support.command (); okitd_cmd ]))
