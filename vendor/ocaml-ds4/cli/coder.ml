(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module V4 = Ds4.V4
module Agent = Ds4.Agent
module Toolbox = Ds4.Toolbox

(* The capability rule has to be stated, not implied. A model that is not told
   to mint a capability passes an empty one everywhere and never narrows its
   reach. The rules after it are upstream's, which exist because a model that
   has not been told how slow local inference is reads and rewrites whole
   files as though prefill were free. *)
let default_system =
  "You are a coding agent working in a local workspace. Use the tools for file \
   work. Do not print large file contents or large code blocks as an answer. \
   Create or edit files with the tools, then summarise what you did briefly.\n\n\
   Every file tool takes a cap argument naming a capability. Before you read, \
   search, list or write anywhere, call open_dir on the directory you want and \
   pass the name it returns as cap in every later call. A directory outside \
   the one you started in must be requested with open_dir before any other \
   tool can reach it. Write \"~/x\" for a path under the home directory rather \
   than guessing where home is.\n\n\
   Start with tree to get your bearings, then grep or read_lines rather than \
   reading whole files. Change a file with edit, which replaces one passage of \
   it: the old text must occur exactly once, so read enough of the file to \
   quote it exactly, and to insert text, replace a unique anchor with the \
   anchor plus the new text. Keep write for creating a file or replacing the \
   whole of one. A file longer than one reply can hold is written in parts: \
   write the first part, then append the rest, since a call that runs past the \
   end of a reply is discarded whole.\n\n\
   # Rules\n\n\
   - Finish thinking before you call a tool, and write every tool call in \
   exactly the syntax given.\n\
   - This model runs on local inference, at a few hundred tokens a second of \
   prefill and a few tens of generation. Read only the text you need, and edit \
   rather than rewrite.\n\
   - Write code that is reliable, and keep a clear model of what the complex \
   parts of it do.\n\
   - Leave the rest of the system as you found it unless you are asked \
   otherwise."

(* Capped, because the file joins every prompt and a long one would quietly
   consume the context the conversation needs. *)
let max_instructions = 8000

let instructions ws =
  match Eio.Path.load Eio.Path.(ws / "AGENTS.md") with
  | exception Eio.Exn.Io _ -> None
  | text when String.trim text = "" -> None
  | text when String.length text <= max_instructions -> Some text
  | text ->
      (* The cut falls at a line boundary, since half a sentence quoted as
         though it were whole is worse than saying less. *)
      let cut =
        match String.rindex_from_opt text max_instructions '\n' with
        | Some i when i > 0 -> i
        | _ -> max_instructions
      in
      Some
        (String.sub text 0 cut
       ^ "\n\n\
          [AGENTS.md is longer than what joins every prompt and stops here. \
          Read AGENTS.md itself, from this line on, for the rest.]")

let date now =
  let tm = Unix.localtime now in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min

let system_prompt ?(base = default_system) ~now ws =
  let parts =
    [
      Some base;
      Some
        (Printf.sprintf
           "The local date and time at the start of this session is %s. Use it \
            only when the date or time matters."
           (date now));
      Option.map
        (fun text ->
          "The workspace's AGENTS.md sets these conventions for it:\n\n" ^ text)
        (instructions ws);
    ]
  in
  String.concat "\n\n" (List.filter_map Fun.id parts)

let tools ?(vision = false) caps =
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
  ]
  @ if vision then [ Toolbox.view_image ~caps ] else []

(* A tool result is shown by its first line, since the whole of it is already
   in the conversation and a person following along wants to see that the call
   answered, not to reread the file it returned. *)
let first_line s =
  match String.index_opt s '\n' with
  | None -> s
  | Some i ->
      Printf.sprintf "%s [%d bytes]" (String.sub s 0 i) (String.length s)

(* Text a turn says before its tool call is ended with a newline, so that it
   does not run into the next turn's. *)
let printer ~stdout =
  let open_line = ref false in
  let end_line () =
    if !open_line then Eio.Flow.copy_string "\n" stdout;
    open_line := false
  in
  function
  | Agent.Content c ->
      if c <> "" then begin
        Eio.Flow.copy_string c stdout;
        open_line := c.[String.length c - 1] <> '\n'
      end
  | Agent.Reasoning _ -> ()
  | Agent.Tool_call tc ->
      end_line ();
      Printf.eprintf "> %s %s\n%!" tc.Dsml.name tc.Dsml.arguments
  | Agent.Tool_result (name, result) ->
      Printf.eprintf "< %s: %s\n%!" name (first_line result)
  | Agent.Stats s ->
      end_line ();
      Logs.info (fun m ->
          m
            "turn %d, %d tool calls, context %d/%d, prefill %.1fs, %d tokens \
             (%d drafted) at %.1f/s"
            s.Agent.turns s.Agent.tool_calls s.Agent.ctx_used s.Agent.ctx_size
            s.Agent.prefill_seconds s.Agent.generated s.Agent.drafted
            (if s.Agent.generate_seconds > 0. then
               float_of_int s.Agent.generated /. s.Agent.generate_seconds
             else 0.))
  | Agent.Expanded n -> Printf.eprintf "= context expanded to %d\n%!" n
  | Agent.Squeezed n ->
      Printf.eprintf "= context full, %d tokens to reply in\n%!" n
  | Agent.Cut_off c ->
      Printf.eprintf "= reply cut off at %d tokens%s\n%!" c.Agent.tokens
        (if c.Agent.tool_call then ", tool call discarded" else "")
  | Agent.Compacted c ->
      end_line ();
      Printf.eprintf
        "= context compacted from %d to %d tokens, with a summary of %d bytes\n\
         %!"
        c.Agent.before c.Agent.after
        (String.length c.Agent.summary)
  | Agent.Done -> end_line ()

(* The prompt in flight, which Ctrl-C interrupts. With none, Ctrl-C leaves. *)
let in_flight : Agent.t option ref = ref None

let on_interrupt _ =
  match !in_flight with Some agent -> Agent.cancel agent | None -> exit 130

let send ~on_event agent prompt =
  in_flight := Some agent;
  Fun.protect
    ~finally:(fun () -> in_flight := None)
    (fun () -> Agent.send agent ~on_event prompt)

let repl ~stdin ~stdout ?(interactive = Unix.isatty Unix.stdin) agent =
  let on_event = printer ~stdout in
  let input = Eio.Buf_read.of_flow ~max_size:1_000_000 stdin in
  (* A model that has loaded and is waiting for input otherwise looks like one
     that hangs. The prompt goes to standard error, so a captured reply does
     not hold it. *)
  if interactive then
    prerr_endline
      "Type a prompt and press Enter. Ctrl-C interrupts a reply, and Ctrl-D \
       ends the session.";
  let rec loop () =
    if interactive then prerr_string "? ";
    flush stderr;
    match Eio.Buf_read.line input with
    | exception End_of_file -> ()
    | line when String.trim line = "" -> loop ()
    | line ->
        (try send ~on_event agent line with
        | Agent.Context_exhausted { needed; ctx } ->
            Printf.eprintf
              "= the conversation needs %d tokens and the context is at its \
               ceiling of %d, so that prompt was not sent\n\
               %!"
              needed ctx
        | V4.Session_interrupted ->
            on_event Agent.Done;
            prerr_endline "= interrupted, and the prompt was withdrawn");
        loop ()
  in
  loop ()

let run ~model ~workspace ~system ~thinking ~seed ~ctx_size ~max_ctx_size
    ~max_tokens ~temperature ~mtp ~vision prompt =
  Cli.run @@ fun env xdg ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let stdout = Eio.Stdenv.stdout env in
  let model_path = Cli.resolve_model ~dir:(Model.dir xdg) model in
  let vision_path = Option.map (fun p -> Eio.Path.(fs / p)) vision in
  (* The file tools hold only this capability and those minted from it, and
     Eio refuses ".." and symlinks that leave a subtree, so no path the model
     supplies reaches outside what it was granted. *)
  Eio.Path.with_subtree Eio.Path.(fs / workspace) @@ fun ws ->
  let approve path =
    Printf.eprintf "= allowed %s\n%!" path;
    true
  in
  let caps = Toolbox.Caps.create ~sw ~fs ~approve ws in
  let engine =
    V4.create ~mtp ?vision:vision_path ~cache:(Xdge.cache_dir xdg)
      ~model:Eio.Path.(fs / model_path)
      ()
  in
  let agent =
    Agent.create engine
      ~system:(system_prompt ~base:system ~now:(Unix.time ()) ws)
      ~thinking ~ctx_size ~max_ctx_size ~max_tokens ?temperature
      ~seed:(Cli.resolve_seed seed)
      ~tools:(tools ~vision:(Option.is_some vision) caps)
  in
  (* Eio has no signal-waiting API. This handler is installed and restored on
     the main domain, and [on_interrupt] only sets the session's C atomic. *)
  let previous =
    (Sys.signal Sys.sigint (Sys.Signal_handle on_interrupt)
     [@alert "-unsafe_multidomain"])
  in
  Fun.protect ~finally:(fun () ->
      (Sys.set_signal Sys.sigint previous [@alert "-unsafe_multidomain"]))
  @@ fun () ->
  match prompt with
  | Some p -> send ~on_event:(printer ~stdout) agent p
  | None -> repl ~stdin:(Eio.Stdenv.stdin env) ~stdout agent

open Cmdliner

let workspace =
  Arg.(
    value & opt string "."
    & info [ "d"; "dir" ] ~docv:"DIR"
        ~doc:"The workspace directory the agent's file tools start confined to.")

let system =
  Arg.(
    value & opt string default_system
    & info [ "s"; "system" ] ~docv:"TEXT"
        ~doc:
          "The system prompt that sets the agent's role and behaviour. The \
           date and the workspace's AGENTS.md are added to it.")

let ctx =
  Arg.(
    value & opt int 32768
    & info [ "ctx" ] ~docv:"N"
        ~doc:
          "The context window, in tokens. It must hold the whole conversation, \
           including tool output. Memory use grows with it.")

let max_ctx =
  Arg.(
    value & opt int 262144
    & info [ "max-ctx" ] ~docv:"N"
        ~doc:
          "The largest context the conversation may grow to. Reaching it ends \
           a turn rather than growing further.")

let max_tokens =
  Arg.(
    value & opt int 16384
    & info [ "n"; "max-tokens" ] ~docv:"N"
        ~doc:
          "The most tokens one reply may take, reasoning included. A tool call \
           that does not finish inside it is discarded, and the context grows \
           to keep room for a reply this long.")

let temperature =
  Arg.(
    value
    & opt (some float) None
    & info [ "t"; "temperature" ] ~docv:"T"
        ~doc:
          "The sampling temperature. The default is the one the model family \
           was tuned for. Tool call syntax is always sampled greedily.")

let prompt =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"PROMPT"
        ~doc:
          "The prompt to send. When it is absent, prompts are read from \
           standard input, one per line, into one conversation.")

let vision =
  Arg.(
    value
    & opt (some string) None
    & info [ "vision" ] ~docv:"FILE"
        ~doc:
          "Load the vision sidecar that matches the model and enable the \
           capability-confined view_image tool. GLM 5.3, DeepSeek V4.1 Flash \
           and Qwen3.8 Flash Next each have one of their own.")

let cmd =
  let doc = "Run a model in a loop with tools that act on a workspace." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The model can list, read, search and write files to carry out a \
         request over several turns. The file tools start confined to \
         $(b,--dir). A directory outside it is granted when the model asks for \
         one, and the grant is reported. There is no shell.";
      `P
        "The reply is written to standard output. Each tool call, a summary of \
         its result, and any change to the context are written to standard \
         error. Ctrl-C interrupts a reply and withdraws its prompt.";
      `P
        "$(b,--vision) loads the sidecar that matches the model and adds a \
         view_image tool confined to the same workspace capability as the \
         other file tools.";
    ]
  in
  let run model workspace system thinking seed ctx_size max_ctx_size max_tokens
      temperature mtp vision prompt =
    run ~model ~workspace ~system ~thinking ~seed ~ctx_size ~max_ctx_size
      ~max_tokens ~temperature ~mtp ~vision prompt
  in
  let term =
    Cli.with_logs
      Term.(
        const run $ Model.arg $ workspace $ system $ Cli.think $ Cli.seed $ ctx
        $ max_ctx $ max_tokens $ temperature $ Cli.mtp $ vision $ prompt)
  in
  Cmd.v (Cmd.info "agent" ~doc ~man) term
