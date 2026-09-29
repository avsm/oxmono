open Cmdliner

type dir = Eio.Fs.dir_ty Eio.Path.t

let tool_path root path =
  if Filename.is_relative path then Eio.Path.(root / path)
  else invalid_arg "paths must be relative to the workspace"

let read_tool ~root ~maximum_output =
  let codec =
    let open Apple_fm.Codec in
    Invoke.map "read" (fun path start_line count -> (path, start_line, count))
    |> Invoke.param
         ~enc:(fun (path, _, _) -> path)
         "path" string ~description:"workspace-relative file path"
    |> Invoke.param
         ~enc:(fun (_, start, _) -> start)
         ~default:1 "start_line" int ~description:"first line, starting at 1"
    |> Invoke.param
         ~enc:(fun (_, _, count) -> count)
         ~default:200 "count" int ~description:"maximum number of lines"
    |> Invoke.seal
  in
  Apple_fm.Tool.v
    ~description:"Read numbered lines from a UTF-8 text file in the workspace."
    codec (fun (path, start_line, count) ->
      if start_line < 1 || count < 1 then
        invalid_arg "start_line and count must be positive";
      Eio.Path.with_lines (tool_path root path) @@ fun lines ->
      let buffer = Buffer.create 4096 in
      let rec loop number remaining lines =
        if remaining = 0 then Buffer.contents buffer
        else
          match lines () with
          | Seq.Nil -> Buffer.contents buffer
          | Seq.Cons (line, rest) when number < start_line ->
              loop (number + 1) remaining rest
          | Seq.Cons (line, rest) ->
              let rendered = Printf.sprintf "%d: %s\n" number line in
              let available = maximum_output - Buffer.length buffer in
              if String.length rendered <= available then (
                Buffer.add_string buffer rendered;
                loop (number + 1) (remaining - 1) rest)
              else (
                if available > 0 then
                  Buffer.add_substring buffer rendered 0 available;
                Buffer.add_string buffer "\n[read output truncated]\n";
                Buffer.contents buffer)
      in
      loop 1 count lines)

let temporary_id = Atomic.make 0

let create_temporary parent basename perm contents =
  let rec create () =
    let id = Atomic.fetch_and_add temporary_id 1 in
    let name = Printf.sprintf ".%s.apple-fm-%d-tmp" basename id in
    let temporary = Eio.Path.(parent / name) in
    try
      Eio.Path.save ~create:(`Exclusive perm) temporary contents;
      Eio.Path.chmod ~follow:true ~perm temporary;
      temporary
    with
    | Eio.Io (Eio.Fs.E (Already_exists _), _) -> create ()
    | exn ->
        Eio.Path.unlink ~missing_ok:true temporary;
        raise exn
  in
  create ()

let replace_file root path contents =
  let target = tool_path root path in
  let save_in_place perm =
    Eio.Path.save ~create:(`Or_truncate perm) target contents
  in
  match Eio.Path.stat ~follow:false target with
  | stat when stat.kind = `Symbolic_link || Int64.compare stat.nlink 1L > 0 ->
      save_in_place 0o644
  | stat -> (
      match Eio.Path.split target with
      | None -> invalid_arg "invalid output path"
      | Some (parent, basename) ->
          let perm = if stat.kind = `Regular_file then stat.perm else 0o644 in
          let temporary = create_temporary parent basename perm contents in
          Fun.protect
            ~finally:(fun () -> Eio.Path.unlink ~missing_ok:true temporary)
            (fun () -> Eio.Path.rename temporary target))
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> save_in_place 0o644

let write_result path contents =
  Printf.sprintf "wrote %d bytes to %s" (String.length contents) path

let write_tool ~root =
  let codec =
    let open Apple_fm.Codec in
    Invoke.map "write" (fun path content -> (path, content))
    |> Invoke.param ~enc:fst "path" string
         ~description:"workspace-relative output path"
    |> Invoke.param ~enc:snd "content" string
         ~description:"complete new file contents"
    |> Invoke.seal
  in
  Apple_fm.Tool.v
    ~description:"Create a file or atomically replace its complete contents."
    codec (fun (path, content) ->
      replace_file root path content;
      write_result path content)

let find_substring ~needle haystack offset =
  let needle_length = String.length needle in
  let limit = String.length haystack - needle_length in
  let rec loop index =
    if index > limit then None
    else if String.sub haystack index needle_length = needle then Some index
    else loop (index + 1)
  in
  if needle_length = 0 then None else loop offset

let edit_tool ~root =
  let codec =
    let open Apple_fm.Codec in
    Invoke.map "edit" (fun path old new_ -> (path, old, new_))
    |> Invoke.param
         ~enc:(fun (path, _, _) -> path)
         "path" string ~description:"workspace-relative file path"
    |> Invoke.param
         ~enc:(fun (_, old, _) -> old)
         "old" string ~description:"exact text that occurs once"
    |> Invoke.param
         ~enc:(fun (_, _, new_) -> new_)
         "new" string ~description:"replacement text"
    |> Invoke.seal
  in
  Apple_fm.Tool.v
    ~description:
      "Replace exactly one passage in a workspace file. Read the file first."
    codec (fun (path, old, new_) ->
      let contents = Eio.Path.load (tool_path root path) in
      match find_substring ~needle:old contents 0 with
      | None -> "Error: old was not found"
      | Some first -> (
          match
            find_substring ~needle:old contents (first + String.length old)
          with
          | Some _ -> "Error: old occurs more than once"
          | None ->
              let before = String.sub contents 0 first in
              let after_start = first + String.length old in
              let after =
                String.sub contents after_start
                  (String.length contents - after_start)
              in
              let updated = before ^ new_ ^ after in
              replace_file root path updated;
              write_result path updated))

let bash_tool ~root ~maximum_output process_mgr =
  let codec =
    let open Apple_fm.Codec in
    Invoke.map "bash" Fun.id
    |> Invoke.param ~enc:Fun.id "command" string
         ~description:"shell command line, run with 'bash -c'"
    |> Invoke.seal
  in
  Apple_fm.Tool.v
    ~description:"Run a shell command and capture its combined output." codec
    (fun command ->
      let output =
        Eio.Process.parse_out process_mgr Eio.Buf_read.take_all ~cwd:root
          ~is_success:(fun _ -> true)
          [ "bash"; "-c"; "exec 2>&1; " ^ command ]
      in
      let output =
        if String.length output <= maximum_output then output
        else
          let omitted = String.length output - maximum_output in
          Printf.sprintf "[... %d earlier bytes omitted ...]\n%s" omitted
            (String.sub output omitted maximum_output)
      in
      output)

let default_instructions =
  "You are a coding agent working in a local workspace. Inspect relevant files \
   before editing. Use bash for searches and tests, read for focused context, \
   edit for precise changes, and write for new files. Keep explanations \
   concise. Never claim a command or edit succeeded without using the tool."

let print_availability_error = function
  | `Available -> None
  | `Device_not_eligible -> Some "this Mac does not support Apple Intelligence"
  | `Apple_intelligence_not_enabled -> Some "Apple Intelligence is not enabled"
  | `Model_not_ready -> Some "the on-device model is not ready"
  | `Unavailable message -> Some message

let compaction_prompt reason =
  Printf.sprintf
    "Internal agent context compaction request. This is not a user request.\n\
     Write a durable task-state summary of the conversation so far. Preserve \
     only facts needed to continue the work:\n\
     - user goals, constraints, and preferences\n\
     - files inspected or edited\n\
     - commands run and important results\n\
     - decisions, rejected approaches, known bugs, and pending next steps\n\
     - reloadable bulky data with exact paths, ranges, or commands\n\n\
     Do not invent facts, solve unfinished tasks, call tools, or include \
     generic narration. Return only the compact summary.\n\n\
     Compaction reason: %s"
    reason

let validate_compaction_options compact_at compact_tokens =
  if compact_at <> 0 && (compact_at < 10 || compact_at > 95) then
    invalid_arg "--compact-at must be zero or between 10 and 95";
  Option.iter
    (fun value ->
      if value < 64 then invalid_arg "--compact-tokens must be at least 64")
    compact_tokens

let run_eio env root instructions temperature maximum_response_tokens
    session_file compact_at compact_tokens prompt =
  Eio.Switch.run @@ fun sw ->
  let stdout = Eio.Stdenv.stdout env in
  let stderr = Eio.Stdenv.stderr env in
  let write flow text = Eio.Flow.copy_string text flow in
  let root =
    (Eio.Path.open_subtree ~sw Eio.Path.(Eio.Stdenv.fs env / root) :> dir)
  in
  match print_availability_error (Apple_fm.Availability.get ()) with
  | Some message ->
      write stderr ("apple-fm-agent: " ^ message ^ "\n");
      1
  | None -> (
      try
        validate_compaction_options compact_at compact_tokens;
        let model_info = Apple_fm.Model.info () in
        let context_size = model_info.context_size in
        let maximum_tool_output = max 4_096 (min 32_768 (context_size * 2)) in
        let tools =
          [
            bash_tool ~root ~maximum_output:maximum_tool_output
              (Eio.Stdenv.process_mgr env);
            read_tool ~root ~maximum_output:maximum_tool_output;
            write_tool ~root;
            edit_tool ~root;
          ]
        in
        let compact_tokens =
          Option.value compact_tokens
            ~default:(max 256 (min 1_024 (context_size / 8)))
        in
        let options =
          Apple_fm.Generation.options ?temperature ?maximum_response_tokens ()
        in
        let checkpoint = ref session_file in
        let load_transcript path =
          let json = Eio.Path.load (tool_path root path) in
          match Apple_fm.Transcript.of_json json with
          | Ok transcript -> transcript
          | Error message ->
              invalid_arg
                (Printf.sprintf "cannot decode session file %s: %s" path message)
        in
        let create_session ?transcript ?instructions () =
          let session =
            Apple_fm.Session.create ~sw ?transcript ?instructions tools
          in
          Apple_fm.Session.prewarm session;
          session
        in
        let restored =
          Option.bind !checkpoint (fun path ->
              try Some (load_transcript path)
              with Eio.Io (Eio.Fs.E (Not_found _), _) -> None)
        in
        let session =
          ref
            (match restored with
            | Some transcript -> create_session ~transcript ()
            | None -> create_session ~instructions ())
        in
        let save_to path =
          let transcript = Apple_fm.Session.transcript !session in
          replace_file root path (Apple_fm.Transcript.to_json transcript)
        in
        let save_checkpoint () =
          Option.iter
            (fun path ->
              try save_to path
              with exn ->
                write stderr
                  (Format.asprintf "warning: cannot save %s: %a\n" path
                     Eio.Exn.pp exn))
            !checkpoint
        in
        let replace_session replacement =
          let old = !session in
          session := replacement;
          Apple_fm.Session.close old
        in
        let restore transcript =
          replace_session (create_session ~transcript ())
        in
        let transcript_tokens () =
          Apple_fm.Session.transcript !session
          |> Apple_fm.Model.count_transcript_tokens
        in
        let summary_options =
          Apple_fm.Generation.options ~sampling:`Greedy
            ~maximum_response_tokens:compact_tokens ()
        in
        let safe_summary_options =
          Apple_fm.Generation.options ~sampling:`Greedy
            ~maximum_response_tokens:compact_tokens ~tool_calling:`Disallowed ()
        in
        let compact reason =
          let before = Apple_fm.Session.transcript !session in
          let old_tokens =
            try Some (Apple_fm.Model.count_transcript_tokens before)
            with Eio.Io (Apple_fm.Error.E _, _) -> None
          in
          write stderr (Printf.sprintf "compacting: %s\n" reason);
          try
            let summary =
              try
                Apple_fm.Session.respond ~options:safe_summary_options !session
                  (compaction_prompt reason)
              with Eio.Io (Apple_fm.Error.E (`Unsupported_version _), _) ->
                (* macOS 26 has no tool-calling policy. The prompt itself asks
                   the model not to call tools. *)
                Apple_fm.Session.respond ~options:summary_options !session
                  (compaction_prompt reason)
            in
            if String.trim summary = "" then failwith "empty compaction summary";
            let response_reserve =
              Option.value maximum_response_tokens ~default:1_024
            in
            let rec retain turns =
              let transcript =
                Apple_fm.Model.compact_transcript ~keep_last_turns:turns
                  ~summary before
              in
              if turns = 0 || context_size <= 0 then transcript
              else
                try
                  let tokens =
                    Apple_fm.Model.count_transcript_tokens transcript
                  in
                  if tokens + response_reserve + 128 < context_size then
                    transcript
                  else retain (turns - 1)
                with Eio.Io (Apple_fm.Error.E _, _) ->
                  Apple_fm.Model.compact_transcript ~keep_last_turns:0 ~summary
                    before
            in
            let compacted = retain 2 in
            let replacement = create_session ~transcript:compacted () in
            replace_session replacement;
            save_checkpoint ();
            let new_tokens =
              try Some (transcript_tokens ())
              with Eio.Io (Apple_fm.Error.E _, _) -> None
            in
            (match (old_tokens, new_tokens) with
            | Some old_tokens, Some new_tokens ->
                write stderr
                  (Printf.sprintf "compacted context: %d -> %d tokens\n"
                     old_tokens new_tokens)
            | _ -> write stderr "compaction complete\n");
            true
          with exn ->
            (* The private compaction prompt must not become conversational
               history if generation partially mutated the old session. *)
            (try restore before
             with restore_exn ->
               write stderr
                 (Format.asprintf "error: cannot restore after compaction: %a\n"
                    Eio.Exn.pp restore_exn));
            write stderr
              (Format.asprintf "error: compaction failed: %a\n" Eio.Exn.pp exn);
            false
        in
        let accounting_available = ref (compact_at <> 0) in
        let warned_about_accounting = ref false in
        let should_compact prompt =
          if (not !accounting_available) || context_size <= 0 then false
          else
            try
              let used = transcript_tokens () in
              let incoming = Apple_fm.Model.count_text_tokens prompt in
              let compact_prompt_tokens =
                Apple_fm.Model.count_text_tokens
                  (compaction_prompt "context pressure before the next turn")
              in
              let response_reserve =
                Option.value maximum_response_tokens ~default:1_024
              in
              let percentage_limit = context_size * compact_at / 100 in
              let reserve_limit =
                context_size - compact_prompt_tokens - compact_tokens - 128
              in
              let turn_limit = context_size - response_reserve - 128 in
              let limit = min percentage_limit (min reserve_limit turn_limit) in
              used + incoming >= max 1 limit
            with Eio.Io (Apple_fm.Error.E _, _) as exn ->
              accounting_available := false;
              if not !warned_about_accounting then (
                warned_about_accounting := true;
                write stderr
                  (Format.asprintf
                     "warning: automatic compaction disabled: %a\n" Eio.Exn.pp
                     exn));
              false
        in
        let respond prompt =
          ignore
            (Apple_fm.Session.respond_stream ~options ~output:stdout !session
               prompt);
          write stdout "\n";
          save_checkpoint ()
        in
        let complete_turn prompt =
          respond prompt;
          if should_compact "" then
            ignore (compact "soft limit after assistant turn")
        in
        let ask prompt =
          try
            if should_compact prompt then
              ignore (compact "soft limit before user turn");
            complete_turn prompt;
            true
          with
          | Eio.Io (Apple_fm.Error.E (`Context_size_exceeded _), _) ->
              if compact "incoming user turn exceeded the context" then (
                try
                  complete_turn prompt;
                  true
                with Eio.Io (Apple_fm.Error.E _, _) as exn ->
                  write stderr (Format.asprintf "\nerror: %a\n" Eio.Exn.pp exn);
                  false)
              else false
          | Eio.Io (Apple_fm.Error.E _, _) as exn ->
              write stderr (Format.asprintf "\nerror: %a\n" Eio.Exn.pp exn);
              false
        in
        let status () =
          let count =
            try Printf.sprintf "%d/%d" (transcript_tokens ()) context_size
            with Eio.Io (Apple_fm.Error.E _, _) ->
              Printf.sprintf "?/%d" context_size
          in
          let usage =
            match Apple_fm.Session.usage !session with
            | None -> "usage unavailable before macOS 27"
            | Some usage -> Format.asprintf "%a" Apple_fm.Context.pp_usage usage
          in
          write stdout (Printf.sprintf "context %s tokens; %s\n" count usage)
        in
        let help () =
          write stdout
            "Commands:\n\
            \  /help          Show this help.\n\
            \  /status        Show context and cumulative token usage.\n\
            \  /compact       Compact the current conversation now.\n\
            \  /save [FILE]   Save Apple's transcript JSON.\n\
            \  /load FILE     Restore an Apple transcript.\n\
            \  /new           Start again from the system instructions.\n\
            \  /quit, /exit   Exit.\n"
        in
        let command line =
          let words =
            String.split_on_char ' ' (String.trim line)
            |> List.filter (fun word -> word <> "")
          in
          match words with
          | [ "/help" ] ->
              help ();
              `Continue
          | [ "/status" ] ->
              status ();
              `Continue
          | [ "/compact" ] ->
              ignore (compact "user requested compaction");
              `Continue
          | [ "/save" ] -> (
              match !checkpoint with
              | None ->
                  write stderr
                    "no session file; use /save FILE or --session FILE\n";
                  `Continue
              | Some path ->
                  save_to path;
                  write stdout ("saved " ^ path ^ "\n");
                  `Continue)
          | [ "/save"; path ] ->
              save_to path;
              checkpoint := Some path;
              write stdout ("saved " ^ path ^ "\n");
              `Continue
          | [ "/load"; path ] ->
              restore (load_transcript path);
              checkpoint := Some path;
              write stdout ("loaded " ^ path ^ "\n");
              `Continue
          | [ "/new" ] ->
              replace_session (create_session ~instructions ());
              save_checkpoint ();
              write stdout "started a new session\n";
              `Continue
          | [ "/quit" ] | [ "/exit" ] -> `Quit
          | command :: _ when String.starts_with ~prefix:"/" command ->
              write stderr ("unknown command: " ^ command ^ "\n");
              `Continue
          | _ -> `Prompt line
        in
        let command line =
          try command line with
          | Eio.Io _ as exn ->
              write stderr (Format.asprintf "error: %a\n" Eio.Exn.pp exn);
              `Continue
          | Invalid_argument message | Failure message ->
              write stderr ("error: " ^ message ^ "\n");
              `Continue
        in
        match prompt with
        | Some prompt -> if ask prompt then 0 else 1
        | None ->
            write stdout
              "Apple Foundation Models coding agent. /help for commands; \
               Ctrl-D to quit.\n";
            let input =
              Eio.Buf_read.of_flow ~max_size:1_000_000 (Eio.Stdenv.stdin env)
            in
            let rec loop () =
              write stdout "> ";
              match Eio.Buf_read.line input with
              | line -> (
                  match command line with
                  | `Quit -> 0
                  | `Continue -> loop ()
                  | `Prompt prompt ->
                      if String.trim prompt = "" || ask prompt then loop ()
                      else 1)
              | exception End_of_file ->
                  write stdout "\n";
                  0
            in
            loop ()
      with
      | Eio.Io (Apple_fm.Error.E _, _) as exn ->
          write stderr (Format.asprintf "apple-fm-agent: %a\n" Eio.Exn.pp exn);
          1
      | Invalid_argument message | Failure message ->
          write stderr ("apple-fm-agent: " ^ message ^ "\n");
          1)

let run root instructions temperature maximum_response_tokens session_file
    compact_at compact_tokens prompt =
  Eio_main.run (fun env ->
      run_eio env root instructions temperature maximum_response_tokens
        session_file compact_at compact_tokens prompt)

let root =
  let doc = "Workspace root. File tools cannot access paths outside it." in
  Arg.(value & opt dir "." & info [ "C"; "root" ] ~docv:"DIR" ~doc)

let instructions =
  let doc = "Model instructions." in
  Arg.(
    value
    & opt string default_instructions
    & info [ "system" ] ~docv:"TEXT" ~doc)

let temperature =
  let doc = "Generation temperature." in
  Arg.(
    value & opt (some float) None & info [ "temperature" ] ~docv:"FLOAT" ~doc)

let maximum_response_tokens =
  let doc = "Maximum tokens in each response." in
  Arg.(value & opt (some int) None & info [ "max-tokens" ] ~docv:"N" ~doc)

let session_file =
  let doc =
    "Restore this workspace-relative transcript file if it exists, and save it \
     after each successful turn."
  in
  Arg.(value & opt (some string) None & info [ "session" ] ~docv:"FILE" ~doc)

let compact_at =
  let doc =
    "Compact before the transcript reaches this percentage of the model \
     context. Zero disables automatic compaction."
  in
  Arg.(value & opt int 75 & info [ "compact-at" ] ~docv:"PERCENT" ~doc)

let compact_tokens =
  let doc = "Maximum tokens in the durable compaction summary." in
  Arg.(value & opt (some int) None & info [ "compact-tokens" ] ~docv:"N" ~doc)

let prompt =
  let doc = "Run one request instead of starting an interactive session." in
  Arg.(value & pos_all string [] & info [] ~docv:"PROMPT" ~doc)

let command =
  let doc = "coding agent using Apple's on-device Foundation Model" in
  let info = Cmd.info "apple-fm-agent" ~doc in
  let term =
    Term.(
      const
        (fun
          root
          instructions
          temperature
          maximum_response_tokens
          session_file
          compact_at
          compact_tokens
          words
        ->
          let prompt =
            match words with
            | [] -> None
            | words -> Some (String.concat " " words)
          in
          run root instructions temperature maximum_response_tokens session_file
            compact_at compact_tokens prompt)
      $ root $ instructions $ temperature $ maximum_response_tokens
      $ session_file $ compact_at $ compact_tokens $ prompt)
  in
  Cmd.v info term

let () = exit (Cmd.eval' command)
