(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module V4 = Ds4.V4

let backend_name =
  match V4.backend with `Metal -> "Metal" | `Cuda -> "CUDA" | `Cpu -> "CPU"

let guard f =
  try Ok (f ()) with
  | Failure msg -> Error msg
  | e -> Error (Printexc.to_string e)

let run f =
  guard @@ fun () ->
  Eio_main.run @@ fun env -> f env (Xdge.create (Eio.Stdenv.fs env) "ds4")

(* [Sys.executable_name] is the path the command was run from, resolved against
   the PATH at startup, which is what a spawn needs and what [Sys.argv.(0)]
   alone would not give for a binary found on the PATH. It is used only when it
   names something that is there, since a binary unlinked or renamed since
   startup leaves it naming nothing. *)
let self () =
  if Sys.file_exists Sys.executable_name then Sys.executable_name
  else Sys.argv.(0)

(* The sampler's state must not be zero, so a request for 0 becomes a seed taken
   from the clock and the process id. The value is logged so that a random run
   can be repeated with --seed. *)
let resolve_seed n =
  let s =
    match n with
    | 0 ->
        let t = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
        let p = Int64.shift_left (Int64.of_int (Unix.getpid ())) 32 in
        let s = Int64.logxor t p in
        if s = 0L then 0x2545F4914F6CDD1DL else s
    | n -> Int64.of_int n
  in
  Logs.info (fun m ->
      m "seed: %Lu%s" s
        (if n = 0 then " (random; pass --seed N to reproduce)" else ""));
  s

(* The model catalogue and the --model policy live in Model. *)
let resolve_model ~dir model_opt =
  let path =
    Result.fold ~ok:Fun.id ~error:failwith (Model.resolve ~dir model_opt)
  in
  (* A path that could not be looked at is reported as that rather than as an
     absent model, which would send a person to download what they have. *)
  let absent () =
    failwith
      (Printf.sprintf
         "model %s is not there. The list subcommand says which models this \
          machine has and the download subcommand fetches one."
         path)
  in
  match Unix.stat path with
  | { Unix.st_kind = Unix.S_REG; _ } -> path
  | _ -> absent ()
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> absent ()
  | exception Unix.Unix_error (e, _, _) ->
      failwith
        (Printf.sprintf "model %s cannot be read: %s" path
           (Unix.error_message e))

open Cmdliner

let logs ?(threaded = false) () =
  let setup style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    if threaded then Logs_threaded.enable ();
    V4.forward_logs ()
  in
  Term.(const setup $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let with_logs ?threaded t = Term.(const (fun () v -> v) $ logs ?threaded () $ t)

let seed =
  Arg.(
    value & opt int 0
    & info [ "seed" ] ~docv:"N"
        ~doc:"The seed for the random sampler. 0 chooses a fresh seed each run.")

let mtp =
  Arg.(
    value
    & vflag true
        [
          ( false,
            info [ "no-mtp" ]
              ~doc:
                "Decode one token at a time rather than letting the model's \
                 multi-token prediction head draft tokens ahead. GLM 5.3 and \
                 Qwen3.8 Flash Next carry such a head, and it is armed by \
                 default on a GPU backend. Greedy decoding gives the same text \
                 either way." );
        ])

let think =
  Arg.(
    value
    & opt (enum [ ("chat", Dsml.Chat); ("thinking", Dsml.Thinking) ]) Dsml.Chat
    & info [ "think" ] ~docv:"MODE"
        ~doc:
          "Whether the agent replies directly (chat) or reasons step by step \
           before acting (thinking).")

(* ---- list -------------------------------------------------------------- *)

let list () =
  run @@ fun _env xdg ->
  let dir = Model.dir xdg in
  Printf.printf "Models (download dir: %s)\n\n" dir;
  Printf.printf "  %-3s %-22s %-19s %s\n" "" "TARGET" "ALIASES" "DESCRIPTION";
  List.iter
    (fun (m : Model.t) ->
      (* A superseded target is dimmed rather than hidden, since it may still
         be the one already on disk. *)
      let line =
        Printf.sprintf "  %-3s %-22s %-19s %s"
          (if Model.present ~dir m then "[*]" else "[ ]")
          m.name
          (String.concat "," m.aliases)
          m.descr
      in
      if m.deprecated then Printf.printf "\027[2m%s\027[0m\n" line
      else print_endline line)
    Model.all;
  (match Model.others ~dir with
  | [] -> ()
  | others ->
      Printf.printf "\nOther GGUF files present (not download targets):\n";
      List.iter (fun f -> Printf.printf "  [*] %s\n" f) others);
  Printf.printf
    "\n[*] = present, [ ] = not downloaded. Dimmed targets are superseded.\n"

let list_cmd =
  let doc = "List the known models and show which are already downloaded." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Show every model that can be downloaded, and mark those already \
         present on this machine. Use it to choose a model to pass to \
         $(b,download).";
    ]
  in
  Cmd.v (Cmd.info "list" ~doc ~man) Term.(const list $ const ())

(* ---- download ---------------------------------------------------------- *)

let download m token =
  run @@ fun env xdg ->
  let dir = Model.dir xdg in
  Model.download ~fs:(Eio.Stdenv.fs env)
    ~proc:(Eio.Stdenv.process_mgr env)
    ~dir ?token m;
  Printf.eprintf
    "\nDone. Models are kept in %s, where every command finds them.\n%!" dir

let token =
  Arg.(
    value
    & opt (some string) None
    & info [ "token" ] ~docv:"TOKEN"
        ~doc:
          "A Hugging Face access token, used for models that require one. When \
           omitted, the HF_TOKEN environment variable and then the local \
           Hugging Face login are tried in turn.")

let download_cmd =
  let doc = "Download a model's weights onto this machine." in
  let man =
    `S Manpage.s_description
    :: `P
         "A model must be present locally before a command can load it. This \
          fetches its files from Hugging Face and stores them where every \
          command looks."
    :: `P
         "Either $(b,uv) or the Hugging Face command line tool must be on your \
          PATH, since the download is done by running it."
    :: `P "Choose a target by name or by alias." :: `S "TARGETS"
    :: List.map
         (fun (m : Model.t) ->
           let label =
             match m.aliases with
             | [] -> m.name
             | a -> Printf.sprintf "%s (%s)" m.name (String.concat ", " a)
           in
           `I (label, m.descr))
         Model.all
  in
  Cmd.v
    (Cmd.info "download" ~doc ~man)
    Term.(const download $ Model.target_arg $ token)

(* ---- chat -------------------------------------------------------------- *)

let chat model_opt system think max_tokens temperature seed ctx_size mtp prompt
    =
  run @@ fun env xdg ->
  let fs = Eio.Stdenv.fs env in
  let model_path = resolve_model ~dir:(Model.dir xdg) model_opt in
  let stdout = Eio.Stdenv.stdout env in
  let engine =
    V4.create ~mtp ~cache:(Xdge.cache_dir xdg)
      ~model:Eio.Path.(fs / model_path)
      ()
  in
  Logs.info (fun m ->
      m "model: %s (vocab %d, %d draft tokens)" (V4.model_name engine)
        (V4.vocab_size engine) (V4.draft_tokens engine));
  V4.generate engine ~system ~think ~max_tokens ~temperature ~ctx_size
    ~seed:(resolve_seed seed)
    ~on_token:(fun s -> Eio.Flow.copy_string s stdout)
    prompt;
  Eio.Flow.copy_string "\n" stdout

let chat_system =
  Arg.(
    value
    & opt string "You are a helpful assistant"
    & info [ "s"; "system" ] ~docv:"TEXT"
        ~doc:"The system prompt that sets the model's role and behaviour.")

let chat_think =
  Arg.(
    value
    & opt (enum [ ("none", `None); ("high", `High); ("max", `Max) ]) `None
    & info [ "think" ] ~docv:"MODE"
        ~doc:
          "How much the model reasons before answering, one of none, high, or \
           max.")

let max_tokens =
  Arg.(
    value & opt int 2048
    & info [ "n"; "max-tokens" ] ~docv:"N"
        ~doc:"The maximum number of tokens to generate.")

let temperature =
  Arg.(
    value & opt float 1.0
    & info [ "t"; "temperature" ] ~docv:"T"
        ~doc:"The sampling temperature. Higher values give more varied replies.")

(* Smaller than an agent's default, since one prompt and its reply fit a small
   window where an agent also accumulates every tool result it reads. *)
let chat_ctx =
  Arg.(
    value & opt int 4096
    & info [ "ctx" ] ~docv:"N"
        ~doc:
          "The context window, in tokens. The prompt and the reply must both \
           fit within it. Memory use grows with it.")

let prompt =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PROMPT" ~doc:"The prompt to send to the model.")

let chat_cmd =
  let doc = "Send a single prompt to a model and print its reply." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Send one prompt, print the model's reply as it is generated, and exit.";
      `P
        "The model keeps nothing between runs and cannot act on your behalf. \
         An agent is what does that.";
    ]
  in
  let term =
    with_logs ~threaded:true
      Term.(
        const chat $ Model.arg $ chat_system $ chat_think $ max_tokens
        $ temperature $ seed $ chat_ctx $ mtp $ prompt)
  in
  Cmd.v (Cmd.info "chat" ~doc ~man) term
