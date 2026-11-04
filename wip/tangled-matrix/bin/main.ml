(* SPDX-License-Identifier: ISC *)
open Cmdliner
open Tangled_matrix

let read_secret path =
  let s = In_channel.with_open_bin path In_channel.input_all |> String.trim in
  if s = "" then Fmt.failwith "%s is empty" path;
  s

let run () state bot password_file recovery_key_file homeserver accounts rooms
    spindles interval edit_interval appview allow_unencrypted plc =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let state = Eio.Path.(Eio.Stdenv.fs env / state) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 state;
  let saved = Eio.Path.(state / "recovery-key") in
  let recovery_key =
    match recovery_key_file with
    | Some f -> Some (read_secret f)
    | None ->
        if Eio.Path.is_file saved then Some (String.trim (Eio.Path.load saved))
        else None
  in
  let config =
    {
      Bot.user = bot;
      password = read_secret password_file;
      homeserver;
      recovery_key;
      allow_unencrypted;
    }
  in
  let api =
    Tangled.Api.create ~sw ~env ~app_name:"tangled-matrix" ?plc
      ~pds:"https://bsky.social" ~http:(Fetch_httpz.std env) ()
  in
  let accounts =
    List.map
      (fun a ->
        if String.starts_with ~prefix:"did:" a then a
        else Tangled.Api.resolve_handle api a)
      accounts
  in
  let bot = Bot.connect ~sw ~env ~state config in
  Logs.app (fun m ->
      m "Posting as %s, device %s, for %s" (Bot.user_id bot) (Bot.device_id bot)
        (String.concat ", " accounts));
  let messages = Messages.load Eio.Path.(state / "messages.json") in
  Poster.run ~sw ~clock:env#clock ~api ~bot ~messages ~accounts ~rooms ~spindles
    ~interval ~edit_interval ?appview ()

let setup_log =
  let setup style level =
    Fmt_tty.setup_std_outputs ?style_renderer:style ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Term.(const setup $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let state =
  Arg.(
    value
    & opt string "tangled-matrix-state"
    & info [ "state-dir" ] ~docv:"DIR"
        ~doc:
          "Where the bot's session, device keys, encryption state and messages \
           are kept. They are not encrypted at rest, so the directory is made \
           readable only by its owner.")

let bot =
  Arg.(
    required
    & opt (some string) None
    & info [ "bot" ] ~docv:"USER"
        ~doc:"The bot's Matrix user ID, such as $(i,@ci:example.org).")

let password_file =
  Arg.(
    required
    & opt (some file) None
    & info [ "password-file" ] ~docv:"FILE"
        ~doc:
          "A file holding the bot's password, used to log in the first time \
           and to create its cross-signing keys.")

let recovery_key_file =
  Arg.(
    value
    & opt (some file) None
    & info [ "recovery-key-file" ] ~docv:"FILE"
        ~doc:
          "A file holding the bot account's recovery key or passphrase, to \
           verify a new device of an account that has cross-signing keys. \
           Without it, the key the bot created and saved in its state \
           directory, if any, is used.")

let homeserver =
  Arg.(
    value
    & opt (some string) None
    & info [ "homeserver" ] ~docv:"URL"
        ~doc:
          "The bot's homeserver. Without it, it is found from the bot's server \
           name through $(i,.well-known/matrix/client).")

let accounts =
  Arg.(
    non_empty & opt_all string []
    & info [ "account" ] ~docv:"HANDLE"
        ~doc:
          "Report the repositories that this Tangled account, by handle or \
           DID, binds to the bot with $(b,tangled pipeline matrix set). A \
           binding counts only when the account owns or collaborates on the \
           repository. Repeat the option for more accounts.")

let rooms =
  Arg.(
    value & opt_all string []
    & info [ "room" ] ~docv:"ROOM"
        ~doc:
          "Report every repository of the $(b,--account) accounts that has CI \
           in ROOM, by alias or ID, without bindings, as a firehose. The bot \
           must be invited to it. Repeat the option for more rooms.")

let spindles =
  Arg.(
    value & opt_all string []
    & info [ "spindle" ] ~docv:"SPINDLE"
        ~doc:
          "Report only repositories whose CI runs on SPINDLE, by host such as \
           $(i,spindle.example.org) or by DID. Repeat the option for more \
           spindles. Without it, repositories on any spindle are reported.")

let interval =
  Arg.(
    value & opt float 15.
    & info [ "interval" ] ~docv:"SECONDS"
        ~doc:"Poll each repository's spindle every SECONDS.")

let edit_interval =
  Arg.(
    value & opt float 10.
    & info [ "edit-interval" ] ~docv:"SECONDS"
        ~doc:"Edit a pipeline's message at most once every SECONDS.")

let appview =
  Arg.(
    value
    & opt (some string) None
    & info [ "appview" ] ~docv:"URL"
        ~doc:"Link to pipelines on URL, $(i,https://tangled.org) by default.")

let allow_unencrypted =
  Arg.(
    value & flag
    & info [ "allow-unencrypted" ]
        ~doc:"Also post to rooms that are not end-to-end encrypted.")

let plc =
  Arg.(
    value
    & opt (some string) None
    & info [ "plc" ] ~docv:"URL" ~docs:Manpage.s_none
        ~doc:"Resolve did:plc identifiers through URL.")

let cmd =
  Cmd.v
    (Cmd.info "tangled-matrix"
       ~doc:"Report Tangled CI pipelines in Matrix rooms"
       ~man:
         [
           `S Manpage.s_description;
           `P
             "$(tname) logs in to Matrix as a bot and keeps one message per \
              pipeline up to date, editing it in place as the pipeline's \
              workflows start, change and finish. Maintainers choose the room \
              for a repository with $(b,tangled pipeline matrix set), and \
              invite the bot to it.";
           `P
             "The bot verifies its own device with the account's cross-signing \
              keys, creating them and a recovery key on first use if the \
              account has none. It posts only in rooms it has joined and knows \
              to be encrypted, unless $(b,--allow-unencrypted).";
         ])
    Term.(
      const run $ setup_log $ state $ bot $ password_file $ recovery_key_file
      $ homeserver $ accounts $ rooms $ spindles $ interval $ edit_interval
      $ appview $ allow_unencrypted $ plc)

let () = exit (Cmd.eval cmd)
