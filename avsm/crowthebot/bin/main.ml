open Cmdliner

let profile =
  Arg.(
    value & opt string "crowthebot"
    & info [ "profile" ] ~docv:"NAME"
        ~doc:"Isolated Matrix profile under XDG_DATA_HOME/matrix/profiles.")

let required names doc =
  Arg.(required & opt (some string) None & info names ~doc)

let optional names doc = Arg.(value & opt (some string) None & info names ~doc)

let api_key_file =
  optional [ "api-key-file" ] "0600 file containing the model API key."

let run action =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Warning);
  try
    Eio_main.run (fun env -> Eio.Switch.run (fun sw -> action env sw));
    0
  with
  | Invalid_argument message | Failure message ->
      prerr_endline message;
      1
  | Eio.Io (Openrouter.E (Openrouter.Http_error { status; _ }), _) ->
      Printf.eprintf
        "Model returned HTTP %d. Check model, tool support and authentication.\n"
        status;
      1
  | Eio.Time.Timeout ->
      prerr_endline "Operation timed out.";
      1
  | Eio.Cancel.Cancelled _ -> 130
  | _ ->
      prerr_endline
        "Operation failed. Check the profile and network connection.";
      1

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let init =
  command "init" "Create a profile with its immutable primary admin."
    Term.(
      const (fun profile admin homeserver ->
          run (fun env sw ->
              Crowthebot.App.init ~env ~sw ~profile ~admin ~homeserver))
      $ profile
      $ required [ "admin" ] "Primary admin's full Matrix ID."
      $ required [ "homeserver" ] "HTTPS Matrix homeserver URL.")

let login =
  command "login" "Log in and store this bot's Matrix device keys."
    Term.(
      const (fun profile username password_file ->
          run (fun env sw ->
              Crowthebot.App.login ~env ~sw ~profile ~username ~password_file))
      $ profile
      $ required [ "username" ] "Bot account's full @user:server Matrix ID."
      $ optional [ "password-file" ]
          "0600 password file. Otherwise prompt without echo.")

let join =
  command "join" "Join and enable a room in this profile."
    Term.(
      const (fun profile room ->
          run (fun env sw -> Crowthebot.App.join ~env ~sw ~profile ~room))
      $ profile
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ROOM"))

let serve =
  command "run" "Run Crow in this profile's explicitly enabled rooms."
    Term.(
      const (fun profile api_key_file ->
          run (fun env sw -> Crowthebot.App.run ~env ~sw ~profile ~api_key_file))
      $ profile $ api_key_file)

let people =
  command "people" "List the whitelist and people awaiting admin approval."
    Term.(
      const (fun profile ->
          run (fun env sw -> Crowthebot.App.people ~env ~sw ~profile))
      $ profile)

let blogroll =
  command "blogroll" "Read the public blogroll without a Matrix login."
    Term.(
      const (fun query ->
          run (fun env _ -> Crowthebot.App.blogroll ~env ~query))
      $ Arg.(value & pos 0 string "" & info [] ~docv:"QUERY"))

let probe =
  command "probe" "Test the configured model without sending Matrix messages."
    Term.(
      const (fun profile api_key_file ->
          run (fun env sw ->
              Crowthebot.App.probe ~env ~sw ~profile ~api_key_file))
      $ profile $ api_key_file)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "crowthebot" ~version:"dev"
             ~doc:"A Matrix assistant with per-profile authority and context.")
          [ init; login; join; serve; people; blogroll; probe ]))
