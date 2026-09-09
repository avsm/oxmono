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
  command "run" "Run Crow in enabled rooms and approved direct messages."
    Term.(
      const (fun profile api_key_file ->
          run (fun env sw -> Crowthebot.App.run ~env ~sw ~profile ~api_key_file))
      $ profile $ api_key_file)

let verify =
  command "verify" "Verify Crow's saved Matrix device by comparing emoji."
    Term.(
      const (fun profile user listen room recovery_key_file ->
          run (fun env sw ->
              Crowthebot.App.verify ~env ~sw ~profile ~user ~listen ~room
                ~recovery_key_file))
      $ profile
      $ Arg.(
          value
          & pos 0 (some string) None
          & info [] ~docv:"USER"
              ~doc:"Matrix user to verify with. Defaults to the primary admin.")
      $ Arg.(
          value & flag
          & info [ "listen" ]
              ~doc:
                "Wait for USER to request verification in their Matrix client.")
      $ optional [ "room" ] "Use this joined room ID for verification."
      $ optional [ "recovery-key-file" ]
          "0600 file containing Crow's account recovery key for cross-signing.")

let people =
  command "people" "List the whitelist and people awaiting admin approval."
    Term.(
      const (fun profile ->
          run (fun env sw -> Crowthebot.App.people ~env ~sw ~profile))
      $ profile)

let memory =
  command "memory" "Store, search, retrieve or erase shared profile facts."
    Term.(
      const (fun profile args ->
          run (fun env sw ->
              Crowthebot.App.memory ~env ~sw ~profile
                ~command:(String.concat " " args)))
      $ profile
      $ Arg.(
          non_empty & pos_all string []
          & info [] ~docv:"COMMAND"
              ~doc:"store FACT, search QUERY, list, get ID, or erase ID."))

let day = optional [ "day" ] "UTC day in YYYY-MM-DD format."

let tools =
  command "tools" "Read the persisted tool-use log (100 records per page)."
    Term.(
      const (fun profile day after ->
          run (fun env sw -> Crowthebot.App.tools ~env ~sw ~profile ~day ~after))
      $ profile $ day
      $ Arg.(
          value & opt int 0
          & info [ "after" ] ~doc:"Read records after this log ID."))

let note =
  command "note"
    "Read or generate a daily tool-use note. Defaults to yesterday."
    Term.(
      const (fun profile day generate api_key_file ->
          run (fun env sw ->
              Crowthebot.App.note ~env ~sw ~profile ~day ~generate ~api_key_file))
      $ profile $ day
      $ Arg.(
          value & flag
          & info [ "generate" ]
              ~doc:
                "Generate a missing or outdated note with the configured model.")
      $ api_key_file)

let feeds =
  command "feeds"
    "Inspect, poll or remove feed subscriptions. Add feeds from Matrix."
    Term.(
      const (fun profile args ->
          run (fun env sw ->
              Crowthebot.App.feeds ~env ~sw ~profile
                ~command:(String.concat " " args)))
      $ profile
      $ Arg.(non_empty & pos_all string [] & info [] ~docv:"COMMAND"))

let probe =
  command "probe" "Test the configured model without sending Matrix messages."
    Term.(
      const (fun profile api_key_file ->
          run (fun env sw ->
              Crowthebot.App.probe ~env ~sw ~profile ~api_key_file))
      $ profile $ api_key_file)

let config =
  Cmd.group
    (Cmd.info "config"
       ~doc:"Maintain private named tool configurations outside chat.")
    (List.map
       (Crowthebot.Tool_config.command ~profile ~run:(fun ~profile action ->
            run (fun env sw ->
                Crowthebot.App.configure ~env ~sw ~profile action)))
       [
         Crowthebot.Locations.configuration;
         Crowthebot.Model_config.configuration;
       ])

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "crowthebot" ~version:"dev"
             ~doc:"A Matrix assistant with per-profile authority and context.")
          [
            init;
            login;
            join;
            serve;
            verify;
            people;
            memory;
            tools;
            note;
            feeds;
            probe;
            config;
          ]))
