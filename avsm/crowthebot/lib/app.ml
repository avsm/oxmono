module Context = Matrix_bot.Context
module Id = Matrix_proto.Id
module Log = Diagnostics.Log

let with_timeout env seconds f =
  Eio.Time.Timeout.run_exn
    (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) seconds)
    f

let with_profile ~env ~sw ~profile f =
  let dir = Profile.directory env profile in
  Profile.with_lock dir (fun () ->
      let dir = Eio.Path.open_subtree ~sw dir in
      let config = Profile.load dir in
      let store = Profile.database ~sw dir ~admin:config.admin in
      f dir config store)

let init ~env ~sw ~profile ~admin ~homeserver =
  let dir = Profile.directory env profile in
  Profile.with_lock dir (fun () ->
      Profile.init ~sw dir (Config.default ~admin ~homeserver));
  Printf.printf
    "Created %s\nEdit crowthebot.json to choose a model, prompt or plugins.\n"
    (Eio.Path.native_exn dir)

let password_prompt () =
  if not (Unix.isatty Unix.stdin) then
    invalid_arg "use --password-file outside an interactive terminal";
  let before = Unix.tcgetattr Unix.stdin in
  prerr_string "Matrix password: ";
  flush stderr;
  Fun.protect
    ~finally:(fun () ->
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH before;
      prerr_newline ())
    (fun () ->
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH { before with c_echo = false };
      read_line ())

let connect ~env ~sw ~profile config ?username ?password () =
  Log.info (fun m -> m "Connecting Matrix profile=%S" profile);
  let store =
    Matrix_client.Profile_store.create
      ~xdg:(Xdge.create (Eio.Stdenv.fs env) "matrix")
      ~profile
  in
  (match Matrix_client.Profile_store.load_session store with
  | Error _ -> failwith "stored Matrix session is unreadable"
  | Ok (Some session) ->
      if Id.User_id.to_string session.server.user_id = config.Config.admin then
        invalid_arg "use a separate Matrix account for the bot";
      if Uriz.to_string session.server.homeserver <> config.Config.homeserver
      then invalid_arg "stored session belongs to a different homeserver"
  | Ok None ->
      if username = None || password = None then
        invalid_arg "no saved Matrix session. Run crowthebot login first");
  match
    Context.connect ~env ~sw ~profile
      ~homeserver:(Uriz.of_string_exn config.homeserver)
      ?username ?password ~persist_events:false ()
  with
  | Error _ ->
      failwith
        "Matrix connection failed. Check the profile, login and homeserver"
  | Ok ctx ->
      if Id.User_id.to_string (Context.user_id ctx) = config.admin then
        invalid_arg "use a separate Matrix account for the bot";
      Log.info (fun m ->
          m "Matrix connected user=%S encryption=%b"
            (Id.User_id.to_string (Context.user_id ctx))
            (Context.encryption ctx <> None));
      ctx

let login ~env ~sw ~profile ~username ~password_file =
  with_profile ~env ~sw ~profile @@ fun _ config _ ->
  ignore (Id.User_id.of_string_exn username);
  if username = config.admin then
    invalid_arg "use a separate Matrix account for the bot";
  let password =
    match password_file with
    | Some path -> Profile.read_secret path
    | None -> password_prompt ()
  in
  let ctx = connect ~env ~sw ~profile config ~username ~password () in
  Context.save ctx;
  Printf.printf "Logged in as %s. Use crowthebot join to choose a room.\n"
    (Id.User_id.to_string (Context.user_id ctx))

let join ~env ~sw ~profile ~room =
  with_profile ~env ~sw ~profile @@ fun _ config store ->
  let target =
    if String.starts_with ~prefix:"#" room then
      `Room_alias (Id.Room_alias.of_string_exn room)
    else `Room_id (Id.Room_id.of_string_exn room)
  in
  let ctx = connect ~env ~sw ~profile config () in
  Fun.protect ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> Context.save ctx))
  @@ fun () ->
  let room =
    Matrix_eio.Rooms.join (Context.client ctx) ~room_id_or_alias:target ()
  in
  Store.add_room store (Id.Room_id.to_string room);
  Printf.printf "Joined and enabled %s\n" (Id.Room_id.to_string room)

let fetch env =
  Fetch_httpz.std ~connect_timeout:(Duration.of_sec 5)
    ~idle_timeout:(Duration.of_sec 30) ~cookies:`Off env

let now env =
  let clock = Eio.Stdenv.mono_clock env in
  fun () -> Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) /. 1e9

let feed_tools env store =
  let client =
    Fetch_httpz.std ~cookies:`Off
      ~connect:(Feed_http.public_connect (Eio.Stdenv.net env))
      ~connect_timeout:(Duration.of_sec 5) ~idle_timeout:(Duration.of_sec 15)
      env
  in
  Feeds.create ~state:(Store.feeds store)
    ~download:
      (Feed_http.create ~fetch:client ~clock:(Eio.Stdenv.mono_clock env))

let with_secrets ~env ~sw ~profile ~dir f =
  Secret_store.with_xdg ~sw ~fs:(Eio.Stdenv.fs env) ~profile ~profile_dir:dir f

let configure ~env ~sw ~profile action =
  let dir = Profile.directory env profile in
  with_secrets ~env ~sw ~profile ~dir action

let model ~env ~sw ~profile ~dir ~store config client api_key_file =
  let client = Trace.wrap (Store.trace store) client in
  let fallback () =
    Openrouter.of_fetch ~base_url:config.Config.base_url
      ~max_response_bytes:(1024 * 1024)
      ?api_key:(Option.map Profile.read_secret api_key_file)
      client
  in
  if api_key_file <> None then fallback ()
  else
    with_secrets ~env ~sw ~profile ~dir (fun secrets ->
        match Secret_store.selected secrets ~tool:"openrouter" with
        | Some (_, settings) -> Model_config.initialize ~fetch:client settings
        | None ->
            if Secret_store.list secrets ~tool:"openrouter" <> [] then
              invalid_arg
                "Select a named OpenRouter configuration with crowthebot \
                 config openrouter select.";
            fallback ())

let calendar_tools ~env ~sw ~profile ~dir store =
  let fetch = fetch env and clock = Eio.Stdenv.clock env in
  with_secrets ~env ~sw ~profile ~dir (fun secrets ->
      let names = Secret_store.list secrets ~tool:"calendar" in
      let default = List.find_opt snd names |> Option.map fst in
      let sources =
        List.map
          (fun (name, _) ->
            let settings =
              Option.get (Secret_store.get secrets ~tool:"calendar" ~name)
            in
            (name, Calendar_source.initialize ~sw ~fetch ~clock settings))
          names
      in
      Calendars.create ~state:(Store.calendars store) ~sources ~default)

let caldav_tools ~env ~sw ~profile ~dir store =
  let fetch = fetch env and clock = Eio.Stdenv.clock env in
  with_secrets ~env ~sw ~profile ~dir (fun secrets ->
      let names = Secret_store.list secrets ~tool:"caldav" in
      let default = List.find_opt snd names |> Option.map fst in
      let sources =
        List.map
          (fun (name, _) ->
            let settings =
              Option.get (Secret_store.get secrets ~tool:"caldav" ~name)
            in
            (name, Caldav_source.initialize ~sw ~fetch ~clock settings))
          names
      in
      Caldav_tools.create ~state:(Store.caldav store) ~sources ~default)

let email_tools ~env ~sw ~profile ~dir store =
  let fetch = fetch env and clock = Eio.Stdenv.clock env in
  with_secrets ~env ~sw ~profile ~dir (fun secrets ->
      let load tool initialize =
        let names = Secret_store.list secrets ~tool in
        let default = List.find_opt snd names |> Option.map fst in
        let sources =
          List.map
            (fun (name, _) ->
              let settings =
                Option.get (Secret_store.get secrets ~tool ~name)
              in
              (name, initialize ~sw ~fetch ~clock settings))
            names
        in
        (sources, default)
      in
      let readers, default_reader =
        load "email-ro" Email_source.initialize_reader
      in
      let writers, default_writer =
        load "email-rw" Email_source.initialize_writer
      in
      Emails.create ~state:(Store.emails store) ~readers ~writers
        ~default_reader ~default_writer)

let location_tools ~env ~sw ~profile ~dir store =
  let client = fetch env in
  let clock = Eio.Stdenv.mono_clock env in
  let now =
    let clock = Eio.Stdenv.clock env in
    fun () -> Eio.Time.now clock
  in
  let load path =
    let profiles = Filename.dirname (Unix.realpath (Eio.Path.native_exn dir)) in
    let native = Unix.realpath path in
    if native = profiles || String.starts_with ~prefix:(profiles ^ "/") native
    then
      invalid_arg
        "OwnTracks configuration must be outside Matrix profile data and tool \
         workspaces.";
    Owntracks_source.load_config path
  in
  with_secrets ~env ~sw ~profile ~dir (fun secrets ->
      let sources = Secret_store.list secrets ~tool:"owntracks" in
      let default = List.find_opt snd sources |> Option.map fst in
      let sources =
        List.map
          (fun (name, _) ->
            let settings =
              Option.get (Secret_store.get secrets ~tool:"owntracks" ~name)
            in
            ( name,
              Owntracks_source.initialize ~load ~fetch:client ~clock ~now
                settings ))
          sources
      in
      Locations.create ~state:(Store.locations store) ~sources ~default)

let complete env (config : Config.t) client =
  let clock = Eio.Stdenv.mono_clock env in
  fun messages tools ->
    let started = Eio.Time.Mono.now clock in
    Log.info (fun m ->
        m "Model request started model=%S messages=%d tools=%d" config.model
          (List.length messages) (List.length tools));
    try
      Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 90.) @@ fun () ->
      let compacting =
        match Trace.current () with
        | Some context -> context.source = "context-compaction"
        | None -> false
      in
      let max_tokens =
        if compacting then max 4096 config.max_tokens else config.max_tokens
      in
      let request =
        Openrouter.Chat.request ~model:config.Config.model ~max_tokens ~messages
          ?tools:(if tools = [] then None else Some tools)
          ?parallel_tool_calls:(if tools = [] then None else Some false)
          ()
      in
      let result = Openrouter.Chat.complete client request in
      match
        List.find_opt
          (fun (c : Openrouter.Chat.choice) -> c.index = 0)
          result.choices
      with
      | None -> failwith "no model choice"
      | Some choice ->
          Log.info (fun m ->
              m
                "Model request completed elapsed_ms=%.0f text_bytes=%d \
                 tool_calls=%d finish_reason=%s"
                (Mtime.Span.to_float_ns
                   (Mtime.span started (Eio.Time.Mono.now clock))
                /. 1e6)
                (Option.fold ~none:0 ~some:String.length choice.text)
                (List.length choice.tool_calls)
                (match choice.finish_reason with
                | Some Openrouter.Chat.Stop -> "stop"
                | Some Length -> "length"
                | Some Tool_calls -> "tool_calls"
                | Some Content_filter -> "content_filter"
                | Some (Other _) -> "other"
                | None -> "absent"));
          if compacting && choice.finish_reason = Some Openrouter.Chat.Length
          then raise Diagnostics.Model_output_limit;
          (choice.text, choice.tool_calls)
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Log.err (fun m -> m "Model request failed: %s" (Diagnostics.error exn));
      Printexc.raise_with_backtrace exn bt

let people ~env ~sw ~profile =
  with_profile ~env ~sw ~profile @@ fun _ _ store ->
  List.iter (fun p -> print_endline (Engine.person_line p)) (Store.people store)

let memory ~env ~sw ~profile ~command =
  with_profile ~env ~sw ~profile @@ fun _ _ store ->
  let name, arguments =
    match Memory.command command with
    | Ok call -> call
    | Error message -> invalid_arg message
  in
  let actor = Store.admin store in
  let output =
    Audit.run store ~actor ~room:"local" ~event:"" ~source:"command" ~call_id:""
      ~tool:name ~arguments (fun () ->
        Memory.invoke
          (Memory.for_request store ~actor ~room:"local" ~event:""
             ~source:"command")
          name arguments)
  in
  print_endline output

let tools ~env ~sw ~profile ~day ~after =
  with_profile ~env ~sw ~profile @@ fun _ _ store ->
  let day = Option.value ~default:(Store.today store) day in
  List.iter
    (fun use -> print_endline (Audit.line use))
    (Store.tool_uses store ~day ~after ~through:max_int ~limit:100)

let note ~env ~sw ~profile ~day ~generate ~api_key_file =
  with_profile ~env ~sw ~profile @@ fun dir config store ->
  let day = Option.value ~default:(Store.yesterday store) day in
  let note =
    if generate then
      let client =
        model ~env ~sw ~profile ~dir ~store config (fetch env) api_key_file
      in
      Some
        (Daily.generate ~store ~config
           ~complete:(complete env config client)
           ~day)
    else Store.get_note store day
  in
  match note with
  | Some note -> print_endline (Daily.render note)
  | None -> print_endline ("No daily note for " ^ day ^ " yet.")

let feeds ~env ~sw ~profile ~command =
  with_profile ~env ~sw ~profile @@ fun _ _ store ->
  let name, arguments =
    match Feeds.command command with
    | Ok call -> call
    | Error message -> invalid_arg message
  in
  if name = "feeds_add" then
    invalid_arg
      "Add feeds from a Matrix room or DM to set their delivery source.";
  let actor = Store.admin store in
  let capability =
    Feeds.for_request (feed_tools env store) ~actor ~room:"local" ~event:""
  in
  print_endline
    (Audit.run store ~actor ~room:"local" ~event:"" ~source:"command"
       ~call_id:"" ~tool:name ~arguments (fun () ->
         Feeds.invoke capability name arguments))

let probe ~env ~sw ~profile ~api_key_file ~target =
  with_profile ~env ~sw ~profile @@ fun dir config store ->
  let model_check () =
    let client =
      model ~env ~sw ~profile ~dir ~store config (fetch env) api_key_file
    in
    Trace.with_context
      {
        actor = Store.admin store;
        room = "local";
        event = "";
        source_event = "";
        source = "probe";
      }
    @@ fun () ->
    let text, calls =
      complete env config client
        [ Openrouter.Message.user "Reply with CROW_OK." ]
        []
    in
    match (text, calls) with
    | Some text, [] when String.trim text <> "" ->
        print_endline (Plugin.clip ~bytes:1024 text)
    | _ -> failwith "Model probe returned no text or unexpected tool calls."
  in
  let caldav_check selected =
    let configurations =
      with_secrets ~env ~sw ~profile ~dir (fun secrets ->
          let names =
            match selected with
            | None -> Secret_store.list secrets ~tool:"caldav" |> List.map fst
            | Some name ->
                Secret_store.validate_name name;
                [ name ]
          in
          List.map
            (fun name -> (name, Secret_store.get secrets ~tool:"caldav" ~name))
            names)
    in
    let client = fetch env and clock = Eio.Stdenv.clock env in
    let sources =
      List.map
        (fun (name, settings) ->
          ( name,
            fun () ->
              match settings with
              | Some settings ->
                  Caldav_source.initialize ~sw ~fetch:client ~clock settings
              | None ->
                  invalid_arg
                    "Connection is not configured. Use config caldav set NAME."
          ))
        configurations
    in
    Caldav_probe.run ~emit:print_endline sources
  in
  let model_ok =
    match target with
    | `Caldav _ -> true
    | `All | `Model -> (
        try
          model_check ();
          true
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            Printf.printf "Model: FAILED (%s)\n%!" (Diagnostics.error exn);
            false)
  in
  let caldav_ok =
    match target with
    | `Model -> true
    | (`All | `Caldav _) as target -> (
        try
          caldav_check
            (match target with `Caldav name -> Some name | _ -> None)
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            Printf.printf "CalDAV: FAILED (%s)\n%!" (Diagnostics.error exn);
            false)
  in
  if not (model_ok && caldav_ok) then
    failwith "One or more probe checks failed."

let verify ~env ~sw ~profile ~user ~listen ~room ~recovery_key_file =
  with_profile ~env ~sw ~profile @@ fun _ config _ ->
  if not (Unix.isatty Unix.stdin) then
    invalid_arg "verification requires an interactive terminal";
  let target =
    Id.User_id.of_string_exn (Option.value ~default:config.admin user)
  in
  let room = Option.map Id.Room_id.of_string_exn room in
  let ctx = connect ~env ~sw ~profile config () in
  Fun.protect ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> Context.save ctx))
  @@ fun () ->
  let encryption =
    match Context.encryption ctx with
    | Some encryption -> encryption
    | None -> failwith "verification requires an encrypted Matrix device"
  in
  let client = Context.client ctx in
  let private_identity =
    Option.map
      (fun path ->
        with_timeout env 90. (fun () ->
            let secret_store =
              Matrix_eio.Secrets.open_secret_store client
                ~credential:(Profile.read_secret path)
            in
            let identity =
              Matrix_eio.Secrets.import_cross_signing secret_store ~encryption
            in
            let module Cs = Matrix_client.Cross_signing in
            if
              Cs.master_secret identity = None
              || Cs.self_signing_secret identity = None
              || Cs.user_signing_secret identity = None
            then
              failwith "Crow's recovery store has incomplete cross-signing keys";
            identity))
      recovery_key_file
  in
  Printf.printf "Crow account: %s\nSaved device: %s\n%!"
    (Id.User_id.to_string (Context.user_id ctx))
    (Id.Device_id.to_string (Matrix_eio.Client.device_id client));
  if private_identity = None then
    print_endline
      "Device-only verification. For user verification across clients, supply \
       --recovery-key-file with Crow's account recovery key.";
  let input = Eio.Buf_read.of_flow ~max_size:4096 (Eio.Stdenv.stdin env) in
  let ask question =
    print_string question;
    flush stdout;
    try Verification.affirmative (Eio.Buf_read.line input)
    with End_of_file -> false
  in
  let result =
    Verification.run ~env ~client ~encryption ?private_identity ~target ~listen
      ?room ~ask ()
  in
  Matrix_eio.Encryption.save encryption;
  match result with
  | Matrix_eio.Verification_service.Verified { user_id; device_id } ->
      Printf.printf "Verified %s%s. Saved this device's trust.\n"
        (Id.User_id.to_string user_id)
        (Option.fold ~none:""
           ~some:(fun d -> " (" ^ Id.Device_id.to_string d ^ ")")
           device_id)
  | Matrix_eio.Verification_service.Publication_failed _ ->
      failwith
        "SAS matched, but publishing the cross-signing signature failed. \
         Verification is incomplete. Retry after checking the homeserver."
  | Matrix_eio.Verification_service.Cancelled { code; _ } ->
      failwith
        ("Verification cancelled: "
        ^ Matrix_client.Verification.Cancel_code.reason code)

let run ~env ~sw ~profile ~api_key_file =
  Log.info (fun m -> m "Starting Crow profile=%S" profile);
  with_profile ~env ~sw ~profile @@ fun dir config store ->
  Log.info (fun m ->
      m "Profile loaded admin=%S enabled_rooms=%d" config.admin
        (List.length (Store.rooms store)));
  let ctx = connect ~env ~sw ~profile config () in
  let client = fetch env in
  let model = model ~env ~sw ~profile ~dir ~store config client api_key_file in
  let matrix_state = ref (fun () -> None) in
  let engine =
    Engine.create ~config ~store
      ~self:(Id.User_id.to_string (Context.user_id ctx))
      ~plugins:[]
      ~complete:(complete env config model)
      ~now:(now env)
    |> fun engine ->
    Engine.with_feeds engine (feed_tools env store) |> fun engine ->
    Engine.with_calendars engine (calendar_tools ~env ~sw ~profile ~dir store)
    |> fun engine ->
    Engine.with_caldav engine (caldav_tools ~env ~sw ~profile ~dir store)
    |> fun engine ->
    Engine.with_emails engine (email_tools ~env ~sw ~profile ~dir store)
    |> fun engine ->
    Engine.with_locations engine (location_tools ~env ~sw ~profile ~dir store)
    |> Engine.with_room_observation
    |> fun engine ->
    Engine.with_matrix engine
      (Matrix_rooms.create ~store ~state:(fun () -> !matrix_state ()))
  in
  Log.info (fun m -> m "Model and tool configurations loaded");
  let self = Id.User_id.to_string (Context.user_id ctx) in
  let direct_peer bot room ~actor =
    let room_id = Matrix_bot.Room.id room in
    let saved = Store.direct_peer store (Id.Room_id.to_string room_id) in
    let marked = Matrix_bot.Room.is_dm room || saved <> None in
    let admin = if actor = Store.admin store then Some actor else None in
    if (not marked) && admin = None then begin
      Log.info (fun m ->
          m "DM check room=%S marked=false; requires an enabled group room"
            (Id.Room_id.to_string room_id));
      None
    end
    else begin
      (match Matrix_bot.Room.sync_members room with
      | Ok () -> ()
      | Error error ->
          Log.err (fun m ->
              m "DM membership fetch failed room=%S error=%s"
                (Id.Room_id.to_string room_id)
                (Diagnostics.matrix_error
                   (Matrix_eio.Error.of_client_error error)));
          failwith "could not check DM membership");
      let state =
        Matrix_eio.Sync_service.state
          (Matrix_ui.Runtime.sync_service (Matrix_bot.Bot.runtime bot))
      in
      let complete =
        match Matrix_client.Base_client.find_room state room_id with
        | Some info -> info.members_complete
        | None -> false
      in
      let members = Matrix_bot.Room.members room in
      let peer =
        Address.direct_peer ?admin ~self ~marked ~complete
          (List.map Id.User_id.to_string members)
      in
      let matches = saved = None || saved = peer in
      Log.info (fun m ->
          m
            "DM check room=%S marked=%b admin_fallback=%b complete=%b \
             members=%d peer=%s saved_peer_matches=%b"
            (Id.Room_id.to_string room_id)
            marked (admin <> None) complete (List.length members)
            (Option.fold ~none:"none" ~some:(Printf.sprintf "%S") peer)
            matches);
      if matches then begin
        Option.iter
          (fun peer ->
            if saved = None && (Store.person store peer).allowed then
              Store.add_direct_room store
                ~room:(Id.Room_id.to_string room_id)
                ~peer)
          peer;
        peer
      end
      else None
    end
  in
  let on_message bot ({ message; original } : Matrix_input.t) =
    if message.content.kind = Matrix_ui.Presentation.Text then begin
      let e = message.envelope in
      let event : Engine.event =
        {
          room = Id.Room_id.to_string (Matrix_bot.Room.id e.room);
          sender = Id.User_id.to_string e.sender;
          id = Id.Event_id.to_string e.event_id;
          body =
            Address.body ~reply:(message.reply_to <> None) message.content.body;
        }
      in
      Log.info (fun m ->
          m "Received text event=%S room=%S sender=%S bytes=%d edit_of=%s"
            event.id event.room event.sender (String.length event.body)
            (Option.fold ~none:"none" ~some:Id.Event_id.to_string original));
      let send text =
        Log.info (fun m ->
            m "Reply queued event=%S room=%S bytes=%d" event.id event.room
              (String.length text));
        match
          Matrix_bot.Sent.await
            (Matrix_bot.Event.reply
               { e with event_id = Option.value ~default:e.event_id original }
               ~html:(Rich_text.html text) text)
        with
        | Matrix_bot.Sent.Sent sent_id ->
            Log.info (fun m ->
                m "Reply sent event=%S reply=%S" event.id
                  (Id.Event_id.to_string sent_id))
        | outcome ->
            Log.err (fun m ->
                m "Reply failed event=%S error=%s" event.id
                  (Diagnostics.sent outcome));
            failwith "Matrix reply could not be confirmed"
      in
      try
        with_timeout env 180. (fun () ->
            Typing.with_session ~clock:(Eio.Stdenv.mono_clock env)
              ~room:event.room ~event:event.id ~set:(fun typing ->
                Matrix_eio.Typing.set_typing (Context.client ctx)
                  ~room_id:(Matrix_bot.Room.id e.room)
                  ~typing
                  ?timeout:(if typing then Some 30000 else None)
                  ())
            @@ fun typing ->
            let direct =
              direct_peer bot e.room ~actor:event.sender = Some event.sender
            in
            let mentioned =
              Address.mentions ~self message.presentation.raw.content
            in
            let send text =
              Typing.stop typing;
              if not (Store.person store event.sender).allowed then
                failwith "access revoked before reply delivery";
              if
                direct
                && direct_peer bot e.room ~actor:event.sender
                   <> Some event.sender
              then failwith "DM membership changed during the request";
              send text
            in
            Engine.handle engine ~mentioned ~direct
              ~on_accept:(fun () -> Typing.start typing)
              ~send event)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
          Log.err (fun m ->
              m
                "Crow request failed event=%S room=%S error=%s; context was \
                 not advanced"
                event.id event.room (Diagnostics.error exn))
    end
    else
      Log.info (fun m ->
          m "Ignored non-text message event=%S"
            (Id.Event_id.to_string message.envelope.event_id))
  in
  let watchers = Hashtbl.create 8 in
  let spec =
    Matrix_bot.Bot.v ~name:"crowthebot" ~prefix:"!" ~auto_join:false
      ~ignore_notices:true ~ignore_own:true ~backlog:`Skip ~queue_depth:32 ()
    |> Matrix_input.register on_message
    |> Matrix_bot.Bot.on_invite (fun bot invitation ->
        let runtime = Matrix_bot.Bot.runtime bot in
        let state =
          Matrix_eio.Sync_service.state (Matrix_ui.Runtime.sync_service runtime)
        in
        Log.info (fun m ->
            m "Invitation room=%S inviter=%s"
              (Id.Room_id.to_string invitation.room_id)
              (Option.fold ~none:"unknown"
                 ~some:(fun user ->
                   Printf.sprintf "%S" (Id.User_id.to_string user))
                 invitation.inviter));
        match
          ( invitation.inviter,
            Matrix_client.Base_client.find_room state invitation.room_id )
        with
        | Some inviter, Some info
          when (info.is_dm || Id.User_id.to_string inviter = Store.admin store)
               && (Store.person store (Id.User_id.to_string inviter)).allowed ->
            with_timeout env 30. (fun () ->
                match Matrix_ui.Runtime.join runtime invitation.room_id with
                | Error error ->
                    Log.err (fun m ->
                        m
                          "DM join failed room=%S inviter=%S error=%s; \
                           invitation remains pending"
                          (Id.Room_id.to_string invitation.room_id)
                          (Id.User_id.to_string inviter)
                          (Diagnostics.matrix_error
                             (Matrix_eio.Error.of_client_error error)))
                | Ok () ->
                    if info.is_dm then
                      Store.add_direct_room store
                        ~room:(Id.Room_id.to_string invitation.room_id)
                        ~peer:(Id.User_id.to_string inviter);
                    Log.info (fun m ->
                        m "Accepted invitation marked_dm=%b" info.is_dm))
        | _ ->
            Log.info (fun m ->
                m
                  "Ignored invitation: requires the admin or a direct invite \
                   from an approved account"))
    |> Matrix_bot.Bot.on_join (fun bot room ->
        let id = Matrix_bot.Room.id room in
        Log.info (fun m ->
            m
              "Watching room=%S marked_dm=%b enabled=%b saved_dm=%b; existing \
               timeline skipped"
              (Id.Room_id.to_string id)
              (Matrix_bot.Room.is_dm room)
              (List.mem (Id.Room_id.to_string id) (Store.rooms store))
              (Store.direct_peer store (Id.Room_id.to_string id) <> None));
        if Diagnostics.enabled () then begin
          Option.iter (fun stop -> stop ()) (Hashtbl.find_opt watchers id);
          let stop =
            Diagnostics.watch_room ~sw ~self:(Context.user_id ctx)
              ~cache:
                (Matrix_ui.Runtime.event_cache (Matrix_bot.Bot.runtime bot))
              ~room:id
          in
          Hashtbl.replace watchers id stop
        end;
        if
          List.mem
            (Id.Room_id.to_string (Matrix_bot.Room.id room))
            (Store.rooms store)
        then
          List.iter
            (fun user ->
              if not (Id.User_id.equal user (Context.user_id ctx)) then
                Store.observe store (Id.User_id.to_string user))
            (Matrix_bot.Room.members room))
    |> Matrix_bot.Bot.on_leave (fun _ id ->
        Option.iter (fun stop -> stop ()) (Hashtbl.find_opt watchers id);
        Hashtbl.remove watchers id;
        Log.info (fun m -> m "Left room=%S" (Id.Room_id.to_string id)))
    |> Matrix_bot.Bot.on_sync (fun _ state ->
        match state with
        | Matrix_ui.Runtime.Failed _ ->
            Log.err (fun m -> m "Matrix sync failed; waiting for retry or stop")
        | _ -> Log.info (fun m -> m "Matrix sync %s" (Diagnostics.sync state)))
    |> Matrix_bot.Bot.on_membership (fun _ event ->
        if
          List.mem
            (Id.Room_id.to_string (Matrix_bot.Room.id event.envelope.room))
            (Store.rooms store)
          && not (Id.User_id.equal event.user (Context.user_id ctx))
        then Store.observe store (Id.User_id.to_string event.user))
    |> Matrix_bot.Bot.on_error (fun _ event exn ->
        Log.err (fun m ->
            m "Matrix handler failed room=%s error=%s"
              (Option.fold ~none:"none"
                 ~some:(fun id -> Printf.sprintf "%S" (Id.Room_id.to_string id))
                 (Matrix_bot.Event.room_id event))
              (Diagnostics.error exn)))
  in
  (* Signal handlers only touch an atomic flag. Shutdown happens in a fiber. *)
  let stopping = Atomic.make false in
  let previous =
    List.map
      (fun signal ->
        ( signal,
          Sys.Safe.signal signal
            (Sys.Signal_handle (fun _ -> Atomic.set stopping true)) ))
      [ Sys.sigint; Sys.sigterm ]
  in
  Fun.protect ~finally:(fun () ->
      (* Previous handlers may predate the portable signal API. *)
      List.iter
        (fun (signal, handler) ->
          (Sys.set_signal [@alert "-unsafe_multidomain"]) signal handler)
        previous)
  @@ fun () ->
  Log.info (fun m ->
      m
        "Starting Matrix sync; messages in the initial timeline are skipped. \
         Send a fresh message once sync is live");
  Matrix_bot.Bot.run ctx spec ~on_start:(fun bot ->
      (matrix_state :=
         fun () ->
           Some
             (Matrix_eio.Sync_service.state
                (Matrix_ui.Runtime.sync_service (Matrix_bot.Bot.runtime bot))));
      Log.info (fun m ->
          m "Crow handlers ready; sync=%s"
            (Diagnostics.sync
               (Matrix_ui.Observable.Value.get
                  (Matrix_ui.Runtime.sync_state (Matrix_bot.Bot.runtime bot)))));
      let clock = Eio.Stdenv.mono_clock env in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          let readiness = ref None in
          let ready () =
            let runtime = Matrix_bot.Bot.runtime bot in
            let ready =
              match
                Matrix_ui.Observable.Value.get
                  (Matrix_ui.Runtime.sync_state runtime)
              with
              | Matrix_ui.Runtime.Live _ ->
                  Matrix_ui.Room_list.all_rooms
                    (Matrix_ui.Runtime.room_list runtime)
                  |> Matrix_ui.Observable.List.snapshot
                  |> Array.for_all (fun (room : Matrix_ui.Room_list.room) ->
                      room.membership <> Matrix_client.Base_client.Joined
                      || Matrix_bot.Bot.find_room bot room.id <> None)
              | _ -> false
            in
            if !readiness <> Some ready then begin
              readiness := Some ready;
              Log.info (fun m ->
                  m "Scheduler ready=%b; requires live sync and restored rooms"
                    ready)
            end;
            ready
          in
          let fire (job : Store.reminder) ~run_id =
            Log.info (fun m ->
                m "Reminder firing id=%d run_id=%d room=%S" job.reminder_id
                  run_id job.room);
            Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 180.)
            @@ fun () ->
            let room =
              match
                Matrix_bot.Bot.find_room bot (Id.Room_id.of_string_exn job.room)
              with
              | Some room -> room
              | None -> failwith "reminder room is unavailable"
            in
            let check_room () =
              if
                (not (List.mem job.room (Store.rooms store)))
                && direct_peer bot room ~actor:job.creator <> Some job.creator
              then failwith "reminder room is no longer enabled"
            in
            check_room ();
            Engine.fire engine job ~run_id ~send:(fun text ->
                check_room ();
                let reply_to = Id.Event_id.of_string_exn job.event in
                match
                  Matrix_bot.Sent.await
                    (Matrix_bot.Room.send_notice room ~reply_to
                       ~html:(Rich_text.html text) text)
                with
                | Matrix_bot.Sent.Sent _ ->
                    Log.info (fun m ->
                        m "Reminder reply sent id=%d" job.reminder_id)
                | outcome ->
                    Log.err (fun m ->
                        m "Reminder reply failed id=%d error=%s" job.reminder_id
                          (Diagnostics.sent outcome));
                    failwith "reminder delivery failed")
          in
          while not (Atomic.get stopping) do
            (try Cron.run_due ~ready store ~fire with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
                Log.err (fun m ->
                    m "Reminder scheduler failed: %s; will retry"
                      (Diagnostics.error exn)));
            Eio.Time.Mono.sleep clock 10.
          done;
          `Stop_daemon);
      Eio.Fiber.fork_daemon ~sw (fun () ->
          while not (Atomic.get stopping) do
            (try
               List.iter
                 (fun day ->
                   Log.info (fun m -> m "Generating daily note day=%s" day);
                   ignore
                     (Daily.generate ~store ~config
                        ~complete:(complete env config model)
                        ~day))
                 (Store.pending_note_days store)
             with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn ->
                Log.err (fun m ->
                    m "Daily note failed: %s; will retry"
                      (Diagnostics.error exn)));
            Eio.Time.Mono.sleep clock 60.
          done;
          `Stop_daemon);
      Eio.Fiber.fork_daemon ~sw (fun () ->
          while not (Atomic.get stopping) do
            Eio.Time.Mono.sleep clock 0.2
          done;
          Log.info (fun m -> m "Stopping Crow");
          Matrix_bot.Bot.stop bot;
          `Stop_daemon))
