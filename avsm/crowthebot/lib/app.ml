module Context = Matrix_bot.Context
module Id = Matrix_proto.Id

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

let model ~env ~sw ~profile ~dir config client api_key_file =
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

let location_tools ~env ~sw ~profile ~dir store =
  let client = fetch env in
  let clock = Eio.Stdenv.mono_clock env in
  let now =
    let clock = Eio.Stdenv.clock env in
    fun () -> Eio.Time.now clock
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
              Owntracks_source.initialize ~fetch:client ~clock ~now settings ))
          sources
      in
      Locations.create ~state:(Store.locations store) ~sources ~default)

let complete env (config : Config.t) client =
  let clock = Eio.Stdenv.mono_clock env in
  fun messages tools ->
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 90.) @@ fun () ->
    let request =
      Openrouter.Chat.request ~model:config.Config.model
        ~max_tokens:config.max_tokens ~messages
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
    | Some choice -> (choice.text, choice.tool_calls)

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
        model ~env ~sw ~profile ~dir config (fetch env) api_key_file
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

let probe ~env ~sw ~profile ~api_key_file =
  with_profile ~env ~sw ~profile @@ fun dir config _ ->
  let client = model ~env ~sw ~profile ~dir config (fetch env) api_key_file in
  let text, _ =
    complete env config client
      [ Openrouter.Message.user "Reply with CROW_OK." ]
      []
  in
  print_endline (Option.value ~default:"No text returned" text)

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
  with_profile ~env ~sw ~profile @@ fun dir config store ->
  let ctx = connect ~env ~sw ~profile config () in
  let client = fetch env in
  let model = model ~env ~sw ~profile ~dir config client api_key_file in
  let engine =
    Engine.create ~config ~store
      ~self:(Id.User_id.to_string (Context.user_id ctx))
      ~plugins:[]
      ~complete:(complete env config model)
      ~now:(now env)
    |> fun engine ->
    Engine.with_feeds engine (feed_tools env store) |> fun engine ->
    Engine.with_locations engine (location_tools ~env ~sw ~profile ~dir store)
  in
  let self = Id.User_id.to_string (Context.user_id ctx) in
  let direct_peer bot room =
    let room_id = Matrix_bot.Room.id room in
    let saved = Store.direct_peer store (Id.Room_id.to_string room_id) in
    let marked = Matrix_bot.Room.is_dm room || saved <> None in
    if not marked then None
    else begin
      (match Matrix_bot.Room.sync_members room with
      | Ok () -> ()
      | Error _ -> failwith "could not check DM membership");
      let state =
        Matrix_eio.Sync_service.state
          (Matrix_ui.Runtime.sync_service (Matrix_bot.Bot.runtime bot))
      in
      let complete =
        match Matrix_client.Base_client.find_room state room_id with
        | Some info -> info.members_complete
        | None -> false
      in
      match
        Address.direct_peer ~self ~marked ~complete
          (List.map Id.User_id.to_string (Matrix_bot.Room.members room))
      with
      | Some peer when saved = None || saved = Some peer -> Some peer
      | _ -> None
    end
  in
  let on_message bot (message : Matrix_bot.Event.message) =
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
      let send text =
        match Matrix_bot.Sent.await (Matrix_bot.Event.reply e text) with
        | Matrix_bot.Sent.Sent _ -> ()
        | _ -> failwith "Matrix reply could not be confirmed"
      in
      try
        with_timeout env 180. (fun () ->
            let direct = direct_peer bot e.room = Some event.sender in
            let mentioned =
              Address.mentions ~self message.presentation.raw.content
            in
            let send text =
              if direct && direct_peer bot e.room <> Some event.sender then
                failwith "DM membership changed during the request";
              send text
            in
            Engine.handle engine ~mentioned ~direct ~send event)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ ->
          Logs.err (fun m -> m "Crow request failed. Context was not advanced")
    end
  in
  let spec =
    Matrix_bot.Bot.v ~name:"crowthebot" ~prefix:"!" ~auto_join:false
      ~ignore_notices:true ~ignore_own:true ~backlog:`Skip ~queue_depth:32 ()
    |> Matrix_bot.Bot.command ~name:"crow" (fun bot command ->
        on_message bot command.message)
    |> Matrix_bot.Bot.on_invite (fun bot invitation ->
        let runtime = Matrix_bot.Bot.runtime bot in
        let state =
          Matrix_eio.Sync_service.state (Matrix_ui.Runtime.sync_service runtime)
        in
        match
          ( invitation.inviter,
            Matrix_client.Base_client.find_room state invitation.room_id )
        with
        | Some inviter, Some info
          when info.is_dm
               && (Store.person store (Id.User_id.to_string inviter)).allowed ->
            with_timeout env 30. (fun () ->
                match Matrix_ui.Runtime.join runtime invitation.room_id with
                | Error _ -> failwith "could not join DM"
                | Ok () ->
                    Store.add_direct_room store
                      ~room:(Id.Room_id.to_string invitation.room_id)
                      ~peer:(Id.User_id.to_string inviter))
        | _ -> ())
    |> Matrix_bot.Bot.on_join (fun _ room ->
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
    |> Matrix_bot.Bot.on_membership (fun _ event ->
        if
          List.mem
            (Id.Room_id.to_string (Matrix_bot.Room.id event.envelope.room))
            (Store.rooms store)
          && not (Id.User_id.equal event.user (Context.user_id ctx))
        then Store.observe store (Id.User_id.to_string event.user))
    |> Matrix_bot.Bot.on_unknown_command (fun bot command ->
        on_message bot command.message)
    |> Matrix_bot.Bot.on_message on_message
    |> Matrix_bot.Bot.on_error (fun _ _ _ ->
        Logs.err (fun m -> m "Matrix handler failed"))
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
  Matrix_bot.Bot.run ctx spec ~on_start:(fun bot ->
      let clock = Eio.Stdenv.mono_clock env in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          let fire (job : Store.reminder) ~run_id =
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
                && direct_peer bot room <> Some job.creator
              then failwith "reminder room is no longer enabled"
            in
            check_room ();
            Engine.fire engine job ~run_id ~send:(fun text ->
                check_room ();
                let reply_to = Id.Event_id.of_string_exn job.event in
                match
                  Matrix_bot.Sent.await
                    (Matrix_bot.Room.send_notice room ~reply_to text)
                with
                | Matrix_bot.Sent.Sent _ -> ()
                | _ -> failwith "reminder delivery failed")
          in
          while not (Atomic.get stopping) do
            (try Cron.run_due store ~fire with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | _ -> Logs.err (fun m -> m "Reminder scheduler failed; will retry"));
            Eio.Time.Mono.sleep clock 10.
          done;
          `Stop_daemon);
      Eio.Fiber.fork_daemon ~sw (fun () ->
          while not (Atomic.get stopping) do
            (try
               List.iter
                 (fun day ->
                   ignore
                     (Daily.generate ~store ~config
                        ~complete:(complete env config model)
                        ~day))
                 (Store.pending_note_days store)
             with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | _ -> Logs.err (fun m -> m "Daily note failed; will retry"));
            Eio.Time.Mono.sleep clock 60.
          done;
          `Stop_daemon);
      Eio.Fiber.fork_daemon ~sw (fun () ->
          while not (Atomic.get stopping) do
            Eio.Time.Mono.sleep clock 0.2
          done;
          Matrix_bot.Bot.stop bot;
          `Stop_daemon))
