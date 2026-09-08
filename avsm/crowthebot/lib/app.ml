module Context = Matrix_bot.Context
module Id = Matrix_proto.Id

let with_timeout env seconds f =
  Eio.Time.Timeout.run_exn
    (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) seconds)
    f

let with_profile ~env ~sw ~profile f =
  let dir = Profile.directory env profile in
  Profile.with_lock dir (fun () ->
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
  | Ok None -> ());
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

let now env () =
  Int64.to_float
    (Mtime.to_uint64_ns (Eio.Time.Mono.now (Eio.Stdenv.mono_clock env)))
  /. 1e9

let plugins env client = [ Plugin.blogroll ~fetch:client ~now:(now env) ]

let model config client api_key_file =
  Openrouter.of_fetch ~base_url:config.Config.base_url
    ~max_response_bytes:(1024 * 1024)
    ?api_key:(Option.map Profile.read_secret api_key_file)
    client

let complete env (config : Config.t) client messages tools =
  with_timeout env 90. @@ fun () ->
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

let blogroll ~env ~query =
  let plugin = Plugin.blogroll ~fetch:(fetch env) ~now:(now env) in
  with_timeout env 30. (fun () -> print_endline (plugin.run ~query))

let probe ~env ~sw ~profile ~api_key_file =
  with_profile ~env ~sw ~profile @@ fun _ config _ ->
  let client = model config (fetch env) api_key_file in
  let text, _ =
    complete env config client
      [ Openrouter.Message.user "Reply with CROW_OK." ]
      []
  in
  print_endline (Option.value ~default:"No text returned" text)

let run ~env ~sw ~profile ~api_key_file =
  with_profile ~env ~sw ~profile @@ fun _ config store ->
  if Store.rooms store = [] then
    invalid_arg "no rooms enabled. Run crowthebot join first";
  let ctx = connect ~env ~sw ~profile config () in
  let client = fetch env in
  let model = model config client api_key_file in
  let engine =
    Engine.create ~config ~store
      ~self:(Id.User_id.to_string (Context.user_id ctx))
      ~plugins:(plugins env client)
      ~complete:(complete env config model)
      ~now:(now env)
  in
  let on_message _ (message : Matrix_bot.Event.message) =
    if message.content.kind = Matrix_ui.Presentation.Text then begin
      let e = message.envelope in
      let event : Engine.event =
        {
          room = Id.Room_id.to_string (Matrix_bot.Room.id e.room);
          sender = Id.User_id.to_string e.sender;
          id = Id.Event_id.to_string e.event_id;
          body = message.content.body;
        }
      in
      let send text =
        match Matrix_bot.Sent.await (Matrix_bot.Event.reply e text) with
        | Matrix_bot.Sent.Sent _ -> ()
        | _ -> failwith "Matrix reply could not be confirmed"
      in
      try
        with_timeout env 180. (fun () -> Engine.handle engine ~send event)
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
    |> Matrix_bot.Bot.on_unknown_command (fun _ _ -> ())
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
      Eio.Fiber.fork_daemon ~sw (fun () ->
          while not (Atomic.get stopping) do
            Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.2
          done;
          Matrix_bot.Bot.stop bot;
          `Stop_daemon))
