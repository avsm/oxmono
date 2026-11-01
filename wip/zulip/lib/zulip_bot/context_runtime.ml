type clock = float Eio.Time.clock_ty Eio.Resource.t

type identity = {
  user_id : Zulip.Id.User.t;
  email : string;
  full_name : string;
}

type send =
  destination:Zulip.Message.destination ->
  content:string ->
  (Zulip.Id.Message.t, Zulip_eio.Error.t) result

type request = {
  sent : Sent.t;
  destination : Zulip.Message.destination;
  content : string;
}

type location = { cached_destination : Zulip.Message.destination }

type t = {
  client : Zulip_eio.Client.t;
  mutable identity : identity;
  clock : clock;
  store : Plugin_store.t;
  is_bot : Zulip.Id.User.t -> bool;
  known_bots : (int, bool) Hashtbl.t;
  send : send;
  tracker : Sent_runtime.tracker;
  requests : request Eio.Stream.t;
  locations : (int, location) Hashtbl.t;
  location_order : (int * location) Queue.t;
}

let default_send ~own client ~destination ~content =
  match destination with
  | Zulip.Message.Channel { channel_id; topic; _ } ->
      Zulip_eio.Messages.send_channel_id client ~channel_id ~topic ~content ()
  | Zulip.Message.Direct { participants; _ } ->
      let recipients =
        List.filter
          (fun user -> not (Zulip.Id.User.equal user own))
          participants
      in
      let recipients =
        if recipients = [] && List.exists (Zulip.Id.User.equal own) participants
        then [ own ]
        else recipients
      in
      Zulip_eio.Messages.send_direct client ~recipients ~content ()

let run_sender t =
  let outcome_of_error = function
    | (Zulip_eio.Error.Api { status; _ } | Zulip_eio.Error.Http { status; _ })
      as error
      when status >= 500 ->
        Sent.Indeterminate (Some error)
    | ( Zulip_eio.Error.Transport _ | Zulip_eio.Error.Timeout _
      | Zulip_eio.Error.Json _ ) as error ->
        Sent.Indeterminate (Some error)
    | error -> Sent.Failed error
  in
  let rec send request attempts =
    try
      match
        t.send ~destination:request.destination ~content:request.content
      with
      | Ok id -> Sent_runtime.resolve request.sent (Sent.Sent id)
      | Error error when attempts < 3 && Zulip_eio.Error.is_rate_limit error ->
          let after =
            Option.value ~default:1. (Zulip_eio.Error.retry_after error)
          in
          Eio.Time.sleep t.clock (Float.max 0. after);
          send request (attempts + 1)
      | Error error ->
          Sent_runtime.resolve request.sent (outcome_of_error error)
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
        Sent_runtime.resolve request.sent
          (Sent.Indeterminate
             (Some
                (Zulip_eio.Error.Transport
                   (Fetch.Protocol_error (Printexc.to_string exn)))))
  in
  let rec loop () =
    let request = Eio.Stream.take t.requests in
    if Sent_runtime.begin_send request.sent then send request 0;
    loop ()
  in
  loop ()

let v ~sw ~client ~identity ?clock ?plugin_store ?(is_bot = Fun.const false)
    ?send ?(send_depth = 128) () =
  if send_depth <= 0 then
    invalid_arg "Zulip_bot.Context.v: send_depth must be positive";
  let clock =
    match clock with
    | Some clock -> (clock :> clock)
    | None -> (
        match Zulip_eio.Transport.clock (Zulip_eio.Client.transport client) with
        | Some clock -> clock
        | None -> invalid_arg "Zulip_bot.Context.v: provide a clock")
  in
  let store = Option.value ~default:(Plugin_store.memory ()) plugin_store in
  let send =
    Option.value ~default:(default_send ~own:identity.user_id client) send
  in
  let tracker = Sent_runtime.tracker ~clock in
  let requests = Eio.Stream.create send_depth in
  let context =
    {
      client;
      identity;
      clock;
      store;
      is_bot;
      known_bots = Hashtbl.create 128;
      send;
      tracker;
      requests;
      locations = Hashtbl.create 256;
      location_order = Queue.create ();
    }
  in
  Eio.Switch.on_release sw (fun () -> Sent_runtime.close tracker);
  Eio.Fiber.fork_daemon ~sw (fun () -> run_sender context);
  context

let client t = t.client
let identity t = t.identity
let user_id t = t.identity.user_id
let clock t = t.clock
let plugin_store t = t.store

let is_bot t id =
  match Hashtbl.find_opt t.known_bots (Zulip.Id.User.to_int id) with
  | Some value -> value
  | None -> t.is_bot id

let remember_user t user =
  let id = Zulip.User.user_id user in
  Hashtbl.replace t.known_bots (Zulip.Id.User.to_int id)
    (Zulip.User.is_bot user);
  if Zulip.Id.User.equal id t.identity.user_id then
    t.identity <-
      {
        user_id = id;
        email = Zulip.User.email user;
        full_name = Zulip.User.full_name user;
      }

let apply_initial_state t state =
  let ( let* ) = Result.bind in
  let* users = Zulip_eio.Initial_state.users state in
  let* inactive = Zulip_eio.Initial_state.inactive_users state in
  let* cross_realm = Zulip_eio.Initial_state.cross_realm_bots state in
  List.iter
    (Option.iter (List.iter (remember_user t)))
    [ users; inactive; cross_realm ];
  Ok ()

let observe_payload t = function
  | Zulip.Event_payload.Realm_user person ->
      Option.iter
        (Hashtbl.replace t.known_bots (Zulip.Id.User.to_int person.user_id))
        person.is_bot;
      if Zulip.Id.User.equal person.user_id t.identity.user_id then
        t.identity <-
          {
            t.identity with
            email =
              Option.value ~default:t.identity.email
                (match person.new_email with
                | Some _ as email -> email
                | None -> person.email);
            full_name =
              Option.value ~default:t.identity.full_name person.full_name;
          }
  | _ -> ()

let observe_event t event =
  match Zulip.Event_payload.of_event event with
  | Ok payload -> observe_payload t payload
  | Error _ -> ()

let remember_destination t message_id destination =
  let id = Zulip.Id.Message.to_int message_id in
  let location = { cached_destination = destination } in
  Hashtbl.replace t.locations id location;
  Queue.add (id, location) t.location_order;
  while Queue.length t.location_order > 1024 do
    let old_id, old_location = Queue.take t.location_order in
    match Hashtbl.find_opt t.locations old_id with
    | Some current when current == old_location ->
        Hashtbl.remove t.locations old_id
    | _ -> ()
  done

let remember_message t message =
  remember_destination t (Zulip.Message.id message)
    (Zulip.Message.destination message)

let find_destination t id =
  Hashtbl.find_opt t.locations (Zulip.Id.Message.to_int id)
  |> Option.map (fun location -> location.cached_destination)

let enqueue t ~destination ~content =
  let sent = Sent_runtime.v t.tracker in
  (match Sent.status sent with
  | Sent.Done _ -> ()
  | Sent.Queued | Sent.Sending -> (
      try
        Eio.Fiber.first
          (fun () -> Eio.Stream.add t.requests { sent; destination; content })
          (fun () -> Sent_runtime.await_closed t.tracker)
      with exn ->
        ignore (Sent.cancel sent);
        raise exn));
  sent

let connect ~sw ~env ~profile ?site ?email ?api_key ?(allow_insecure = false)
    ?transport () =
  Result.bind
    (Zulip_eio.Profile.resolve ~fs:env#fs ?site ?email ?api_key profile)
  @@ fun profile ->
  let transport =
    match transport with
    | Some value -> value
    | None -> Zulip_eio.Transport.v env
  in
  Result.bind
    (Zulip_eio.Client.create ~transport ~allow_insecure
       ~auth:(Zulip_eio.Profile.auth profile)
       ())
  @@ fun client ->
  Result.bind (Zulip_eio.Users.me client) @@ fun user ->
  let identity =
    {
      user_id = Zulip.User.user_id user;
      email = Zulip.User.email user;
      full_name = Zulip.User.full_name user;
    }
  in
  let bot_cache = Hashtbl.create 128 in
  let is_bot id =
    let key = Zulip.Id.User.to_int id in
    match Hashtbl.find_opt bot_cache key with
    | Some is_bot -> is_bot
    | None -> (
        match Zulip_eio.Users.get_by_id client ~user_id:id () with
        | Error _ -> false
        | Ok user ->
            let is_bot = Zulip.User.is_bot user in
            Hashtbl.replace bot_cache key is_bot;
            is_bot)
  in
  Result.bind (Zulip_eio.Profile.data_dir ~fs:env#fs profile) @@ fun data_dir ->
  Result.bind
    (Plugin_store.open_file Eio.Path.(data_dir / "plugins.json")
    |> Result.map_error (fun error ->
        Zulip_eio.Error.Storage (Plugin_store.error_to_string error)))
  @@ fun store ->
  Ok (v ~sw ~client ~identity ~clock:env#clock ~plugin_store:store ~is_bot ())
