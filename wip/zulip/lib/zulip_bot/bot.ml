module Log = (val Logs.src_log (Logs.Src.create "zulip.bot") : Logs.LOG)

type command_info = { name : string; args : string option; doc : string option }

type handler = t -> Event.t -> unit

and spec = {
  prefix : string;
  all_messages : bool;
  ignore_own : bool;
  ignore_bots : bool;
  authorize : Room.t -> Zulip.Id.User.t -> bool;
  queue_depth : int;
  total_queue_depth : int;
  workers : int;
  event_types : Zulip.Event_type.t list;
  registration : Zulip_eio.Event_queue.registration_options;
  handlers : handler ref list;
  commands : command_info list;
  command_scopes : (string * (Event.t -> bool)) list;
  unknown : (t -> Event.command -> unit) option;
  errors : (t -> Event.t -> exn -> unit) option;
}

and room_state = {
  room : Room.t;
  pending : Event.t Queue.t;
  mutable scheduled : bool;
  mutable waiting : int;
  space : Eio.Condition.t;
}

and t = {
  context : Context.t;
  spec_ : spec;
  rooms_ : (string, room_state) Hashtbl.t;
  jobs : room_state Eio.Stream.t;
  capacity : Eio.Semaphore.t;
  stop_ : unit Eio.Promise.t;
  wake_stop : unit Eio.Promise.u;
  mutable stopping : bool;
  mutable failure : Zulip_eio.Error.t option;
}

type plugin = spec -> spec

let v ?(prefix = "!") ?(all_messages = false) ?(ignore_own = true)
    ?(ignore_bots = true) ?(authorize = fun _ _ -> false) ?(queue_depth = 256)
    ?(total_queue_depth = 1024) ?(workers = 4)
    ?(registration =
      {
        Zulip_eio.Event_queue.default_registration with
        fetch_event_types = Some [ Zulip.Event_type.Realm_user ];
        client_capabilities =
          [
            ("empty_topic_name", true);
            ("bulk_message_deletion", true);
            ("user_list_incomplete", true);
          ];
      })
    ?(event_types =
      [
        Zulip.Event_type.Message;
        Zulip.Event_type.Update_message;
        Zulip.Event_type.Reaction;
        Zulip.Event_type.Delete_message;
        Zulip.Event_type.Realm_user;
      ]) () =
  if queue_depth <= 0 || total_queue_depth <= 0 || workers <= 0 then
    invalid_arg "Zulip_bot.Bot.v";
  if registration.apply_markdown then
    invalid_arg "Zulip_bot.Bot.v requires Markdown messages";
  {
    prefix;
    all_messages;
    ignore_own;
    ignore_bots;
    authorize;
    queue_depth;
    total_queue_depth;
    workers;
    event_types;
    registration;
    handlers = [];
    commands = [];
    command_scopes = [];
    unknown = None;
    errors = None;
  }

let on handler spec = { spec with handlers = spec.handlers @ [ ref handler ] }
let on_message f = on (fun b -> function Event.Message m -> f b m | _ -> ())
let on_edit f = on (fun b -> function Event.Edit e -> f b e | _ -> ())
let on_reaction f = on (fun b -> function Event.Reaction e -> f b e | _ -> ())
let on_delete f = on (fun b -> function Event.Delete e -> f b e | _ -> ())
let on_custom f = on (fun b -> function Event.Custom e -> f b e | _ -> ())

let on_malformed f =
  on (fun b -> function Event.Malformed e -> f b e | _ -> ())

let on_sync f = on (fun b -> function Event.Sync e -> f b e | _ -> ())

let command ~name ?args ?doc ?(admin = false) f spec =
  let h bot = function
    | Event.Command c when c.name = name ->
        if
          admin
          && not
               (bot.spec_.authorize c.message.envelope.room
                  c.message.envelope.sender)
        then
          ignore
            (Event.reply c.message.envelope
               "this command requires an administrator")
        else f bot c
    | _ -> ()
  in
  let spec = on h spec in
  {
    spec with
    commands = spec.commands @ [ { name; args; doc } ];
    command_scopes = spec.command_scopes @ [ (name, Fun.const true) ];
  }

let commands spec = spec.commands
let add_command = command

let help ?(command = "help") spec =
  let h bot (c : Event.command) =
    let render (e : command_info) =
      bot.spec_.prefix ^ e.name
      ^ Option.fold ~none:"" ~some:(fun s -> " " ^ s) e.args
      ^ Option.fold ~none:"" ~some:(fun s -> " — " ^ s) e.doc
    in
    ignore
      (Event.reply c.message.envelope
         (match bot.spec_.commands with
         | [] -> "No commands are registered."
         | xs -> String.concat "\n" (List.map render xs)))
  in
  let command_name = command in
  add_command ~name:command_name ~doc:"list commands" h spec

let on_unknown_command f spec = { spec with unknown = Some f }
let on_error f spec = { spec with errors = Some f }

let only p plugin spec =
  let result = plugin spec in
  let handlers =
    List.map
      (fun registration ->
        if List.exists (fun previous -> previous == registration) spec.handlers
        then registration
        else ref (fun bot event -> if p event then !registration bot event))
      result.handlers
  in
  let command_scopes =
    List.map
      (fun ((name, predicate) as scope) ->
        if List.exists (fun previous -> previous == scope) spec.command_scopes
        then scope
        else (name, fun event -> p event && predicate event))
      result.command_scopes
  in
  { result with handlers; command_scopes }

let in_rooms rooms =
  only (fun e ->
      Option.exists
        (fun r -> List.exists (Room.equal_id (Room.id r)) rooms)
        (Event.room e))

let from_users users =
  only (fun e ->
      Option.exists
        (fun s -> List.exists (Zulip.Id.User.equal s) users)
        (Event.sender e))

let context t = t.context
let is_running t = not t.stopping
let user_id t = Context.user_id t.context
let plugin_store t = Context.plugin_store t.context
let spec t = t.spec_

let rooms t =
  Hashtbl.to_seq_values t.rooms_ |> List.of_seq |> List.map (fun r -> r.room)

let room_id_key = function
  | Room.Channel i -> "channel:" ^ string_of_int (Zulip.Id.Channel.to_int i)
  | Room.Direct i -> "direct:" ^ string_of_int (Zulip.Id.Recipient.to_int i)

let find_room t id =
  Option.map (fun r -> r.room) (Hashtbl.find_opt t.rooms_ (room_id_key id))

let stop t =
  if not t.stopping then (
    t.stopping <- true;
    Eio.Promise.resolve t.wake_stop ())

let report t event exn =
  match t.spec_.errors with
  | Some f -> (
      try f t event exn with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | e ->
          Log.err (fun m ->
              m "error callback raised: %s" (Printexc.to_string e)))
  | None ->
      Log.err (fun m ->
          m "%a handler raised: %s" Event.pp event (Printexc.to_string exn))

let guarded t event f =
  try f () with Eio.Cancel.Cancelled _ as e -> raise e | e -> report t event e

let handle t event =
  List.iter
    (fun registration -> guarded t event (fun () -> !registration t event))
    t.spec_.handlers;
  match event with
  | Event.Command c
    when not
           (List.exists
              (fun (n, p) -> n = c.name && p event)
              t.spec_.command_scopes) ->
      guarded t event (fun () ->
          match t.spec_.unknown with
          | Some f -> f t c
          | None ->
              ignore
                (Event.reply c.message.envelope
                   ("I do not know " ^ t.spec_.prefix ^ c.name ^ ". Try "
                  ^ t.spec_.prefix ^ "help.")))
  | _ -> ()

let activated t (message : Event.message) =
  if
    t.spec_.ignore_own
    && Zulip.Id.User.equal message.envelope.sender (user_id t)
    || (t.spec_.ignore_bots && Context.is_bot t.context message.envelope.sender)
  then None
  else
    let direct = Room.is_direct message.envelope.room in
    let one_to_one =
      direct && List.length (Room.participants message.envelope.room) <= 2
    in
    if t.spec_.all_messages || one_to_one || List.mem `Mentioned message.flags
    then Some message
    else None

let strip_mention identity text =
  if not (String.starts_with ~prefix:"@**" text) then text
  else
    let rec closing i =
      if i + 1 >= String.length text then None
      else if text.[i] = '*' && text.[i + 1] = '*' then Some i
      else closing (i + 1)
    in
    match closing 3 with
    | None -> text
    | Some close ->
        let mention = String.sub text 3 (close - 3) in
        let stable_suffix =
          "|" ^ string_of_int (Zulip.Id.User.to_int identity.Context.user_id)
        in
        if
          mention = identity.full_name
          || String.ends_with ~suffix:stable_suffix mention
        then
          String.trim
            (String.sub text (close + 2) (String.length text - close - 2))
        else text

let project t = function
  | Event.Message message ->
      Option.map
        (fun (message : Event.message) ->
          let identity = Context.identity t.context in
          let text = String.trim message.body in
          let body = strip_mention identity text in
          let message = Event.with_body message ~body in
          match Event.command ~prefix:t.spec_.prefix message with
          | Some c -> Event.Command c
          | None -> Event.Message message)
        (activated t message)
  | event -> Some event

let state_for t room =
  match Hashtbl.find_opt t.rooms_ (Room.key room) with
  | Some x -> x
  | None ->
      let x =
        {
          room;
          pending = Queue.create ();
          scheduled = false;
          waiting = 0;
          space = Eio.Condition.create ();
        }
      in
      Hashtbl.add t.rooms_ (Room.key room) x;
      x

let evict_idle t state =
  if (not state.scheduled) && state.waiting = 0 && Queue.is_empty state.pending
  then Hashtbl.remove t.rooms_ (Room.key state.room)

let enqueue t event =
  match Event.room event with
  | None -> handle t event
  | Some room ->
      Eio.Semaphore.acquire t.capacity;
      if t.stopping then (
        Eio.Semaphore.release t.capacity;
        invalid_arg "Zulip_bot.Bot.dispatch: bot has stopped");
      let state = state_for t room in
      state.waiting <- state.waiting + 1;
      let accepted = ref false in
      Fun.protect
        ~finally:(fun () ->
          state.waiting <- state.waiting - 1;
          if not !accepted then Eio.Semaphore.release t.capacity;
          evict_idle t state)
        (fun () ->
          while Queue.length state.pending >= t.spec_.queue_depth do
            Eio.Condition.await_no_mutex state.space
          done;
          if t.stopping then
            invalid_arg "Zulip_bot.Bot.dispatch: bot has stopped";
          Queue.add event state.pending;
          accepted := true;
          if not state.scheduled then (
            state.scheduled <- true;
            Eio.Stream.add t.jobs state))

let dispatch t event =
  (match event with
  | Event.Sync _ -> ()
  | _ when t.stopping -> invalid_arg "Zulip_bot.Bot.dispatch: bot has stopped"
  | _ -> ());
  match event with
  | Event.Sync _ -> enqueue t event
  | _ ->
      Eio.Fiber.first
        (fun () -> Option.iter (enqueue t) (project t event))
        (fun () ->
          Eio.Promise.await t.stop_;
          invalid_arg "Zulip_bot.Bot.dispatch: bot has stopped")

let worker t () =
  let rec loop () =
    let state = Eio.Stream.take t.jobs in
    match Queue.take_opt state.pending with
    | None ->
        state.scheduled <- false;
        evict_idle t state;
        loop ()
    | Some event ->
        Eio.Condition.broadcast state.space;
        Fun.protect
          ~finally:(fun () -> Eio.Semaphore.release t.capacity)
          (fun () -> handle t event);
        if Queue.is_empty state.pending then (
          state.scheduled <- false;
          evict_idle t state)
        else Eio.Stream.add t.jobs state;
        loop ()
  in
  loop ()

let bad_queue = function
  | Zulip_eio.Error.Api { code; _ } -> String.equal code "BAD_EVENT_QUEUE_ID"
  | _ -> false

let queue_loop t =
  let current = ref None in
  let fail error =
    t.failure <- Some error;
    stop t
  in
  let sleep error delay =
    Eio.Time.sleep (Context.clock t.context)
      (Option.value (Zulip_eio.Error.retry_after error) ~default:delay)
  in
  let terminal error =
    Zulip_eio.Error.is_terminal error
    ||
    match error with
    | Zulip_eio.Error.Api { status; _ } | Zulip_eio.Error.Http { status; _ } ->
        status >= 400 && status < 500 && status <> 408 && status <> 429
    | _ -> false
  in
  Fun.protect ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
          Option.iter
            (fun q ->
              ignore
                (Eio.Time.with_timeout (Context.clock t.context) 2. (fun () ->
                     ignore
                       (Zulip_eio.Event_queue.delete q
                          (Context.client t.context));
                     Ok ())))
            !current;
          ignore
            (Eio.Time.with_timeout (Context.clock t.context) 2. (fun () ->
                 dispatch t (Event.Sync Event.Stopped);
                 Ok ()))))
  @@ fun () ->
  let rec registered delay =
    if t.stopping then ()
    else
      match
        Zulip_eio.Event_queue.register (Context.client t.context)
          ~options:t.spec_.registration
          ~event_types:
            (List.sort_uniq Stdlib.compare
               (Zulip.Event_type.Realm_user :: t.spec_.event_types))
          ()
      with
      | Error e when terminal e ->
          dispatch t (Event.Sync (Event.Recovering e));
          fail e
      | Error (Zulip_eio.Error.Json _ as e) ->
          dispatch t (Event.Sync (Event.Recovering e));
          fail e
      | Error e ->
          dispatch t (Event.Sync (Event.Recovering e));
          Log.warn (fun m ->
              m "registering event queue: %a" Zulip_eio.Error.pp e);
          sleep e delay;
          registered (min 30. (delay *. 2.))
      | Ok q -> (
          current := Some q;
          match
            Context.apply_initial_state t.context
              (Zulip_eio.Event_queue.state q)
          with
          | Error e ->
              dispatch t (Event.Sync (Event.Recovering e));
              fail e
          | Ok () ->
              Log.info (fun m ->
                  m "connected as %s at %s"
                    (Context.identity t.context).full_name
                    (Zulip_eio.Client.site (Context.client t.context)));
              dispatch t (Event.Sync Event.Live);
              polling q 1.)
  and polling q delay =
    if t.stopping then ()
    else
      match
        Zulip_eio.Event_queue.get_events q (Context.client t.context) ()
      with
      | Ok batch ->
          let rec accept count = function
            | [] -> ()
            | _ when t.stopping -> ()
            | raw :: rest ->
                Option.iter (enqueue t)
                  (Option.bind (Event.of_zulip t.context raw) (project t));
                Zulip_eio.Event_queue.ack ~count:(count + 1) q batch
                |> Zulip_eio.Error.or_raise;
                accept (count + 1) rest
          in
          accept 0 (Zulip_eio.Event_queue.Batch.events batch);
          polling q 1.
      | Error e when bad_queue e ->
          current := None;
          dispatch t (Event.Sync (Event.Recovering e));
          Log.info (fun m -> m "event queue expired; re-registering");
          registered 1.
      | Error e when terminal e ->
          dispatch t (Event.Sync (Event.Recovering e));
          fail e
      | Error (Zulip_eio.Error.Json _ as e) ->
          dispatch t (Event.Sync (Event.Recovering e));
          fail e
      | Error e ->
          dispatch t (Event.Sync (Event.Recovering e));
          Log.warn (fun m -> m "polling event queue: %a" Zulip_eio.Error.pp e);
          sleep e delay;
          polling q (min 30. (delay *. 2.))
  in
  dispatch t (Event.Sync Event.Connecting);
  registered 1.

let run_result ?on_start context spec_ =
  Eio.Switch.run @@ fun sw ->
  let stop_, wake_stop = Eio.Promise.create () in
  let t =
    {
      context;
      spec_;
      rooms_ = Hashtbl.create 16;
      jobs = Eio.Stream.create (spec_.total_queue_depth + spec_.workers);
      capacity = Eio.Semaphore.make spec_.total_queue_depth;
      stop_;
      wake_stop;
      stopping = false;
      failure = None;
    }
  in
  for _ = 1 to spec_.workers do
    Eio.Fiber.fork_daemon ~sw (worker t)
  done;
  Option.iter
    (fun f ->
      guarded t
        (Event.Custom
           { envelope = None; event_type = "start"; raw = Jsont.Json.null () })
        (fun () -> f t))
    on_start;
  Fun.protect
    ~finally:(fun () -> t.stopping <- true)
    (fun () ->
      Eio.Fiber.first
        (fun () -> queue_loop t)
        (fun () -> Eio.Promise.await stop_));
  match t.failure with None -> Ok () | Some error -> Error error

let run ?on_start context spec =
  run_result ?on_start context spec |> Zulip_eio.Error.or_raise
