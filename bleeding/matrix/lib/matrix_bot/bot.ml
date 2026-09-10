module Ui = Matrix_ui
module Id = Matrix_proto.Id
module P = Matrix_ui.Presentation
module Log = (val Logs.src_log Logging.src : Logs.LOG)

type command_info = { name : string; args : string option; doc : string option }

type spec = {
  spec_name : string;
  prefix : string;
  admin_level : int;
  auto_join : bool;
  ignore_notices : bool;
  ignore_own : bool;
  backlog : [ `Skip | `Handle ];
  queue_depth : int;
  (* In registration order, which is the order a dispatcher runs them in. *)
  handlers : handler list;
  entries : command_info list;
  (* Parallels [entries]: which events a command's handler actually runs
     for. [only]/[in_rooms]/[from_users] narrow this for a command
     registered under them, so [dispatch] can tell a command that exists
     but is out of scope here from one that is genuinely unknown. *)
  scopes : (string * (Event.t -> bool)) list;
  unknown : (t -> Event.command -> unit) option;
  errors : (t -> Event.t -> exn -> unit) option;
}

and handler = t -> Event.t -> unit

(* The dispatcher needs the id to write the cursor with, and not every
   [Event.t] carries one, so the stream holds the pair. *)
and queued = { event : Event.t; at : Id.Event_id.t option }

and room_fibers = {
  handle : Room.t;
  stream : queued Eio.Stream.t;
  mutable subscription :
    Ui.Event_cache.event Ui.Observable.List.subscription option;
  mutable finished : bool;
}

and t = {
  ctx : Context.t;
  the_spec : spec;
  the_runtime : Ui.Runtime.t;
  sender : Sent.Internal.tracker;
  sw : Eio.Switch.t;
  own : Id.User_id.t;
  plugin : string;
  rooms_table : (string, room_fibers) Hashtbl.t;
  global : queued Eio.Stream.t;
  stop_promise : unit Eio.Promise.t;
  stop_resolver : unit Eio.Promise.u;
  mutable stopping : bool;
}

type plugin = spec -> spec

let v ?(name = "bot") ?(prefix = "!") ?(admin_level = 50) ?(auto_join = true)
    ?(ignore_notices = true) ?(ignore_own = true) ?(backlog = `Skip)
    ?(queue_depth = 256) () =
  {
    spec_name = name;
    prefix;
    admin_level;
    auto_join;
    ignore_notices;
    ignore_own;
    backlog;
    queue_depth;
    handlers = [];
    entries = [];
    scopes = [];
    unknown = None;
    errors = None;
  }

let on handler spec = { spec with handlers = spec.handlers @ [ handler ] }

let on_message f =
  on (fun bot -> function Event.Message m -> f bot m | _ -> ())

let on_edit f = on (fun bot -> function Event.Edit e -> f bot e | _ -> ())

let on_reaction f =
  on (fun bot -> function Event.Reaction r -> f bot r | _ -> ())

let on_membership f =
  on (fun bot -> function Event.Membership m -> f bot m | _ -> ())

let on_room_state f =
  on (fun bot -> function Event.Room_state s -> f bot s | _ -> ())

let on_custom f = on (fun bot -> function Event.Custom c -> f bot c | _ -> ())

let on_invite f =
  on (fun bot -> function Event.Invited i -> f bot i | _ -> ())

let on_join f =
  on (fun bot -> function Event.Joined room -> f bot room | _ -> ())

let on_leave f =
  on (fun bot -> function Event.Left room_id -> f bot room_id | _ -> ())

let on_sync f =
  on (fun bot -> function Event.Sync state -> f bot state | _ -> ())

let spec bot = bot.the_spec
let context bot = bot.ctx
let user_id bot = bot.own
let plugin_store bot = Context.plugin_store bot.ctx
let runtime bot = bot.the_runtime

let find_room bot room_id =
  Option.map
    (fun fibers -> fibers.handle)
    (Hashtbl.find_opt bot.rooms_table (Id.Room_id.to_string room_id))

let rooms bot =
  Hashtbl.fold (fun _ fibers acc -> fibers.handle :: acc) bot.rooms_table []

let is_admin bot the_room user =
  Room.power_level the_room user >= bot.the_spec.admin_level

let command ~name ?args ?doc ?(admin = false) f spec =
  let handler bot = function
    | Event.Command c when String.equal c.name name ->
        if
          admin
          && not
               (is_admin bot c.message.envelope.room c.message.envelope.sender)
        then
          ignore
            (Event.reply c.message.envelope
               "you need to be a moderator to do that")
        else f bot c
    | _ -> ()
  in
  let spec = on handler spec in
  {
    spec with
    entries = spec.entries @ [ { name; args; doc } ];
    scopes = spec.scopes @ [ (name, fun _ -> true) ];
  }

let commands spec = spec.entries

(* The list is read from the running bot rather than closed over at
   registration time, so [help] may be registered anywhere in the pipeline
   and still list what came after it. *)
let help ?(command = "help") spec =
  let render prefix (e : command_info) =
    String.concat ""
      [
        prefix;
        e.name;
        (match e.args with None -> "" | Some args -> " " ^ args);
        (match e.doc with None -> "" | Some doc -> " — " ^ doc);
      ]
  in
  let handler bot (c : Event.command) =
    let spec = bot.the_spec in
    let body =
      match spec.entries with
      | [] -> "No commands are registered."
      | entries -> String.concat "\n" (List.map (render spec.prefix) entries)
    in
    ignore (Event.reply c.message.envelope body)
  in
  let spec =
    on
      (fun bot -> function
        | Event.Command c when String.equal c.name command -> handler bot c
        | _ -> ())
      spec
  in
  {
    spec with
    entries =
      spec.entries
      @ [ { name = command; args = None; doc = Some "list the commands" } ];
    scopes = spec.scopes @ [ (command, fun _ -> true) ];
  }

let on_unknown_command f spec = { spec with unknown = Some f }
let on_error f spec = { spec with errors = Some f }

let only predicate plugin spec =
  let before = List.length spec.handlers in
  let before_scopes = List.length spec.scopes in
  let result = plugin spec in
  let handlers =
    List.mapi
      (fun index handler ->
        if index < before then handler
        else fun bot event -> if predicate event then handler bot event)
      result.handlers
  in
  let scopes =
    List.mapi
      (fun index (name, in_scope) ->
        if index < before_scopes then (name, in_scope)
        else (name, fun event -> predicate event && in_scope event))
      result.scopes
  in
  { result with handlers; scopes }

let in_rooms room_ids plugin =
  only
    (fun event ->
      match Event.room_id event with
      | Some id -> List.exists (Id.Room_id.equal id) room_ids
      | None -> false)
    plugin

let from_users user_ids plugin =
  only
    (fun event ->
      match Event.sender event with
      | Some id -> List.exists (Id.User_id.equal id) user_ids
      | None -> false)
    plugin

let stop bot =
  if not bot.stopping then (
    bot.stopping <- true;
    Eio.Promise.resolve bot.stop_resolver ())

let report bot event exn =
  match bot.the_spec.errors with
  | None ->
      Log.err (fun m ->
          m "%a raised %s" Event.pp event (Printexc.to_string exn))
  | Some f -> (
      try f bot event exn with
      | Eio.Cancel.Cancelled _ as again ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace again bt
      | second ->
          Log.err (fun m ->
              m "the error handler for %a raised %s" Event.pp event
                (Printexc.to_string second)))

(* A handler is user code doing I/O: letting it out would cancel the switch
   and take every room down with it. Cancellation is not an error. *)
let guard bot event f =
  try f () with
  | Eio.Cancel.Cancelled _ as exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
  | exn -> report bot event exn

let unknown_command bot (c : Event.command) =
  guard bot (Event.Command c) @@ fun () ->
  match bot.the_spec.unknown with
  | Some f -> f bot c
  | None ->
      ignore
        (Event.reply c.message.envelope
           (Printf.sprintf "I do not know %s%s. Try %shelp." bot.the_spec.prefix
              c.name bot.the_spec.prefix))

let join bot room_id = Ui.Runtime.join bot.the_runtime room_id

let dispatch bot (item : queued) =
  List.iter
    (fun handler -> guard bot item.event (fun () -> handler bot item.event))
    bot.the_spec.handlers;
  match item.event with
  | Event.Command c
    when not
           (List.exists
              (fun (name, in_scope) ->
                String.equal name c.name && in_scope item.event)
              bot.the_spec.scopes) ->
      unknown_command bot c
  | Event.Invited { room_id; _ } when bot.the_spec.auto_join -> (
      guard bot item.event @@ fun () ->
      match join bot room_id with
      | Ok () ->
          Log.info (fun m ->
              m "Joined %s on an invite" (Id.Room_id.to_string room_id))
      | Error error ->
          Log.warn (fun m ->
              m "Cannot join %s: %a"
                (Id.Room_id.to_string room_id)
                Matrix_client.Error.pp error))
  | _ -> ()

let write_cursor bot room_id event_id =
  match
    Plugin_store.set (plugin_store bot) ~room:room_id ~plugin:bot.plugin
      ~key:"cursor" Matrix_proto.Json.Codec.string
      (Id.Event_id.to_string event_id)
  with
  | Ok () -> ()
  | Error error ->
      Log.warn (fun m ->
          m "Cannot record the cursor of %s: %a"
            (Id.Room_id.to_string room_id)
            Plugin_store.pp_error error)

let advance bot room_id (item : queued) =
  Option.iter (write_cursor bot room_id) item.at

let global_loop bot =
  let rec loop () =
    let item = Eio.Stream.take bot.global in
    dispatch bot item;
    loop ()
  in
  loop ()

let room_loop bot fibers =
  let room_id = Room.id fibers.handle in
  let rec loop () =
    let item = Eio.Stream.take fibers.stream in
    dispatch bot item;
    advance bot room_id item;
    match item.event with
    | Event.Left _ -> Ui.Runtime.close_timeline bot.the_runtime room_id
    | _ -> loop ()
  in
  loop ()

let message_of the_room (p : P.t) event_id (m : P.message) =
  {
    Event.envelope = { Event.room = the_room; sender = p.sender; event_id };
    content = m;
    presentation = p;
    reply_to =
      (match p.relation with
      | Some { kind = P.Reply; target } -> Some target
      | _ -> None);
  }

let event_of bot the_room (p : P.t) =
  match p.event_id with
  | None -> None
  | Some event_id -> (
      let envelope = { Event.room = the_room; sender = p.sender; event_id } in
      if bot.the_spec.ignore_own && Id.User_id.equal p.sender bot.own then None
      else
        match p.content with
        | P.Message m when bot.the_spec.ignore_notices && m.kind = P.Notice ->
            None
        | P.Message m -> (
            match p.relation with
            | Some { kind = P.Replacement; target } ->
                let replaced = Option.value ~default:p (P.replacement p) in
                let presentation, content =
                  match replaced.content with
                  | P.Message m' -> (replaced, m')
                  | _ -> (p, m)
                in
                Some
                  (Event.Edit
                     {
                       message =
                         message_of the_room presentation event_id content;
                       original = target;
                     })
            | _ -> (
                let message = message_of the_room p event_id m in
                match Args.parse ~prefix:bot.the_spec.prefix m.body with
                | Some (name, args) ->
                    Some
                      (Event.Command
                         { message; name; args; argv = Args.argv args })
                | None -> Some (Event.Message message)))
        | P.Sticker { body; url; _ } ->
            Some (Event.Sticker { envelope; body; url })
        | P.Poll { text } -> Some (Event.Poll { envelope; text })
        | P.Reaction { key; target } ->
            Some (Event.Reaction { envelope; key; relates_to = target })
        | P.Redaction { target; reason } ->
            Some (Event.Redaction { envelope; target; reason })
        | P.Membership { user; change; reason } ->
            Some (Event.Membership { envelope; user; change; reason })
        | P.Profile { user; change } ->
            Some (Event.Profile { envelope; user; change })
        | P.State { state; _ } -> Some (Event.Room_state { envelope; state })
        | P.Custom { event_type; content } ->
            Some
              (Event.Custom { envelope; event_type; content; presentation = p })
        | P.Unable_to_decrypt | P.Malformed _ -> None)

let cursor bot room_id =
  match
    Plugin_store.find (plugin_store bot) ~room:room_id ~plugin:bot.plugin
      ~key:"cursor" Matrix_proto.Json.Codec.string
  with
  | Ok None -> None
  | Ok (Some text) -> Result.to_option (Id.Event_id.of_string text)
  | Error error ->
      Log.warn (fun m ->
          m "Cannot read the cursor of %s: %a"
            (Id.Room_id.to_string room_id)
            Plugin_store.pp_error error);
      None

(* The backlog is everything the cache already holds when the bot starts.
   Under [`Skip] it is passed over and the cursor jumps to the newest event,
   so that a later [`Handle] run does not replay what this one ignored;
   under [`Handle] the events after the cursor are delivered in order. A
   room with no cursor, or one whose cursor the cache no longer holds, is
   skipped: a bot joining a room does not answer its history. *)
let backlog_boundary bot cache room_id total =
  match (bot.the_spec.backlog, cursor bot room_id) with
  | `Handle, Some event_id -> (
      match Ui.Event_cache.position cache room_id event_id with
      | Some index -> index + 1
      | None -> total)
  | `Handle, None | `Skip, _ -> total

let collect bot fibers =
  let room_id = Room.id fibers.handle in
  let cache = Ui.Runtime.event_cache bot.the_runtime in
  let delivered = Hashtbl.create 64 in
  let snapshot = Ui.Event_cache.snapshot cache room_id in
  let boundary = backlog_boundary bot cache room_id (Array.length snapshot) in
  Array.iteri
    (fun index (event : Ui.Event_cache.event) ->
      if index < boundary then Hashtbl.replace delivered event.stable_id ())
    snapshot;
  (* Everything before the boundary counts as handled, so the cursor moves
     to the newest of it: a [`Skip] run must not leave a [`Handle] one to
     replay the history this one passed over. *)
  let rec newest index =
    if index < 0 then None
    else
      match (Ui.Event_cache.effective snapshot.(index)).event_id with
      | Some event_id -> Some event_id
      | None -> newest (index - 1)
  in
  Option.iter (write_cursor bot room_id) (newest (boundary - 1));
  let scan () =
    let snapshot = Ui.Event_cache.snapshot cache room_id in
    (* Back-pagination splices older events in front of what is already
       handled, so an event's position says whether it advances the cursor.
       Only one newer than everything delivered so far carries an id: the
       cursor moves forward or not at all. *)
    let newest = ref (-1) in
    Array.iteri
      (fun index (event : Ui.Event_cache.event) ->
        if Hashtbl.mem delivered event.stable_id then newest := index)
      snapshot;
    Array.iteri
      (fun index (event : Ui.Event_cache.event) ->
        if not (Hashtbl.mem delivered event.stable_id) then
          let p = P.of_event (Ui.Event_cache.effective event) in
          match p.content with
          (* A local echo has no id yet, and an event whose key has not
             arrived is delivered on decryption rather than as itself. *)
          | _ when p.event_id = None -> ()
          | P.Unable_to_decrypt -> ()
          | _ ->
              Hashtbl.replace delivered event.stable_id ();
              let at = if index > !newest then p.event_id else None in
              Option.iter
                (fun projected ->
                  Eio.Stream.add fibers.stream { event = projected; at })
                (event_of bot fibers.handle p))
      snapshot
  in
  let _initial, subscription =
    Ui.Observable.List.subscribe ~sw:bot.sw
      (Ui.Event_cache.events cache room_id)
  in
  fibers.subscription <- Some subscription;
  if fibers.finished then Ui.Observable.List.unsubscribe subscription
  else (
    scan ();
    let rec loop () =
      match Ui.Observable.List.next subscription with
      | None -> ()
      | Some _ ->
          scan ();
          loop ()
    in
    loop ())

let start_room bot room_id =
  let key = Id.Room_id.to_string room_id in
  if not (Hashtbl.mem bot.rooms_table key) then (
    let handle =
      Room.Internal.v ~runtime:bot.the_runtime ~client:(Context.client bot.ctx)
        ~sender:bot.sender ~clock:(Context.clock bot.ctx)
        ~encryption:(Context.encryption bot.ctx)
        room_id
    in
    let fibers =
      {
        handle;
        stream = Eio.Stream.create bot.the_spec.queue_depth;
        subscription = None;
        finished = false;
      }
    in
    Hashtbl.replace bot.rooms_table key fibers;
    (* The collector is forked first so that it takes its snapshot, and with
       it the backlog boundary, before any handler runs: a [Joined] handler
       that calls {!Room.backfill} would otherwise splice history in ahead of
       the boundary and have it counted as backlog. [Joined] is on the stream
       before the dispatcher exists, so it is still the room's first event. *)
    Eio.Fiber.fork ~sw:bot.sw (fun () -> collect bot fibers);
    Eio.Stream.add fibers.stream { event = Event.Joined handle; at = None };
    Eio.Fiber.fork ~sw:bot.sw (fun () -> room_loop bot fibers))

let finish_room bot room_id =
  let key = Id.Room_id.to_string room_id in
  match Hashtbl.find_opt bot.rooms_table key with
  | None -> ()
  | Some fibers ->
      fibers.finished <- true;
      (* Removed here, synchronously with the decision to leave, rather than
         left for [room_loop] to remove once it drains as far as the [Left]
         marker below: that can lag behind a backlog of already-queued
         events, and a rejoin in the meantime would find the old entry still
         present and silently do nothing, orphaning the room. *)
      Hashtbl.remove bot.rooms_table key;
      Option.iter Ui.Observable.List.unsubscribe fibers.subscription;
      Eio.Stream.add fibers.stream { event = Event.Left room_id; at = None }

let watch_rooms bot =
  let list = Ui.Room_list.all_rooms (Ui.Runtime.room_list bot.the_runtime) in
  let known = Hashtbl.create 8 in
  let scan () =
    Array.iter
      (fun (info : Ui.Room_list.room) ->
        let key = Id.Room_id.to_string info.id in
        if Hashtbl.find_opt known key <> Some info.membership then (
          Hashtbl.replace known key info.membership;
          match info.membership with
          | Matrix_client.Base_client.Invited ->
              Eio.Stream.add bot.global
                {
                  event =
                    Event.Invited
                      {
                        room_id = info.id;
                        inviter =
                          Matrix_client.Base_client.inviter
                            (Matrix_eio.Sync_service.state
                               (Ui.Runtime.sync_service bot.the_runtime))
                            info.id;
                      };
                  at = None;
                }
          | Matrix_client.Base_client.Joined -> start_room bot info.id
          | Matrix_client.Base_client.Left -> finish_room bot info.id
          | Matrix_client.Base_client.Knocked -> ()))
      (Ui.Observable.List.snapshot list)
  in
  let _, subscription = Ui.Observable.List.subscribe ~sw:bot.sw list in
  scan ();
  let rec loop () =
    match Ui.Observable.List.next subscription with
    | None -> ()
    | Some _ ->
        scan ();
        loop ()
  in
  loop ()

let deliverable (state : Ui.Runtime.sync_state) =
  match state with
  | Ui.Runtime.Live _ | Ui.Runtime.Failed _ | Ui.Runtime.Offline
  | Ui.Runtime.Stopped ->
      true
  | Ui.Runtime.Not_started | Ui.Runtime.Syncing -> false

(* [Live] carries the batch token, so it changes on every round; a bot is
   told the loop went live once, not once a second. *)
let same_state (a : Ui.Runtime.sync_state) (b : Ui.Runtime.sync_state) =
  match (a, b) with
  | Ui.Runtime.Live _, Ui.Runtime.Live _ -> true
  | Ui.Runtime.Failed a, Ui.Runtime.Failed b -> String.equal a b
  | Ui.Runtime.Offline, Ui.Runtime.Offline -> true
  | Ui.Runtime.Stopped, Ui.Runtime.Stopped -> true
  | Ui.Runtime.Not_started, Ui.Runtime.Not_started -> true
  | Ui.Runtime.Syncing, Ui.Runtime.Syncing -> true
  | _ -> false

let watch_sync bot =
  let value = Ui.Runtime.sync_state bot.the_runtime in
  let initial, subscription = Ui.Observable.Value.subscribe ~sw:bot.sw value in
  let last = ref None in
  let publish state =
    if
      deliverable state
      && not (Option.fold ~none:false ~some:(same_state state) !last)
    then (
      last := Some state;
      Eio.Stream.add bot.global { event = Event.Sync state; at = None };
      if state = Ui.Runtime.Stopped then stop bot)
  in
  publish initial;
  let rec loop () =
    match Ui.Observable.Value.next subscription with
    | None -> ()
    | Some state ->
        publish state;
        loop ()
  in
  loop ()

exception Finished

let wait_first_response bot =
  let value = Ui.Runtime.sync_state bot.the_runtime in
  let rec loop () =
    match Ui.Observable.Value.get value with
    | Ui.Runtime.Not_started | Ui.Runtime.Syncing ->
        if not bot.stopping then (
          Eio.Time.sleep (Context.clock bot.ctx) 0.02;
          loop ())
    | _ -> ()
  in
  loop ()

let start ?params ?on_start ctx the_spec =
  Eio.Switch.run @@ fun sw ->
  let own = Context.user_id ctx in
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:own
      ~display_name:(Id.User_id.localpart own) ()
  in
  let the_runtime =
    Ui.Runtime.create ~sw ~clock:(Context.clock ctx)
      ~client:(Context.client ctx) ~sync ?encryption:(Context.encryption ctx)
      ?event_store:(Context.event_store ctx) ()
  in
  let stop_promise, stop_resolver = Eio.Promise.create () in
  let bot =
    {
      ctx;
      the_spec;
      the_runtime;
      sender =
        Sent.Internal.tracker ~sw ~clock:(Context.clock ctx)
          (Ui.Runtime.send_queue the_runtime);
      sw;
      own;
      plugin = "matrix.bot/" ^ the_spec.spec_name;
      rooms_table = Hashtbl.create 8;
      global = Eio.Stream.create the_spec.queue_depth;
      stop_promise;
      stop_resolver;
      stopping = false;
    }
  in
  Ui.Runtime.start ?params the_runtime;
  Eio.Fiber.fork ~sw (fun () -> global_loop bot);
  Eio.Fiber.fork ~sw (fun () -> watch_sync bot);
  wait_first_response bot;
  Eio.Fiber.fork ~sw (fun () -> watch_rooms bot);
  (* A handler's exception is isolated (see [guard]); [on_start] gets the
     same treatment, so a caller's mistake there does not cancel every room
     fiber and take the bot down with it. There is no [Event.t] to report it
     against, so this logs directly rather than going through [on_error]. *)
  (match on_start with
  | None -> ()
  | Some f -> (
      try f bot with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Log.err (fun m -> m "on_start raised %s" (Printexc.to_string exn))));
  Eio.Promise.await bot.stop_promise;
  Ui.Runtime.stop the_runtime;
  (* The forked fibers never return of their own accord, so leaving the
     switch body by an exception is what cancels them. *)
  raise Finished

let run ?params ?on_start ctx the_spec =
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Context.save ctx))
    (fun () -> try start ?params ?on_start ctx the_spec with Finished -> ())
