(** Concurrent Zulip bot collection and dispatch.

    Bot specifications compose handlers and commands before a run. A running bot
    preserves event order within each room and can process different rooms
    concurrently. *)

type t
(** The type for running bots. *)

type spec
(** The type for immutable bot specifications. *)

type plugin = spec -> spec
(** The type for specification transformations. *)

type handler = t -> Event.t -> unit
(** The type for event handlers. *)

type command_info = { name : string; args : string option; doc : string option }
(** The type for command metadata in registration order. *)

val v :
  ?prefix:string ->
  ?all_messages:bool ->
  ?ignore_own:bool ->
  ?ignore_bots:bool ->
  ?authorize:(Room.t -> Zulip.Id.User.t -> bool) ->
  ?queue_depth:int ->
  ?total_queue_depth:int ->
  ?workers:int ->
  ?registration:Zulip_eio.Event_queue.registration_options ->
  ?event_types:Zulip.Event_type.t list ->
  unit ->
  spec
(** [v ~prefix ~all_messages ~ignore_own ~ignore_bots ~authorize ~queue_depth
     ~total_queue_depth ~workers ~registration ~event_types ()] is an empty bot
    specification. [prefix] defaults to [!]. [all_messages] defaults to [false].
    [ignore_own] and [ignore_bots] default to [true]. [authorize] defaults to
    rejecting every administrative command.

    [queue_depth] defaults to [256] and bounds admitted events waiting in one
    room. [total_queue_depth] defaults to [1024] and bounds admitted events
    across rooms. [workers] defaults to [4] and limits rooms processed
    concurrently. Each room is processed sequentially.

    [registration] defaults to {!Zulip_eio.Event_queue.default_registration}
    with realm-user state and the empty-topic-name, bulk-message-deletion, and
    incomplete-user-list capabilities enabled. It must request Markdown message
    content. [event_types] defaults to messages, message edits, reactions,
    message deletions, and realm-user updates. Realm-user events are always
    requested for context updates. Additional valid event types are delivered
    through {!on_custom}.

    @raise Stdlib.exception-Invalid_argument
      if a queue depth or [workers] is not positive, or if [registration]
      requests rendered HTML message content. *)

val on : handler -> spec -> spec
(** [on handler spec] is [spec] with [handler] appended. Handlers run in
    registration order. *)

val on_message : (t -> Event.message -> unit) -> spec -> spec
(** [on_message handler spec] is [spec] with [handler] appended for message
    events. *)

val on_edit : (t -> Event.edit -> unit) -> spec -> spec
(** [on_edit handler spec] is [spec] with [handler] appended for message-edit
    events. *)

val on_reaction : (t -> Event.reaction -> unit) -> spec -> spec
(** [on_reaction handler spec] is [spec] with [handler] appended for reaction
    events. *)

val on_delete : (t -> Event.delete -> unit) -> spec -> spec
(** [on_delete handler spec] is [spec] with [handler] appended for
    message-deletion events. *)

val on_custom : (t -> Event.custom -> unit) -> spec -> spec
(** [on_custom handler spec] is [spec] with [handler] appended for valid custom
    events from the collector or an adapter. *)

val on_malformed : (t -> Event.malformed -> unit) -> spec -> spec
(** [on_malformed handler spec] is [spec] with [handler] appended for malformed
    supported protocol events. *)

val on_sync : (t -> Event.sync -> unit) -> spec -> spec
(** [on_sync handler spec] is [spec] with [handler] appended for collector state
    changes. *)

val command :
  name:string ->
  ?args:string ->
  ?doc:string ->
  ?admin:bool ->
  (t -> Event.command -> unit) ->
  spec ->
  spec
(** [command ~name ~args ~doc ~admin handler spec] is [spec] with [handler]
    appended for commands named [name]. [args] and [doc] default to omitted help
    metadata. [admin] defaults to [false]. An unauthorized administrative
    command replies with an error and does not call [handler]. Commands with the
    same name run in registration order. *)

val help : ?command:string -> spec -> spec
(** [help ~command spec] is [spec] with a command that replies with the
    registered command list. [command] defaults to [help]. *)

val on_unknown_command : (t -> Event.command -> unit) -> spec -> spec
(** [on_unknown_command handler spec] is [spec] with [handler] as the callback
    for commands having no registered handler in their active plugin scope. It
    replaces any previous unknown-command callback. Without one, the bot sends a
    default unknown-command reply. *)

val on_error : (t -> Event.t -> exn -> unit) -> spec -> spec
(** [on_error handler spec] is [spec] with [handler] as the callback for
    exceptions raised by event handlers and the start callback. It replaces any
    previous error callback. Handler exceptions do not stop later handlers.
    Exceptions from the error callback are logged. Eio cancellation propagates.
*)

val only : (Event.t -> bool) -> plugin -> plugin
(** [only predicate plugin] is [plugin] with [predicate] applied to handlers and
    command registrations added by [plugin]. Registrations already present in
    the input specification retain their existing scope. *)

val in_rooms : Room.id list -> plugin -> plugin
(** [in_rooms rooms plugin] is [plugin] restricted to events whose room has an
    identifier in [rooms]. Events without a room do not match. *)

val from_users : Zulip.Id.User.t list -> plugin -> plugin
(** [from_users users plugin] is [plugin] restricted to events whose envelope
    sender is in [users]. Events without a sender do not match. *)

val commands : spec -> command_info list
(** [commands spec] is the command metadata of [spec] in registration order. *)

val run : ?on_start:(t -> unit) -> Context.t -> spec -> unit
(** [run ~on_start context spec] runs [spec] with [context] until stopped,
    cancelled, or failed. [on_start] defaults to no callback and runs before
    collector registration. Calling {!stop} ends the collector and cancels
    queued room work without draining it. Event-queue deregistration and the
    [Event.Stopped] callback each have a two-second shutdown budget.

    The run owns a nested switch for its collector and workers. It does not
    release the caller-owned switch of [context], so the context and its sender
    remain usable after a normal stop. Callback exceptions are reported through
    {!on_error}. Cancellation propagates.

    @raise Zulip_eio.Error.exception-E
      if the collector ends with a terminal error. *)

val run_result :
  ?on_start:(t -> unit) -> Context.t -> spec -> (unit, Zulip_eio.Error.t) result
(** [run_result ~on_start context spec] runs [spec] as {!run} does and returns
    terminal collector failures as [Error]. [on_start] defaults to no callback.
    Callback exceptions are reported through {!on_error}. Cancellation
    propagates. *)

val stop : t -> unit
(** [stop bot] requests termination of [bot]. It is idempotent. It does not
    release the switch that owns the bot's context or wait for queued handlers.
*)

val is_running : t -> bool
(** [is_running bot] is [true] until termination of [bot] is requested. *)

val context : t -> Context.t
(** [context bot] is the context used by [bot]. *)

val user_id : t -> Zulip.Id.User.t
(** [user_id bot] is the authenticated user identifier of [bot]. *)

val plugin_store : t -> Plugin_store.t
(** [plugin_store bot] is the plugin store of [bot]'s context. *)

val spec : t -> spec
(** [spec bot] is the specification used to start [bot]. *)

val rooms : t -> Room.t list
(** [rooms bot] is the unordered list of rooms with queued, waiting, or active
    dispatch work. Idle rooms are removed. *)

val find_room : t -> Room.id -> Room.t option
(** [find_room bot id] is the room with active dispatch work identified by [id],
    if present. *)

val dispatch : t -> Event.t -> unit
(** [dispatch bot event] projects and dispatches [event] through [bot]'s
    filters, commands, and handlers. Room events that pass projection block
    until both global and per-room capacity admit them, then return before
    handlers necessarily run. Events in one room run sequentially. Different
    rooms can run concurrently. Events without a room run immediately in the
    calling fiber and return after their handlers complete. Custom adapter
    events use their optional envelope to select the same dispatch behavior.

    Message dispatch applies own-user, bot-user, activation, mention removal,
    and command parsing rules. Other event variants bypass message projection.
    Sync events remain accepted during collector shutdown.

    @raise Stdlib.exception-Invalid_argument if [bot] has stopped. *)
