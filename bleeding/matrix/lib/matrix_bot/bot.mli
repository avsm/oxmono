(** bot — building a bot from handlers, and running it.

    A {!type-spec} is immutable. Every registration returns a new one, so a
    plugin is a function [spec -> spec] and a bot is a pipeline of them. {!run}
    turns a spec into fibers and returns when {!stop} is called or the sync loop
    stops. *)

type t
(** The type for running bots. A handler is handed one, and reads the runtime,
    the rooms and the plugin store through it. *)

type spec
(** The type for what a bot will do, before it runs. *)

type plugin = spec -> spec
(** The type for a unit of behaviour, as the registrations it adds to a spec. *)

type handler = t -> Event.t -> unit
(** The type for what runs on every event of a room, in registration order. *)

val v :
  ?name:string ->
  ?prefix:string ->
  ?admin_level:int ->
  ?auto_join:bool ->
  ?ignore_notices:bool ->
  ?ignore_own:bool ->
  ?backlog:[ `Skip | `Handle ] ->
  ?queue_depth:int ->
  unit ->
  spec
(** [v ()] is a spec with no handlers. [name] is used in logs and as the plugin
    the bot's own cursors are stored under, and defaults to ["bot"]. [prefix] is
    what a command begins with, and defaults to ["!"]. [admin_level] is the
    power level {!is_admin} and [~admin:true] commands require, and defaults to
    50. [auto_join] accepts every invitation, and is [true] by default.
    [ignore_notices] drops [m.notice] messages, and is [true] by default.
    [ignore_own] drops the bot's own events, and is [true] by default. [backlog]
    says what to do with events that arrived while the bot was down, and
    defaults to [`Skip]. [queue_depth] bounds each room's pending events, and
    defaults to 256, beyond which the collector waits and the sync loop is not
    slowed.

    The cursor is the id of the last event the bot handled in a room, kept in
    {!Plugin_store} under the key ["cursor"] for the plugin
    ["matrix.bot/" ^ name]. On start, [`Skip] passes over what the event cache
    already holds and moves the cursor to the newest of it, and [`Handle]
    delivers what follows the cursor. A room with no cursor, or one whose cursor
    the cache no longer holds, is skipped either way, so a bot joining a room
    does not answer its history. Without a {!Matrix_ui.Event_store} the cache
    starts empty and the first sync window is all a restarted bot can see.

    The cursor only moves forward. History a handler pages in with
    {!Room.backfill} is delivered like anything else, but it is older than what
    the bot has handled and so does not rewind the cursor. *)

(** {1 Registering handlers}

    Every registration takes the spec last, so a partial application is a
    {!type-plugin} and a bot is a pipeline of them:
    [Bot.v () |> Bot.on_message say |> Bot.command ~name:"ping" pong].

    Handlers run in registration order, one room at a time, in that room's event
    order. A handler that raises is reported through {!on_error} and the next
    handler runs. *)

val on : handler -> spec -> spec
(** [on f spec] runs [f] on every event, whatever its kind. *)

val on_message : (t -> Event.message -> unit) -> spec -> spec
(** [on_message f spec] runs [f] on every {!Event.Message}, which is a message
    that is not a command. *)

val on_edit : (t -> Event.edit -> unit) -> spec -> spec
(** [on_edit f spec] runs [f] on every {!Event.Edit}. *)

val on_reaction : (t -> Event.reaction -> unit) -> spec -> spec
(** [on_reaction f spec] runs [f] on every {!Event.Reaction}. *)

val on_membership : (t -> Event.membership -> unit) -> spec -> spec
(** [on_membership f spec] runs [f] on every {!Event.Membership}, the bot's own
    included. *)

val on_room_state : (t -> Event.room_state -> unit) -> spec -> spec
(** [on_room_state f spec] runs [f] on every {!Event.Room_state}. *)

val on_custom : (t -> Event.custom -> unit) -> spec -> spec
(** [on_custom f spec] runs [f] on every {!Event.Custom}. *)

val on_invite : (t -> Event.invitation -> unit) -> spec -> spec
(** [on_invite f spec] runs [f] on every {!Event.Invited}, before the automatic
    join [~auto_join] makes. *)

val on_join : (t -> Room.t -> unit) -> spec -> spec
(** [on_join f spec] runs [f] on every {!Event.Joined}, which is the first event
    of a room the bot handles. *)

val on_leave : (t -> Matrix_proto.Id.Room_id.t -> unit) -> spec -> spec
(** [on_leave f spec] runs [f] on every {!Event.Left}, which is the last event
    of a room the bot handled. *)

val on_sync : (t -> Matrix_ui.Runtime.sync_state -> unit) -> spec -> spec
(** [on_sync f spec] runs [f] on every {!Event.Sync}, outside any room. *)

val command :
  name:string ->
  ?args:string ->
  ?doc:string ->
  ?admin:bool ->
  (t -> Event.command -> unit) ->
  spec ->
  spec
(** [command ~name f spec] runs [f] on a command whose name is [name], that is
    on a message beginning with the spec's prefix and [name]. [args] is how the
    arguments are written in {!help}, and defaults to nothing. [doc] is the
    one-line description {!help} shows, and defaults to nothing. With
    [~admin:true] the handler runs only for a sender whose power level in the
    room is at least the spec's [admin_level], and others are told so. [admin]
    is [false] by default. *)

val help : ?command:string -> spec -> spec
(** [help spec] registers a command that answers with every registered command,
    its arguments and its doc, as a notice in reply. [command] is the name it
    answers to, and defaults to ["help"]. The list is read when the command
    runs, so [help] may be registered anywhere in the pipeline and still names
    what came after it. *)

val on_unknown_command : (t -> Event.command -> unit) -> spec -> spec
(** [on_unknown_command f spec] runs [f] on a command no handler claims, in
    place of the default, which answers with a notice pointing at [help]. *)

val only : (Event.t -> bool) -> plugin -> plugin
(** [only p plugin] is [plugin] with the handlers it registers run only on
    events that satisfy [p]. Handlers registered before it are untouched. *)

val in_rooms : Matrix_proto.Id.Room_id.t list -> plugin -> plugin
(** [in_rooms room_ids plugin] is {!only} for events of those rooms. An event
    with no room, {!Event.Sync} among them, reaches no handler of [plugin]. *)

val from_users : Matrix_proto.Id.User_id.t list -> plugin -> plugin
(** [from_users user_ids plugin] is {!only} for events those users caused. An
    event with no sender reaches no handler of [plugin]. *)

val on_error : (t -> Event.t -> exn -> unit) -> spec -> spec
(** [on_error f spec] reports a handler that raised through [f], in place of the
    default, which logs the exception with the event on {!Logging.src}. An [f]
    that itself raises is logged. *)

type command_info = {
  name : string;  (** Without the prefix. *)
  args : string option;
  doc : string option;
}
(** The type for a registered command, as {!help} renders it. *)

val commands : spec -> command_info list
(** [commands spec] is the registered commands, in registration order. *)

(** {1 Running} *)

val run :
  ?params:Matrix_client.Sync.params ->
  ?on_start:(t -> unit) ->
  Context.t ->
  spec ->
  unit
(** [run ctx spec] starts a {!Matrix_ui.Runtime} over the context, waits for its
    first response, joins invitations when the spec says so, and delivers events
    until {!stop} or a {!Event.Sync} {!Matrix_ui.Runtime.Stopped}. [params] is
    what each [/sync] asks for, and defaults to
    {!Matrix_client.Sync.default_params}.

    Each room has a fiber. The sync loop, the send queue and the collectors live
    on a switch [run] owns, so returning tears them all down. The encryption
    machine is saved on the way out, on an exception and on cancellation alike.

    [on_start] is called with the running bot once its fibers are up and before
    any event is handled, which is how a caller outside the handlers reaches
    {!stop}. Without it nothing but a handler can stop the bot. An exception
    from [on_start] is logged and does not stop the bot, the same as an
    exception from a handler. *)

val stop : t -> unit
(** [stop bot] asks {!run} to return. Idempotent, and safe from any fiber. *)

(** {1 From inside a handler} *)

val context : t -> Context.t
(** [context bot] is what the bot runs with. *)

val user_id : t -> Matrix_proto.Id.User_id.t
(** [user_id bot] is who the bot is logged in as. *)

val plugin_store : t -> Plugin_store.t
(** [plugin_store bot] is where a plugin keeps its values. *)

val spec : t -> spec
(** [spec bot] is what [bot] was run with, which is where {!commands} reads the
    list a running bot shows. *)

val runtime : t -> Matrix_ui.Runtime.t
(** [runtime bot] is the runtime [run] built, for a plugin that wants the room
    list, the event cache or {!Matrix_ui.Runtime.join}. *)

val find_room : t -> Matrix_proto.Id.Room_id.t -> Room.t option
(** [find_room bot room_id] is the handle for a room the bot is handling, and
    [None] for any other room. *)

val rooms : t -> Room.t list
(** [rooms bot] is a handle for every room the bot is handling, in no particular
    order. *)

val is_admin : t -> Room.t -> Matrix_proto.Id.User_id.t -> bool
(** [is_admin bot room user_id] is [true] when the user's power level in the
    room is at least the spec's [admin_level]. *)
