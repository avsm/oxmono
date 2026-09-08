(** Live event queues and callback collectors.

    A registration owns a server queue and an in-memory acknowledgement cursor.
    Endpoint failures are returned as {!Error.t}. Cancellation propagates. *)

module Narrow : sig
  type t
  (** The type for conjunction terms in a registration filter. *)

  val channel : string -> t
  (** [channel name] is a filter for messages in the channel named [name]. *)

  val topic : string -> t
  (** [topic name] is a filter for the topic named [name]. *)

  val sender : Zulip.Id.User.t -> t
  (** [sender id] is a filter for messages sent by [id]. *)

  val direct : t
  (** [direct] is a filter for direct messages. *)

  val mentioned : t
  (** [mentioned] is a filter for messages mentioning the authenticated user. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a two-string operator and operand pair. *)
end

type t
(** The type for registered queue generations. A queue permits one outstanding
    poll. *)

type subscribers = [ `None | `All | `Partial ]
(** The type for subscriber detail in the initial snapshot. [`None] omits
    subscriber lists. [`All] requests complete lists. [`Partial] allows Zulip to
    return a partial list for a large channel. A partial list contains every
    subscribed bot and every subscriber active within the last 14 days. Zulip
    returns a complete list for channels with fewer than 250 subscribers. *)

type idle_timeout =
  | Seconds of int
  | Mobile
      (** The type for queue expiry after inactivity. [Seconds n] requires [n]
          between [1] and [604800]. [Mobile] requests Zulip's mobile queue
          lifetime. *)

type registration_options = {
  apply_markdown : bool;
  client_gravatar : bool;
  include_subscribers : subscribers;
  slim_presence : bool;
  presence_history_limit_days : int option;
  client_capabilities : (string * bool) list;
  fetch_event_types : Zulip.Event_type.t list option;
  idle_queue_timeout : idle_timeout option;
}
(** The type for queue registration options. [presence_history_limit_days]
    limits presence history in days. [fetch_event_types] selects initial
    snapshot families independently of live [event_types]. [client_capabilities]
    declares supported wire formats. Options absent from these fields are left
    to Zulip. *)

val default_registration : registration_options
(** [default_registration] disables Markdown rendering and subscriber lists,
    reports that the client cannot compute Gravatar URLs, and enables slim
    presence. The presence history limit is omitted, for which Zulip defaults to
    14 days. The queue lifetime is omitted, for which Zulip defaults to 10
    minutes. Snapshot families default to the selected live event types.
    [client_capabilities] is empty. Registration adds
    [notification_settings_null = true] when absent. *)

val register :
  Client.t ->
  ?options:registration_options ->
  ?event_types:Zulip.Event_type.t list ->
  ?narrow:Narrow.t list ->
  ?all_public_streams:bool ->
  unit ->
  (t, Error.t) result
(** [register client ~options ~event_types ~narrow ~all_public_streams ()] is a
    new event queue. [options] defaults to {!default_registration}.
    [event_types] defaults to all supported event types. [narrow] defaults to no
    filtering. [all_public_streams] defaults to [false]. Setting it to [true]
    includes accessible public channels without requiring subscriptions. A
    negative presence history limit or invalid idle timeout returns
    {!Error.t.constructor-Invalid_request} before sending a request. Invalid
    registration metadata also returns {!Error.t.constructor-Invalid_request}.
    Encoding, request, and response-decoding failures are returned as
    {!Error.t}. *)

val id : t -> string
(** [id queue] is the server-assigned identifier of [queue]. *)

val last_event_id : t -> int
(** [last_event_id queue] is the acknowledged event cursor. [-1] means that no
    event has been acknowledged. *)

val longpoll_timeout : t -> float
(** [longpoll_timeout queue] is the advertised blocking poll duration in
    seconds. An absent server value defaults to [90.]. *)

val initial_state : t -> Jsont.json
(** [initial_state queue] is the complete registration response, including
    unknown fields. *)

val state : t -> Initial_state.t
(** [state queue] is the initial snapshot of [queue]. Individual snapshot
    families are decoded by {!Initial_state} accessors. *)

module Batch : sig
  type t
  (** Events fetched from one queue generation, ordered by increasing event ID.
  *)

  val events : t -> Zulip.Event.t list
  (** [events batch] is the fetched sequence, without acknowledging it. *)

  val length : t -> int
  (** [length batch] is the number of fetched events. *)
end

val get_events :
  t -> Client.t -> ?dont_block:bool -> unit -> (Batch.t, Error.t) result
(** [get_events queue client ~dont_block ()] fetches a batch without advancing
    the cursor. [dont_block] defaults to [false]. Blocking polls allow ten
    seconds beyond the advertised server timeout. Nonblocking polls have a
    30-second request timeout. Concurrent polls of a queue return
    {!Error.t.constructor-Invalid_request}. Redeliveries at or before the
    acknowledged cursor are filtered, and duplicate identifiers within a
    response are removed. *)

val ack : ?count:int -> t -> Batch.t -> (unit, Error.t) result
(** [ack queue batch] acknowledges the first [count] events, defaulting to the
    whole batch. [count] is a cumulative prefix length, not an increment. Call
    after accepting those events. Repeated acknowledgement never moves the
    cursor backwards. A batch from another registration, or a count outside [0]
    through {!Batch.length}, returns {!Error.t.constructor-Invalid_request}. *)

val delete : ?timeout:float -> t -> Client.t -> (unit, Error.t) result
(** [delete ~timeout queue client] removes the server queue. [timeout] defaults
    to the deadline configured on [client]. Existing batches and the local
    cursor remain readable. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf queue] writes the queue identifier and cursor to [ppf]. *)

type control =
  | Continue
  | Stop
      (** The type for callback decisions. [Stop] accepts the current event and
          ends collection before later events in the batch are accepted. *)

val iter :
  Client.t ->
  ?options:registration_options ->
  ?event_types:Zulip.Event_type.t list ->
  ?narrow:Narrow.t list ->
  ?all_public_streams:bool ->
  ?on_recover:(Error.t -> unit) ->
  ?on_registered:(t -> unit) ->
  (Zulip.Event.t -> control) ->
  (unit, Error.t) result
(** [iter client ~options ~event_types ~narrow ~all_public_streams ~on_recover
     ~on_registered callback] collects events until [callback] returns [Stop] or
    a terminal error occurs. Registration arguments use the defaults of
    {!register}. [on_recover] and [on_registered] default to doing nothing.
    [on_registered] runs after each successful registration, before polling.
    [on_recover] receives registration and polling failures, including terminal
    failures. Heartbeats are acknowledged without calling [callback]. Other
    events are acknowledged individually after [callback] returns.

    Expired queues are replaced. Transient failures honor Retry-After and
    otherwise use exponential delays from one to 30 seconds. The active queue is
    deleted on exit with a two-second deadline. A missing transport clock
    returns {!Error.t.constructor-Invalid_request}. Callback exceptions and
    cancellation propagate after cleanup. *)

val iter_messages :
  Client.t ->
  ?options:registration_options ->
  ?narrow:Narrow.t list ->
  ?all_public_streams:bool ->
  ?on_recover:(Error.t -> unit) ->
  (Zulip.Message.t -> control) ->
  (unit, Error.t) result
(** [iter_messages client ~options ~narrow ~all_public_streams ~on_recover
     callback] collects message events with the lifecycle and argument defaults
    of {!iter}. A malformed message returns {!Error.t.constructor-Json} after
    unregistering the queue. *)
