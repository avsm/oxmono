(** delayed_events — events handed to the server now and sent later, MSC4140.

    A delayed event is scheduled with a timer. The server sends it when the
    timer elapses, unless the client cancels it or restarts the timer first.
    MatrixRTC uses one to hang up a call whose client vanished.

    The wire names carry the [org.matrix.msc4140] prefix and may change. The
    original functions retain the first endpoint generation for compatibility;
    functions suffixed [_current] expose the current MSC4140 routes explicitly.

    - Scheduling reuses the stable send endpoints with an extra query parameter,
      [org.matrix.msc4140.delay]:
      [PUT /_matrix/client/v3/rooms/{roomId}/send/{eventType}/{txnId}] and
      [PUT /_matrix/client/v3/rooms/{roomId}/state/{eventType}/{stateKey}].
    - [POST
       /_matrix/client/unstable/org.matrix.msc4140/delayed_events/{delayId}]
      updates one.
    - [GET /_matrix/client/unstable/org.matrix.msc4140/delayed_events] lists
      them.
    - The current scheduling route is
      [PUT .../rooms/{roomId}/delayed_event/{eventType}/{txnId}], with delay
      parameters in its body, and its management route adds the action as a
      final path segment. *)

val unstable_prefix : string
(** [unstable_prefix] is ["org.matrix.msc4140"]. A server that serves these
    endpoints lists it in {!Server.versions.unstable_features}. *)

(** {1 Delay identifiers} *)

type delay_id = private string
(** The type for the handle the server gives a scheduled event. It is opaque and
    is only ever echoed back to the server. *)

val delay_id_of_string : string -> delay_id
(** [delay_id_of_string s] is the delay id [s] spells. *)

val delay_id_to_string : delay_id -> string
(** [delay_id_to_string id] is [id] as the server spelled it. *)

(** {1 Scheduling} *)

val send :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  delay_ms:int ->
  ?txn_id:string ->
  unit ->
  (delay_id, Error.t) result
(** [send t ~room_id ~event_type ~content ~delay_ms ()] is
    [PUT /_matrix/client/v3/rooms/{roomId}/send/{eventType}/{txnId}] with
    [?org.matrix.msc4140.delay={delay_ms}] (MSC4140), and is the delay id the
    server allocated. [delay_ms] is how long the event is held, in milliseconds.
    [txn_id] defaults to a fresh transaction id drawn from the client's
    randomness, as for a normal send. *)

val send_state :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  state_key:string ->
  content:Jsont.json ->
  delay_ms:int ->
  (delay_id, Error.t) result
(** [send_state t ~room_id ~event_type ~state_key ~content ~delay_ms] is
    [PUT /_matrix/client/v3/rooms/{roomId}/state/{eventType}/{stateKey}] with
    [?org.matrix.msc4140.delay={delay_ms}] (MSC4140), and is the delay id the
    server allocated. *)

(** {1 Current MSC4140 scheduling} *)

val send_current :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  delay_ms:int ->
  ?txn_id:string ->
  ?sticky_duration_ms:int ->
  ?state_key:string ->
  unit ->
  (delay_id, Error.t) result
(** [send_current] uses the current unified MSC4140 endpoint. The JSON body
    contains [delay], [content] and, when supplied, [state_key]. A transaction
    ID is generated before the request when absent. Sticky duration is sent as
    the MSC4354 query parameter; callers must separately authorize it by
    checking the server's MSC4354 capability. A negative delay, or a sticky
    duration outside the MSC4354 range of zero through one hour, raises
    [Invalid_argument] before any request is made. *)

val send_state_current :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  state_key:string ->
  content:Jsont.json ->
  delay_ms:int ->
  ?txn_id:string ->
  ?sticky_duration_ms:int ->
  unit ->
  (delay_id, Error.t) result
(** [send_state_current] is [send_current] with a state key. *)

(** {1 Managing a scheduled event} *)

(** The type for what a server is asked to do with a scheduled event. *)
type action =
  | Send  (** ["send"], send it now. *)
  | Cancel  (** ["cancel"], drop it. *)
  | Restart  (** ["restart"], reset its timer to the full delay. *)

val update :
  Client.t -> delay_id:delay_id -> action:action -> (unit, Error.t) result
(** [update t ~delay_id ~action] is
    [POST /_matrix/client/unstable/org.matrix.msc4140/delayed_events/{delayId}]
    (MSC4140). *)

val send_now : Client.t -> delay_id:delay_id -> (unit, Error.t) result
(** [send_now t ~delay_id] is {!update} with {!Send}. *)

val cancel : Client.t -> delay_id:delay_id -> (unit, Error.t) result
(** [cancel t ~delay_id] is {!update} with {!Cancel}. *)

val restart : Client.t -> delay_id:delay_id -> (unit, Error.t) result
(** [restart t ~delay_id] is {!update} with {!Restart}, which moves
    {!delayed_event.running_since} to now and gives the event its full
    {!delayed_event.delay} again. An event whose timer is not restarted before
    it elapses is sent. *)

val update_current :
  Client.t -> delay_id:delay_id -> action:action -> (unit, Error.t) result
(** [update_current] uses the current unauthenticated path-action form:
    [POST .../delayed_events/{delayId}/{action}] with an empty JSON object. The
    delay ID is the capability for this management request; no bearer token is
    sent and no fallback or retry is attempted. *)

val send_now_current : Client.t -> delay_id:delay_id -> (unit, Error.t) result
(** [send_now_current] is {!update_current} with {!Send}. *)

val cancel_current : Client.t -> delay_id:delay_id -> (unit, Error.t) result
(** [cancel_current] is {!update_current} with {!Cancel}. *)

val restart_current : Client.t -> delay_id:delay_id -> (unit, Error.t) result
(** [restart_current] is {!update_current} with {!Restart}. *)

(** {1 Listing} *)

type delayed_event = {
  delay_id : delay_id;  (** The handle the other calls take. *)
  room_id : Matrix_proto.Id.Room_id.t;  (** Where the event will be sent. *)
  event_type : string;  (** The event's type. *)
  state_key : string option;  (** Present for a delayed state event. *)
  content : Jsont.json;  (** The event's content. *)
  delay : int;  (** The delay it was scheduled with, in milliseconds. *)
  running_since : Matrix_proto.Event.Timestamp.t;
      (** When the current timer started. A {!restart} moves it forward. *)
  event_id : Matrix_proto.Id.Event_id.t option;
      (** Set once the event has been sent. *)
  finalised_ts : Matrix_proto.Event.Timestamp.t option;
      (** When the event was sent or cancelled. *)
  error : Error.matrix_error option;
      (** Why the event was cancelled, when an error cancelled it. *)
}
(** The type for one scheduled event, as the server reports it. Current
    responses require [content], [delay] and [running_since]; the legacy list
    retains its historical absent-field defaults. *)

val list :
  Client.t ->
  ?from:string ->
  unit ->
  (delayed_event Matrix_proto.Common.Page.t, Error.t) result
(** [list t ()] is
    [GET /_matrix/client/unstable/org.matrix.msc4140/delayed_events] (MSC4140),
    a page of everything this user has scheduled and not yet finalised. [from]
    is the {!Matrix_proto.Common.Page.t.next_batch} of an earlier page and
    defaults to absent, which starts at the first page. *)

val get_current :
  Client.t -> delay_id:delay_id -> (delayed_event, Error.t) result
(** [get_current] fetches one delayed event from the current MSC4140 endpoint.
*)

val list_current : Client.t -> unit -> (delayed_event list, Error.t) result
(** [list_current] fetches the current plain [{"delayed_events":[...]}]
    response. *)

type status =
  | Scheduled
  | Sent
  | Failed
  | Cancelled
      (** The status derived from a delayed event's finalisation fields. *)

val status : delayed_event -> status
(** [status event] follows MSC4140's status precedence: scheduled when not
    finalised, sent when an event ID exists, failed when an error exists, and
    cancelled otherwise. *)
