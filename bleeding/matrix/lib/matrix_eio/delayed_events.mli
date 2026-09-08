(** delayed_events — events handed to the server now and sent later, raising
    instead of returning.

    Every function that performs a request raises [Eio.Io] carrying [Error.E e]
    where {!Matrix_client.Delayed_events} returns [Error e]. That module
    documents what each call does, which endpoint it uses and which errors it
    produces.

    MSC4140 is unstable, so check for {!unstable_prefix} in the homeserver's
    unstable features first. *)

val unstable_prefix : string
(** [unstable_prefix] is {!Matrix_client.Delayed_events.unstable_prefix}. *)

(** {1 Delay identifiers} *)

type delay_id = Matrix_client.Delayed_events.delay_id
(** The handle a server gives a scheduled event. *)

val delay_id_of_string : string -> delay_id
(** [delay_id_of_string s] is
    {!Matrix_client.Delayed_events.delay_id_of_string}. It performs no request
    and raises nothing. *)

val delay_id_to_string : delay_id -> string
(** [delay_id_to_string d] is
    {!Matrix_client.Delayed_events.delay_id_to_string}. *)

(** {1 Scheduling} *)

val send :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  delay_ms:int ->
  ?txn_id:string ->
  unit ->
  delay_id
(** [send c ~room_id ~event_type ~content ~delay_ms ()] is
    {!Matrix_client.Delayed_events.send} with the result unwrapped. *)

val send_state :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  state_key:string ->
  content:Jsont.json ->
  delay_ms:int ->
  delay_id
(** [send_state c ~room_id ~event_type ~state_key ~content ~delay_ms] is
    {!Matrix_client.Delayed_events.send_state} with the result unwrapped. *)

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
  delay_id
(** [send_current] is the current unified MSC4140 scheduling call, with the
    result unwrapped. Negative delays and sticky values outside zero through one
    hour raise before I/O. *)

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
  delay_id
(** [send_state_current] is {!send_current} with a state key. *)

(** {1 Managing a scheduled event} *)

(** What to do with a scheduled event. *)
type action = Matrix_client.Delayed_events.action =
  | Send  (** Send it now. *)
  | Cancel  (** Drop it without sending. *)
  | Restart  (** Reset its timer to the full delay. *)

val update : Client.t -> delay_id:delay_id -> action:action -> unit
(** [update c ~delay_id ~action] is {!Matrix_client.Delayed_events.update} with
    the result unwrapped. *)

val send_now : Client.t -> delay_id:delay_id -> unit
(** [send_now c ~delay_id] is {!Matrix_client.Delayed_events.send_now} with the
    result unwrapped. *)

val cancel : Client.t -> delay_id:delay_id -> unit
(** [cancel c ~delay_id] is {!Matrix_client.Delayed_events.cancel} with the
    result unwrapped. *)

val restart : Client.t -> delay_id:delay_id -> unit
(** [restart c ~delay_id] is {!Matrix_client.Delayed_events.restart} with the
    result unwrapped. An event whose timer is not restarted before it elapses is
    sent. *)

val update_current : Client.t -> delay_id:delay_id -> action:action -> unit
(** [update_current] uses the current unauthenticated path-action endpoint. *)

val send_now_current : Client.t -> delay_id:delay_id -> unit
val cancel_current : Client.t -> delay_id:delay_id -> unit
val restart_current : Client.t -> delay_id:delay_id -> unit

(** {1 Listing} *)

type delayed_event = Matrix_client.Delayed_events.delayed_event = {
  delay_id : delay_id;
  room_id : Matrix_proto.Id.Room_id.t;
  event_type : string;
  state_key : string option;
  content : Jsont.json;
  delay : int;
  running_since : Matrix_proto.Event.Timestamp.t;
  event_id : Matrix_proto.Id.Event_id.t option;
  finalised_ts : Matrix_proto.Event.Timestamp.t option;
  error : Matrix_client.Error.matrix_error option;
}
(** One scheduled event and how far it has got. *)

val list :
  Client.t -> ?from:string -> unit -> delayed_event Matrix_proto.Common.Page.t
(** [list c ()] is {!Matrix_client.Delayed_events.list} with the result
    unwrapped. *)

val get_current : Client.t -> delay_id:delay_id -> delayed_event
(** [get_current] fetches one current-format delayed event. *)

val list_current : Client.t -> unit -> delayed_event list
(** [list_current] fetches the current plain delayed-event list. *)

type status = Matrix_client.Delayed_events.status =
  | Scheduled
  | Sent
  | Failed
  | Cancelled

val status : delayed_event -> status
(** [status event] derives the current delayed-event status. *)
