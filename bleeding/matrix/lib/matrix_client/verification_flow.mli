(** verification_flow — a table of in-flight verifications.

    A device may be running several verifications at once, with different users
    and by different methods. This is the table that holds them, keyed by flow
    identifier, and routes each incoming event to the state machine that owns
    it.

    Every transition returns a {!type-step}, which is the session it touched and
    the events to send in order. A failure is not an error but a session whose
    stage has become {!Cancelled}, together with the [m.key.verification.cancel]
    to send. The transport is the caller's, and nothing here does any I/O.

    @see <https://spec.matrix.org/v1.11/client-server-api/#key-verification-framework>
      Key verification framework *)

(** What a session is doing. *)
type stage =
  | Requested  (** A request was sent or received and nobody has answered. *)
  | Ready of Verification_base.Method.t list
      (** Both sides answered, with these methods in common. *)
  | Sas of Verification_sas.t  (** A short authentication string is running. *)
  | Showing_qr of Verification_qr.t  (** This device is displaying a code. *)
  | Scanned_qr of Verification_qr.t  (** This device scanned the peer's code. *)
  | Done  (** The flow finished and the keys are verified. *)
  | Cancelled of Verification_base.Cancel_code.t

type session
(** The type for one verification, from the request to the last event. *)

type t
(** The type for a table of sessions. *)

type routed_send = session * Verification_base.Message.t
(** A message paired with the session whose transport and recipient it uses. *)

type directed_send = Matrix_proto.Id.Device_id.t * Verification_base.Message.t
(** A message to one concrete device of the user, rather than to the selected
    device of a session. *)

type step = {
  session : session option;
      (** The session the event belonged to, and [None] when there was none. *)
  send : Verification_base.Message.t list;
      (** Messages addressed to the session's selected device. *)
  send_to : directed_send list;
      (** Messages addressed to the listed concrete devices. Wildcard recipients
          are never represented here. *)
}
(** The type for the result of a transition. *)

val create : ?timeout:int64 -> unit -> t
(** [create ()] is an empty table. [timeout] is how long a session may run
    before {!tick} fails it, in milliseconds, and defaults to 600000. *)

val max_active_inbound_per_device : int
(** Maximum number of active inbound flows accepted from one peer device. *)

val max_active_inbound : int
(** Maximum number of active inbound flows accepted by one table. *)

val find : t -> string -> session option
(** [find t id] is the session of [t] whose flow identifier is [id]. *)

val sessions : t -> session list
(** [sessions t] is every session [t] holds, in no particular order. *)

val remove : t -> string -> unit
(** [remove t id] forgets the session whose flow identifier is [id]. A finished
    session is kept until it is removed, so a long-lived table has to be swept.
*)

(** {1 Reading a session} *)

val session_transaction : session -> Verification_base.Transaction.t
(** [session_transaction s] is how [s] is addressed. *)

val session_stage : session -> stage
(** [session_stage s] is what [s] is doing. *)

val session_their_user_id : session -> Matrix_proto.Id.User_id.t
(** [session_their_user_id s] is the user on the other side of [s]. *)

val session_their_device_id : session -> Matrix_proto.Id.Device_id.t option
(** [session_their_device_id s] is the device on the other side of [s], and
    [None] until one has answered. *)

val session_their_methods : session -> Verification_base.Method.t list
(** [session_their_methods s] is the methods the other side offered, and the
    empty list until it has. *)

val session_we_requested : session -> bool
(** [session_we_requested s] is [true] when this device opened [s]. *)

val session_requested_devices : session -> To_device.recipient list
(** [session_requested_devices s] is the concrete or wildcard recipient set of a
    locally opened to-device request. It is empty for incoming and in-room
    requests. *)

val session_sas : session -> Verification_sas.t option
(** [session_sas s] is the short authentication string state of [s], and [None]
    when [s] never ran one. It survives the end of the flow, so the keys a
    completed session verified can still be read from it. *)

(** {1 Driving a flow} *)

type request = {
  session : session;  (** The session opened for the request. *)
  message : Verification_base.Message.t;
      (** The request, for the caller's own bookkeeping. *)
  to_device : To_device.messages;  (** What to hand to {!To_device.send}. *)
}
(** The type for a verification request that has been built but not yet sent. *)

val request :
  random:Random.t ->
  now:Matrix_proto.Event.Timestamp.t ->
  from_device:Matrix_proto.Id.Device_id.t ->
  ?methods:Verification_base.Method.t list ->
  their_user_id:Matrix_proto.Id.User_id.t ->
  devices:To_device.recipient list ->
  t ->
  request
(** [request ~random ~now ~from_device ~their_user_id ~devices t] is
    {!Verification.request_to_device} with the new session tracked in [t]. The
    concrete [devices] are retained so that when one accepts, the other concrete
    recipients can be cancelled with [m.accepted], and so a cancellation from
    one recipient can be mirrored to all devices that received the request. A
    wildcard recipient is retained but never expanded. [methods] defaults to
    {!Verification.Method.all}. *)

val take_pending_sends : t -> routed_send list
(** [take_pending_sends t] removes and returns transport-routed cancellation
    messages produced while inserting a request. As in matrix-rust-sdk, a new
    request for a user with another active, differently identified request
    cancels both with [m.user]; an exact duplicate leaves the original alone.
    Callers should drain this after {!val-request}, {!request_in_room}, and
    {!handle}. *)

val request_in_room :
  now:Matrix_proto.Event.Timestamp.t ->
  from_device:Matrix_proto.Id.Device_id.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  their_user_id:Matrix_proto.Id.User_id.t ->
  content:Matrix_proto.Event.Key_verification_request_message_content.t ->
  t ->
  request
(** [request_in_room ~now ~from_device ~room_id ~event_id ~their_user_id
     ~content t] tracks an in-room request after its [m.room.message] has been
    sent and the server has assigned [event_id]. [content] must be the same
    request body that was sent. The returned [message] is for bookkeeping and
    [to_device] is empty; follow-up messages are addressed by the room
    transaction. Raises [Invalid_argument] when [content]'s sender device or
    target user disagrees with the corresponding argument. *)

val accept :
  session ->
  from_device:Matrix_proto.Id.Device_id.t ->
  our_methods:Verification_base.Method.t list ->
  step
(** [accept s ~from_device ~our_methods] answers an incoming request with an
    [m.key.verification.ready] offering the methods both sides support, and
    moves [s] to {!Ready}. With nothing in common it cancels [s] with
    [m.unknown_method]. A session that is past {!Requested} is left alone and
    nothing is sent. *)

val start_sas :
  random:Random.t ->
  now:Matrix_proto.Event.Timestamp.t ->
  ours:Verification_sas.identity ->
  theirs:Verification_sas.identity ->
  session ->
  step
(** [start_sas ~random ~now ~ours ~theirs s] begins a short authentication
    string on [s] and returns the [m.key.verification.start] to send. *)

val confirm : session -> step
(** [confirm s] records that the user said the strings of [s] match, and returns
    this side's [m.key.verification.mac]. A session not running a short
    authentication string is left alone and nothing is sent. *)

val mismatch : session -> step
(** [mismatch s] is [cancel s Cancel_code.Mismatched_sas], which is what the
    user saying the strings differ amounts to. *)

val cancel :
  ?reason:string -> session -> Verification_base.Cancel_code.t -> step
(** [cancel s code] cancels [s] with [code] and returns the
    [m.key.verification.cancel] to send. [reason] defaults to
    [Cancel_code.reason code]. *)

val show_qr : session -> Verification_qr.t -> unit
(** [show_qr s qr] records that this device is displaying [qr] for [s], so that
    the [m.reciprocate.v1] the peer sends back can be checked against it. *)

val scanned_qr : session -> Verification_qr.t -> unit
(** [scanned_qr s qr] records that this device scanned [qr] for [s]. *)

val handle :
  t ->
  random:Random.t ->
  now:Matrix_proto.Event.Timestamp.t ->
  ?event_timestamp:Matrix_proto.Event.Timestamp.t ->
  ours:Verification_sas.identity ->
  lookup:
    (user_id:Matrix_proto.Id.User_id.t ->
    device_id:Matrix_proto.Id.Device_id.t ->
    Verification_sas.identity option) ->
  sender:Matrix_proto.Id.User_id.t ->
  Verification_base.Message.t ->
  step
(** [handle t ~random ~now ~ours ~lookup ~sender msg] routes [msg] to its
    session, opening one for a request or for an unsolicited start. A request is
    ignored unless [lookup] knows its [from_device] and its timestamp is at most
    ten minutes old or five minutes in the future. To-device requests carry that
    timestamp in their content; callers must pass [event_timestamp] for an
    in-room request. [lookup] also supplies the sending device's keys when an
    [m.key.verification.start] needs a peer identity, and a start whose sender
    it does not know is cancelled rather than verified against a guess. An event
    for an unknown transaction is answered with [m.unknown_transaction], except
    a [done] or a [cancel], which are dropped. *)

val tick :
  t -> now:Matrix_proto.Event.Timestamp.t -> Verification_base.Message.t list
(** [tick t ~now] cancels every session older than the table's timeout and is
    the cancellations to send. *)
