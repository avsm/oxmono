(** verification_base — the key verification framework.

    A verification flow is a sequence of events sharing one identifier. This is
    what every method has in common. {!Transaction} says how a flow is
    addressed, {!Message} is the events it is made of, {!Method} names the
    methods a device offers and {!Cancel_code} says why a flow stopped.

    The methods themselves are {!Verification.Sas} and {!Verification.Qr}, and
    {!Verification.Flow} runs several flows at once.

    @see <https://spec.matrix.org/v1.11/client-server-api/#key-verification-framework>
      Key verification framework *)

type error = [ `Msg of string ]
(** The type for a codec failure. The message names what was wrong with the
    event. *)

(** {1 Cancellation codes} *)

module Cancel_code : sig
  (** The [code] of an [m.key.verification.cancel] event.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationcancel>
        m.key.verification.cancel *)

  type t =
    | User  (** [m.user], the user cancelled. *)
    | Timeout  (** [m.timeout]. *)
    | Unknown_transaction  (** [m.unknown_transaction]. *)
    | Unknown_method  (** [m.unknown_method], no common algorithm. *)
    | Unexpected_message  (** [m.unexpected_message], out of order. *)
    | Key_mismatch  (** [m.key_mismatch], a MAC did not verify. *)
    | User_mismatch  (** [m.user_mismatch]. *)
    | Invalid_message  (** [m.invalid_message]. *)
    | Accepted  (** [m.accepted], another device took the request. *)
    | Mismatched_commitment  (** [m.mismatched_commitment]. *)
    | Mismatched_sas  (** [m.mismatched_sas], the users said "no match". *)
    | Other of string  (** Any code this library does not model. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t], such as ["m.user"]. *)

  val of_string : string -> t
  (** [of_string s] is the code [s] names. An unrecognised [s] is [Other s]. *)

  val reason : t -> string
  (** [reason t] is a description of [t] in English, for the [reason] member. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same code. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)
end

(** {1 Verification methods} *)

module Method : sig
  (** The method names advertised in [m.key.verification.request] and
      [m.key.verification.ready]. *)

  type t =
    | Sas_v1  (** [m.sas.v1]. *)
    | Qr_code_show_v1
        (** [m.qr_code.show.v1], the device can display a code. *)
    | Qr_code_scan_v1  (** [m.qr_code.scan.v1], the device can scan one. *)
    | Reciprocate_v1  (** [m.reciprocate.v1]. *)
    | Other of string  (** Any method this library does not model. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t], such as ["m.sas.v1"]. *)

  val of_string : string -> t
  (** [of_string s] is the method [s] names. An unrecognised [s] is [Other s].
  *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same method. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val all : t list
  (** [all] is every method this library implements, most preferred first. It is
      what a request offers when the caller names no methods. *)

  val common : ours:t list -> theirs:t list -> t list
  (** [common ~ours ~theirs] is the methods in [ours] that also appear in
      [theirs], in the order of [ours]. *)
end

(** {1 Transactions} *)

module Transaction : sig
  (** How a verification flow is addressed.

      A to-device flow carries a transaction identifier chosen by the requesting
      device. An in-room flow is identified by the event identifier of the
      [m.room.message] that opened it, and each of its events carries an
      [m.reference] relation to that message. *)

  type t
  (** The type for flow addresses. *)

  val to_device : Matrix_proto.Id.Transaction_id.t -> t
  (** [to_device id] addresses the to-device flow [id]. *)

  val in_room :
    room_id:Matrix_proto.Id.Room_id.t ->
    event_id:Matrix_proto.Id.Event_id.t ->
    t
  (** [in_room ~room_id ~event_id] addresses the in-room flow opened by
      [event_id] in [room_id]. *)

  val id : t -> string
  (** [id t] is an opaque string that disambiguates [t] from every flow with a
      different transport, transaction identifier, room identifier or event
      identifier. Two flows are the same exactly when their identifiers are. *)

  val room_id : t -> Matrix_proto.Id.Room_id.t option
  (** [room_id t] is the room an in-room flow runs in, and [None] for a
      to-device flow. *)

  val transaction_id : t -> Matrix_proto.Id.Transaction_id.t option
  (** [transaction_id t] is the [transaction_id] member the events of [t] carry,
      and [None] for an in-room flow. *)

  val relates_to : t -> Matrix_proto.Event.Relates_to.t option
  (** [relates_to t] is the [m.relates_to] member the events of [t] carry, and
      [None] for a to-device flow. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] address the same flow. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the transport and the identifier. *)
end

(** {1 Messages} *)

module Message : sig
  (** A verification event, addressed to a transaction.

      This is what the state machines consume and produce. It carries no sender
      and no recipient, because the transport decides those. *)

  (** The content of one verification event. *)
  type payload =
    | Request of Matrix_proto.Event.Key_verification_request_content.t
    | Request_in_room of
        Matrix_proto.Event.Key_verification_request_message_content.t
    | Ready of Matrix_proto.Event.Key_verification_ready_content.t
    | Start of Matrix_proto.Event.Key_verification_start_content.t
    | Accept of Matrix_proto.Event.Key_verification_accept_content.t
    | Key of Matrix_proto.Event.Key_verification_key_content.t
    | Mac of Matrix_proto.Event.Key_verification_mac_content.t
    | Done of Matrix_proto.Event.Key_verification_done_content.t
    | Cancel of Matrix_proto.Event.Key_verification_cancel_content.t

  type t
  (** The type for verification events. The addressing in the payload agrees
      with the transaction. *)

  val v : Transaction.t -> payload -> t
  (** [v transaction payload] is [payload] sent under [transaction]. *)

  val transaction : t -> Transaction.t
  (** [transaction t] is the flow [t] belongs to. *)

  val payload : t -> payload
  (** [payload t] is the content of [t]. *)

  val event_type : t -> string
  (** [event_type t] is the Matrix event type to send [t] as. It is
      ["m.room.message"] for an in-room request and ["m.key.verification.*"]
      otherwise. *)

  val to_json : t -> (Jsont.json, error) result
  (** [to_json t] is the content of [t] as JSON. *)

  val to_string : t -> (string, error) result
  (** [to_string t] is the canonical JSON encoding of the content of [t]. *)

  val of_json :
    event_type:string ->
    ?room_id:Matrix_proto.Id.Room_id.t ->
    ?event_id:Matrix_proto.Id.Event_id.t ->
    Jsont.json ->
    (t, error) result
  (** [of_json ~event_type json] is the incoming content [json] of type
      [event_type].

      [room_id] and [event_id] default to absent and are what an in-room flow is
      addressed by. An [m.room.message] request has no addressing of its own, so
      both are needed for it and it fails without them. A room follow-up must
      carry an [m.reference] relation; it never falls back to a to-device
      [transaction_id]. Conversely, an absent [room_id] requires a
      [transaction_id], and [m.key.verification.request] is accepted only as a
      to-device event. *)

  val of_string :
    event_type:string ->
    ?room_id:Matrix_proto.Id.Room_id.t ->
    ?event_id:Matrix_proto.Id.Event_id.t ->
    string ->
    (t, error) result
  (** [of_string ~event_type s] is {!of_json} on the JSON [s] encodes. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the event type and the transaction. *)

  val cancel : ?reason:string -> Transaction.t -> Cancel_code.t -> t
  (** [cancel transaction code] is the [m.key.verification.cancel] that ends
      [transaction] with [code]. [reason] defaults to [Cancel_code.reason code].
  *)

  val done_ : Transaction.t -> t
  (** [done_ transaction] is the [m.key.verification.done] of [transaction]. *)

  val ready :
    Transaction.t ->
    from_device:Matrix_proto.Id.Device_id.t ->
    methods:Method.t list ->
    t
  (** [ready transaction ~from_device ~methods] is the
      [m.key.verification.ready] that answers a request, offering [methods]. *)
end

(** {1 Opening a flow} *)

type request = {
  transaction : Transaction.t;  (** The flow that was opened. *)
  message : Message.t;  (** The request, for the caller's own bookkeeping. *)
  to_device : To_device.messages;  (** What to hand to {!To_device.send}. *)
}
(** The type for a verification request that has been built but not yet sent. *)

val request_to_device :
  random:Random.t ->
  now:Matrix_proto.Event.Timestamp.t ->
  from_device:Matrix_proto.Id.Device_id.t ->
  ?methods:Method.t list ->
  their_user_id:Matrix_proto.Id.User_id.t ->
  devices:To_device.recipient list ->
  unit ->
  request
(** [request_to_device ~random ~now ~from_device ~their_user_id ~devices ()] is
    an [m.key.verification.request] fanned out to every recipient in [devices]
    under one transaction identifier, which is drawn from [random]. Pass
    [[To_device.All]] to reach every device the user has. [methods] defaults to
    {!Method.all}. *)

val request_in_room :
  from_device:Matrix_proto.Id.Device_id.t ->
  ?methods:Method.t list ->
  their_user_id:Matrix_proto.Id.User_id.t ->
  ?body:string ->
  unit ->
  Matrix_proto.Event.Key_verification_request_message_content.t
(** [request_in_room ~from_device ~their_user_id ()] is the [m.room.message]
    that opens an in-room flow. The flow identifier is the event identifier the
    server assigns to it. [methods] defaults to {!Method.all}. [body] defaults
    to a sentence telling a client without verification support what the message
    is. *)

val ready_response :
  transaction:Transaction.t ->
  from_device:Matrix_proto.Id.Device_id.t ->
  our_methods:Method.t list ->
  their_methods:Method.t list ->
  (Method.t list * Message.t, Cancel_code.t) result
(** [ready_response ~transaction ~from_device ~our_methods ~their_methods] is
    the methods both sides support and the [m.key.verification.ready] offering
    them. It is [Error Cancel_code.Unknown_method] when nothing is in common. *)
