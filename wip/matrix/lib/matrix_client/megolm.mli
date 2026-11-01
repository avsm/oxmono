(** megolm — the forward-only hash ratchet that encrypts room messages.

    One outbound session serves every recipient in a room. Each recipient
    imports it as an inbound session and can then decrypt from the index the
    imported key carries onwards, and no earlier. A session is shared over Olm,
    as an [m.room_key] carrying {!Outbound.session_key}.

    @see <https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/megolm.md>
      megolm.md *)

type decrypted = {
  plaintext : string;
  message_index : int;  (** The ratchet index the message was encrypted at. *)
}
(** The type for a decrypted Megolm message. *)

type encrypted = {
  message_index : int;  (** The ratchet index the message was encrypted at. *)
  ciphertext : string;  (** The encoded message, unpadded base64. *)
}
(** The type for an encrypted Megolm message. *)

module Inbound : sig
  (** A session imported from another device, for decrypting what it sent. *)

  type t
  (** The type for inbound Megolm sessions. It carries ratchet secrets, so it
      has no [pp] or [equal]. *)

  val of_session_key :
    ?claimed_ed25519:Crypto_key.Ed25519.Public.t ->
    sender_key:Crypto_key.Curve25519.Public.t ->
    room_id:Matrix_proto.Id.Room_id.t ->
    session_key:string ->
    unit ->
    (t, Olm_error.t) result
  (** [of_session_key ~sender_key ~room_id ~session_key ()] imports a signed
      version-2 session key, the format an [m.room_key] event carries. The
      Ed25519 signature is checked against the key embedded in the blob.

      [claimed_ed25519] is the sender's {e device} Ed25519 key, as claimed in
      the Olm plaintext that carried the key. It is recorded but not checked
      here, and defaults to absent. See {!sender_claimed_ed25519_key}.

      It fails when the blob is not base64, is the wrong length or version, or
      when the signature does not verify. *)

  val of_exported_session_key :
    ?claimed_ed25519:Crypto_key.Ed25519.Public.t ->
    sender_key:Crypto_key.Curve25519.Public.t ->
    room_id:Matrix_proto.Id.Room_id.t ->
    session_key:string ->
    unit ->
    (t, Olm_error.t) result
  (** [of_exported_session_key ~sender_key ~room_id ~session_key ()] imports an
      unsigned version-1 exported session key, the format used for key backup
      and [m.forwarded_room_key]. There is no signature to check, so the caller
      must establish authenticity by other means.

      [claimed_ed25519] defaults to absent. *)

  val validate_exported_session_key : string -> (unit, Olm_error.t) result
  (** [validate_exported_session_key key] checks that [key] is the exact
      unsigned version-1 Megolm export format accepted by
      {!of_exported_session_key}, without constructing a room session. *)

  val from_room_key :
    ?signing_key:Crypto_key.Ed25519.Public.t ->
    sender_key:Crypto_key.Curve25519.Public.t ->
    room_id:Matrix_proto.Id.Room_id.t ->
    session_id:Matrix_proto.Id.Session_id.t ->
    session_key:string ->
    unit ->
    (t, Olm_error.t) result
  (** [from_room_key ~sender_key ~room_id ~session_id ~session_key ()] imports
      an [m.room_key] event, accepting either the version-2 or the version-1
      format.

      [session_id] is checked against the Ed25519 key embedded in [session_key],
      so a mismatch is an error rather than a session that silently decrypts
      nothing.

      [signing_key] is the sender's {e device} Ed25519 key from the enclosing
      Olm plaintext, which is a different key from the one inside [session_key].
      It is recorded as {!sender_claimed_ed25519_key} and is not checked. It
      defaults to absent. *)

  val session_id : t -> Matrix_proto.Id.Session_id.t
  (** [session_id t] is the unpadded base64 Ed25519 public key of the sender's
      group session. *)

  val sender_key : t -> Crypto_key.Curve25519.Public.t
  (** [sender_key t] is the Curve25519 key of the device that shared [t]. *)

  val room_id : t -> Matrix_proto.Id.Room_id.t
  (** [room_id t] is the room [t] belongs to. *)

  val signing_key : t -> Crypto_key.Ed25519.Public.t
  (** [signing_key t] is the key that signs [t]'s messages. Its base64 is
      {!session_id}. *)

  val sender_claimed_ed25519_key : t -> Crypto_key.Ed25519.Public.t option
  (** [sender_claimed_ed25519_key t] is the sender's device Ed25519 key as
      claimed in the Olm plaintext that carried [t], and [None] when none was
      supplied. Checking that claim against the device list is the caller's job.
  *)

  val signing_key_verified : t -> bool
  (** [signing_key_verified t] is [true] when [t] arrived as a signed version-2
      session key rather than an unsigned export. *)

  val first_known_index : t -> int
  (** [first_known_index t] is the earliest message index [t] can decrypt. *)

  val latest_index : t -> int
  (** [latest_index t] is the current cached receiving-ratchet index. *)

  val creation_time : t -> Ptime.t
  (** [creation_time t] is when [t] was imported. *)

  val decrypt : t -> ciphertext:string -> (decrypted, Olm_error.t) result
  (** [decrypt t ~ciphertext] checks the Ed25519 signature and the MAC and
      decrypts. The message index is read out of the message, so no caller has
      to supply it.

      It fails when the signature or the MAC is wrong, when the padding is
      invalid, or when [t] has already been ratcheted past the message. *)

  val export_at : t -> index:int -> (string, Olm_error.t) result
  (** [export_at t ~index] is the version-1 exported session key at [index],
      unpadded base64. It fails when [t] has been ratcheted past [index]. *)

  val export_at_first_known_index : t -> string
  (** [export_at_first_known_index t] is the version-1 exported session key at
      {!first_known_index}, which is always available. *)

  type pickle = {
    ratchet : string;  (** The 128 bytes of the first known ratchet. *)
    index : int;  (** The message index that ratchet stands at. *)
    signing_key : Crypto_key.Ed25519.Public.t;
    signing_key_verified : bool;
    sender_key : Crypto_key.Curve25519.Public.t;
    claimed_ed25519 : Crypto_key.Ed25519.Public.t option;
    room_id : Matrix_proto.Id.Room_id.t;
    creation_time : Ptime.t;
  }
  (** The type for an inbound session's whole state, which is what a store
      writes. *)

  val to_pickle : t -> pickle
  (** [to_pickle t] is the state of [t] at its first known index. *)

  val of_pickle : pickle -> (t, Olm_error.t) result
  (** [of_pickle p] is the session whose state is [p]. It fails when the ratchet
      is not 128 bytes. *)
end

module Outbound : sig
  (** A session this device created, for encrypting into one room. *)

  type t
  (** The type for outbound Megolm sessions. It carries ratchet secrets, so it
      has no [pp] or [equal]. *)

  val create :
    ?rotation_period:Ptime.Span.t ->
    ?rotation_messages:int ->
    random:Random.t ->
    room_id:Matrix_proto.Id.Room_id.t ->
    unit ->
    t
  (** [create ~random ~room_id ()] is a fresh session for [room_id] whose
      ratchet and Ed25519 signing key are drawn from [random].

      [rotation_period] is how long the session may be used for, and defaults to
      one week. [rotation_messages] is how many messages it may encrypt, and
      defaults to 100. Both are the defaults the specification gives
      [m.room.encryption]. *)

  val session_id : t -> Matrix_proto.Id.Session_id.t
  (** [session_id t] is the unpadded base64 Ed25519 public key of [t]. *)

  val room_id : t -> Matrix_proto.Id.Room_id.t
  (** [room_id t] is the room [t] encrypts for. *)

  val signing_key : t -> Crypto_key.Ed25519.Public.t
  (** [signing_key t] is the key [t]'s messages are signed with. *)

  val message_index : t -> int
  (** [message_index t] is the index the next message will carry. *)

  val message_count : t -> int
  (** [message_count t] is how many messages [t] has encrypted. It differs from
      {!val-message_index} for a session restored from a pickle taken part-way.
  *)

  val creation_time : t -> Ptime.t
  (** [creation_time t] is when [t] was created. *)

  val rotation_period : t -> Ptime.Span.t
  (** [rotation_period t] is how long [t] may be used for. *)

  val rotation_messages : t -> int
  (** [rotation_messages t] is how many messages [t] may encrypt. *)

  val needs_rotation : t -> bool
  (** [needs_rotation t] is [true] once [t] has reached {!val-rotation_messages}
      or {!val-rotation_period}, after which it must be replaced before the next
      send. *)

  val session_key : t -> string
  (** [session_key t] is the signed version-2 session key, for sharing through
      [m.room_key]. It carries the ratchet at its {e current} index, so an
      importer starts from there rather than from zero. *)

  val exported_session_key : t -> string
  (** [exported_session_key t] is the unsigned version-1 export of the ratchet
      at its current index. *)

  val encrypt : t -> string -> encrypted
  (** [encrypt t plaintext] encrypts [plaintext] and advances the ratchet. *)

  val mark_shared_with :
    t ->
    user_id:Matrix_proto.Id.User_id.t ->
    device_id:Matrix_proto.Id.Device_id.t ->
    unit
  (** [mark_shared_with t ~user_id ~device_id] records that an [m.room_key]
      carrying [t] reached a device, so that a later send does not share it
      again. *)

  val is_shared_with :
    t ->
    user_id:Matrix_proto.Id.User_id.t ->
    device_id:Matrix_proto.Id.Device_id.t ->
    bool
  (** [is_shared_with t ~user_id ~device_id] is [true] when {!mark_shared_with}
      has recorded that device. *)

  val shared_with :
    t -> (Matrix_proto.Id.User_id.t * Matrix_proto.Id.Device_id.t) list
  (** [shared_with t] is every device {!mark_shared_with} has recorded. *)

  type pickle = {
    room_id : Matrix_proto.Id.Room_id.t;
    ratchet : string;  (** The 128 bytes of ratchet state. *)
    index : int;  (** The message index that ratchet stands at. *)
    signing_key : Crypto_key.Ed25519.Private.t;
    creation_time : Ptime.t;
    message_count : int;
    rotation_messages : int;
    rotation_period : Ptime.Span.t;
    shared_with : (Matrix_proto.Id.User_id.t * Matrix_proto.Id.Device_id.t) list;
  }
  (** The type for an outbound session's whole state, which is what a store
      writes. It carries the signing key in the clear and is only as safe as
      what it is written to. *)

  val to_pickle : t -> pickle
  (** [to_pickle t] is the current state of [t]. *)

  val of_pickle : pickle -> (t, Olm_error.t) result
  (** [of_pickle p] is the session whose state is [p]. It fails when the ratchet
      is not 128 bytes. *)
end
