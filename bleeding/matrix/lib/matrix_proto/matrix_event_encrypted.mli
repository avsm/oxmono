@@ portable

(** The content of the encrypted event family.

    These are the wire shapes of [m.room.encrypted] under each of the two
    algorithms, of the plaintext an Olm message carries, and of the two events
    that share a Megolm session. {!Matrix_event.Encrypted_content} reads the
    same [m.room.encrypted] object with its [ciphertext] left as JSON, which is
    what a caller that only wants to know the algorithm needs.

    Keys and ciphertext travel as unpadded base64.

    @see <https://spec.matrix.org/v1.11/client-server-api/#mroomencrypted>
      m.room.encrypted *)

(** {1 Olm messages} *)

module Olm_message_type : sig
  (** Which of the two Olm message formats a ciphertext holds. *)

  type t =
    | Pre_key
        (** A message that also carries the keys establishing the session. *)
    | Normal  (** A message on a session both sides already hold. *)

  val to_int : t -> int
  (** [to_int t] is the wire form of [t], [0] for {!Pre_key} and [1] for
      {!Normal}. *)

  val of_int : int -> (t, [> `Msg of string ]) result
  (** [of_int n] is the message type [n] names. It is an error for any [n] other
      than [0] and [1]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same message type. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}, which reads and writes the integer
      {!to_int} gives. *)
end

module Olm_ciphertext : sig
  (** The [ciphertext] member of an Olm [m.room.encrypted], one message per
      recipient device. *)

  type entry = {
    recipient_key : string;
        (** The Curve25519 identity key of the device the message is for. *)
    message_type : Olm_message_type.t;
    body : string;  (** The encoded Olm message. *)
  }
  (** The type for one recipient's message. *)

  type t = entry list
  (** The type for a whole [ciphertext] object. Entries come out sorted by
      recipient key however the sender ordered them. *)

  val find : t -> recipient_key:string -> entry option
  (** [find t ~recipient_key] is the entry addressed to [recipient_key], or
      [None] when the event names no such device. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Olm_plaintext : sig
  (** The event an Olm message decrypts to.

      It names both parties and both Ed25519 keys, which is what lets a
      recipient reject a message replayed from another conversation. A recipient
      must check that [recipient] and [recipient_ed25519] are its own and that
      [sender] is who the enclosing event claimed.

      @see <https://spec.matrix.org/v1.11/client-server-api/#molmv1curve25519-aes-sha2>
        m.olm.v1.curve25519-aes-sha2 *)

  type t = {
    event_type : string;  (** The [type] of the event carried inside. *)
    content : Jsont.json;  (** Its content. *)
    sender : Matrix_id.User_id.t;
    sender_ed25519 : string;
        (** The sending device's Ed25519 key, unpadded base64. *)
    recipient : Matrix_id.User_id.t;
    recipient_ed25519 : string;
        (** The receiving device's Ed25519 key, unpadded base64. *)
    sender_device_keys : Jsont.json option;
        (** Optional MSC4147 signed sender device keys. The unstable
            [org.matrix.msc4147.device_keys] spelling is accepted on decode. *)
  }
  (** The type for an Olm plaintext. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

(** {1 [m.room.encrypted]} *)

module Encrypted : sig
  (** The content of [m.room.encrypted] under each algorithm. Decoding fails
      when the event's [algorithm] is not the one the submodule reads, so a
      caller that must tell an unsupported algorithm from a malformed event
      reads {!Matrix_event.Encrypted_content} first. *)

  module Olm : sig
    (** [m.room.encrypted] with [m.olm.v1.curve25519-aes-sha2], the to-device
        form addressed at individual devices. *)

    type t = {
      sender_key : string;
          (** The sending device's Curve25519 identity key, unpadded base64. *)
      ciphertext : Olm_ciphertext.t;
    }
    (** The type for an Olm-encrypted content. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON codec for {!t}. *)
  end

  module Megolm : sig
    (** [m.room.encrypted] with [m.megolm.v1.aes-sha2], the form room messages
        take. *)

    type t = {
      sender_key : string option;
          (** The sending device's Curve25519 identity key, unpadded base64.
              Deprecated in Matrix 1.3, so recent senders omit it. *)
      session_id : Matrix_id.Session_id.t;
          (** The Megolm session that encrypted the message. *)
      device_id : Matrix_id.Device_id.t option;
          (** The sending device. Deprecated in Matrix 1.3. *)
      ciphertext : string;  (** The encoded Megolm message. *)
    }
    (** The type for a Megolm-encrypted content. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON codec for {!t}. *)
  end
end

(** {1 Sharing a Megolm session} *)

module Room_key_content : sig
  (** The content of [m.room_key], which hands a Megolm session to another
      device over Olm.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroom_key>
        m.room_key *)

  type t = {
    room_id : Matrix_id.Room_id.t;  (** The room the session encrypts for. *)
    session_id : Matrix_id.Session_id.t;
    session_key : string;
        (** The signed session key, unpadded base64. It carries the ratchet at
            the index the sender had reached, so the recipient can decrypt from
            there onwards and no earlier. *)
    shared_history : bool;
        (** Stable MSC3061 [m.shared_history]. The legacy
            [org.matrix.msc3061.shared_history] spelling is accepted. *)
  }
  (** The type for an [m.room_key] content. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Forwarded_room_key_content : sig
  (** The content of [m.forwarded_room_key], which passes on a session this
      device received rather than created.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mforwarded_room_key>
        m.forwarded_room_key *)

  type t = {
    room_id : Matrix_id.Room_id.t;
    sender_key : string;
        (** The Curve25519 key of the device that created the session, unpadded
            base64. *)
    session_id : Matrix_id.Session_id.t;
    session_key : string;
        (** The unsigned export of the session, unpadded base64. Unlike
            {!Room_key_content.session_key} it carries no signature, so the
            recipient must judge its authenticity from who forwarded it. *)
    sender_claimed_ed25519_key : string;
        (** The Ed25519 key the creating device claimed, unpadded base64. *)
    forwarding_curve25519_key_chain : string list;
        (** Every device the session passed through before this one, oldest
            first. *)
  }
  (** The type for an [m.forwarded_room_key] content. Shared-history permission
      is intentionally not carried by forwarded keys. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end
