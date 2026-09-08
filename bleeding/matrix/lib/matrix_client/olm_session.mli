(** olm_session — one double ratchet with one other device.

    An Olm session carries to-device messages between two devices. It is opened
    from a device's published identity and one-time keys, and from then on each
    side ratchets forward as it sends and as it receives. Messages that arrive
    out of order or on an older chain still decrypt, within the limits below.

    A session keeps at most 5 receiving chains, at most 40 skipped message keys
    per chain, and refuses a message more than 2000 ahead of the chain it
    arrives on. Those are vodozemac's limits and are tighter than libolm's.

    @see <https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/olm.md>
      olm.md *)

type t
(** The type for Olm sessions. It carries ratchet secrets, so it has no [pp] or
    [equal]. *)

type message_type = Matrix_proto.Event.Olm_message_type.t =
  | Pre_key
  | Normal  (** Which of the two Olm message formats a ciphertext holds. *)

type message = {
  message_type : message_type;
  ciphertext : string;  (** The encoded message, unpadded base64. *)
}
(** The type for an encrypted Olm message, in the shape the [m.room.encrypted]
    content wants it. *)

(** {1 Opening a session} *)

val create_outbound :
  random:Random.t ->
  Olm_account.t ->
  their_identity_key:Crypto_key.Curve25519.Public.t ->
  their_one_time_key:Crypto_key.Curve25519.Public.t ->
  (t, Olm_error.t) result
(** [create_outbound ~random account ~their_identity_key ~their_one_time_key]
    runs the handshake against a device's published keys. The ephemeral base key
    and the initial ratchet key are drawn from [random].

    It fails when a Diffie-Hellman step is non-contributory. *)

val create_inbound :
  Olm_account.t ->
  their_identity_key:Crypto_key.Curve25519.Public.t ->
  ciphertext:string ->
  (t * string, Olm_error.t) result
(** [create_inbound account ~their_identity_key ~ciphertext] is the receiving
    side of a session built from the base64 pre-key message [ciphertext],
    together with that message's plaintext.

    The one-time key the message names is consumed only once decryption has
    succeeded, so a forged pre-key message cannot make [account] forget a key.

    It fails when the message is malformed, when it names a sender other than
    [their_identity_key], when [account] holds no matching one-time key, or when
    decryption fails. *)

(** {1 Using a session} *)

val session_id : t -> string
(** [session_id t] is the unpadded base64 SHA-256 of the identity key, base key
    and one-time key that opened [t]. Both sides derive the same value. It is
    local bookkeeping and appears in no event, so it is not a Megolm session
    identifier. *)

val their_identity_key : t -> Crypto_key.Curve25519.Public.t
(** [their_identity_key t] is the other device's Curve25519 identity key. *)

val creation_time : t -> Ptime.t
(** [creation_time t] is when [t] was opened. *)

val last_used_at : t -> Ptime.t
(** [last_used_at t] is the most recent successful encrypt or decrypt. *)

val last_received_at : t -> Ptime.t
(** [last_received_at t] is the most recent successful decrypt. This is the
    selection timestamp; successful encryption does not promote it. *)

val has_received_message : t -> bool
(** [has_received_message t] is [true] once a message from the other side has
    been decrypted. Until then {!encrypt} keeps producing {!Pre_key} messages,
    since the other side may not hold the session yet. *)

val encrypt : random:Random.t -> t -> string -> (message, Olm_error.t) result
(** [encrypt ~random t plaintext] encrypts [plaintext] and advances the sending
    chain. [random] is consumed only when the ratchet has to turn, which is when
    the other side has sent since this one last did.

    It fails when the ratchet cannot advance because a Diffie-Hellman step is
    non-contributory. *)

val decrypt : random:Random.t -> t -> message -> (string, Olm_error.t) result
(** [decrypt ~random t message] is the plaintext of [message].

    [random] is needed because receiving on an unseen chain may first have to
    activate this side's sending ratchet, which draws a new ratchet key. The
    session is left untouched when decryption fails. *)

(** {1 Persistence} *)

type chain_key = {
  key : string;  (** The 32 raw bytes of the chain key. *)
  index : int;  (** How far the chain has been advanced. *)
}
(** The type for a symmetric-ratchet chain key. *)

type active_chain = {
  root_key : string;
  ratchet_secret : Crypto_key.Curve25519.Secret.t;
  ratchet_public : Crypto_key.Curve25519.Public.t;
  chain : chain_key;
}
(** The type for a sending ratchet this side owns and can encrypt on at once. *)

(** The type for the sending half of the ratchet. *)
type sending =
  | Active of active_chain
  | Inactive of {
      root_key : string;
      their_ratchet_key : Crypto_key.Curve25519.Public.t;
    }
      (** The other side moved the ratchet on, so a new chain must be derived
          before encryption can resume. *)

type receiver_chain = {
  ratchet_key : Crypto_key.Curve25519.Public.t;
      (** The sender's ratchet key this chain hangs off. *)
  chain : chain_key;
  skipped : (int * string) list;
      (** Message keys kept for indices out-of-order delivery jumped over. *)
}
(** The type for one receiving chain. *)

type pickle = {
  identity_key : Crypto_key.Curve25519.Public.t;
      (** The session initiator's identity key. *)
  base_key : Crypto_key.Curve25519.Public.t;
      (** The initiator's ephemeral key. *)
  one_time_key : Crypto_key.Curve25519.Public.t;
      (** The recipient's one-time key. *)
  their_identity_key : Crypto_key.Curve25519.Public.t;
  sending : sending;
  receiving_chains : receiver_chain list;
  creation_time : Ptime.t;
}
(** The type for a session's whole state, which is what a store writes. It
    carries secret key material in the clear and is only as safe as what it is
    written to. *)

val to_pickle : t -> pickle
(** [to_pickle t] is the state of [t]. *)

val of_pickle :
  ?last_used_at:Ptime.t -> ?last_received_at:Ptime.t -> pickle -> t
(** [of_pickle ?last_used_at ?last_received_at p] is the session whose state is
    [p]. Both recency timestamps default to [p.creation_time], which migrates
    stored sessions written before recency was persisted. *)
