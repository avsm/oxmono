(** verification_sas — the Short Authentication String protocol.

    A flow runs [start], [accept], [key], [key], [mac], [mac] and [done]. The
    device that sent the [m.key.verification.start] is the starter. The other
    side commits to its ephemeral key in the [m.key.verification.accept] before
    either key is revealed, so an attacker gets a single guess at the 42 bits
    the two users compare.

    Every transition takes a state and returns the next state with the events to
    send, as {!type-step}. Nothing here raises and nothing returns an error. A
    protocol failure moves the state to {!Cancelled} and returns the
    [m.key.verification.cancel] to send, which is also what a cancellation asked
    for by the user looks like.

    @see <https://spec.matrix.org/v1.11/client-server-api/#short-authentication-string-sas-verification>
      SAS verification *)

(** {1 MAC algorithms} *)

module Mac_method : sig
  (** The algorithm both sides compute their [m.key.verification.mac] with. *)

  type t =
    | Hkdf_hmac_sha256_v2  (** [hkdf-hmac-sha256.v2]. *)
    | Hkdf_hmac_sha256
        (** [hkdf-hmac-sha256], which reproduces a base64 buffer overrun in
            libolm so that its MACs agree with the clients that have it. It is
            offered only for peers that cannot do the other one. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> t option
  (** [of_string s] is the method [s] names, and [None] for anything else. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same method. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val all : t list
  (** [all] is every method this library implements, most preferred first. *)
end

(** {1 The emoji table} *)

type emoji = { number : int; symbol : string; description : string }
(** The type for one of the 64 emoji, with its index and the English description
    the specification gives for it. *)

val emoji_table : emoji array
(** [emoji_table] is the specification's table, 64 entries long, indexed by the
    emoji number. It runs from a dog at 0 to a pin at 63.

    @see <https://spec.matrix.org/v1.11/client-server-api/#sas-method-emoji>
      SAS method: emoji *)

val emoji_indices : string -> int list
(** [emoji_indices bytes] is the first 42 bits of [bytes] as seven 6-bit
    numbers. It is the empty list when [bytes] is shorter than 6 bytes. *)

val emoji_of_bytes : string -> emoji list
(** [emoji_of_bytes bytes] is the entries of {!emoji_table} that
    [emoji_indices bytes] names. *)

val decimals_of_bytes : string -> int * int * int
(** [decimals_of_bytes bytes] is the three numbers between 1000 and 9191 taken
    as 13-bit groups from the first 5 bytes of [bytes]. It is [(0, 0, 0)] when
    [bytes] is shorter than 5 bytes.

    @see <https://spec.matrix.org/v1.11/client-server-api/#sas-method-decimal>
      SAS method: decimal *)

val commitment :
  public_key:Crypto_key.Curve25519.Public.t -> start_json:Jsont.json -> string
(** [commitment ~public_key ~start_json] is the unpadded base64 SHA-256 of the
    accepting device's ephemeral [public_key], as unpadded base64, followed by
    the canonical JSON of the [m.key.verification.start] content [start_json].
*)

(** {1 Identities} *)

type identity = {
  user_id : Matrix_proto.Id.User_id.t;
  device_id : Matrix_proto.Id.Device_id.t;
  device_key : Crypto_key.Ed25519.Public.t;
  master_key : Crypto_key.Ed25519.Public.t option;
      (** The user's cross-signing master key, when this device holds it. *)
}
(** The type for the keys of one side of a flow. These are the keys the MACs
    assert. *)

val identity :
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  device_key:Crypto_key.Ed25519.Public.t ->
  ?master_key:Crypto_key.Ed25519.Public.t ->
  unit ->
  identity
(** [identity ~user_id ~device_id ~device_key ()] is the identity of one side.
    [master_key] defaults to absent, which leaves the master key out of the
    MACs. *)

val identity_keys : identity -> (Crypto_key.Key_id.t * string) list
(** [identity_keys t] is the key identifiers a MAC covers and the unpadded
    base64 keys they name. It is the [ed25519:] key of the device and, when
    there is one, the [ed25519:] key of the master key. *)

(** {1 State} *)

(** How far a flow has got. *)
type stage =
  | Start_sent  (** This side started and awaits the peer's accept. *)
  | Accept_sent  (** The peer started and this side awaits its key. *)
  | Key_sent  (** This side revealed its key and awaits the peer's. *)
  | Sas_ready  (** Both keys are known, so the strings can be shown. *)
  | Mac_sent  (** The user confirmed and this side sent its MAC. *)
  | Waiting_done  (** Both MACs verified, awaiting the peer's done. *)
  | Done  (** Both sides finished. *)
  | Cancelled of Verification_base.Cancel_code.t

type t
(** The type for the state of one SAS flow. It is immutable. *)

type step = { sas : t; send : Verification_base.Message.t list }
(** The type for the result of a transition, as the state it reached and the
    events to send in order. *)

val stage : t -> stage
(** [stage t] is how far [t] has got. *)

val transaction : t -> Verification_base.Transaction.t
(** [transaction t] is the flow [t] belongs to. *)

val we_started : t -> bool
(** [we_started t] is [true] when this side sent the [m.key.verification.start].
*)

val our_identity : t -> identity
(** [our_identity t] is the keys this side asserts. *)

val their_identity : t -> identity
(** [their_identity t] is the keys the peer asserts. *)

val mac_method : t -> Mac_method.t
(** [mac_method t] is the algorithm the accept settled on. *)

val is_done : t -> bool
(** [is_done t] is [true] when [stage t] is {!Done}. *)

val is_cancelled : t -> bool
(** [is_cancelled t] is [true] when [stage t] is {!Cancelled}. *)

val cancel_code : t -> Verification_base.Cancel_code.t option
(** [cancel_code t] is why [t] was cancelled, and [None] when it was not. *)

val verified_keys : t -> (Crypto_key.Key_id.t * string) list
(** [verified_keys t] is the peer's key identifiers and keys whose MAC checked
    out. It is empty until the peer's [m.key.verification.mac] has arrived. *)

val emoji : t -> emoji list option
(** [emoji t] is the seven emoji to show the user when [emoji] was negotiated,
    and [None] otherwise or until both ephemeral keys are known. *)

val decimals : t -> (int * int * int) option
(** [decimals t] is the same secret as three numbers, for a device that cannot
    render emoji, when [decimal] was negotiated. It is [None] otherwise or until
    both ephemeral keys are known. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the transaction and the stage. It prints no key material.
*)

(** {1 Transitions} *)

val start :
  random:Random.t ->
  now:Matrix_proto.Event.Timestamp.t ->
  ?timeout:int64 ->
  transaction:Verification_base.Transaction.t ->
  ours:identity ->
  theirs:identity ->
  ?mac_methods:Mac_method.t list ->
  unit ->
  step
(** [start ~random ~now ~transaction ~ours ~theirs ()] begins a flow this side
    initiates, and returns the [m.key.verification.start] to send. The ephemeral
    Curve25519 key is drawn from [random] here, so that no later transition
    needs randomness.

    [timeout] is how long the flow may run, in milliseconds, and defaults to
    600000. [mac_methods] is what to offer, most preferred first, and defaults
    to {!Mac_method.all}. Pass [[Mac_method.Hkdf_hmac_sha256_v2]] to refuse the
    deprecated algorithm outright. *)

val from_start :
  random:Random.t ->
  now:Matrix_proto.Event.Timestamp.t ->
  ?timeout:int64 ->
  transaction:Verification_base.Transaction.t ->
  ours:identity ->
  theirs:identity ->
  Matrix_proto.Event.Key_verification_start_content.t ->
  step
(** [from_start ~random ~now ~transaction ~ours ~theirs content] answers a flow
    the peer started with [content]. It picks a key agreement protocol, a hash,
    a MAC algorithm and the comparison formats from what [content] offers, and
    returns the [m.key.verification.accept] carrying the commitment to this
    side's ephemeral key. With nothing in common it cancels with
    [m.unknown_method].

    [timeout] is how long the flow may run, in milliseconds, and defaults to
    600000. *)

val handle :
  t -> now:Matrix_proto.Event.Timestamp.t -> Verification_base.Message.t -> step
(** [handle t ~now msg] applies the incoming [msg] to [t]. An event that arrives
    in the wrong stage cancels with [m.unexpected_message], a commitment that
    does not match cancels with [m.mismatched_commitment], and a MAC that does
    not verify cancels with [m.key_mismatch]. An incoming
    [m.key.verification.cancel] records the peer's code and is never answered.
    Anything arriving after the flow has finished or been cancelled is dropped.
    {!tick} runs first, so a flow that has timed out is cancelled rather than
    advanced. *)

val confirm : t -> step
(** [confirm t] records that the user said the strings match. It returns this
    side's [m.key.verification.mac], and the [m.key.verification.done] as well
    when the peer's MAC has already been verified. Called in any stage but
    {!Sas_ready} it cancels with [m.unexpected_message]. *)

val cancel : ?reason:string -> t -> Verification_base.Cancel_code.t -> step
(** [cancel t code] cancels [t] with [code] and returns the
    [m.key.verification.cancel] to send. A flow already finished or cancelled is
    left alone and nothing is sent. [reason] defaults to
    [Cancel_code.reason code]. *)

val mismatch : t -> step
(** [mismatch t] is [cancel t Cancel_code.Mismatched_sas], which is what the
    user saying the strings differ amounts to. *)

val tick : t -> now:Matrix_proto.Event.Timestamp.t -> step
(** [tick t ~now] cancels [t] with [m.timeout] once its timeout has passed since
    it started, and otherwise does nothing. *)
