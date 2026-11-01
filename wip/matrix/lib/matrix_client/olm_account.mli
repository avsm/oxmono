(** olm_account — a device's long-lived Olm keys.

    An account holds the Ed25519 signing key and the Curve25519 identity key
    other devices know a device by, together with the one-time keys they claim
    to open a session with it. Its identity keys outlive any one process, so a
    device that regenerates them publishes itself as a new device.

    The secret halves never leave except through {!to_pickle}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-keys>
      Device keys *)

type t
(** The type for Olm accounts. It carries the secret halves of both keys, so it
    has no [pp] or [equal]. *)

val create : random:Random.t -> unit -> t
(** [create ~random ()] is a new account whose identity keys are drawn from
    [random]. It holds no one-time key and no fallback key. *)

val one_time_key_algorithm : string
(** [one_time_key_algorithm] is ["signed_curve25519"], the algorithm under which
    an account's one-time and fallback keys are published. *)

(** {1 Identity} *)

val ed25519_key : t -> Crypto_key.Ed25519.Public.t
(** [ed25519_key t] is the key [t]'s signatures verify under. *)

val curve25519_key : t -> Crypto_key.Curve25519.Public.t
(** [curve25519_key t] is the key other devices run the Olm handshake against.
*)

val identity_keys :
  t -> Crypto_key.Ed25519.Public.t * Crypto_key.Curve25519.Public.t
(** [identity_keys t] is [(ed25519_key t, curve25519_key t)]. *)

val sign : t -> string -> Crypto_key.Signature.t
(** [sign t data] is [t]'s Ed25519 signature over the bytes of [data]. *)

(** {1 One-time keys} *)

val generate_one_time_keys : random:Random.t -> t -> int -> unit
(** [generate_one_time_keys ~random t n] adds up to [n] fresh one-time keys
    drawn from [random]. The public pool target is {!val-max_one_time_keys}; the
    account retains up to 100 times that number of private keys, evicting the
    oldest retained key only when that private limit is reached. *)

val one_time_keys :
  t -> (Crypto_key.Key_id.t * Crypto_key.Curve25519.Public.t) list
(** [one_time_keys t] is every retained one-time key [t] holds, most recently
    generated first. Each identifier names the key under
    {!one_time_key_algorithm}. The encryption layer separately tracks which
    retained keys have already been published. *)

val one_time_key_ids : t -> Crypto_key.Key_id.t list
(** [one_time_key_ids t] is the retained key-id set without deriving public
    Curve25519 keys. *)

val signed_one_time_keys :
  ?exclude:(Crypto_key.Key_id.t -> bool) ->
  t ->
  (Crypto_key.Key_id.t
  * Crypto_key.Curve25519.Public.t
  * Crypto_key.Signature.t)
  list
(** [signed_one_time_keys ?exclude t] is {!one_time_keys}, excluding keys for
    which [exclude] returns [true], with [t]'s signature over each remaining
    key's canonical JSON, which is the form [/keys/upload] wants. *)

val one_time_keys_count : t -> int
(** [one_time_keys_count t] is how many private one-time keys [t] retains. *)

val max_one_time_keys : t -> int
(** [max_one_time_keys t] is the public one-time-key pool target. The account
    may retain a larger private reserve; see {!generate_one_time_keys}. *)

val generate_fallback_key : random:Random.t -> t -> unit
(** [generate_fallback_key ~random t] replaces [t]'s current fallback key with a
    fresh one drawn from [random], retaining the replaced key as the previous
    fallback key for inbound handshakes. At most one previous key is retained.
*)

val fallback_key :
  t -> (Crypto_key.Key_id.t * Crypto_key.Curve25519.Public.t) option
(** [fallback_key t] is [t]'s current fallback key. A pre-key message naming it
    opens a session when no one-time key is left, and it is not consumed by
    doing so.

    @see <https://spec.matrix.org/v1.11/client-server-api/#one-time-and-fallback-keys>
      One-time and fallback keys *)

val forget_previous_fallback_key : t -> bool
(** [forget_previous_fallback_key t] drops the retained previous fallback key,
    returning [true] when one was present. The current key is unaffected. *)

(** {1 Key agreement}

    These are what {!Olm_session} runs the X3DH handshake with. *)

val identity_exchange :
  t -> Crypto_key.Curve25519.Public.t -> (string, Olm_error.t) result
(** [identity_exchange t public] is the shared secret between [t]'s identity key
    and [public]. *)

val one_time_key_exchange :
  t ->
  key:Crypto_key.Curve25519.Public.t ->
  peer:Crypto_key.Curve25519.Public.t ->
  (string, Olm_error.t) result
(** [one_time_key_exchange t ~key ~peer] is the shared secret between the
    one-time key whose public half is [key] and [peer]. The fallback key is
    searched as well as the pool. It is [Error Unknown_one_time_key] when [t]
    holds no such key. *)

val consume_one_time_key : t -> Crypto_key.Curve25519.Public.t -> unit
(** [consume_one_time_key t key] drops the one-time key whose public half is
    [key], so that it is never used twice. The fallback key is left in place,
    since it is meant to be reused. *)

(** {1 Persistence} *)

type stored_key = {
  key_id : string;
      (** The account-local identifier, which {!one_time_keys} names under
          {!one_time_key_algorithm}. *)
  secret : Crypto_key.Curve25519.Secret.t;
}
(** The type for one stored one-time key. *)

type pickle = {
  ed25519 : Crypto_key.Ed25519.Private.t;
  curve25519 : Crypto_key.Curve25519.Secret.t;
  stored_one_time_keys : stored_key list;
  stored_fallback_key : stored_key option;
  stored_previous_fallback_key : stored_key option;
  next_key_id : int;  (** The counter the next identifier is drawn from. *)
  max_one_time_keys : int;
}
(** The type for an account's whole state, which is what a store writes. It
    carries the secret keys in the clear and is only as safe as what it is
    written to. *)

val to_pickle : t -> pickle
(** [to_pickle t] is the state of [t]. *)

val of_pickle : pickle -> t
(** [of_pickle p] is the account whose state is [p]. *)
