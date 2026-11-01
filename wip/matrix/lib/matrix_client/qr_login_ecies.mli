(** The ECIES secure-channel primitive used by the original MSC4108 flow.

    This module is only the cryptographic channel layer. It does not implement
    QR-login roles, rendezvous orchestration, login approval, or secret
    handover; callers must build and authenticate those protocol steps around
    it. The wire formats and key derivation are compatible with vodozemac's
    MSC4108 ECIES implementation. *)

type error =
  | Malformed_initial_message
  | Malformed_message
  | Invalid_public_key
  | Non_contributory_key
  | Authentication_failed
  | Counter_exhausted
  | Pending_consumed

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] prints a concise description of [error]. *)

type pending
(** An unestablished channel containing ephemeral secret key material. A pending
    channel is consumed by the first valid establishment attempt, including
    attempts that fail during key agreement or authentication. Consumption
    removes the retained key reference before key agreement begins. The
    underlying GC-managed value cannot carry a reliable memory-wipe guarantee;
    see {!Crypto_key}. *)

type t
(** An established ECIES channel. The pending secret key is not retained after
    key agreement completes, although derived keys remain for the channel's
    lifetime and are subject to {!Crypto_key}'s process-memory limitation. *)

val create : random:Random.t -> unit -> pending
(** [create ~random ()] creates a fresh ephemeral X25519 channel. *)

val public_key : pending -> Crypto_key.Curve25519.Public.t
(** [public_key t] is the channel's X25519 public key. *)

val establish_outbound :
  pending ->
  recipient:Crypto_key.Curve25519.Public.t ->
  initial_plaintext:string ->
  (t * string, error) result
(** [establish_outbound t ~recipient ~initial_plaintext] creates the channel
    initiator's initial message. The returned string is
    [base64(ciphertext)|base64(ephemeral-public-key)]. *)

val establish_inbound : pending -> string -> (t * string, error) result
(** [establish_inbound t message] decrypts an initiator's initial wire message
    and returns the established recipient channel and plaintext. Malformed wire
    data is rejected without consuming [t]; a valid message consumes it even
    when its key is non-contributory or its ciphertext fails authentication. *)

val encrypt : t -> string -> (string, error) result
(** [encrypt t plaintext] encrypts one subsequent message and returns its
    unpadded Base64 wire representation. Message nonces are twelve-byte
    little-endian counters starting at zero. The all-FF nonce is used once;
    subsequent calls fail with {!Counter_exhausted} rather than wrapping and
    reusing a 96-bit nonce. *)

val decrypt : t -> string -> (string, error) result
(** [decrypt t message] authenticates and decrypts one subsequent Base64 wire
    message. The receive counter advances even when authentication fails, as
    required by vodozemac. Malformed Base64 is rejected before the counter is
    touched. *)

val check_code_bytes : t -> string
(** [check_code_bytes t] is the two raw bytes of the out-of-band check code. *)

val check_code : t -> int
(** [check_code t] maps the two check-code bytes to a number from 0 to 99. *)
