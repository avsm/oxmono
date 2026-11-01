(** verification_qr — the QR code payload and the [m.reciprocate.v1] handshake.

    The payload is the ASCII string [MATRIX], a version byte [0x02], a mode
    byte, the flow identifier prefixed by its big-endian 16-bit length, two
    32-byte Ed25519 keys, and the rest of the segment as a shared secret. It
    goes into a single byte-mode segment of the code.

    A device that scans a code checks the keys it holds against the two in the
    payload and then echoes the secret back in an [m.key.verification.start]
    with method [m.reciprocate.v1], which is how the displaying side learns that
    the scan succeeded.

    @see <https://spec.matrix.org/v1.11/client-server-api/#qr-code-format>
      QR code format *)

type error = [ `Msg of string ]
(** The type for a payload that is not well formed. The message names what was
    wrong with it. *)

(** Which keys the two 32-byte slots hold. *)
type mode =
  | Verifying_another_user
      (** [0x00], the displaying user's master key, then what they believe the
          scanning user's master key to be. *)
  | Self_verifying_master_key_trusted
      (** [0x01], the master key, then what the displaying device believes the
          scanning device's Ed25519 key to be. *)
  | Self_verifying_master_key_untrusted
      (** [0x02], the displaying device's Ed25519 key, then what it believes the
          master key to be. *)

val mode_to_int : mode -> int
(** [mode_to_int m] is the mode byte of [m]. *)

val mode_of_int : int -> mode option
(** [mode_of_int b] is the mode the byte [b] names, and [None] for any other
    byte. *)

type t
(** The type for a QR payload, decoded or about to be encoded. Its keys are 32
    bytes each and its secret is at least 8 bytes. *)

val mode : t -> mode
(** [mode t] is which keys the two slots of [t] hold. *)

val flow_id : t -> string
(** [flow_id t] is the verification flow [t] belongs to. *)

val first_key : t -> string
(** [first_key t] is the first 32-byte slot of [t], as unpadded base64. *)

val second_key : t -> string
(** [second_key t] is the second 32-byte slot of [t], as unpadded base64. *)

val shared_secret : t -> string
(** [shared_secret t] is the secret of [t], as unpadded base64. It is what the
    scanning side echoes back, so a caller that discloses it to anybody but the
    peer has given away the proof that the code was seen. *)

val shared_secret_raw : t -> string
(** [shared_secret_raw t] is the bytes {!shared_secret} encodes, for a caller
    that measures or compares them. It is the same secret and carries the same
    warning. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] have the same mode, flow, keys and
    secret. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the mode and the flow identifier. It prints neither the
    keys nor the secret. *)

(** {1 Building a payload} *)

val make :
  mode:mode ->
  flow_id:string ->
  first_key:string ->
  second_key:string ->
  shared_secret:string ->
  (t, error) result
(** [make ~mode ~flow_id ~first_key ~second_key ~shared_secret] is the payload
    those parts describe. The keys and the secret are base64, padded or not.
    Each key must be 32 bytes, the secret at least 8, and the flow identifier at
    most 65535 bytes. *)

val create :
  random:Random.t ->
  mode:mode ->
  flow_id:string ->
  first_key:string ->
  second_key:string ->
  (t, error) result
(** [create ~random ~mode ~flow_id ~first_key ~second_key] is {!make} with a
    fresh 8-byte secret drawn from [random]. *)

val for_other_user :
  random:Random.t ->
  flow_id:string ->
  our_master_key:string ->
  their_master_key:string ->
  (t, error) result
(** [for_other_user ~random ~flow_id ~our_master_key ~their_master_key] is a
    {!Verifying_another_user} payload. *)

val for_self_trusted :
  random:Random.t ->
  flow_id:string ->
  master_key:string ->
  their_device_key:string ->
  (t, error) result
(** [for_self_trusted ~random ~flow_id ~master_key ~their_device_key] is a
    {!Self_verifying_master_key_trusted} payload, shown by a device that already
    trusts the master key. *)

val for_self_untrusted :
  random:Random.t ->
  flow_id:string ->
  our_device_key:string ->
  master_key:string ->
  (t, error) result
(** [for_self_untrusted ~random ~flow_id ~our_device_key ~master_key] is a
    {!Self_verifying_master_key_untrusted} payload, shown by a device that does
    not yet trust the master key. *)

(** {1 The wire format} *)

val encode : t -> (string, error) result
(** [encode t] is the byte string to put in the code's single byte-mode segment.
*)

val decode : string -> (t, error) result
(** [decode s] is the payload the scanned segment [s] holds. It refuses a bad
    header, any version but 2, an unknown mode, a truncated payload and a shared
    secret shorter than 8 bytes. *)

(** {1 Checking a scanned code} *)

val check :
  t ->
  flow_id:string ->
  our_master_key:string option ->
  our_device_key:string option ->
  their_master_key:string option ->
  their_device_key:string option ->
  (unit, Verification_base.Cancel_code.t) result
(** [check t ~flow_id ~our_master_key ~our_device_key ~their_master_key
     ~their_device_key] compares the two slots of [t] against the keys this
    device already holds, as {!type-mode} says to. The keys are unpadded base64
    and each is [None] when this device holds no such key.

    A flow identifier that does not match gives [m.unknown_transaction]. A key
    that does not match gives [m.key_mismatch], and so does a key that is
    absent, since nothing can be concluded from a comparison against a key this
    device does not have. *)

val reciprocate_start :
  transaction:Verification_base.Transaction.t ->
  from_device:Matrix_proto.Id.Device_id.t ->
  t ->
  Verification_base.Message.t
(** [reciprocate_start ~transaction ~from_device t] is the
    [m.key.verification.start] with method [m.reciprocate.v1] that the scanning
    side sends to tell the displaying side the scan succeeded. It carries the
    secret read out of [t]. *)

val check_reciprocate :
  t -> secret:string -> (unit, Verification_base.Cancel_code.t) result
(** [check_reciprocate t ~secret] is the check the displaying side runs on the
    base64 [secret] it gets back. A secret that does not match gives
    [m.key_mismatch]. *)
