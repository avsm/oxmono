(** olm_primitives — the encodings and the cipher the Olm ratchets share.

    The wire formats are version [0x03] and are byte-compatible with libolm and
    vodozemac. Olm's HKDF uses a one-zero-byte salt rather than an empty one,
    which is a different function from an empty salt.

    @see <https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/olm.md>
      olm.md
    @see <https://gitlab.matrix.org/matrix-org/olm/-/blob/master/docs/megolm.md>
      megolm.md *)

val ct_equal : string -> string -> bool
(** [ct_equal a b] is [true] when [a] and [b] hold the same bytes, compared in
    time that depends on their lengths, not on where or whether they differ.
    Callers validate any fixed-length protocol field before using this
    comparator. This is the project's one comparator for a secret-derived value,
    such as a MAC or a shared verification secret, against a value an untrusted
    peer supplied. *)

val version : char
(** [version] is the byte every Olm and Megolm message starts with. *)

val default_salt : string
(** [default_salt] is the one zero byte libolm uses where a salt is not
    otherwise fixed. *)

val sha256 : string -> string
(** [sha256 s] is the raw 32-byte SHA-256 digest of [s]. *)

val hmac_sha256 : key:string -> string -> string
(** [hmac_sha256 ~key s] is the raw 32-byte HMAC-SHA-256 of [s] under [key]. *)

val hkdf : salt:string -> info:string -> ikm:string -> int -> string
(** [hkdf ~salt ~info ~ikm n] is [n] bytes of HKDF-SHA-256 output. [salt] is
    used as given. *)

val now : unit -> Ptime.t
(** [now ()] is the current system-clock instant. *)

val base64_decode : string -> what:string -> (string, Olm_error.t) result
(** [base64_decode s ~what] is the bytes [s] encodes. [what] names the value in
    the error. *)

val base64_encode : string -> string
(** [base64_encode b] is [b] in the unpadded base64 Matrix puts on the wire. *)

(** {1 Protocol buffers} *)

module Varint : sig
  (** The base-128 varint encoding the message formats use for integers. *)

  val encode : int -> string
  (** [encode n] is the varint encoding of [n].

      Raises [Invalid_argument] if [n] is negative. *)

  val decode : string -> int -> (int * int, Olm_error.t) result
  (** [decode s off] is the varint at offset [off] in [s], paired with the
      offset just past it. *)
end

module Pb : sig
  (** The subset of the protocol buffer wire format the messages use. *)

  (** The type for a decoded field value. *)
  type value =
    | Varint of int  (** A varint-encoded field. *)
    | Bytes of string  (** A length-delimited field. *)

  val parse : string -> ((int * value) list, Olm_error.t) result
  (** [parse s] is the fields of [s], as (field number, value) in the order they
      appear. A field of any wire type other than varint and length-delimited is
      an error rather than skipped, since no producer this interoperates with
      emits one. *)

  val bytes : (int * value) list -> int -> (string, Olm_error.t) result
  (** [bytes fields n] is the length-delimited field numbered [n]. *)

  val varint :
    (int * value) list -> int -> default:int -> (int, Olm_error.t) result
  (** [varint fields n ~default] is the varint field numbered [n], or [default]
      when the field is absent. *)

  val tag_bytes : tag:string -> string -> string
  (** [tag_bytes ~tag s] is [tag] followed by [s] length-delimited. *)

  val tag_varint : string -> int -> string
  (** [tag_varint tag n] is [tag] followed by the varint [n]. *)
end

(** {1 The message cipher} *)

module Cipher : sig
  (** AES-256-CBC with an HMAC-SHA-256 tag, keyed by a ratchet's output. *)

  type t
  (** The type for a message's derived keys. *)

  val olm : string -> t
  (** [olm message_key] derives the Olm keys from a message key. *)

  val megolm : string -> t
  (** [megolm ratchet] derives the Megolm keys from a ratchet's state. *)

  val of_expanded : string -> t
  (** [of_expanded b] builds a cipher from its 80-byte AES/MAC/IV expansion. *)

  val encrypt : t -> string -> string
  (** [encrypt t plaintext] is [plaintext] padded per PKCS#7 and encrypted. *)

  val decrypt : t -> string -> (string, Olm_error.t) result
  (** [decrypt t ciphertext] is the plaintext, with the padding checked. *)

  val mac8 : t -> string -> string
  (** [mac8 t msg] is the first 8 bytes of the MAC over [msg], which is the tag
      both message formats carry. *)

  val verify_mac8 : t -> msg:string -> tag:string -> bool
  (** [verify_mac8 t ~msg ~tag] compares [tag] against [mac8 t msg] in constant
      time. *)
end
