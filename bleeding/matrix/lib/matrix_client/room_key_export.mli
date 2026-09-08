(** Portable, passphrase-protected Megolm room-key exports.

    Matrix key-export files contain a JSON array encrypted with
    PBKDF2-HMAC-SHA512, AES-256-CTR and HMAC-SHA256, then wrapped in the
    [MEGOLM SESSION DATA] ASCII armour. The format is shared with Element and
    matrix-rust-sdk; it is separate from server-side key backup.

    @see <https://spec.matrix.org/v1.17/client-server-api/#key-exports>
      Key exports *)

type room_key = {
  algorithm : string;
      (** The room encryption algorithm, normally [m.megolm.v1.aes-sha2]. *)
  room_id : Matrix_proto.Id.Room_id.t;
  sender_key : string;  (** The original sender's Curve25519 public key. *)
  session_id : Matrix_proto.Id.Session_id.t;
  session_key : string;  (** A Megolm exported-session key. *)
  sender_claimed_keys : (string * string) list;
      (** Signing keys claimed by the original sender, keyed by algorithm. *)
  forwarding_curve25519_key_chain : string list;
  shared_history : bool;  (** The stable [m.shared_history] flag from MSC3061. *)
}
(** One entry in the decrypted JSON array. Unknown encryption algorithms and key
    strings are retained so callers can decide which entries they support. *)

type history_not_shared = {
  algorithm : string;
  room_id : Matrix_proto.Id.Room_id.t;
  session_id : Matrix_proto.Id.Session_id.t;
  sender_key : string;
  from_device : string option;
  code : string;
  reason : string option;
}
(** A [m.history_not_shared] entry carried by an MSC4268 room-key bundle. The
    reason is optional for compatibility with older bundle producers. *)

type historic_room_key = {
  algorithm : string;
  room_id : Matrix_proto.Id.Room_id.t;
  sender_key : string;
  session_id : Matrix_proto.Id.Session_id.t;
  session_key : string;
  sender_claimed_keys : (string * string) list;
}
(** A historic room key in an MSC4268 bundle. Unlike a portable export, its wire
    representation has no forwarding-chain or shared-history fields. *)

type room_key_bundle = {
  room_keys : historic_room_key list;
  withheld : history_not_shared list;
}
(** The MSC4268 room-history bundle payload. *)

val room_key_jsont : room_key Jsont.t

val room_keys_jsont : room_key list Jsont.t
(** JSON codecs for the plaintext export contents. *)

val history_not_shared_jsont : history_not_shared Jsont.t
val historic_room_key_jsont : historic_room_key Jsont.t
val room_key_bundle_jsont : room_key_bundle Jsont.t

type error =
  | Invalid_headers
  | Invalid_base64
  | Truncated
  | Unsupported_version of int
  | Invalid_rounds of int64
  | Invalid_mac
  | Invalid_utf8
  | Invalid_json of string
      (** A malformed or unauthentic export. Authentication is checked before
          UTF-8 and JSON decoding. *)

val pp_error : Format.formatter -> error -> unit

val encrypt :
  random:Random.t ->
  passphrase:string ->
  ?rounds:int ->
  room_key list ->
  (string, error) result
(** [encrypt ~random ~passphrase ~rounds keys] serializes and encrypts [keys].
    [rounds] defaults to [500_000] and must be a positive value representable by
    the unsigned 32-bit wire field (and by the host [int]). Salt and IV bytes
    come only from [random]; bit 63 of the IV is cleared for Matrix AES-CTR
    interoperability. The returned armour contains unpadded Base64. Choosing
    more than [2_000_000] rounds deliberately creates a file whose reader must
    raise its default untrusted-input policy with [decrypt ~max_rounds]. *)

val decrypt :
  ?max_rounds:int64 ->
  passphrase:string ->
  string ->
  (room_key list, error) result
(** [decrypt ?max_rounds ~passphrase armour] authenticates and decrypts a
    version-1 export. It accepts padded or unpadded Base64 split across
    arbitrary armour lines. By default, wire PBKDF2 counts above [2_000_000] are
    rejected before deriving keys. A trusted caller may raise this explicit
    policy up to [0xffffffff] with [max_rounds] to import Rust-compatible
    exports; values above the host [int] range remain unavailable on that
    platform. A nonpositive or greater-than-[0xffffffff] [max_rounds] raises
    [Invalid_argument]. A wrong passphrase is [Error Invalid_mac]. *)
