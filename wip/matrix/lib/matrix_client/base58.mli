(** base58 — the Matrix cryptographic key representation.

    Matrix shows a user a private key, the secret-storage key or the key-backup
    decryption key, as base58 in groups of four framed by a header and a parity
    byte. The alphabet is the Bitcoin one, which omits [0], [O], [I] and [l] so
    that a transcribed key cannot be misread. Only [matrix-chat.client] uses this
    module. {!Secret_storage} and {!Backup} are what a caller reaches for.

    @see <https://spec.matrix.org/v1.11/appendices/#cryptographic-key-representation>
      Cryptographic key representation *)

type error = [ `Msg of string ]
(** The type for decoding failures. The message says what was rejected. *)

val alphabet : string
(** The 58 characters the encoding uses, in digit order. *)

val encode : string -> string
(** [encode s] is [s] in base58. Each leading [\x00] byte becomes a leading
    ['1'], as in Base58Check, and [encode ""] is [""]. *)

val decode : string -> (string, error) result
(** [decode s] is the byte string [s] encodes. It fails on any character outside
    {!alphabet}, whitespace included. {!decode_key} is what a key a user may
    have retyped goes through. *)

val encode_key : string -> string
(** [encode_key key] is [key] in the cryptographic key representation as shown
    to a user, namely base58 over a framed [key] split into groups of four
    separated by single spaces. *)

val decode_key : string -> (string, error) result
(** [decode_key s] is the key {!encode_key} was given. All whitespace in [s] is
    ignored, so the grouping need not be reproduced. A frame that is intact but
    has a mistyped character fails the parity check rather than yielding a wrong
    key. *)
