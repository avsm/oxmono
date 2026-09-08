@@ portable

(** Base64 as Matrix uses it.

    The specification writes keys, signatures and ciphertext in the standard
    alphabet without padding, and asks implementations to accept both the padded
    and the unpadded form on input.

    @see <https://spec.matrix.org/v1.11/appendices/#unpadded-base64>
      Unpadded Base64 *)

val encode : string -> string
(** [encode s] is [s] in the standard alphabet without padding. *)

val decode : string -> (string, [> `Msg of string ]) result
(** [decode s] is the bytes [s] encodes. [s] may use either canonical padded or
    canonical unpadded standard Base64. Whitespace, the URL-safe alphabet,
    malformed padding and non-zero unused trailing bits are rejected. *)

val decode_opt : string -> string option
(** [decode_opt s] is {!decode} with the error dropped. *)
