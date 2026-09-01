(** This module provides the [application/x-www-form-urlencoded] codec.

    A URL query string and an HTML form body of that media type hold a sequence
    of name and value pairs joined by [&], each pair joined by [=], with the
    bytes outside a small literal set percent-encoded and a space written as
    [+]. The serializer and the parser here are the ones the
    {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}WHATWG URL
     Standard} defines at the byte-serialization layer. For valid UTF-8 input,
    {!encode} produces the same bytes as a browser form submission.

    Both directions are byte oriented. No charset is applied: {!encode} writes
    the bytes it is given and {!decode} returns the bytes it read, so a UTF-8
    page round trips without further work. Malformed UTF-8 is preserved as
    input bytes rather than replaced as the complete WHATWG scalar-value-string
    algorithm would require. The [_charset_] form-field convention is not
    interpreted. This codec is not the OAuth 1.0 signature-base-string encoder,
    whose escaping and sorting rules differ.

    The typed codec for this media type is {!Media.form}. *)

val encode : (string * string) list -> string @@ portable
(** [encode pairs] is [pairs] serialized. Each name and value is written with
    the urlencoded byte serializer: a space becomes [+], an ASCII alphanumeric
    and the bytes [*], [-], [.] and [_] are written as they are, and every other
    byte becomes [%] and two uppercase hexadecimal digits. Pairs are joined by
    [&] and each name and value by [=]. Order and repeated names are preserved,
    and an empty list is the empty string. *)

val decode : string @ local -> (string * string) list @@ portable
(** [decode s] is the pairs [s] holds. [s] is split on [&], an empty sequence is
    dropped, each remaining sequence is split at its first [=] with a missing
    [=] giving an empty value, then [+] becomes a space and percent escapes are
    decoded in both halves. An escape that is truncated or not two hexadecimal
    digits is left as it stands rather than rejected, so decoding never fails.
*)
