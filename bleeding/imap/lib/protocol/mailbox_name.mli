(** IMAP mailbox names retain their exact wire identity. Rev1 names use
    modified UTF-7 (RFC 3501 section 5.1.3). UTF-8 mode uses RFC 6855/9051.
    Both modes reject malformed UTF-8 and the controls U+0000 to U+001F and
    U+007F. A malformed received name remains available as [raw]. *)

type mode = Rev1 | Utf8
type t = { raw : string; mode : mode; utf8 : (string, string) result }

val of_wire : mode:mode -> string -> t
val encode : mode:mode -> string -> (string, string) result
val decode : mode:mode -> string -> (string, string) result
val encode_rev1 : string -> (string, string) result
val decode_rev1 : string -> (string, string) result
