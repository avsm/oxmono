(** ASCII character classification and conversion.

    [Ascii] has character predicates and digit/value conversions over the ASCII
    range. Everything is total over [char]: bytes outside the relevant class
    answer [false] or [None]. *)

(** {1:classification Classification} *)

val is_digit : char -> bool
(** [is_digit c] holds for ['0'] .. ['9']. *)

val is_hex_digit : char -> bool
(** [is_hex_digit c] holds for ['0'] .. ['9'], ['a'] .. ['f'] and ['A'] ..
    ['F']. Both cases are accepted, unlike RFC 5234's uppercase-only [HEXDIG].
*)

(** {1:conversion Conversion} *)

val hex_value : char -> int option
(** [hex_value c] is the value of the hex digit [c], in \[[0];[15]\]. This is
    [None] if [c] is not a hex digit. *)

val hex_value_int : char -> int
(** [hex_value_int c] is like {!hex_value} but is [-1], rather than [None], if
    [c] is not a hex digit. The unboxed result keeps per-character hot loops
    allocation-free. *)

val hex_char : int -> char
(** [hex_char v] is the lowercase hex digit for [v].

    @raise Invalid_argument if [v] is not in \[[0];[15]\]. *)

val is_printable : char -> bool
(** [is_printable c] holds for the printable ASCII range [' '] (0x20) to ['~']
    (0x7e). *)
