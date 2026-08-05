(** Low-level buffer writing primitives for HTTP response generation.

    Mirrors {!Buf_read} which provides reading primitives. All functions
    write to a bigstring buffer at a given offset and return the new offset.

    Uses [int16#] for offsets to match {!Buf_read} and enable unboxed arithmetic. *)

(** {1 Basic Writers} *)

(** Write a single character. Returns [off + 1]. *)
val char : bytes -> off:int16# -> char -> int16#

(** Write an unboxed character. Returns [off + 1]. *)
val char_u : bytes -> off:int16# -> char# -> int16#

(** Write a string. Returns [off + String.length s]. *)
val string : bytes -> off:int16# -> local_ string -> int16#

(** Write CRLF ([\r\n]). Returns [off + 2]. *)
val crlf : bytes -> off:int16# -> int16#

(** {1 Integer Writers} *)

(** Write a non-negative integer in decimal. Returns new offset. *)
val int : bytes -> off:int16# -> int -> int16#

(** Write an int64# in decimal. Returns new offset. *)
val int64 : bytes -> off:int16# -> int64# -> int16#

(** Write a non-negative integer in lowercase hexadecimal. Returns new offset. *)
val hex : bytes -> off:int16# -> int -> int16#

(** {1 Fixed-Width Writers} *)

(** Write a 2-digit decimal number (zero-padded). Returns [off + 2]. *)
val digit2 : bytes -> off:int16# -> int -> int16#

(** Write a 4-digit decimal number (zero-padded). Returns [off + 4]. *)
val digit4 : bytes -> off:int16# -> int -> int16#

(** {1 Conversion Helpers} *)

(** Convert int to int16#. *)
val i16 : int -> int16#

(** Convert int16# to int. *)
val to_int : int16# -> int
