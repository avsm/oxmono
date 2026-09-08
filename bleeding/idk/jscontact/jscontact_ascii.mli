@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Character classes.

    The predicates the syntactic checks of this library are written with. They
    take one byte, so a check written with them runs over the bytes of a UTF-8
    string rather than its characters. *)

val is_alpha : char -> bool
(** [is_alpha c] is [true] if [c] is an ASCII letter. *)

val is_digit : char -> bool
(** [is_digit c] is [true] if [c] is an ASCII digit. *)

val is_alnum : char -> bool
(** [is_alnum c] is [true] if [c] is an ASCII letter or digit. *)

val is_graphic : char -> bool
(** [is_graphic c] is [true] if [c] is neither an ASCII control character nor
    the space. A byte above [0x7f] is graphic, so that a check written with it
    passes the bytes of a non-ASCII character through. *)
