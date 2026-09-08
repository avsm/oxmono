@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text values.

    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.4} RFC 6350 Section
     3.4} escapes a backslash, a comma and a line break in a text value with a
    backslash, and a semicolon in a component of a structured value likewise.
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.1} Section 4.1}
    defines the text value type.

    @canonical Vcard.Text *)

val unescape : string -> string
(** [unescape s] is [s] with its backslash escapes replaced. [\\n] and [\\N]
    become a line feed. A backslash before any character other than a backslash,
    a comma, a semicolon, an [n] or an [N] is kept as it is. *)

val escape : string -> string
(** [escape s] is [s] with every backslash, comma and line feed escaped, as a
    single text value is written. A carriage return is dropped. *)

val escape_component : string -> string
(** [escape_component s] is [escape s] with the semicolon escaped as well, as a
    component of a structured value is written. *)

val split : char -> string -> string list
(** [split sep s] are the pieces of [s] between the occurrences of [sep] that no
    backslash precedes. The escapes stay in the pieces. *)

val list_of_string : string -> string list
(** [list_of_string s] are the comma separated values of [s], unescaped. *)

val list_to_string : string list -> string
(** [list_to_string l] is the comma separated text list of [l], escaped. *)

val structured_of_string : string -> string list list
(** [structured_of_string s] are the semicolon separated components of [s], each
    a list of its comma separated values, unescaped. An absent component is the
    list holding the empty string. *)

val structured_to_string : string list list -> string
(** [structured_to_string cs] is the structured value of the components [cs],
    escaped. *)

val component : string list list -> int -> string list
(** [component cs i] are the values of the [i]th component of [cs], counting
    from zero, and the empty list if [cs] has no such component or the component
    holds the empty string alone. *)
