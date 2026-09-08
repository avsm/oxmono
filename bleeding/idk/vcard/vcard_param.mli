@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Property parameters.

    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-5} RFC 6350 Section
     5} defines the parameters a property carries. A parameter name is case
    insensitive. A parameter may hold several comma separated values, and a
    value that holds a colon, a semicolon or a comma is written between double
    quotes. {{:https://www.rfc-editor.org/rfc/rfc6868.html} RFC 6868} escapes a
    double quote, a line break and a circumflex in a value with a circumflex.

    @canonical Vcard.Param *)

type t : immutable_data
(** The type for a parameter. *)

val v : string -> string list -> t
(** [v name values] is the parameter [name] holding [values], which are
    unescaped. [name] is uppercased. *)

val name : t -> string
(** [name p] is the name of [p], in uppercase. *)

val values : t -> string list
(** [values p] are the values of [p], unescaped. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same name and values. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] formats [p] on [ppf] as it is written on the wire. *)

val values_named : t list -> string -> string list
(** [values_named ps name] are the values of every parameter of [ps] called
    [name], compared case insensitively, in order. *)

val find_values : t list -> string -> string list option
(** [find_values ps name] is {!values_named}, or [None] if no parameter of [ps]
    is called [name]. *)

val find_first : t list -> string -> string option
(** [find_first ps name] is the first value of the first parameter of [ps]
    called [name], or [None] if there is none. *)

val decode_value : string -> string
(** [decode_value s] is [s] with the circumflex escapes of
    {{:https://www.rfc-editor.org/rfc/rfc6868.html} RFC 6868} replaced. A
    circumflex before any character other than [n], a circumflex or an
    apostrophe is kept as it is. *)

val encode_value : string -> string
(** [encode_value s] is [s] escaped as
    {{:https://www.rfc-editor.org/rfc/rfc6868.html} RFC 6868} requires, and
    between double quotes if it holds a colon, a semicolon or a comma. A
    carriage return is dropped. *)

val to_string : t -> string
(** [to_string p] is [p] as it is written on the wire, its name followed by [=]
    and its encoded values. A parameter with no value is its name alone. *)
