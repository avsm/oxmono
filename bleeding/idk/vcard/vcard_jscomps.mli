@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JSCOMPS parameter.

    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-3.3.1} RFC 9555
     Section 3.3.1} defines the [JSCOMPS] parameter of the [N] and [ADR]
    properties, which records the order of the components of a JSContact name or
    address and the verbatim values of its separators. Its value is a semicolon
    separated list whose first entry is the default separator, followed by a
    positional entry for each component value or a separator entry.

    @canonical Vcard.Jscomps *)

(** The type for an entry. *)
type entry =
  | Position of int * int
      (** The component at the first index of the structured value and the value
          at the second index within it. The second index is [0] for the first
          value. *)
  | Separator of string  (** A separator holding the verbatim value. *)

type t = { default_separator : string option; entries : entry list }
(** The type for a parameter value. *)

val of_string : string -> (t, string) result
(** [of_string s] is the parameter value [s], decoded from its parameter
    quoting. The error holds a message if [s] has no entry after the first, if
    the first entry is neither empty nor a separator, if an entry is neither
    positional nor a separator, or if an index does not fit an [int]. *)

val to_string : t -> string
(** [to_string t] is the parameter value of [t], before its parameter quoting.
*)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same default separator and
    entries. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf]. *)
