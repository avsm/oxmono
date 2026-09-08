@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** vCard contact data.

    An implementation of the vCard 4.0 format of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html} RFC 6350}, with the
    parameter escapes of
    {{:https://www.rfc-editor.org/rfc/rfc6868.html} RFC 6868} and the
    properties, parameters and components that
    {{:https://www.rfc-editor.org/rfc/rfc9554.html} RFC 9554} and
    {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555} add for JSContact.

    A card is a list of properties. Each property keeps its wire value and is
    read through the typed accessors of {!Property}, so that a card passes
    through this library with every property it holds, known or not. *)

(** {1 Values and parameters} *)

module Text = Vcard_text
(** Text values and their backslash escapes,
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.4} RFC 6350 Section
     3.4}. *)

module Param = Vcard_param
(** Property parameters and their circumflex escapes,
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-5} RFC 6350 Section
     5} and {{:https://www.rfc-editor.org/rfc/rfc6868.html} RFC 6868}. *)

module Value_type = Vcard_value_type
(** The value data types of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4} RFC 6350 Section
     4} and the [VALUE] parameter. *)

module Registry = Vcard_registry
(** The registered properties, with their default value types and cardinalities.
*)

module Date = Vcard_date
(** The date and time value types of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.3} RFC 6350 Section
     4.3}. *)

(** {1 Properties} *)

module Property = Vcard_property
(** Content lines,
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.3} RFC 6350 Section
     3.3}. *)

module N = Vcard_n
(** The components of the [N] property,
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-6.2.2} RFC 6350
     Section 6.2.2} and
    {{:https://www.rfc-editor.org/rfc/rfc9554.html#section-2.2} RFC 9554 Section
     2.2}. *)

module Adr = Vcard_adr
(** The components of the [ADR] property,
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-6.3.1} RFC 6350
     Section 6.3.1} and
    {{:https://www.rfc-editor.org/rfc/rfc9554.html#section-2.1} RFC 9554 Section
     2.1}. *)

module Jscomps = Vcard_jscomps
(** The [JSCOMPS] parameter of
    {{:https://www.rfc-editor.org/rfc/rfc9555.html#section-3.3.1} RFC 9555
     Section 3.3.1}. *)

(** {1 Cards} *)

type t : immutable_data
(** The type for a vCard. *)

val v : ?version:string -> Property.t list -> t
(** [v ~version properties] is the vCard holding [properties], in order.
    [version] defaults to ["4.0"]. *)

val version : t -> string
(** [version t] is the value of the [VERSION] property of [t]. *)

val properties : t -> Property.t list
(** [properties t] are the properties of [t] other than [BEGIN], [VERSION] and
    [END], in order. *)

val find : t -> string -> Property.t option
(** [find t name] is the first property of [t] called [name], compared case
    insensitively, or [None] if there is none. *)

val find_all : t -> string -> Property.t list
(** [find_all t name] are the properties of [t] called [name], in order. *)

val group : t -> string -> Property.t list
(** [group t g] are the properties of [t] in the group [g], compared case
    insensitively, in order. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same version and the same
    properties in the same order. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] as {!to_string} writes it. *)

val validate : t -> (t, string) result
(** [validate t] is [Ok t] if [t] meets the rules of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html} RFC 6350} that its shape
    does not enforce. The version is [4.0]. Every property meets the cardinality
    {!Registry} records for it, counting the instances that share an [ALTID] as
    one, so an [FN] is present and an [N] appears at most once. A [PREF]
    parameter is one or two digits or [100], per Section 5.3, and in the range 1
    to 100. A [PID] parameter has the syntax of Section 5.5 and appears only on
    a property a card may hold several times. A [PROP-ID] parameter has the
    syntax of
    {{:https://www.rfc-editor.org/rfc/rfc9554.html#section-4.7} RFC 9554 Section
     4.7}. [MEMBER] appears only on a card whose [KIND] is [group]. *)

(** {1 Reading and writing} *)

val of_string : string -> (t list, string) result
(** [of_string s] are the vCards in [s], in order. Lines end in CRLF or LF,
    folded lines are unfolded, and blank lines are ignored wherever they occur.
    Each card starts with [BEGIN:VCARD] followed by [VERSION], holds no other
    [VERSION], and ends with [END:VCARD], and neither delimiter takes a
    parameter. The error names the logical line at fault, counting unfolded
    lines from one. *)

val one_of_string : string -> (t, string) result
(** [one_of_string s] is the vCard in [s], or an error if [s] holds none or
    several. *)

val to_string : t -> string
(** [to_string t] is [t] as text, with lines ending in CRLF and folded at 75
    octets. Values are written as they are held, so a property built with a raw
    line break in its value is written broken. {!Property.of_text} escapes a
    text value. *)

val unfold : string -> string list
(** [unfold s] are the logical lines of [s], with every folded line joined to
    the line it continues and line breaks removed. A blank line is a logical
    line of its own, so that counting the result gives the line numbers
    {!of_string} reports. *)

val fold : string -> string
(** [fold line] is [line] folded at 75 octets as
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.2} RFC 6350 Section
     3.2} requires, without splitting a multi-octet character. *)
