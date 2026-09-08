@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Properties.

    A property is one content line of a vCard, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-3.3} RFC 6350 Section
     3.3}. It has an optional group, a name, parameters and a value. The value
    is kept as it appears on the wire after unfolding, and the typed accessors
    below read it as the data type of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4} Section 4} the
    caller asks for.

    @canonical Vcard.Property *)

type t : immutable_data
(** The type for a property. *)

(** {1 Constructors} *)

val v : ?group:string -> ?params:Vcard_param.t list -> string -> string -> t
(** [v ~group ~params name value] is the property [name] holding the wire
    [value], which is escaped as the value type requires. [name] is uppercased.
    [group] defaults to none and [params] to the empty list. *)

val of_text :
  ?group:string -> ?params:Vcard_param.t list -> string -> string -> t
(** [of_text ~group ~params name s] is the property [name] whose text value is
    [s], escaped by {!Vcard.Text.escape}. *)

val of_text_list :
  ?group:string -> ?params:Vcard_param.t list -> string -> string list -> t
(** [of_text_list ~group ~params name l] is the property [name] whose value is
    the text list [l], such as a [NICKNAME] or [CATEGORIES]. *)

val of_structured :
  ?group:string -> ?params:Vcard_param.t list -> string -> string list list -> t
(** [of_structured ~group ~params name cs] is the property [name] whose value is
    the structured value of the components [cs], such as an [N] or [ADR]. *)

val of_uri :
  ?group:string -> ?params:Vcard_param.t list -> string -> string -> t
(** [of_uri ~group ~params name u] is the property [name] whose value is the URI
    [u], written as it is. *)

val of_string : string -> (t, string) result
(** [of_string line] is the property on the unfolded content line [line]. The
    error holds a message if [line] has no property name, no colon, or a
    malformed parameter. *)

(** {1 Accessors} *)

val group : t -> string option
(** [group p] is the group of [p]. *)

val name : t -> string
(** [name p] is the name of [p], in uppercase. *)

val params : t -> Vcard_param.t list
(** [params p] are the parameters of [p], in order. *)

val value : t -> string
(** [value p] is the wire value of [p], escaped. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same group, compared case
    insensitively, and the same name, parameters and value. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] formats [p] on [ppf] as one unfolded content line. *)

val to_string : t -> string
(** [to_string p] is [p] as one unfolded content line, without its line break.
*)

(** {1 Parameters} *)

val find_values : t -> string -> string list option
(** [find_values p name] are the values of the parameters of [p] called [name],
    or [None] if there is none. A parameter written without a value gives
    [Some []]. {!Vcard.find} is the one that names a property rather than a
    parameter, and is the first match rather than every one. *)

val find_first : t -> string -> string option
(** [find_first p name] is the first value of the parameter [name] of [p]. *)

val value_type : t -> Vcard_value_type.t
(** [value_type p] is the value of the [VALUE] parameter of [p], and otherwise
    the default type of its name, per {!Vcard.Registry.value_type}. *)

val pref : t -> int option
(** [pref p] is the [PREF] parameter of [p], or [None] if it is absent or not an
    integer. {!Vcard.validate} holds it to the range 1 to 100. *)

val types : t -> string list
(** [types p] are the values of the [TYPE] parameters of [p], in lowercase. A
    quoted value holding commas, such as [TYPE="voice,home"], is split into its
    values. *)

val sort_as : t -> string list
(** [sort_as p] are the values of the [SORT-AS] parameter of [p], split on the
    comma, one per component of the property value. *)

val language : t -> string option
(** [language p] is the [LANGUAGE] parameter of [p]. *)

val altid : t -> string option
(** [altid p] is the [ALTID] parameter of [p]. *)

val prop_id : t -> string option
(** [prop_id p] is the [PROP-ID] parameter of [p], as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9554.html#section-4.7} RFC 9554 Section
     4.7}. *)

val pids : t -> string list
(** [pids p] are the values of the [PID] parameters of [p], split on the comma.
*)

val media_type : t -> string option
(** [media_type p] is the [MEDIATYPE] parameter of [p]. *)

(** {1 Typed values} *)

val text : t -> string
(** [text p] is the value of [p] as a single text, unescaped. *)

val text_list : t -> string list
(** [text_list p] is the value of [p] as a comma separated text list. *)

val structured : t -> string list list
(** [structured p] is the value of [p] as semicolon separated components, each a
    list of comma separated values. *)

val uri : t -> string
(** [uri p] is the value of [p] as a URI, which is the wire value itself. *)

val boolean : t -> (bool, string) result
(** [boolean p] is the value of [p] as a boolean, compared case insensitively.
*)

val integers : t -> (int64 list, string) result
(** [integers p] is the value of [p] as a comma separated list of integers, each
    an optional sign and decimal digits within the 64-bit range of
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4.5} RFC 6350 Section
     4.5}. *)

val floats : t -> (float list, string) result
(** [floats p] is the value of [p] as a comma separated list of floats, each an
    optional sign, decimal digits and an optional fraction. *)

val date_and_or_time : t -> (Vcard_date.t, string) result
(** [date_and_or_time p] is the value of [p] as a date, a time or both. *)

val timestamp : t -> (Vcard_date.Timestamp.t, string) result
(** [timestamp p] is the value of [p] as a timestamp. *)

val utc_offset : t -> (Vcard_date.Utc_offset.t, string) result
(** [utc_offset p] is the value of [p] as a UTC offset. *)
