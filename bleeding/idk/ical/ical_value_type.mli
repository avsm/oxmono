@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Value data types.

    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.3} RFC 5545 Section
     3.3} defines the data types a property value may have, and
    {{:https://www.rfc-editor.org/rfc/rfc5545.html#section-3.2.20} Section
     3.2.20} the [VALUE] parameter that names the type when it is not the
    default of the property. The types are those of iCalendar, which are not the
    types of {!Vcard.Value_type} although the content line syntax is shared.

    @canonical Ical.Value_type *)

(** The type for value data types. *)
type t =
  | Binary
  | Boolean
  | Cal_address
  | Date
  | Date_time
  | Duration
  | Float
  | Integer
  | Period
  | Recur
  | Text
  | Time
  | Uri
  | Utc_offset
  | Other of string  (** A registered or experimental type, in upper case. *)

val of_string : string -> t
(** [of_string s] is the type named [s], compared case insensitively, and
    [Other s] in upper case if [s] names none of the types of Section 3.3. *)

val to_string : t -> string
(** [to_string t] is the name of [t] as the [VALUE] parameter spells it, in
    upper case. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same type. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the name of [t] on [ppf]. *)
