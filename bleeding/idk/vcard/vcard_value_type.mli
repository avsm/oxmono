@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Value data types.

    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-4} RFC 6350 Section
     4} defines the data types a property value may have, and
    {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-5.2} Section 5.2} the
    [VALUE] parameter that names the type when it is not the default of the
    property.

    @canonical Vcard.Value_type *)

(** The type for value data types. *)
type t =
  | Text
  | Uri
  | Date
  | Time
  | Date_time
  | Date_and_or_time
  | Timestamp
  | Boolean
  | Integer
  | Float
  | Utc_offset
  | Language_tag
  | Other of string  (** A registered or experimental type, in lowercase. *)

val of_string : string -> t
(** [of_string s] is the type named [s], compared case insensitively, and
    [Other s] if [s] names none of the types of Section 4. *)

val to_string : t -> string
(** [to_string t] is the name of [t] as the [VALUE] parameter spells it, in
    lowercase. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same type. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats the name of [t] on [ppf]. *)
