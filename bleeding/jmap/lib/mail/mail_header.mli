@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Email headers.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.1.2} RFC 8621
     Section 4.1.2} defines the raw EmailHeader object and the seven parsed
    forms in which a client may ask for a header field. Section 4.1.3 names the
    [header:{name}:{form}:all] properties that carry them on an Email.

    @canonical Jmap.Proto.Email_header *)

(** {1 Raw headers} *)

type t = {
  name : string;  (** The field name, without the colon. *)
  value : string;
      (** The field value, as it appears in the message after the colon and
          before the terminating line break. *)
}
(** The type for EmailHeader objects. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an EmailHeader. *)

(** {1 Header fields} *)

type address_header =
  [ `From
  | `Sender
  | `Reply_to
  | `To
  | `Cc
  | `Bcc
  | `Resent_from
  | `Resent_sender
  | `Resent_reply_to
  | `Resent_to
  | `Resent_cc
  | `Resent_bcc ]
(** The type for the RFC 5322 originator, destination and resent address fields,
    the fields RFC 8621 Section 4.1.2.3 and Section 4.1.2.4 allow the
    [asAddresses] and [asGroupedAddresses] forms for. *)

type message_id_header =
  [ `Message_id | `In_reply_to | `References | `Resent_message_id ]
(** The type for the fields RFC 8621 Section 4.1.2.5 allows the [asMessageIds]
    form for. *)

type date_header = [ `Date | `Resent_date ]
(** The type for the fields RFC 8621 Section 4.1.2.6 allows the [asDate] form
    for. *)

type url_header =
  [ `List_help
  | `List_unsubscribe
  | `List_subscribe
  | `List_post
  | `List_owner
  | `List_archive ]
(** The type for the RFC 2369 fields RFC 8621 Section 4.1.2.7 allows the
    [asURLs] form for. *)

type text_header = [ `Subject | `Comments | `Keywords ]
(** The type for the fields RFC 8621 Section 4.1.2.2 allows the [asText] form
    for. List-Id is not among them. It is defined in RFC 2919 rather than in RFC
    5322 or RFC 2369, so every form is open to it and it is named as a custom
    field. *)

type trace_header = [ `Received | `Return_path ]
(** The type for the RFC 5322 Section 3.6.7 trace fields. They are defined in
    RFC 5322 and RFC 8621 Section 4.1.2 gives them no parsed form, so only the
    raw form may be asked for. *)

type standard_header =
  [ address_header
  | message_id_header
  | date_header
  | url_header
  | text_header
  | trace_header ]
(** The type for the header fields defined in RFC 5322 or RFC 2369, the fields
    whose parsed forms RFC 8621 Section 4.1.2 restricts. *)

type custom_header = [ `Custom of string ]
(** The type for a header field outside {!standard_header}. Any form may be
    asked for. *)

type any_header = [ standard_header | custom_header ]
(** The type for header fields. *)

val standard_header_to_string : [< standard_header ] -> string
(** [standard_header_to_string h] is the field name of [h], in the case its
    defining specification gives it. *)

val standard_header_of_string : string -> standard_header option
(** [standard_header_of_string name] is the standard header field called [name],
    compared without regard to case, or [None] if [name] is not defined in RFC
    5322 or RFC 2369. *)

val any_header_to_string : [< any_header ] -> string
(** [any_header_to_string h] is the field name of [h]. A custom field is named
    exactly as it was given. *)

(** {1 Parsed forms} *)

type form =
  [ `Raw
  | `Text
  | `Addresses
  | `Grouped_addresses
  | `Message_ids
  | `Date
  | `Urls ]
(** The type for the parsed forms of RFC 8621 Section 4.1.2. *)

val form_to_string : [< form ] -> string
(** [form_to_string f] is the property name suffix of [f], such as
    ["asAddresses"]. It is the empty string for [`Raw], which is the form a
    property with no suffix asks for. *)

val form_of_string : string -> form option
(** [form_of_string s] is the form whose suffix is [s], or [None] if [s] is not
    a form suffix. The empty string is [`Raw]. *)

val form_allows_header : form -> string -> bool
(** [form_allows_header f name] is [true] if RFC 8621 Section 4.1.2 allows the
    field [name] to be asked for in the form [f]. Every form is allowed for a
    field defined neither in RFC 5322 nor in RFC 2369, and the raw form is
    allowed for every field. *)

(** {1 Header properties}

    RFC 8621 Section 4.1.3 makes a header field name "any series of one or more
    printable ASCII characters (i.e., characters that have values between 33 and
    126, inclusive), except for colon (:)". A builder below raises
    [Invalid_argument] when the field it is given is a [`Custom name] whose
    [name] is not such a series, and when [name] denotes a standard field that
    Section 4.1.2 does not allow the builder's form for. *)

(** The type for the [header:{name}:{form}:all] properties of RFC 8621 Section
    4.1.3. The [header] field of each constructor holds the fields Section 4.1.2
    allows that form for, so a combination the specification rejects cannot be
    built. [all] asks for every instance of a repeated field rather than the
    last one. *)
type header_property =
  | Raw of { name : string; all : bool }
  | Text of { header : [ text_header | custom_header ]; all : bool }
  | Addresses of { header : [ address_header | custom_header ]; all : bool }
  | Grouped_addresses of {
      header : [ address_header | custom_header ];
      all : bool;
    }
  | Message_ids of {
      header : [ message_id_header | custom_header ];
      all : bool;
    }
  | Date of { header : [ date_header | custom_header ]; all : bool }
  | Urls of { header : [ url_header | custom_header ]; all : bool }

val raw : ?all:bool -> string -> header_property
(** [raw ~all name] is the raw form of the field [name]. [all] defaults to
    [false].

    @raise Invalid_argument if [name] is not a header field name, as above. *)

val text : ?all:bool -> [ text_header | custom_header ] -> header_property
(** [text ~all h] is the [asText] form of [h]. [all] defaults to [false].

    @raise Invalid_argument
      if [h] is not a field [asText] may be asked for, as above. *)

val addresses :
  ?all:bool -> [ address_header | custom_header ] -> header_property
(** [addresses ~all h] is the [asAddresses] form of [h]. [all] defaults to
    [false].

    @raise Invalid_argument
      if [h] is not a field [asAddresses] may be asked for, as above. *)

val grouped_addresses :
  ?all:bool -> [ address_header | custom_header ] -> header_property
(** [grouped_addresses ~all h] is the [asGroupedAddresses] form of [h]. [all]
    defaults to [false].

    @raise Invalid_argument
      if [h] is not a field [asGroupedAddresses] may be asked for, as above. *)

val message_ids :
  ?all:bool -> [ message_id_header | custom_header ] -> header_property
(** [message_ids ~all h] is the [asMessageIds] form of [h]. [all] defaults to
    [false].

    @raise Invalid_argument
      if [h] is not a field [asMessageIds] may be asked for, as above. *)

val date : ?all:bool -> [ date_header | custom_header ] -> header_property
(** [date ~all h] is the [asDate] form of [h]. [all] defaults to [false].

    @raise Invalid_argument
      if [h] is not a field [asDate] may be asked for, as above. *)

val urls : ?all:bool -> [ url_header | custom_header ] -> header_property
(** [urls ~all h] is the [asURLs] form of [h]. [all] defaults to [false].

    @raise Invalid_argument
      if [h] is not a field [asURLs] may be asked for, as above. *)

val header_property_to_string : header_property -> string
(** [header_property_to_string p] is the property name of [p], such as
    ["header:From:asAddresses:all"].

    @raise Invalid_argument
      if [p] contains an invalid field name or a form not allowed for the field.
*)

val header_property_of_string : string -> header_property option
(** [header_property_of_string s] is the property [s] names. A standard field is
    held as its own constructor, so
    [header_property_of_string "header:From:asAddresses"] is
    [Some (Addresses { header = `From; all = false })].

    It is [None] if [s] does not start with ["header:"], if the field name is
    not one or more printable ASCII characters, 33 to 126, other than colon, if
    it names a form that does not exist, or if it pairs a form with a field RFC
    8621 Section 4.1.2 does not allow it for, such as ["header:From:asDate"],
    which Section 4.2 requires to be rejected with [invalidArguments]. *)

val property_form : string -> (form * bool) option
(** [property_form s] is the form and the [:all] flag of the property [s], or
    [None] where {!header_property_of_string} is [None]. It is the parse a
    decoder needs, which the field name does not enter. *)

(** {1 Header values} *)

(** The type for the value of a header property. The constructor follows the
    form the property asked for and whether it asked for [:all]. The
    [asMessageIds] and [asURLs] forms share [Strings_single] and [Strings_all],
    both being [String[]|null] on the wire.

    A [_single] value is [None] when the message does not carry the field. A
    [_all] value is the empty list then, and an element of a [Date_all] or
    [Strings_all] list is [None] when that instance of the field could not be
    parsed in the form asked for. *)
type header_value =
  | String_single of string option
  | String_all of string list
  | Addresses_single of Mail_address.t list option
  | Addresses_all of Mail_address.t list list
  | Grouped_single of Mail_address.Group.t list option
  | Grouped_all of Mail_address.Group.t list list
  | Date_single of Ptime.t option
  | Date_all of Ptime.t option list
  | Strings_single of string list option
  | Strings_all of string list option list

val header_value_jsont : form:form -> all:bool -> header_value Jsont.t
(** [header_value_jsont ~form ~all] is the codec for the value of a property
    asked for in [form], with [all] as its [:all] flag. There are fourteen such
    codecs and each call is one of them. Encoding a value whose constructor is
    not the one [form] and [all] decode to raises [Jsont.Error] naming the form
    that was expected. *)

val pp_value : Format.formatter -> header_value -> unit
(** [pp_value ppf v] prints [v] on [ppf] in a form fit for a terminal, whatever
    shape RFC 8621 Section 4.1.2 gave it. Addresses print as [Name <addr>]
    lists, dates as RFC 3339, and the instances of an [:all] form separated by
    [ | ]. A field the message does not carry prints as [(absent)], which is a
    [_single] value of [None] and an empty [_all] list alike, and an instance
    that could not be parsed prints as [(unparsable)]. Control bytes in
    server-supplied strings are escaped. *)

val value_to_string : header_value -> string
(** [value_to_string v] is {!pp_value} of [v] as a string. *)
