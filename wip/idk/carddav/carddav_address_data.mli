(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The address-data element.

    [CARDDAV:address-data] appears among the properties of a report request to
    ask for the vCard of each matching resource, and among the properties of a
    response to carry it,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.4} RFC 6352
     Section 10.4}. It is not a WebDAV property and a PROPFIND does not return
    it.

    @canonical Carddav.Address_data *)

val name : Httpz_dav.name
(** [name] is [CARDDAV:address-data]. *)

type t = {
  content_type : string option;
      (** The media type wanted, defaulting to [text/vcard]. *)
  version : string option;  (** The version wanted, defaulting to [3.0]. *)
  props : [ `All | `Props of (string * bool) list ];
      (** The vCard properties wanted, each with whether only its name is
          wanted, the [novalue] attribute. [`Props []] asks for the whole vCard.
      *)
}
(** The type for the request form. *)

val v :
  ?content_type:string ->
  ?version:string ->
  ?props:(string * bool) list ->
  unit ->
  t
(** [v ~content_type ~version ~props ()] asks for [props], which defaults to the
    whole vCard. *)

val vcard4 : t
(** [vcard4] asks for [text/vcard] version [4.0]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] ask for the same media type, version
    and properties, in the same order. *)

val to_xml : t -> Httpz_dav.element
(** [to_xml t] is [t] as a request element. *)

val of_xml : Httpz_dav.element -> (t, string) result
(** [of_xml x] is the request [x] holds. *)

(** {1 Responses} *)

val data : Httpz_dav.element -> string option
(** [data p] is the vCard text a response [CARDDAV:address-data] carries, or
    [None] if [p] is another property. A line ending of a bare line feed is
    restored to a carriage return and line feed, since an XML parser folds them,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.4} RFC 6352
     Section 10.4}. Whitespace around the value is dropped, so a card that was
    indented by a pretty-printing server reads correctly and one that was not
    loses its final line break. *)

val content_type_of : Httpz_dav.element -> string * string
(** [content_type_of p] are the [content-type] and [version] attributes of [p],
    with their defaults. *)
