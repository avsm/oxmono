(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Address book collections.

    An address book is a collection whose resource type holds
    [CARDDAV:addressbook],
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-5.2} RFC 6352 Section
     5.2}. The record here is what a PROPFIND of
    {!Carddav.Property.collection_props} says about one.

    @canonical Carddav.Addressbook *)

type t = {
  href : string;  (** The URL of the address book. *)
  display_name : string option;  (** The [DAV:displayname], if set. *)
  description : string option;
      (** The [CARDDAV:addressbook-description], if set. *)
  etag : string option;  (** The [DAV:getetag] of the collection, if set. *)
  sync_token : string option;
      (** The [DAV:sync-token] of the collection, if the server supports the
          sync-collection report of
          {{:https://www.rfc-editor.org/rfc/rfc6578.html} RFC 6578}. *)
  reports : Httpz_dav.name list;
      (** The reports the collection supports,
          {{:https://www.rfc-editor.org/rfc/rfc3253.html#section-3.1.5} RFC 3253
           Section 3.1.5}. *)
  data_types : (string * string) list;
      (** The content type and version pairs it accepts,
          {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.2.2} RFC 6352
           Section 6.2.2}. *)
  max_size : int option;
      (** The [CARDDAV:max-resource-size] of the collection, if set. *)
  collations : string list;
      (** The collations the [CARDDAV:supported-collation-set] of the collection
          lists. *)
  privileges : Httpz_dav.name list;
      (** The privileges of the current user,
          {{:https://www.rfc-editor.org/rfc/rfc3744.html#section-5.4} RFC 3744
           Section 5.4}, or none if the server does not say. *)
}
(** The type for address books. *)

val of_response : Httpz_dav.response -> t option
(** [of_response r] is the address book [r] describes, or [None] if [r] is not
    an address book collection. *)

val of_multistatus : Httpz_dav.multistatus -> t list
(** [of_multistatus m] are the address books among the responses of [m], in
    order. *)

val supports : Httpz_dav.name -> t -> bool
(** [supports report t] is [true] if [t] advertises [report]. *)

val accepts : version:string -> t -> bool
(** [accepts ~version t] is [true] if [t] lists [text/vcard] at [version], or
    lists nothing and [version] is ["3.0"]. *)

val mkcol :
  ?display_name:string -> ?description:string -> unit -> Httpz_dav.element list
(** [mkcol ~display_name ~description ()] are the properties of the extended
    MKCOL that creates an address book,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.1} RFC 6352
     Section 6.3.1}. *)

val propfind : Httpz_dav.propfind
(** [propfind] asks for {!Carddav.Property.collection_props}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the href and display name of [t]. *)
