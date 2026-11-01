(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The address book reports.

    [CARDDAV:addressbook-query] finds the address objects of a collection that
    match a filter,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.6} RFC 6352 Section
     8.6}, and [CARDDAV:addressbook-multiget] fetches the ones named, Section
    8.7. Both answer with a multistatus in which each matching resource carries
    the properties asked for and, when asked, its vCard.

    @canonical Carddav.Report *)

(** The type for what a report asks of each matching resource. *)
type props =
  | Prop of Httpz_dav.name list * Carddav_address_data.t option
      (** The named properties, and the vCard when address data is given. *)
  | Allprop
      (** Every dead property of each resource and the live properties
          {{:https://www.rfc-editor.org/rfc/rfc4918.html} RFC 4918} defines,
          Section 14.2. The vCard is not among them, since
          {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.4} RFC 6352
           Section 10.4} carries address-data inside [DAV:prop] alone. *)
  | Propname
      (** The property names alone,
          {{:https://www.rfc-editor.org/rfc/rfc4918.html#section-14.21} RFC 4918
           Section 14.21}. *)

type query = { props : props; filter : Carddav_filter.t; limit : int option }
(** The type for [CARDDAV:addressbook-query] requests,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.3} RFC 6352
     Section 10.3}. [limit] asks for at most that many responses, Section 10.6.
*)

type multiget = { props : props; hrefs : string list }
(** The type for [CARDDAV:addressbook-multiget] requests,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.7} RFC 6352
     Section 10.7}. *)

val query :
  ?props:Httpz_dav.name list ->
  ?data:Carddav_address_data.t ->
  ?limit:int ->
  Carddav_filter.t ->
  query
(** [query ~props ~data ~limit filter] asks for [props], which default to
    [DAV:getetag], and for the vCard as [data] describes, which defaults to the
    whole vCard. *)

val multiget :
  ?props:Httpz_dav.name list ->
  ?data:Carddav_address_data.t ->
  string list ->
  multiget
(** [multiget ~props ~data hrefs] asks for [hrefs] with the same defaults as
    {!val-query}. *)

val equal_query : query -> query -> bool
(** [equal_query a b] is [true] if [a] and [b] ask for the same properties, the
    same filter and the same limit. *)

val equal_multiget : multiget -> multiget -> bool
(** [equal_multiget a b] is [true] if [a] and [b] ask for the same properties
    and the same hrefs, in the same order. *)

val query_to_xml : query -> Httpz_dav.element
(** [query_to_xml q] is [q] as a [CARDDAV:addressbook-query] element. *)

val multiget_to_xml : multiget -> Httpz_dav.element
(** [multiget_to_xml m] is [m] as a [CARDDAV:addressbook-multiget] element. *)

val query_of_xml : Httpz_dav.element -> (query, string) result
(** [query_of_xml x] is the query [x] holds. *)

val multiget_of_xml : Httpz_dav.element -> (multiget, string) result
(** [multiget_of_xml x] is the multiget [x] holds. *)

(** {1 Responses} *)

type entry = {
  href : string;  (** The href of the address object. *)
  etag : string option;  (** Its ETag, if the response carried one. *)
  data : string option;  (** The vCard text, if asked for and returned. *)
  response : Httpz_dav.response;  (** The underlying response. *)
}
(** The type for one matching address object. *)

type outcome = {
  entries : entry list;  (** The matching address objects. *)
  truncated : bool;
}
(** The type for what a report returns. [truncated] is [true] if the collection
    itself answered [507],
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.6.2} RFC 6352
     Section 8.6.2}. *)

val outcome_of_multistatus : base:string -> Httpz_dav.multistatus -> outcome
(** [outcome_of_multistatus ~base m] reads the entries of [m], a response to a
    report on [base]. A response for [base] itself, or one with a failure
    status, is not an entry. *)

val missing : Httpz_dav.multistatus -> string list
(** [missing m] are the hrefs [m] answers with [404], which a multiget uses for
    the resources that do not exist. *)
