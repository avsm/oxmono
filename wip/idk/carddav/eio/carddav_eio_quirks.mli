(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a server does differently.

    {{:https://www.rfc-editor.org/rfc/rfc6352.html} RFC 6352} leaves a server
    room a client has to live with, and some depart from it outright. A profile
    names the departures of one service, and {!Carddav_eio.Client} consults it
    in the functions the departure affects and nowhere else, so that the
    protocol layer stays what the RFC says.

    @canonical Carddav_eio.Quirks *)

type t = {
  vcard3 : bool;
      (** The server stores and serves vCard 3.0 whatever version it was given,
          {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-5.1} RFC 6352
           Section 5.1}. A 4.0 card read back has [KIND] as
          [X-ADDRESSBOOKSERVER-KIND], [PREF=1] folded into [TYPE=PREF], an [N]
          added when it had none, and its 4.0-only properties dropped. The
          client undoes the first two so that a JSContact card round trips. *)
  lenient_hrefs : bool;
      (** The server writes member names into hrefs unencoded, which the strict
          validator of {!Httpz_dav} rejects. See {!Fetch_dav.v}. *)
}
(** The type for profiles. Each field is [false] in {!standard} and set only
    where a service is known to depart from the RFC. *)

val standard : t
(** [standard] expects the RFC and is the default. *)

val fastmail : t
(** [fastmail] is Fastmail's CardDAV, which it serves through Cyrus. [vcard3] is
    set. Its address books honour conditional PUT and DELETE, support
    [param-filter], [match-type] and sync-collection, and refuse a second
    resource with the same UID. The profile records the service as it behaved on
    2026-09-08. *)

val of_url : string -> t
(** [of_url url] is the profile of the service at [url] by its host, which is
    {!fastmail} for [fastmail.com] and [messagingengine.com] hosts and
    {!standard} otherwise. *)

val upgrade_vcard3 : string -> string
(** [upgrade_vcard3 text] is the vCard [text] with the 3.0 conventions of
    [vcard3] mapped back to 4.0, and left as it is when it is not a 3.0 card. *)
