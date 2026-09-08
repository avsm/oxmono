@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSContact identifiers.

    An Id is a string of 1 to 255 octets drawn from the alphabet [A-Za-z0-9_-],
    the "URL and Filename Safe" base64url alphabet without its pad character, as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.1} RFC 9553
     Section 1.4.1}.

    Ids are the keys of the maps that hold the emails, phones, addresses and
    other repeated properties of a {!Jscontact.Card.t}. Section 1.4.1 requires a
    key to be preserved across versions of a Card, since a {!Jscontact.Patch.t}
    addresses a value through it. Ids carry no meaning of their own. An Id is
    unique only within the map that holds it, so the same Id may name an email
    in one map and a phone in another.

    @canonical Jscontact.Id *)

type t : immutable_data
(** The type for identifiers. *)

val of_string : string -> (t, string) result
(** [of_string s] is the identifier [s]. The error holds a human readable
    message when [s] is empty, longer than 255 octets, or holds an octet outside
    the Section 1.4.1 alphabet. *)

val v : string -> t
(** [v s] is {!of_string}.

    @raise Invalid_argument if [s] is not a well formed Id. *)

val to_string : t -> string
(** [to_string id] is the wire spelling of [id]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same identifier. Comparison is
    on the octets, since Section 1.7.1 makes JSContact strings case-sensitive.
*)

val compare : t -> t -> int
(** [compare a b] orders identifiers by their octets. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf id] formats [id] on [ppf]. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an identifier. Decoding errors on a string outside
    the Section 1.4.1 alphabet. *)
