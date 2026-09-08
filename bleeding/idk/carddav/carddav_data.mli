(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Address object representations.

    An address object travels as vCard text,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-5.1} RFC 6352 Section
     5.1}, but a program may hold it as a {!Vcard.t} or as a JSContact card. A
    codec is a representation with its media type and the conversions to and
    from the wire, and every client function that reads or writes an address
    object takes one. {!vcard} and {!raw} are here, and [carddav.jscontact] adds
    the card of {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} by way
    of {{:https://www.rfc-editor.org/rfc/rfc9555.html} RFC 9555}.

    @canonical Carddav.Data *)

type 'a t = {
  content_type : string;  (** The media type sent, such as [text/vcard]. *)
  version : string;  (** The version sent, such as [4.0]. *)
  decode : string -> ('a, string) result;  (** Reads the wire text as ['a]. *)
  encode : 'a -> (string, string) result;  (** Writes ['a] as the wire text. *)
}
(** The type for codecs of ['a]. *)

val v :
  ?content_type:string ->
  ?version:string ->
  decode:(string -> ('a, string) result) ->
  encode:('a -> (string, string) result) ->
  unit ->
  'a t
(** [v ~content_type ~version ~decode ~encode ()] is a codec. [content_type]
    defaults to [text/vcard] and [version] to [4.0]. *)

val map :
  decode:('a -> ('b, string) result) ->
  encode:('b -> ('a, string) result) ->
  'a t ->
  'b t
(** [map ~decode ~encode c] is the codec of ['b] that reads by [c] then [decode]
    and writes by [encode] then [c]. *)

val raw : string t
(** [raw] is the vCard text itself. *)

val vcard : Vcard.t t
(** [vcard] reads and writes a {!Vcard.t}. *)

val address_data : 'a t -> Carddav_address_data.t
(** [address_data c] asks a report for the whole address object in the media
    type and version of [c]. *)

val uid : 'a t -> 'a -> string option
(** [uid c v] is the UID of [v] if its vCard has one. It names the resource a
    new address object is stored at. *)
