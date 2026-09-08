(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Calendar object representations.

    A calendar object travels as iCalendar text,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-4.1} RFC 4791 Section
     4.1}, and a program holds it as text or as an {!Ical.t}. A codec is a
    representation with its media type and the conversions to and from the wire,
    and every client function that reads or writes a calendar object takes one.

    @canonical Caldav.Data *)

type 'a t = {
  content_type : string;  (** The media type sent, such as [text/calendar]. *)
  version : string;  (** The version sent, such as [2.0]. *)
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
    defaults to [text/calendar] and [version] to [2.0]. *)

val map :
  decode:('a -> ('b, string) result) ->
  encode:('b -> ('a, string) result) ->
  'a t ->
  'b t
(** [map ~decode ~encode c] is the codec of ['b] that reads by [c] then [decode]
    and writes by [encode] then [c]. *)

val raw : string t
(** [raw] is the iCalendar text itself. *)

val ical : Ical.t t
(** [ical] reads and writes an {!Ical.t}. *)

val calendar_data : 'a t -> Caldav_calendar_data.t
(** [calendar_data c] asks a report for the whole calendar object in the media
    type and version of [c]. *)

val uid : 'a t -> 'a -> string option
(** [uid c v] is the UID the components of [v] share, which names the resource a
    new calendar object is stored at. *)
