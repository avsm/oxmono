(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A CalDAV client.

    A client is connected to one server as one principal. {!connect} finds the
    principal and its calendar home from a URL by the steps of
    {{:https://www.rfc-editor.org/rfc/rfc6764.html#section-6} RFC 6764 Section
     6}, and every other function is one request through {!Fetch_dav}. Calendar
    objects are read and written through a {!Caldav.Data.t}, so that a program
    chooses whether it sees iCalendar text or an {!Ical.t}.

    @canonical Caldav_eio.Client *)

type t
(** The type for clients. A client may be shared by the fibers of one domain. *)

type error = Fetch_dav.Session.error =
  | Http of int * string
      (** A failure status the server gave, with the response body. *)
  | Dav of int * Httpz_dav.element list
      (** A failure status with a [DAV:error] body naming the conditions that
          failed,
          {{:https://www.rfc-editor.org/rfc/rfc4918.html#section-16} RFC 4918
           Section 16}. *)
  | Precondition_failed of string
      (** A [412] to a request with [If-Match] or [If-None-Match], with the
          target. The resource changed under the client, or exists where a new
          one was to be created. *)
  | Not_found of string  (** A [404], with the target. *)
  | Xml of string  (** A response body the XML reader rejected. *)
  | Data of string  (** A calendar object the codec rejected. *)
  | Discovery of string  (** No principal or calendar home was found. *)
  | Transport of Fetch.error * string
      (** A network, TLS or policy failure reported by {!Fetch}. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints a one line description of [e], with the CalDAV
    conditions explained. *)

val error_to_string : error -> string
(** [error_to_string e] is [e] as the one line {!pp_error} prints. *)

(** {1 Connecting} *)

val connect :
  sw:Eio.Switch.t ->
  ?credentials:Fetch.Credential.t list ->
  ?allow_insecure:bool ->
  ?limits:Httpz_dav.limits ->
  ?quirks:Caldav_eio_quirks.t ->
  _ Fetch.t ->
  string ->
  (t, error) result
(** [connect ~sw ~credentials ~allow_insecure ~limits ~quirks fetch url] is a
    client for the server at [url] over the HTTP client [fetch], such as
    [Fetch_httpz.std env]. [sw] is the switch the client belongs to. A streaming
    {!download} lives as long as it does, and the fibers a program runs the
    client from should be under it. [url] is the server's root, its well-known
    CardDAV path or the principal itself. The client follows a redirect from the
    well-known path, reads [DAV:current-user-principal] from what it reaches,
    and then [CALDAV:calendar-home-set] from the principal,
    {{:https://www.rfc-editor.org/rfc/rfc6764.html#section-6} RFC 6764 Section
     6} and
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-6.2.1} RFC 4791
     Section 6.2.1}.

    [credentials] are attached to every request on the origin of [url] and
    default to none. [allow_insecure] permits the credential on [http://] and
    defaults to [false]. [limits] bound every XML body read and default to
    {!Httpz_dav.default_limits}. [quirks] is what the server does differently
    and defaults to {!Caldav_eio.Quirks.of_url} of [url]. *)

val principal : t -> string
(** [principal t] is the URL of the principal [t] is connected as. *)

val home_sets : t -> string list
(** [home_sets t] are the URLs of the calendar home collections of the
    principal,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-6.2.1} RFC 4791
     Section 6.2.1}. *)

val quirks : t -> Caldav_eio_quirks.t
(** [quirks t] is the profile [t] applies. *)

val session : t -> Fetch_dav.Session.t
(** [session t] is the session of [t], for a request this module does not make.
*)

val dav : t -> Fetch_dav.t
(** [dav t] is the {!Fetch_dav} client of [t], rooted at the origin. *)

val download : t -> string -> (Fetch.response, error) result
(** [download t url] is a streaming GET of [url] whose response lives as long as
    the switch [t] was connected under, {!Fetch_dav.Session.download}. *)

(** {1 Calendars} *)

val calendars : t -> (Caldav.Calendar.t list, error) result
(** [calendars t] are the calendars of every home set, found by a PROPFIND of
    depth one. *)

val calendar : t -> string -> (Caldav.Calendar.t, error) result
(** [calendar t url] is the calendar at [url]. *)

val create_calendar :
  t ->
  ?display_name:string ->
  ?description:string ->
  ?timezone:string ->
  ?components:string list ->
  string ->
  (unit, error) result
(** [create_calendar t ~display_name ~description ~timezone ~components url]
    creates the calendar [url] by MKCALENDAR,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-5.3.1} RFC 4791
     Section 5.3.1}. *)

val delete_calendar : t -> string -> (unit, error) result
(** [delete_calendar t url] removes the collection [url] and everything in it.
*)

val set_props : t -> string -> Httpz_dav.update list -> (unit, error) result
(** [set_props t url updates] applies [updates] by PROPPATCH. A refused
    instruction is a [Dav] error with its status and the property names. *)

(** {1 Calendar objects} *)

type member = Fetch_dav.Session.member = {
  href : string;  (** The URL of the member. *)
  etag : string option;  (** Its [DAV:getetag], if the server set one. *)
  content_type : string option;
      (** Its [DAV:getcontenttype], if the server set one. *)
}
(** The type for a member named and described but not fetched. *)

type 'a entry = 'a Fetch_dav.Objects.entry = {
  href : string;
  etag : string option;
  value : 'a;
}
(** The type for objects read from the server, with their etags. The type is
    {!Fetch_dav.Objects.entry}, which the CalDAV client shares, so a program
    that speaks both handles what they return alike. *)

type 'a page = 'a Fetch_dav.Objects.page = {
  entries : 'a entry list;  (** The objects the report returned, in order. *)
  truncated : bool;
      (** [true] if the server returned fewer objects than matched. *)
}
(** The type for what a report returns. *)

val list : t -> string -> (member list, error) result
(** [list t url] are the members of the calendar [url], by a PROPFIND of depth
    one. A member is named and described, not fetched. {!val-multiget} reads the
    objects. *)

val get : 'a Caldav.Data.t -> t -> string -> ('a entry, error) result
(** [get codec t url] is the calendar object at [url]. *)

val put :
  'a Caldav.Data.t ->
  t ->
  ?etag:string ->
  ?create:bool ->
  string ->
  'a ->
  (string option, error) result
(** [put codec t ~etag ~create url v] stores [v] at [url] and is the etag the
    server gave it. [etag] makes the request conditional on the resource being
    unchanged, and a value that is not an entity tag is a [Data] error. [create]
    makes it conditional on the resource not existing. A calendar that stores a
    fixed set of components is asked for that set first, so a component it does
    not store is a [Data] error rather than the server's [403]. *)

val add :
  'a Caldav.Data.t ->
  t ->
  ?name:string ->
  string ->
  'a ->
  ('a entry, error) result
(** [add codec t ~name calendar v] stores [v] as a new member of [calendar], and
    is [v] with the URL it was stored at and the etag the server gave it. [name]
    defaults to the UID of [v] with [.ics] appended, or to a random name if it
    has none. A [name] that is not one path segment is a [Data] error. *)

val delete : t -> ?etag:string -> string -> (unit, error) result
(** [delete t ~etag url] removes the object [url]. [etag] makes the request
    conditional on the resource being unchanged. *)

(** {1 Reports} *)

val query :
  'a Caldav.Data.t ->
  t ->
  ?data:Caldav.Calendar_data.t ->
  ?timezone:string ->
  string ->
  Caldav.Filter.t ->
  ('a page, error) result
(** [query codec t ~data ~timezone url filter] are the calendar objects of [url]
    that match [filter]. [data] narrows or expands what is returned and defaults
    to the whole object in the media type of [codec]. [timezone] is an iCalendar
    VTIMEZONE to read floating times in,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.8} RFC 4791 Section
     9.8}, and defaults to none. *)

val events :
  'a Caldav.Data.t ->
  t ->
  ?start:Ical.Date.date_time ->
  ?finish:Ical.Date.date_time ->
  ?expand:bool ->
  string ->
  ('a entry list, error) result
(** [events codec t ~start ~finish ~expand url] are the calendar objects of
    [url] holding a VEVENT that overlaps the range, or every one without a
    range. With [expand], which defaults to [false], recurring events come back
    as their instances in the range,
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-9.6.5} RFC 4791
     Section 9.6.5}. *)

val multiget :
  'a Caldav.Data.t ->
  t ->
  string ->
  string list ->
  ('a entry list, error) result
(** [multiget codec t url hrefs] are the objects among [hrefs] in the collection
    [url],
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.9} RFC 4791 Section
     7.9}. A member that does not exist is left out. *)

val free_busy :
  t ->
  ?start:Ical.Date.date_time ->
  ?finish:Ical.Date.date_time ->
  string ->
  (Ical.t, error) result
(** [free_busy t ~start ~finish url] is the VFREEBUSY the free-busy-query report
    of
    {{:https://www.rfc-editor.org/rfc/rfc4791.html#section-7.10} RFC 4791
     Section 7.10} answers for the calendar [url]. The report needs a range, so
    giving neither bound is a [Data] error. *)

(** The type for changes a synchronisation reports. *)
type 'a change = 'a Fetch_dav.Objects.change =
  | Changed of 'a entry  (** The member as it now stands. *)
  | Removed of string  (** The href of a member that is gone. *)

type 'a sync = 'a Fetch_dav.Objects.sync = {
  token : string option;
  changes : 'a change list;
  truncated : bool;
}
(** The type for what a synchronisation reports. [token] is presented next time,
    and [truncated] asks for the report to be repeated with it. *)

val sync :
  'a Caldav.Data.t ->
  t ->
  ?token:string ->
  ?limit:int ->
  string ->
  ('a sync, error) result
(** [sync codec t ~token ~limit url] are the changes to the calendar [url] since
    [token], or every member without one,
    {{:https://www.rfc-editor.org/rfc/rfc6578.html} RFC 6578}. A changed member
    is fetched by a calendar-multiget of the hrefs reported. A member the report
    names but the multiget does not return went away between the two requests
    and is reported [Removed]. A collection is not a member and is not reported.
*)

val sync_token : t -> string -> (string option, error) result
(** [sync_token t url] is the [DAV:sync-token] of [url]. *)

(** {1 Requests} *)

val propfind :
  t ->
  ?depth:Httpz_dav.depth ->
  string ->
  Httpz_dav.propfind ->
  (Httpz_dav.multistatus, error) result
(** [propfind t ~depth url query] is the multistatus a PROPFIND of [url]
    answers. [depth] defaults to [`Zero]. *)

val report :
  t ->
  ?depth:Httpz_dav.depth ->
  string ->
  Httpz_dav.element ->
  (Httpz_dav.multistatus, error) result
(** [report t ~depth url body] is the multistatus a REPORT of [url] answers. *)
