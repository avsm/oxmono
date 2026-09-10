type window = private { start : Ptime.t; finish : Ptime.t }
(** Bounded server-expanded agenda requests and event summaries. *)

val window : start:Ptime.t -> finish:Ptime.t -> window

val of_strings : start:string -> finish:string -> window
(** [of_strings ~start ~finish] accepts explicit RFC3339 instants with offsets,
    for a positive half-open interval of at most 31 days. *)

val bounds : window -> string * string
val query : window -> Caldav.Report.query
val supports_events : Caldav_data.collection -> bool
val timezone : Caldav_data.collection -> string option

type occurrence = { starts : string; summary : Jsont.json }

type resource = {
  href : string;
  etag : string option;
  raw : string;
  occurrences : occurrence list;
}

val parse : href:string -> etag:string option -> string -> resource
(** [parse ~href ~etag raw] retains the expanded iCalendar and extracts event
    summaries. Recurrence rules remaining in the response are rejected.
    Cancelled instances are excluded. DATE ends remain exclusive. Floating dates
    are labelled and are never silently converted to UTC. *)
