(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Location message type for OwnTracks.

    @canonical Owntracks.Location

    The primary OwnTracks message type, published when the device reports its
    location. Contains GPS coordinates, accuracy, altitude, speed, heading, and
    various device state information.

    Required fields are latitude, longitude, and timestamp. All other fields are
    optional and may not be present depending on device capabilities and
    settings. *)

type t : immutable_data
(** The type for location messages. *)

(** {1 Constructors} *)

val v :
  ?tid:string ->
  tst:int ->
  lat:float ->
  lon:float ->
  ?alt:float ->
  ?acc:float ->
  ?vel:float ->
  ?cog:float ->
  ?batt:int ->
  ?bs:int ->
  ?conn:string ->
  ?t:string ->
  ?m:int ->
  ?poi:string ->
  ?inregions:string list ->
  ?addr:string ->
  ?topic:string ->
  unit ->
  t
(** [v ~tst ~lat ~lon ()] creates a location with the required fields. The
    constructor does not constrain coordinate ranges. Codecs require finite
    numbers. *)

(** {1 Accessors} *)

val tid : t -> string option
(** [tid loc] returns the tracker ID - a short identifier (typically 2
    characters) configured in the app. *)

val tst : t -> int
(** [tst loc] returns the timestamp as Unix epoch (seconds since 1970-01-01
    00:00:00 UTC). *)

val lat : t -> float
(** [lat loc] returns the latitude in decimal degrees. Nominal range: -90 to
    +90. *)

val lon : t -> float
(** [lon loc] returns the longitude in decimal degrees. Nominal range: -180 to
    +180. *)

val alt : t -> float option
(** [alt loc] returns the altitude above sea level in meters, if present. *)

val acc : t -> float option
(** [acc loc] returns the horizontal accuracy (radius) in meters, if present. *)

val vel : t -> float option
(** [vel loc] returns the velocity (speed) in km/h, if present. *)

val cog : t -> float option
(** [cog loc] returns the course over ground (heading) in degrees from true
    north (0-360), if present. *)

val batt : t -> int option
(** [batt loc] returns the battery level as percentage (0-100), if present. *)

val bs : t -> int option
(** [bs loc] returns the battery status, if present:
    - [0] = unknown
    - [1] = unplugged
    - [2] = charging
    - [3] = full *)

val conn : t -> string option
(** [conn loc] returns the connection type, if present:
    - ["w"] = WiFi
    - ["m"] = Mobile/cellular
    - ["o"] = Offline *)

val trigger : t -> string option
(** [trigger loc] returns what caused this location report, if present:
    - ["p"] = Ping (response to request)
    - ["c"] = Circular region event
    - ["b"] = Beacon event
    - ["r"] = Response to reportLocation
    - ["u"] = Manual/user-initiated
    - ["t"] = Timer-based
    - ["v"] = Monitoring mode change *)

val monitoring_mode : t -> int option
(** [monitoring_mode loc] returns the reported monitoring mode. OwnTracks uses
    [1] for significant changes and [2] for move mode. *)

val poi : t -> string option
(** [poi loc] returns the Point of Interest name if the device is currently at a
    defined location. *)

val inregions : t -> string list
(** [inregions loc] returns the list of region names the device is currently
    inside. May be empty. *)

val addr : t -> string option
(** [addr loc] returns the reverse-geocoded address, if present. Typically added
    by the OwnTracks Recorder server. *)

val topic : t -> string option
(** [topic loc] returns the MQTT topic this message was published to, if
    present. Added during parsing. *)

(** {1 Modifiers} *)

val with_topic : string -> t -> t
(** [with_topic topic loc] returns a new location with the topic set. *)

(** {1 JSON Codec} *)

val jsont : t Jsont.t
(** [jsont] is a JSON codec for location messages. Expects the ["_type"] field
    to be ["location"]. *)

val jsont_bare : t Jsont.t
(** [jsont_bare] is a JSON codec that doesn't require the ["_type"] field. Use
    this for parsing recorder API responses which omit the type field. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf loc] pretty-prints a location message. *)

val format_timestamp : int -> string
(** [format_timestamp tst] formats [tst] as [YYYY-MM-DD HH:MM:SS UTC]. *)
