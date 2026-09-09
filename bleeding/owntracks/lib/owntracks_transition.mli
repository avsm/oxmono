(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Transition event type for OwnTracks.

    @canonical Owntracks.Transition

    Published when entering or leaving a monitored region. Transitions are
    triggered by geofences (circular regions) or beacons configured in the
    OwnTracks app. *)

type t : immutable_data
(** The type for transition events. *)

(** {1 Constructors} *)

val v :
  ?tid:string ->
  tst:int ->
  ?lat:float ->
  ?lon:float ->
  ?acc:float ->
  event:string ->
  ?desc:string ->
  ?wtst:int ->
  unit ->
  t
(** [v ~tst ~lat ~lon ~event ()] creates a transition event. *)

(** {1 Accessors} *)

val tid : t -> string option
(** [tid tr] returns the tracker ID of the device. *)

val tst : t -> int
(** [tst tr] returns the timestamp when the transition occurred. *)

val lat : t -> float option
(** [lat tr] returns the latitude where the transition was detected. *)

val lon : t -> float option
(** [lon tr] returns the longitude where the transition was detected. *)

val acc : t -> float option
(** [acc tr] returns the accuracy of the position in meters, if present. *)

val event : t -> string
(** [event tr] returns the event type: ["enter"] when entering a region,
    ["leave"] when leaving. *)

val desc : t -> string option
(** [desc tr] returns the description/name of the region, if present. *)

val wtst : t -> int option
(** [wtst tr] returns the timestamp of the waypoint definition that triggered
    this transition, if present. *)

(** {1 JSON Codec} *)

val jsont : t Jsont.t
(** [jsont] is a JSON codec for transition messages. Expects the ["_type"] field
    to be ["transition"]. *)

val jsont_bare : t Jsont.t
(** [jsont_bare] is a JSON codec that doesn't require the ["_type"] field. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf tr] pretty-prints a transition message. *)
