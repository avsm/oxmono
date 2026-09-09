(** Supported OwnTracks messages. Unknown message types return a decode error.
    Unrecognised fields of supported types are ignored. *)
type t =
  | Location of Owntracks_location.t
  | Transition of Owntracks_transition.t
  | Waypoint of Owntracks_waypoint.t
  | Card of Owntracks_card.t
  | Lwt of Owntracks_lwt.t
  | Waypoints of Owntracks_waypoint.t list

val jsont : t Jsont.t
val of_string : string -> (t, string) result
val to_string : t -> (string, string) result

val decode : Mqttz.Slice.t @ local -> (t, string) result
(** [decode payload] reads the borrowed bytes directly. It does not retain or
    modify [payload]. The result owns its decoded strings and records. *)

val pp : Format.formatter -> t -> unit

val encode : t -> (Mqttz.Slice.t, string) result
(** [encode message] serializes into owned bytes for publishing. *)
