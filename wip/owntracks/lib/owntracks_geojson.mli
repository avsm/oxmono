type t
(** OwnTracks GeoJSON exports, derived from the bundled GeoJSON codec. Only
    Point and LineString features and FeatureCollection are supported.
    Coordinates are longitude, latitude, then optional altitude (RFC 7946). *)

val jsont : t Jsont.t

val point_feature : device_name:string -> Owntracks_location.t -> t
(** [point_feature ~device_name loc] creates a Point with time, accuracy, speed,
    battery and tracker ID properties when available. *)

val linestring_feature : device_name:string -> Owntracks_location.t list -> t
(** [linestring_feature ~device_name locations] sorts by timestamp and creates a
    LineString with point count and start/end times. Fewer than two points or
    differing MQTT topics raise [Invalid_argument]. *)

val collection : t list -> t
(** [collection features] groups features, flattening nested collections. *)

val to_string : t -> (string, string) result
(** [to_string value] encodes finite coordinates and valid geometry shapes.
    Invalid constructed values return [Error]. *)
