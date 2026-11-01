@@ portable

(** Immutable decoded OSM objects. Optional structural fields distinguish
    omitted output from an explicitly empty list. Encoding replays [raw],
    including unknown fields, so records are private. *)

type metadata = private {
  version : int64 option;
  timestamp : string option;
      (** Unparsed edit timestamp, present with metadata output. *)
  changeset : int64 option;
  uid : int64 option;
  user : string option;
}

type common = private {
  id : int64;
  tags : Tags.t;
  metadata : metadata;
  raw : Jsont.json;
}

type node = private { common : common; point : Point.t option }

type way = private {
  common : common;
  nodes : int64 list option;
      (** Ordered node IDs. [None] means omitted, not an empty way. *)
  geometry : Point.t option list option;
  center : Point.t option;
      (** Bounding-box center, not necessarily on the feature. *)
  bounds : Bounds.t option;
}
(** Geometry preserves null entries produced by cropped output. *)

type member = private {
  reference : Reference.t;
  role : string;  (** Member role. Absent roles decode to the empty string. *)
  point : Point.t option;
  geometry : Point.t option list option;
}

type relation = private {
  common : common;
  members : member list option;
  center : Point.t option;
      (** Bounding-box center, not necessarily on the feature. *)
  bounds : Bounds.t option;
}

type count = private {
  nodes : int64;
  ways : int64;
  relations : int64;
  areas : int64 option;  (** Area count when supplied by the server. *)
  total : int64;  (** Server-reported total. The codec does not recompute it. *)
  raw : Jsont.json;
}

type t = private
  | Node of node
  | Way of way
  | Relation of relation
  | Area of common
  | Count of count
  | Other of Jsont.json
      (** [Other] retains unrecognised element types, such as custom [make]
          output. *)

val jsont : t Jsont.t
(** [jsont] decodes known element kinds and preserves unrecognised kinds as
    [Other]. Malformed known kinds fail. Encoding replays the complete raw
    object. *)

val member_jsont : member Jsont.t
(** [member_jsont] maps a relation member, including role and optional geometry.
    Unlike [jsont], it encodes only known fields. *)

val raw : t -> Jsont.json
(** [raw element] is the complete element JSON, including unknown fields. *)

val reference : t -> Reference.t option
(** [reference element] is its OSM identity. Areas and counts have none. *)

val tags : t -> Tags.t
(** [tags element] is its tag map. Counts and unknown types return an empty map. *)

val tag : string -> t -> string option
(** [tag key element] is an OSM tag value, if present. *)
