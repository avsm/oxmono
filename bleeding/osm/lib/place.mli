@@ portable

type t = private {
  place_id : int64;  (** Nominatim database ID, not a stable OSM identifier. *)
  reference : Reference.t option;
      (** Source OSM object, absent for some interpolated results. *)
  point : Point.t;
      (** Representative coordinate. It need not be an entrance or street
          address. *)
  display_name : string;
  address : Address.t option;
  bounds : Bounds.t option;
  category : string option;
      (** Broad OSM category, such as [amenity] or [boundary]. *)
  kind : string option;
      (** Category value, such as [cafe] or [administrative]. *)
  importance : float option;
      (** Nominatim ranking score, not a confidence probability. *)
  raw : Jsont.json;
}

val jsont : t Jsont.t
(** [jsont] maps Nominatim JSONv2 places, including string coordinates and
    provider-specific address fields. Encoding replays the complete raw
    response. Interpolated addresses may have no OSM reference. *)

val area : t -> Reference.t option
(** [area place] is its way or relation reference for [Query.area]. A point
    result has none. This does not guarantee the server has generated an area. *)
