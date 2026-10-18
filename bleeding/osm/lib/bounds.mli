@@ portable

type t = private { south : float; west : float; north : float; east : float }

val v : south:float -> west:float -> north:float -> east:float -> t
(** [v ~south ~west ~north ~east] validates geographic ranges and
    [south <= north]. [west > east] denotes a box crossing the antimeridian.
    Invalid bounds raise [Invalid_argument]. *)

val jsont : t Jsont.t
(** [jsont] maps Overpass bounds objects. *)

val nominatim_jsont : t Jsont.t
(** [nominatim_jsont] maps Nominatim's [south, north, west, east] string array. *)
