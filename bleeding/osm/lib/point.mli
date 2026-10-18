@@ portable

type t = private { lat : float; lon : float }

val v : lat:float -> lon:float -> t
(** [v ~lat ~lon] is a geographic point in degrees. Nonfinite coordinates or
    latitude outside -90..90 or longitude outside -180..180 raise
    [Invalid_argument]. *)

val jsont : t Jsont.t
(** [jsont] maps Overpass objects with numeric [lat] and [lon] members. *)
