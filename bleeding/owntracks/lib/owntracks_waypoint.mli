type t : immutable_data
(** Circular regions and BLE beacons. *)

val v :
  tst:int ->
  desc:string ->
  ?lat:float ->
  ?lon:float ->
  ?rad:int ->
  ?uuid:string ->
  ?major:int ->
  ?minor:int ->
  ?rid:string ->
  unit ->
  t
(** [v ~tst ~desc ()] creates a waypoint. Geographic and beacon fields are
    optional, as in the OwnTracks format. *)

val tst : t -> int
val desc : t -> string
val lat : t -> float option
val lon : t -> float option
val rad : t -> int option
val uuid : t -> string option
val major : t -> int option
val minor : t -> int option
val rid : t -> string option

val jsont : t Jsont.t
(** [jsont] requires the [waypoint] discriminator. *)

val jsont_bare : t Jsont.t
(** [jsont_bare] describes the fields without the discriminator. *)

val pp : Format.formatter -> t -> unit
