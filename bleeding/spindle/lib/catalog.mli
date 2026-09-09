type repo = {
  did : string;
  owner : string;
  rkey : string;
  knot : string;
  source : string;
}
(** PDS assignments checked against canonical repository identities. *)

type t

val v :
  store:Store.t ->
  network:Network.t ->
  owner:string ->
  hostname:string ->
  static:repo option ->
  t

val members : t -> string list
val managed : t -> string -> repo option
val verified : t -> string -> repo
val knots : t -> string list
val authorized : t -> repo -> string -> bool
val bootstrap : t -> unit
val replace : t -> string -> string -> unit

val apply :
  t ->
  owner:string ->
  collection:string ->
  rkey:string ->
  Jsont.json option ->
  unit
