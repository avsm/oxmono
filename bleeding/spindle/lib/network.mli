(** Bounded identity and repository HTTP access with explicit network roots. *)

type t

val v :
  allow_http:bool ->
  plc:string ->
  < net : _ Eio.Net.t ; mono_clock : _ Eio.Time.Mono.t ; .. > ->
  t

val origin : allow_http:bool -> string -> string
(** [origin ~allow_http url] validates and canonicalizes an HTTP(S) origin,
    discarding its path. Credentials, query and fragment are rejected. *)

val read : ?limit:int -> t -> string -> string
val json : t -> string -> Jsont.json
val query : string -> (string * string) list -> string
val knot : t -> string -> string
val resolve : t -> string -> string
val service : Jsont.json -> string -> string -> string option
val pds : t -> string -> string

val records : t -> string -> string -> Jsont.json list
(** [records network owner collection] reads the owner's current collection from
    its PDS, with bounded pagination. *)
