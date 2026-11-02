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

val fetch_status : ?limit:int -> t -> string -> int * string
(** [fetch_status t url] is the status and at most [limit] bytes of the body of
    [GET url]. *)

val read_limited : _ Eio.Flow.source -> int -> string
(** [read_limited flow n] reads at most [n] bytes of [flow]. *)

val query : string -> (string * string) list -> string
val knot : t -> string -> string
val resolve : t -> string -> string
val service : Jsont.json -> string -> string -> string option
val pds : t -> string -> string

val records_at : t -> service:string -> did:string -> string -> Jsont.json list
(** [records_at network ~service ~did collection] reads [did]'s collection from
    [service], with bounded pagination. *)

val records : t -> string -> string -> Jsont.json list
(** [records network owner collection] reads the owner's current collection from
    its PDS, with bounded pagination. *)

val record :
  t ->
  service:string ->
  did:string ->
  collection:string ->
  rkey:string ->
  Jsont.json option
(** [record network ~service ~did ~collection ~rkey] is the
    [com.atproto.repo.getRecord] answer of [service], or [None] if it has no
    such record or repository. Other XRPC errors raise [Failure]. *)
