(** Bounded identity and repository HTTP access with explicit network roots. *)

type t

val v :
  allow_http:bool ->
  plc:string ->
  < net : _ Eio.Net.t ; mono_clock : _ Eio.Time.Mono.t ; .. > ->
  t
(** [v ~allow_http ~plc system] reads over HTTPS, and only from public
    addresses, with each request bounded to 15 seconds. [plc] is the PLC
    directory's origin. With [allow_http], for a development network, it may
    also use HTTP, reach private addresses and resolve [did:web:localhost]. *)

val origin : allow_http:bool -> string -> string
(** [origin ~allow_http url] validates and canonicalizes an HTTP(S) origin,
    discarding its path. Credentials, query and fragment are rejected. *)

exception Unavailable of string
(** [Unavailable reason] means a host answered with a server error or a rate
    limit, or did not answer in time. Callers answer 503 so that the knot
    retries. *)

val json : t -> string -> Jsont.json
(** [json t url] is the JSON body of [GET url], at most 1 MiB. A client error's
    body is returned, since XRPC errors are JSON. It raises {!Unavailable} for a
    5xx or 429 answer or a timeout, and {!Json.Invalid} for a longer or
    malformed body. *)

val fetch_status : ?limit:int -> t -> string -> int * string
(** [fetch_status ~limit t url] is the status and body of [GET url], which may
    be at most [limit] bytes, 1 MiB by default. It raises as {!json} does. *)

val read_limited : _ Eio.Flow.source -> int -> string
(** [read_limited flow n] reads at most [n] bytes of [flow]. *)

val query : string -> (string * string) list -> string
(** [query url fields] is [url] with its query replaced by [fields]. *)

val knot : t -> string -> string
(** [knot t name] is the origin of the knot called [name], a host or a URL. *)

val resolve : t -> string -> string
(** [resolve t did] is the DID document of the [did:plc] or [did:web] [did], at
    most 64 KiB, whose [id] must be [did]. A document is reused for 60
    seconds, and at most 4096 are kept. *)

val service : Jsont.json -> string -> string -> string option
(** [service document name kind] is the endpoint of the service [#name] of type
    [kind] in the DID [document]. *)

val pds : t -> string -> string
(** [pds t did] is the origin of the PDS that [did]'s document names, which must
    have no path. *)

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
