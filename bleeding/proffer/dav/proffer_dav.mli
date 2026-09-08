(** A WebDAV client over an existing Fetch capability. No server handlers or
    transport selection. See [../../davz/SPEC.md]. *)
type t
val v : ?limits:Davz.limits -> root:string -> _ Fetch.t -> t
(** [root] is an absolute collection URL ending in [/], without query or
    fragment. Operations and destinations stay beneath it on the same origin.
    Redirects stop. No retry middleware is added; any supplied middleware still
    applies, so supply a client with retries disabled for single-attempt writes. *)
val root : t -> string
val resolve : t -> string -> string
(** Resolve an encoded reference against the root and check its scope.
    Raises [Invalid_argument] before network access if outside the root. *)
val child : t -> collection:string -> string -> string
(** Append one decoded name, percent-encoding it as a segment. Rejects empty
    names, dot segments, slashes and backslashes. Collection must end in [/]. *)

exception Protocol_error of string
type http_error = {
  status : int;
  headers : Http.Header.t;
  body : string;
  truncated : bool;
  dav_errors : Davz.element list;
}
exception Http_error of http_error
(** Non-success HTTP responses, including redirects. Error diagnostics are
    bounded by the client's XML byte limit. Transport exceptions propagate.
    A failed exchange after a mutation may have an uncertain server outcome. *)

module Header : sig
  val depth : Davz.depth Fetch.Header.t
  val overwrite : bool Fetch.Header.t
  val lock_token : Davz.Token.t Fetch.Header.t
  val dav : string list Fetch.Header.t
  (** Joins repeated DAV fields; retains extension tokens and coded URLs. *)
end

type capabilities = { dav : string list; allow : string list }
val options : t -> string -> capabilities
val propfind : ?depth:Davz.depth -> t -> string -> Davz.propfind -> Davz.multistatus
(** Depth defaults to zero. A 207 preserves every property/resource failure. *)

type condition = Unconditional | If_match of Fetch.Header.etag | If_absent
(** If_match rejects weak validators. If_absent sends If-None-Match: *. *)
type mutation = Complete of int | Multi of Davz.multistatus
val proppatch : ?condition:condition -> ?if_:Davz.if_condition ->
  t -> string -> Davz.update list -> Davz.multistatus
val mkcol : ?if_:Davz.if_condition -> t -> string -> unit
val put : ?condition:condition -> ?if_:Davz.if_condition -> ?content_type:string ->
  t -> string -> Fetch.body -> int
val delete : ?condition:condition -> ?if_:Davz.if_condition -> t -> string -> mutation
val copy : ?depth:Davz.tree_depth -> ?overwrite:bool -> ?if_:Davz.if_condition ->
  t -> src:string -> dst:string -> unit -> mutation
val move : ?overwrite:bool -> ?if_:Davz.if_condition ->
  t -> src:string -> dst:string -> unit -> mutation
(** COPY and MOVE default to no overwrite. COPY defaults to infinite depth;
    MOVE always uses infinite depth. Destination is checked independently. *)

val with_download : ?headers:Fetch.Header.headers -> t -> string ->
  (Fetch.response -> 'a) -> 'a
(** The callback handles 200/206 and 304, including headers and streaming body.
    Its response is closed on return or exception. Other statuses raise
    [Http_error]. It must not retain the response or flow. *)

type lease = { url : string; token : Davz.Token.t; granted : Davz.lock }
val lock : ?scope:Davz.scope -> ?depth:Davz.tree_depth -> ?timeout:Davz.timeout ->
  ?owner:Davz.xml list -> t -> string -> lease
val refresh_lock : ?timeout:Davz.timeout -> t -> lease -> lease
val unlock : t -> lease -> unit
(** Lock tokens, roots and granted timeouts come from the response. No automatic
    refresh is started. Release and refresh operate on the lease's URL. *)
val lock_condition : lease -> Davz.if_condition
(** A tagged condition naming the lease's URL. It can submit a destination
    lock for COPY/MOVE or a collection lock when creating a child, as well as
    a lock on the request URL. Refresh uses its own untagged condition. *)
