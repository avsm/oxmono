(** WebDAV handler generation from a storage backend.

    Given any module implementing the {!STORE} interface, this module
    generates a complete set of {{:https://datatracker.ietf.org/doc/html/rfc4918}RFC 4918}
    WebDAV routes suitable for use with {!Httpz_server.Route}.

    {2 Supported Methods}

    {v
      Method     Section    Status
      ────────   ────────   ──────────────────────────
      PROPFIND   9.1        207 Multi-Status / 400 Bad Depth
      PROPPATCH  9.2        403 Forbidden (stub, DAV:error XML)
      MKCOL      9.3        201 / 405 / 409 / 415
      GET        9.4        200 OK / 404 Not Found
      PUT        9.7        201 + ETag+Location / 204 + ETag / 409
      DELETE     9.6        204 No Content / 404 Not Found
      OPTIONS    9.8        200 OK (DAV: 1, 2)
      LOCK       9.10       200 OK / 201 Created / 423 Locked
      UNLOCK     9.11       204 No Content / 400 / 409
    v}

    {2 Compliance Notes}

    - Class 2 compliance: exclusive write locks via {!Webdavz_lock}
    - Invalid [Depth] header values return [400 Bad Request]
    - MKCOL checks parent collection exists ([409 Conflict])
    - PUT checks parent collection exists ([409 Conflict])
    - PUT returns [ETag] and [Location] headers on [201 Created]
    - Error responses use [DAV:error] XML per Section 16
    - XML responses include [<?xml?>] declaration
    - Write operations check lock tokens ([423 Locked] if blocked)
    - LOCK supports new locks and refresh (via Lock-Token header)
    - COPY and MOVE are not yet implemented

    @see <https://datatracker.ietf.org/doc/html/rfc4918> RFC 4918 — WebDAV
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-18> RFC 4918 Section 18 — DAV compliance classes *)

(** {1 Storage Interface}

    The STORE module type abstracts the backend that holds WebDAV resources.
    Implementations might use a local filesystem (see {!Carddavz_store}),
    an in-memory map, or a database. *)

module type RO_STORE = sig
  type t
  (** The store handle. *)

  val is_collection : t -> path:string -> bool
  (** [is_collection store ~path] returns [true] if [path] is a collection
      (directory).
      @see <https://datatracker.ietf.org/doc/html/rfc4918#section-3> RFC 4918 Section 3 — terminology *)

  val exists : t -> path:string -> bool
  (** [exists store ~path] returns [true] if [path] exists as any resource type. *)

  val get_properties : t -> path:string -> (Webdavz_xml.fqname * Webdavz_xml.tree list) list
  (** [get_properties store ~path] returns all properties for the resource.
      Called by PROPFIND handlers.
      @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.1> RFC 4918 Section 9.1 *)

  val read : t -> path:string -> string option
  (** [read store ~path] returns the resource content, or [None] if
      the resource doesn't exist or is a collection. *)

  val children : t -> path:string -> string list
  (** [children store ~path] returns the names of immediate children
      of a collection. Returns [\[\]] for non-collections. *)
end
(** Read-only storage interface. Sufficient for serving resources
    via PROPFIND and GET without any mutating operations. *)

module type STORE = sig
  include RO_STORE

  val write : t -> path:string -> content_type:string -> string -> unit
  (** [write store ~path ~content_type data] creates or overwrites a
      non-collection resource.
      @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.7> RFC 4918 Section 9.7 *)

  val delete : t -> path:string -> bool
  (** [delete store ~path] removes a resource (or collection with contents).
      Returns [true] on success, [false] if the resource doesn't exist.
      @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.6> RFC 4918 Section 9.6 *)

  val mkdir : t -> path:string -> unit
  (** [mkdir store ~path] creates a collection.
      @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.3> RFC 4918 Section 9.3 — MKCOL *)
end

(** {1 Route Generation} *)

val routes :
  (module STORE with type t = 's) -> 's -> locks:Webdavz_lock.t ->
  Httpz_server.Route.route list
(** [routes (module S) store ~locks] generates httpz routes implementing
    WebDAV class-2 compliance (with locking) for the given [store].

    Write operations (PUT, DELETE, MKCOL, PROPPATCH) check [locks] and
    return [423 Locked] if the resource is locked without the correct token.

    All routes use the {!Httpz_server.Route.tail} pattern, so they match
    any path. Mount them at a prefix using literal path segments:

    {[
      let locks = Webdavz_lock.create () in
      let dav_routes = Webdavz_handler.routes (module My_store) store ~locks in
      let all_routes = Httpz_server.Route.of_list dav_routes
    ]}

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-18.2> RFC 4918 Section 18.2 — class 2 *)

val read_only_routes :
  (module RO_STORE with type t = 's) -> 's -> Httpz_server.Route.route list
(** [read_only_routes (module S) store] generates read-only WebDAV routes.

    Only PROPFIND, GET, and OPTIONS are functional. PUT, DELETE, MKCOL,
    and PROPPATCH return [403 Forbidden]. The OPTIONS response advertises
    only [GET, HEAD, PROPFIND].

    {[
      let routes = Webdavz_handler.read_only_routes (module My_ro_store) store in
      let route_table = Httpz_server.Route.of_list routes
    ]} *)
