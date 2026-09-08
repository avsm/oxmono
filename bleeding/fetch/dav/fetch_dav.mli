(** A WebDAV client over an existing Fetch capability. No server handlers or
    transport selection. See [../../httpz/dav/SPEC.md]. *)

type t
val v : ?limits:Httpz_dav.limits -> ?lenient_hrefs:bool -> root:string -> _ Fetch.t -> t
(** [root] is an absolute collection URL ending in [/], without query or
    fragment. [lenient_hrefs] opts into repairing malformed response hrefs,
    see {!Httpz_dav.multistatus}. The default is strict. Operations and destinations stay beneath it on the same origin.
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
  dav_errors : Httpz_dav.element list;
}
exception Http_error of http_error
(** Non-success HTTP responses, including redirects. Error diagnostics are
    bounded by the client's XML byte limit. Transport exceptions propagate.
    A failed exchange after a mutation may have an uncertain server outcome. *)

module Header : sig
  val depth : Httpz_dav.depth Fetch.Header.t
  val overwrite : bool Fetch.Header.t
  val lock_token : Httpz_dav.Token.t Fetch.Header.t
  val dav : string list Fetch.Header.t
  (** Joins repeated DAV fields; retains extension tokens and coded URLs. *)
end

type capabilities = { dav : string list; allow : string list }
val options : t -> string -> capabilities
val propfind : ?depth:Httpz_dav.depth -> t -> string -> Httpz_dav.propfind -> Httpz_dav.multistatus
(** Depth defaults to zero. A 207 preserves every property/resource failure. *)

type condition = Unconditional | If_match of Fetch.Header.etag | If_absent
(** If_match rejects weak validators. If_absent sends If-None-Match: *. *)

type mutation = Complete of int | Multi of Httpz_dav.multistatus
val proppatch : ?condition:condition -> ?if_:Httpz_dav.if_condition ->
  t -> string -> Httpz_dav.update list -> Httpz_dav.multistatus
val mkcol : ?if_:Httpz_dav.if_condition -> ?props:Httpz_dav.element list -> t -> string -> unit
(** With [props], the extended MKCOL of RFC 5689 sets them on the new
    collection, including a [DAV:resourcetype] naming an address book or
    calendar. *)

val mkcalendar : ?if_:Httpz_dav.if_condition -> ?props:Httpz_dav.element list -> t -> string -> unit
(** The MKCALENDAR of RFC 4791 Section 5.3.1, creating a calendar collection
    with the given properties. *)

type written = { status : int; etag : string option }
(** The status of a PUT and the entity tag the server gave the stored
    representation, RFC 4918 Section 9.7.1. *)

val put : ?condition:condition -> ?if_:Httpz_dav.if_condition -> ?content_type:string ->
  t -> string -> Fetch.body -> written
val get : ?headers:Fetch.Header.headers -> t -> string -> string * string option
(** A 200 body read within the XML byte limit, with its entity tag. Larger
    bodies raise [Protocol_error]; use {!with_download} to stream. *)
val delete : ?condition:condition -> ?if_:Httpz_dav.if_condition -> t -> string -> mutation
val copy : ?depth:Httpz_dav.tree_depth -> ?overwrite:bool -> ?if_:Httpz_dav.if_condition ->
  t -> src:string -> dst:string -> unit -> mutation
val move : ?overwrite:bool -> ?if_:Httpz_dav.if_condition ->
  t -> src:string -> dst:string -> unit -> mutation
(** COPY and MOVE default to no overwrite. COPY defaults to infinite depth;
    MOVE always uses infinite depth. Destination is checked independently. *)

val with_download : ?headers:Fetch.Header.headers -> t -> string ->
  (Fetch.response -> 'a) -> 'a
(** The callback handles 200/206 and 304, including headers and streaming body.
    Its response is closed on return or exception. Other statuses raise
    [Http_error]. It must not retain the response or flow. *)

type lease = { url : string; token : Httpz_dav.Token.t; granted : Httpz_dav.lock }
val lock : ?scope:Httpz_dav.scope -> ?depth:Httpz_dav.tree_depth -> ?timeout:Httpz_dav.timeout ->
  ?owner:Httpz_dav.xml list -> t -> string -> lease
val refresh_lock : ?timeout:Httpz_dav.timeout -> t -> lease -> lease
val unlock : t -> lease -> unit
(** Lock tokens, roots and granted timeouts come from the response. No automatic
    refresh is started. Release and refresh operate on the lease's URL. *)

val lock_condition : lease -> Httpz_dav.if_condition
(** A tagged condition naming the lease's URL. It can submit a destination
    lock for COPY/MOVE or a collection lock when creating a child, as well as
    a lock on the request URL. Refresh uses its own untagged condition. *)

(** {1 Reports and synchronization} *)

val report : ?depth:Httpz_dav.depth -> t -> string -> string -> Httpz_dav.multistatus
(** A REPORT (RFC 3253 Section 3.6) with the given XML body, answered by a
    207. Depth defaults to zero. *)

val report_body : ?depth:Httpz_dav.depth -> t -> string -> string -> string
(** A REPORT answered by a 200 with a non-XML body, such as the VFREEBUSY of
    a CalDAV free-busy-query, read within the XML byte limit. *)

val sync : ?token:string -> ?level:Httpz_dav.Sync.level -> ?limit:int ->
  ?props:Httpz_dav.name list -> t -> string -> Httpz_dav.Sync.t
(** The RFC 6578 sync-collection report on a collection. [props] defaults to
    [DAV:getetag]. A stale token is an [Http_error] whose [dav_errors] name
    [DAV:valid-sync-token]. *)

(** {1 Discovery} *)

val context_path : t -> Httpz_dav.Discovery.service -> string
(** The URL the service's well-known path redirects to, RFC 6764 Section 5,
    or the root when it does not redirect. The well-known path is at the
    origin, so the client must be rooted there. *)

val principal : t -> string -> string
(** The current user's principal URL, read from [DAV:current-user-principal]
    or [DAV:principal-URL] of the target, RFC 5397. Raises [Protocol_error]
    when the server reports the user unauthenticated or names none. *)

val home_set : t -> Httpz_dav.name -> string -> string list
(** The hrefs of a home set property, such as CardDAV's addressbook-home-set,
    read from a principal and resolved against it. Empty when unreported. *)

val read_only : t -> t
(** [read_only t] permits GET, HEAD, OPTIONS and PROPFIND only.
    Restrictions remain enforced when mutation functions are called.
    Existing URL restrictions also apply to COPY/MOVE destinations. *)

(** {1 Mirroring a collection}

    A collection kept as a directory of files, one per member, by the
    client algorithm of RFC 6578 Appendix B. The directory holds an index
    naming the token of the state it mirrors and the entity tag each file was
    fetched at. A run reports the changes since that token, fetches the
    members whose entity tag differs, removes the members reported removed
    and stores the token last, so that an interrupted run repeats work rather
    than losing it. A refused token rebuilds the directory. A collection
    without the sync-collection report is polled by a PROPFIND of depth one
    and the same entity tag comparison. *)
module Mirror : sig
  type action =
    | Initial  (** No token is stored, so every member is listed. *)
    | Restart of string
        (** The stored token was refused for the reason given, so the
            directory is rebuilt. *)
    | Polling  (** The collection has no sync-collection report. *)
    | Fetched of string * string  (** A member was fetched into a file. *)
    | Skipped of string  (** A reported member's entity tag was already held. *)
    | Removed of string * string  (** A member's file was removed. *)
    | Pruned of string  (** A file the rebuilt directory no longer needs. *)
    | Truncated  (** The server left changes out, so the report repeats. *)
    | Token of string  (** The token stored at the end of a page. *)
    | Unchanged  (** Nothing changed. *)
  val pp_action : Format.formatter -> action -> unit
  type summary = { fetched : int; removed : int; token : string option }
  val index_file : string
  (** [".davsync"], holding the token and one [href], [etag], [file] line per
      member. *)

  val file_of_href : string -> string
  (** The last path segment of an href. Unsafe, reserved, dot and separator
      names raise [Protocol_error]. Encoded separators stay encoded. *)

  val run : ?log:(action -> unit) -> ?limit:int -> ?level:Httpz_dav.Sync.level ->
    t -> collection:string -> dir:Eio.Fs.dir_ty Eio.Path.t -> summary
  (** Brings [dir] up to date with [collection], creating it if absent. Each
      step is reported to [log]. [limit] bounds the changes per page and
      [level] defaults to [`One]. Members that are collections are not
      fetched. The directory must be dedicated to this mirror, without other
      local writers or concurrent runs. Existing member files may be replaced.
      Each run confines its file operations to a subtree capability. Unsafe
      index entries, foreign hrefs, filename collisions, incomplete listings
      and non-advancing tokens are rejected. Files and index are replaced
      atomically. A rebuild commits its token only after pruning stale files.
      Runs allow at most 1024 pages and a 16 MiB index. Download bytes are not
      bounded here. These are interruption guarantees, without file fsync or
      power-loss durability. Raises what the underlying operations raise. *)
end
