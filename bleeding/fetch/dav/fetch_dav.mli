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

(** {1 Sessions}

    A session is a client connected to one server as one principal, the ground
    a CardDAV or CalDAV client is built on. {!Session.connect} finds the
    principal and its home set from any URL of the service by RFC 6764, and
    HTTP, DAV, XML and Fetch transport failures are returned as results for
    composition with [Result.bind]. Cancellation and unexpected provider
    exceptions propagate. Invalid configuration may raise [Invalid_argument].
    The switch a session is connected under scopes its streaming downloads. *)
module Session : sig
  type dav := t
  type t
  type error =
    | Http of int * string  (** A status outside 2xx, with the start of the body. *)
    | Dav of int * Httpz_dav.element list
        (** A status with the precondition or postcondition names the server
            gave, RFC 4918 Section 16. *)
    | Precondition_failed of string  (** A 412 for the named resource. *)
    | Not_found of string  (** A 404 for the named resource. *)
    | Xml of string  (** A response that is not the XML expected. *)
    | Data of string  (** A body or argument the client or the caller could not use. *)
    | Discovery of string  (** No principal or home set could be found. *)
    | Transport of Fetch.error * string  (** A network, TLS or policy failure. *)
  val pp_error : ?describe:(Httpz_dav.name -> string) -> Format.formatter -> error -> unit
  (** [describe] names a condition element; it defaults to its local name. *)

  val error_to_string : ?describe:(Httpz_dav.name -> string) -> error -> string
  val connect : sw:Eio.Switch.t -> ?credentials:Fetch.Credential.t list ->
    ?allow_insecure:bool -> ?limits:Httpz_dav.limits -> ?lenient_hrefs:bool ->
    service:Httpz_dav.Discovery.service -> home_set:Httpz_dav.name ->
    _ Fetch.t -> string -> (t, error) result
  (** [connect ~sw ~service ~home_set fetch url] is a session on the origin of
      [url]. A well-known URL of [service] is followed to its context path,
      whose current principal is read, and [home_set] is read from the
      principal. [credentials] are attached to requests on that origin, over
      TLS unless [allow_insecure]. [limits] and [lenient_hrefs] are those of
      {!v}. *)

  val principal : t -> string
  val home_sets : t -> string list
  val client : t -> dav
  val switch : t -> Eio.Switch.t
  val resolve : t -> string -> string
  (** [resolve t href] is [href], an absolute path or URL, resolved against
      the principal. Any other reference is returned as it is. *)

  val propfind : t -> ?depth:Httpz_dav.depth -> string -> Httpz_dav.propfind ->
    (Httpz_dav.multistatus, error) result
  val report : t -> ?depth:Httpz_dav.depth -> string -> Httpz_dav.element ->
    (Httpz_dav.multistatus, error) result
  val report_body : t -> ?depth:Httpz_dav.depth -> string -> Httpz_dav.element ->
    (string, error) result
  (** A REPORT whose answer is a body rather than a multistatus, such as
      CalDAV's free-busy-query. *)

  val mkcol : t -> ?props:Httpz_dav.element list -> string -> (unit, error) result
  val mkcalendar : t -> ?props:Httpz_dav.element list -> string -> (unit, error) result
  val proppatch : t -> string -> Httpz_dav.update list -> (unit, error) result
  (** A failed instruction is a [Dav] error with its status and the property
      names, RFC 4918 Section 9.2.1. *)

  val delete : t -> ?etag:string -> string -> (unit, error) result
  (** With [etag] the delete is conditional on the entity tag. *)

  type member = { href : string; etag : string option; content_type : string option }
  val members : t -> string -> (member list, error) result
  (** The non-collection members of a collection, with absolute hrefs. *)

  val get : t -> ?accept:string -> string -> (string * string option, error) result
  val put : t -> ?etag:string -> ?create:bool -> content_type:string -> string -> string ->
    (string option, error) result
  (** [put t ~content_type url body] is the entity tag the server gave, if any.
      With [etag] the write is conditional on the entity tag and with [create]
      on the resource being absent. *)

  val download : t -> ?headers:Fetch.Header.headers -> string -> (Fetch.response, error) result
  (** A GET whose response body streams until closed with [Fetch.close] or
      until the switch of [t] ends. A 200, 206 or 304 is returned. Rejected
      responses close immediately, including when reading their body fails.
      DAV [If] and [Lock-Token] headers are redacted in request diagnostics. *)

  val sync : t -> ?token:string -> ?limit:int -> string -> (Httpz_dav.Sync.t, error) result
  val sync_token : t -> string -> (string option, error) result
  val member_name : string option -> string -> string
  (** [member_name uid ext] is a member name for a new resource, [uid] and
      [ext] when [uid] is safe as a path segment and a random name otherwise. *)
end

(** {1 Typed objects}

    A collection whose members are documents of one media type, such as a
    CardDAV address book or a CalDAV calendar, is read and written here as
    values of the caller's own type. A {!Objects.codec} says how a member is
    carried in a body, and the operations below keep the entity tags a
    conditional write needs. What a collection holds, and the reports that
    search it, belong to the protocol above this one. *)
module Objects : sig
  type 'a codec = {
    content_type : string;
        (** The media type written, with any charset, such as
            ["text/vcard; charset=utf-8"]. *)
    decode : string -> ('a, string) result;  (** Reads a body as ['a]. *)
    encode : 'a -> (string, string) result;  (** Writes ['a] as a body. *)
  }
  (** The type for the representation of a member. A failure on either side is
      a {!Session.Data} error. *)

  type 'a entry = { href : string; etag : string option; value : 'a }
  (** The type for a member read from the server, with its entity tag. *)

  type 'a page = {
    entries : 'a entry list;  (** The members returned, in order. *)
    truncated : bool;
        (** [true] if the collection itself answered [507], so the server
            returned fewer members than matched, RFC 6578 Section 3.6. *)
  }
  (** The type for what a report returns. *)

  type 'a change =
    | Changed of 'a entry  (** The member as it now stands. *)
    | Removed of string  (** The href of a member that is gone. *)

  type 'a sync = {
    token : string option;  (** The token to present next time. *)
    changes : 'a change list;
    truncated : bool;  (** [true] if the report is to be repeated with [token]. *)
  }
  (** The type for what a synchronisation reports. *)

  val get : Session.t -> 'a codec -> ?accept:string -> string -> ('a entry, Session.error) result
  (** [get t codec url] is the member at [url]. [accept] is sent as the Accept
      header and defaults to the [content_type] of [codec] without its
      parameters. *)

  val put : Session.t -> 'a codec -> ?etag:string -> ?create:bool -> string -> 'a ->
    (string option, Session.error) result
  (** [put t codec url v] stores [v] at [url] and is the entity tag the server
      gave. [etag] makes the write conditional on the member being unchanged
      and [create] on it being absent. *)

  val add : Session.t -> 'a codec -> ?name:string -> uid:('a -> string option) -> ext:string ->
    string -> 'a -> ('a entry, Session.error) result
  (** [add t codec ~uid ~ext collection v] stores [v] as a new member of
      [collection] and is [v] with the href it was stored at and its entity
      tag. [name] is the member name and defaults to {!Session.member_name} of
      [uid v] and [ext]. A [name] that is not one path segment is a
      {!Session.Data} error. *)

  val page_of_multistatus : 'a codec -> data:(Httpz_dav.response -> string option) ->
    base:string -> Httpz_dav.multistatus -> ('a page, Session.error) result
  (** [page_of_multistatus codec ~data ~base m] are the members [m] carries, [m]
      being the answer to a report on [base]. [data] is the body a response
      holds, which each protocol names its own element for. A response for
      [base] itself, one with a failure status, and one carrying no body are
      not entries. Hrefs are resolved against [base]. *)

  val sync : Session.t -> multiget:(string -> string list -> ('a entry list, Session.error) result) ->
    ?token:string -> ?limit:int -> string -> ('a sync, Session.error) result
  (** [sync t ~multiget ~token url] are the changes to the collection [url]
      since [token], or every member without one, RFC 6578. The members the
      report names changed are fetched with [multiget], which takes [url] and
      their hrefs. A member the report names but [multiget] does not return
      went away between the two requests and is reported [Removed]. A member
      collection is not reported. *)
end
