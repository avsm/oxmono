(** A concurrent client-side cookie jar.

    A client passes every [Set-Cookie] response value to {!set} and calls
    {!header_for} before a request. The jar implements storage and retrieval
    from
    {{:https://www.rfc-editor.org/rfc/rfc6265.html#section-5.3}RFC 6265,
     Sections 5.3 and 5.4}. Hosts must be canonical lower-case ASCII names.

    A jar accepts at most 4096 bytes of name and value together, the minimum
    {{:https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis}RFC
     6265bis} section 5.7 asks a user agent to support, and holds this line
    rather than admitting an unbounded name and value; Path and Domain are
    additional budget on top of that, up to 8192 bytes in all, so a cookie at
    the full 4096-byte minimum with an ordinary path and domain is not
    refused for a shortfall this module introduced. It also holds at most 50
    cookies per domain and 3000 cookies overall, evicting least-recently-used
    cookies when a limit is reached. It rejects every [Secure] cookie received
    over plaintext HTTP, as well as plaintext cookies that would shadow a
    stored [Secure] cookie. *)

type t
(** A [t] is a cookie store whose operations may be called concurrently from Eio
    fibers. *)

val in_memory : clock:_ Eio.Time.clock -> unit -> t
(** [in_memory ~clock ()] is an empty jar without persistent storage. *)

val of_file :
  clock:_ Eio.Time.clock ->
  ?save:[ `On_change | `Manual ] ->
  ?missing:[ `Empty | `Error ] ->
  _ Eio.Path.t ->
  t
(** [of_file ~clock path] is a jar backed by [path] in the Netscape
    [cookies.txt] format used by curl, including its [#HttpOnly_] marker. An
    existing file is loaded, skipping any line whose fields do not form a valid
    cookie, violates public-suffix/domain scope, or exceeds the normal jar
    limits. Files over 32 MiB are treated as empty rather than read without a
    bound; a missing file is created on the first save. Saves use a unique,
    exclusively-created private sibling and replace the target atomically after
    the write closes. This promises atomic visibility, not power-loss
    durability, and requires write authority over the containing directory.
    [save] defaults to [`On_change], which saves after each mutation;
    [`Manual] saves only when {!flush} is called. A missing file is treated as
    empty by default; [~missing:`Error] raises that read failure instead. Other
    read failures are always raised. Session cookies use expiry zero and
    survive a file round-trip. *)

val flush : t -> unit
(** [flush jar] is [()] after writing the current contents of [jar] to its
    backing file. It has no effect on an in-memory jar. *)

val clear : t -> unit
(** [clear jar] is [()] after removing every cookie from [jar] and applying its
    save policy. *)

val set :
  t ->
  host:string ->
  path:string ->
  https:bool ->
  string ->
  (unit, string) result
(** [set jar ~host ~path ~https value] is [Ok ()] after processing one
    [Set-Cookie] response value received for a request to [host] and [path].
    [https] states whether that request used HTTPS. [Error reason] means the
    value was rejected and [jar] was not changed. A cookie whose name and
    value together exceed 4096 bytes, or whose name, value, path and domain
    together exceed 8192 bytes, is refused. *)

val header_for : t -> host:string -> path:string -> https:bool -> string option
(** [header_for jar ~host ~path ~https] is the [Cookie] request value for [host]
    and [path], or [None] when no cookie applies. [Secure] cookies are selected
    only when [https] is [true]. The operation removes expired cookies and
    updates the last-access time of selected cookies. *)

val cookies : t -> Cookie.t list
(** [cookies jar] is a snapshot of the cookies in [jar]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf jar] is [()] after writing the contents of [jar] on [ppf] for
    inspection. *)
