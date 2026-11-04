(** The spindle's web pages, for its users only.

    Visitors log in with their atproto account: the spindle is a confidential
    OAuth client, verifies who they are once, revokes the tokens and gives them
    a session of its own. Only the spindle's users get one: its owner and
    admins, allow-listed accounts, and the owners and collaborators of the
    repositories it serves. Admins see every repository. Others see the
    repositories they own or collaborate on, their pipelines and their logs.

    Sessions and logins in progress are kept in the spindle's store, so they
    survive a restart. Every page is sent with a strict Content Security
    Policy and [Cache-Control: no-store], every form that changes anything
    carries a CSRF token and is checked against the site's origin, and the
    session cookie is [__Host-], [Secure], [HttpOnly] and [SameSite=Lax]. *)

type t

val v :
  sw:Eio.Switch.t ->
  engine:Engine.t ->
  store:Store.t ->
  catalog:Catalog.t ->
  directory:Eio.Fs.dir_ty Eio.Path.t ->
  hostname:string ->
  ?jobs:Duraml.plain ->
  ?plc:string ->
  ?origin:string ->
  ?http:Fetch.plain ->
  Eio_unix.Stdenv.base ->
  t
(** [v ~engine ~store ~catalog ~directory ~hostname env] serves pages at
    [https://hostname], or at [origin] for tests. With [jobs], the spindle's
    Duraml store, admins also see its jobs and their activity. The OAuth client's signing
    key is [directory/web-client.key], created readable only by its owner on
    first use. [http] makes the OAuth requests, by default an HTTPS client
    that refuses private and loopback addresses, since identity resolution
    follows addresses that visitors choose, keeps no cookies, does not
    retry and reads at most 1 MiB of an answer. The signing key is replaced
    when the spindle starts after it has been used for 90 days. At most eight logins talk to other servers at once, each for at
    most 30 seconds, and at most 500 logins may be in progress. A fiber on
    [sw] sweeps away expired sessions and logins every ten minutes. *)

val handle :
  t -> string -> Proffer.Req.t @ local -> Proffer.Resp.respond @ local -> unit
(** [handle t page req respond] answers the page called [page]: [home],
    [repo], [pipeline], [log], [login], [logout], [callback], [metadata] or
    [jwks]. *)

val routes :
  unit ->
  (string -> Proffer.Req.t @ local -> Proffer.Resp.respond @ local -> unit)
  Proffer.Route.t
  list
(** [routes ()] is the routes of the pages and of Sitekit's assets. A route
    calls the site's environment with the page's name prefixed by [web:], such
    as [web:home], which a spindle passes on to {!handle}. *)

(**/**)

val login_as : t -> string -> string * string
(** [login_as t did] is a [Cookie] field value holding a new session for
    [did], and its CSRF token, for tests. *)
