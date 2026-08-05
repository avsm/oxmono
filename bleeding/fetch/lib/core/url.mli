(** Absolute http(s) URLs, parsed and normalized at the trust boundary.

    {!Fetch.Middleware.Url} is this module, which is what a policy
    filter inspects and a backend serializes. A value is validated on
    construction. The scheme
    must be http or https, a host is required, userinfo of the form
    [user:password@] is rejected, the fragment is dropped, and the URL is
    canonicalized by lowercasing the scheme and host, eliding a default
    port and removing dot-segments. A backend serializes {!to_string}
    and never re-parses a user string. *)

type t
(** A validated, canonical URL. *)

type scheme = [ `Http | `Https ]
(** The scheme of a URL, the only two a request may use. *)

val of_string : string -> (t, string) result
(** [of_string s] parses and validates [s], or gives a reason it is not
    an acceptable URL. *)

val of_uri : Uri.t -> (t, string) result
(** [of_uri u] validates and normalizes the existing {!Uri.t} [u]. *)

val to_uri : t -> Uri.t
(** [to_uri t] is [t]'s canonical form as a {!Uri.t}, for a transport
    API that takes one. A default port is elided, so a caller needing
    the port supplies 80 or 443 itself. It serializes as {!to_string}
    does. *)

val scheme : t -> scheme
(** [scheme t] is [t]'s scheme. *)

val host : t -> string
(** [host t] is [t]'s host, lowercase ASCII. An IPv6 literal is held
    without its brackets, which {!origin} re-adds. *)

val port : t -> int
(** [port t] is [t]'s port, the scheme's default already applied. *)

val default_port : scheme -> int
(** [default_port s] is 80 for [`Http] and 443 for [`Https]. *)

val same_origin : t -> t -> bool
(** [same_origin a b] is [true] if [a] and [b] agree on scheme, host and
    port. *)

val origin : t -> string
(** [origin t] is [t]'s origin as ["https://host:port"], eliding the
    port when it is the scheme's default and bracketing an IPv6 host.
    Distinct origins give distinct strings, so this also serves as a
    per-origin bucket key. *)

val path_and_query : t -> string
(** [path_and_query t] is the percent-encoded request target, always
    beginning with ["/"]. *)

val path_segments : t -> string list
(** [path_segments t] is the path as decoded segments with ["."] and
    [".."] resolved. This is the form policy checks match against. *)

val has_query : t -> bool
(** [has_query t] is [true] if [t] binds any query parameter. *)

val under : prefix:t -> t -> bool
(** [under ~prefix t] is [true] if [t] has [prefix]'s origin and
    [prefix]'s decoded path segments are a prefix of [t]'s, aligned on
    segments, so that ["https://h/api"] covers ["https://h/api/x"] but
    not ["https://h/apix"]. This is what a policy scope matches with. *)

val set_query_params : t -> (string * string) list -> t
(** [set_query_params t ps] is [t] with each parameter of [ps] bound in
    its query, replacing any existing binding of the same name and
    keeping the rest of the query. *)

val resolve : base:t -> string -> (t, string) result
(** [resolve ~base reference] resolves [reference], relative or absolute,
    against [base] as a [Location] header is resolved, and re-validates
    the result. *)

val to_string : t -> string
(** [to_string t] is the canonical serialization of [t]. It round-trips
    through {!of_string}. *)

val pp : t Fmt.t
(** [pp] formats a URL as {!to_string} does. *)
