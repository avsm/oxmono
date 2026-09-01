(** Absolute HTTP and HTTPS URLs.

    {!Fetch.Middleware.Url} is this module, which is what a policy
    filter inspects and a backend serializes. A value is validated on
    construction. The scheme must be HTTP or HTTPS, a host is required,
    userinfo of the form
    [user:password@] is rejected, and the URL is canonicalized by lowercasing
    the scheme and host, eliding a default port and removing dot-segments
    according to
    {{:https://www.rfc-editor.org/rfc/rfc3986}RFC 3986}. Internationalized host
    names are NFC-normalized and converted to the A-label form defined by
    {{:https://www.rfc-editor.org/rfc/rfc5890#section-2.3.2.1}RFC 5890
    §2.3.2.1}; a supplied A-label is retained. A backend serializes
    {!to_string} and never re-parses a user string. The bundled converter does
    not yet implement the RFC 5892 code-point tables, RFC 5893 bidirectional
    rules, or contextual joiner rules; applications accepting untrusted names
    must apply those checks separately before DNS use.

    A host is stored in one canonical spelling: the lowercase A-label, without
    the root dot of an absolute DNS name, and with an IPv4 address written in
    any of the spellings
    {{:https://man7.org/linux/man-pages/man3/inet_aton.3.html}inet_aton(3)}
    accepts folded to its dotted quad, so that ["http://127.1"],
    ["http://2130706433"] and ["http://0x7f000001"] all name
    ["http://127.0.0.1"]. That spelling is what {!Fetch.restrict}, credential
    scopes, {!Fetch.with_limits} buckets and the cookie jar compare, so a rule
    naming one form covers the rest. IPv6 literals use compressed lowercase canonical notation. IPv4-mapped
    addresses, including hexadecimal spellings, and any embedded dotted-quad
    form are rejected. Zone identifiers are unsupported.

    A name allowed by policy may still resolve to any address at all. That is
    a question about the address the socket will use, which only a check in a
    backend's [~connect] can answer. *)

type t
(** [t] is a validated, canonical HTTP or HTTPS URL. *)

type scheme = [ `Http | `Https ]
(** [scheme] is one of the two URL schemes a request may use. *)

val of_string : string -> (t, string) result
(** [of_string s] is [Ok url] for a valid absolute HTTP or HTTPS URL, or
    [Error reason] otherwise. *)

val of_uri : Httpz.Uriz.t -> (t, string) result
(** [of_uri u] is [u] validated and normalized as an absolute HTTP or HTTPS
    URL, or an error explaining why [u] is not acceptable. *)

val to_uri : t -> Httpz.Uriz.t
(** [to_uri t] is [t]'s canonical form as an {!Httpz.Uriz.t}, for a transport
    API that takes one. Its fragment is omitted because fragments are not part
    of an HTTP request target. A default port is elided, so a caller needing
    the port supplies 80 or 443 itself. It serializes as {!to_string} does. *)

val scheme : t -> scheme
(** [scheme t] is [t]'s scheme. *)

val host : t -> string
(** [host t] is [t]'s host, lowercase ASCII in the canonical spelling
    described above: an A-label with no trailing dot, or an IPv4 address as a
    dotted quad. An IPv6 literal is held without its brackets, which {!origin}
    re-adds. *)

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
(** [path_segments t] is the normalized wire path decoded per segment.
    The leading absolute-path marker is omitted; empty interior and trailing
    segments are preserved. The root path alone is [[]]. *)

val has_query : t -> bool
(** [has_query t] is [true] if [t] binds any query parameter. *)

val has_fragment : t -> bool
(** [has_fragment t] is [true] if its client-side URI contains a fragment,
    including an explicitly empty one. *)

val under : prefix:t -> t -> bool
(** [under ~prefix t] is [true] if [t] has [prefix]'s origin and
    [prefix]'s normalized wire path is a prefix of [t]'s, aligned on
    segments, so that ["https://h/api"] covers ["https://h/api/x"] but
    not ["https://h/apix"]. It is [false] if either path has a percent-decoded
    slash or backslash, since an origin might interpret that as another
    separator, except that an origin-wide prefix such as ["https://h/"] covers
    every path on that origin. Empty segments are significant: [/api/admin]
    does not authorize [/api//admin]. A trailing slash includes its subtree,
    so [/api/] covers [/api/x] but [/api//] does not. This deliberately
    tightens the earlier behavior that collapsed repeated slashes. *)

val set_query_params : t -> (string * string) list -> t
(** [set_query_params t ps] is [t] with each parameter of [ps] bound in
    its query, replacing any existing binding of the same name and
    keeping the rest of the query. Existing keys use form/query decoding, so
    ['+'] and ["%20"] are both spellings of a space. *)

val resolve : base:t -> string -> (t, string) result
(** [resolve ~base reference] is [reference], relative or absolute, resolved
    against [base] as a [Location] value and then revalidated. When [reference]
    has no fragment, [base]'s fragment is inherited as required for HTTP
    redirection by RFC 9110 section 10.2.2. An absolute reference with a
    scheme other than HTTP or HTTPS is rejected after resolution. *)

val to_string : t -> string
(** [to_string t] is the fragment-free canonical serialization a backend sends
    to the origin. *)

val effective_string : t -> string
(** [effective_string t] is the canonical serialization including its
    client-side fragment. *)

val pp : t Fmt.t
(** [pp ppf t] is the formatting of [t] produced by {!to_string}. *)

val pp_redacted : names:string list -> t Fmt.t
(** [pp_redacted ~names ppf t] formats [t] as {!pp} does, replacing each
    value of a query parameter whose decoded key occurs in [names] with
    [<redacted>]. The result is diagnostic text, not a URI serialization. *)
