(** This module provides typed HTTP header values.

    A ['a t] pairs a header field name with an encoder and decoder for
    its value, so requests are built and responses read as typed values
    rather than strings. A request takes a {!type-headers}, which is a
    heterogeneous list of [codec, value] cells. {!raw} is the escape
    hatch for a freeform header, and {!Fetch.header} parses a typed
    value out of a response.

    {[
      let resp =
        Fetch.get ~sw t "https://files.example/big.iso"
          ~headers:Fetch.Header.[
            range, bytes [ `Range (0L, Some 1023L) ];
            raw "X-Trace-Id" "abc123";
          ]
      in
      match Fetch.header Fetch.Header.content_range resp with
      | Some { range = Some (first, last); _ } -> ...
      | _ -> ...
    ]}

    Decoding is total. A malformed value decodes to [None] rather than
    raising. A header whose grammar is a comma-separated list may be
    split across several field lines, and {!get} joins those per
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.3}RFC 9110
    §5.3} before decoding. *)

(** {1 Codecs} *)

type 'a t
(** ['a t] is a header field paired with an encoder and decoder for values of
    type ['a]. *)

val v :
  ?list_valued:bool ->
  string ->
  encode:('a -> string) ->
  decode:(string -> 'a option) ->
  'a t
(** [v name ~encode ~decode] is a typed header, so that an
    application's own headers get the same treatment as the built-in
    ones. [list_valued] declares a comma-separated-list grammar, which
    joins multiple field lines before decoding (default [false]). *)

val name : _ t -> string
(** [name h] is the header field name. *)

val encode : 'a t -> 'a -> string
(** [encode h x] is [x] serialized as [h]'s field value. *)

val decode : 'a t -> string -> 'a option
(** [decode h s] is the value parsed from [s], or [None] if [s] is
    malformed. *)

val get : 'a t -> Http.Header.t -> 'a option
(** [get h hs] is [h]'s value decoded from the wire-form block [hs].
    {!Fetch.header} is the same for a response. A repeated single-valued
    field is rejected as [None], except HSTS which uses the first occurrence
    as RFC 6797 requires. A list-valued field joins its occurrences in arrival
    order. This is a breaking tightening of singleton decoding. *)

val pair : 'a t -> 'a -> string * string
(** [pair h x] is [(name h, encode h x)], for building an
    [Http.Header.t]. *)

val text : string -> string t
(** [text name] is the header [name] with its value left as an
    uninterpreted string. *)

(** {1 The request-header list} *)

type headers =
  | [] : headers
  | ( :: ) : ('a t * 'a) * headers -> headers
      (** [(header, value) :: rest] is [rest] prefixed by the binding of
          [header] to [value]. *)
(** [headers] is the heterogeneous list accepted by request functions. Rebound
    [[]] and [(::)] give it list-literal syntax under a local open, and each
    value is encoded by its paired codec.

    {[
      Header.[ user_agent, "app/1.0";
               accept, [ pref "application/json" ];
               raw "X-Custom" "v" ]
    ]}

    An ordinary list inside a cell, such as [accept]'s, is still an
    ordinary list. *)

val raw : string -> string -> string t * string
(** [raw name value] is a cell for a header with no codec of its own.
    It is [(text name, value)]. *)

val append : headers -> headers -> headers
(** [append a b] is [a] followed by [b]. The standard [(@)] operator does not
    apply to {!type-headers}. *)

val of_http : Http.Header.t -> headers
(** [of_http hs] is the wire-form block [hs] converted to {!type-headers},
    for instance to forward headers received elsewhere. *)

val to_http : headers -> Http.Header.t
(** [to_http hs] is [hs] encoded in wire form, preserving order and
    duplicates. This is what the request functions do with
    [?headers]. *)

val to_list : headers -> (string * string) list
(** [to_list hs] is the encoded name and value pairs of [hs], in
    order. *)

(** {1 Representation metadata}

    These fields describe the media type, size, coding, and language of a
    representation as specified by
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-8}RFC 9110 §8}. *)

type media_type = {
  media : string;
      (** [media] is the [type/subtype]. Decoding lowercases it; encoding
          preserves a caller-constructed value. *)
  params : (string * string) list;
      (** [params] is the list of parameter bindings, such as
          [("charset", "utf-8")]. Decoding lowercases parameter names;
          encoding preserves caller-constructed bindings. *)
}
(** [media_type] is a media type with its parameters. *)

val media : ?params:(string * string) list -> string -> media_type
(** [media ty] is the media type [ty], as in
    [media ~params:[ "charset", "utf-8" ] "text/plain"]. *)

val content_type : media_type t
(** [content_type] is the [Content-Type] header codec. *)

val content_length : int64 t
(** [content_length] is the [Content-Length] header codec. It is for reading a response. A
    backend frames the request body itself, so setting it on a request
    raises [Invalid_request] (see {!Fetch.fetch}). *)

val content_encoding : string list t
(** [content_encoding] is the [Content-Encoding] header codec. Its value is
    the lowercased list of codings applied to the body, such as [["gzip"]]. *)

val content_language : string list t
(** [content_language] is the [Content-Language] header codec. Its value is a
    list of language tags in the case they were sent in. A member that is not
    of the RFC 5646 shape [1*8ALPHA *("-" 1*8alphanum)] rejects the whole
    field. *)

(** {1 Content negotiation}

    These request fields rank the media types, content codings, and languages
    the client can accept, following
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-12}RFC 9110 §12}. *)

type pref = {
  value : string;
      (** [value] is the offered value. Parameters other than [q] stay attached
          verbatim, so [text/html;level=1] survives a round trip. *)
  q : float option;  (** [q] is the optional quality weight from 0 to 1. *)
}
(** [pref] is one entry in a weighted preference list. *)

val pref : ?q:float -> string -> pref
(** [pref v] is a preference for [v], optionally weighted, as in
    [pref ~q:0.5 "text/*"]. *)

val accept : pref list t
(** [accept] is the [Accept] request-header codec, as in
    [accept, [ pref "application/json"; pref ~q:0.5 "text/*" ]]. *)

val accept_encoding : pref list t
(** [accept_encoding] is the [Accept-Encoding] request-header codec. Setting it turns off a
    backend's transparent decompression. *)

val accept_language : pref list t
(** [accept_language] is the [Accept-Language] request-header codec. *)

(** {1 Conditional requests}

    Conditional fields let a client avoid transferring an unchanged
    representation or overwriting one that has changed, following
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-13}RFC 9110 §13}. Entity
    tags are defined in
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3}§8.8.3}.
    HTTP-dates accept all three RFC 9110 forms and decoding canonicalizes them
    to IMF-fixdate. *)

type etag = {
  weak : bool;  (** [weak] is [true] for a weak validator. *)
  tag : string;  (** [tag] is the opaque entity tag without quotes. *)
}
(** [etag] is an entity tag. *)

val etag : etag t
(** [etag] is the [ETag] response-header codec.
    @raise Stdlib.Invalid_argument on encoding an opaque value outside RFC 9110
    [etagc], including quotes, spaces and control bytes. *)

type etags = [ `Any | `Etags of etag list ]
(** [etags] is either [*], meaning any representation, or a list of entity
    tags. *)

val if_match : etags t
(** [if_match] is the [If-Match] request-header codec. The server proceeds only if the
    representation still matches, which guards against a lost update. *)

val if_none_match : etags t
(** [if_none_match] is the [If-None-Match] request-header codec. The
    precondition fails when the selected representation matches; for GET and
    HEAD, the server answers 304. This is how a cache revalidates. *)

val last_modified : string t
(** [last_modified] is the [Last-Modified] response-header codec. *)

val if_modified_since : string t
(** [if_modified_since] is the [If-Modified-Since] request-header codec. Its
    value is an HTTP-date. *)

val if_unmodified_since : string t
(** [if_unmodified_since] is the [If-Unmodified-Since] request-header codec.
    Its value is an HTTP-date. *)

val date : string t
(** [date] is the [Date] header codec. Its value is an HTTP-date. *)

(** {1 Range requests}

    Range requests retrieve selected portions of a representation and report
    which portion a response carries, as specified by
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-14}RFC 9110 §14}. *)

type range_spec =
  [ `Range of int64 * int64 option
    (** [`Range (first, last)] is the inclusive range from [first] to [last],
        or through the end when [last] is [None]. *)
  | `Suffix of int64  (** [`Suffix n] is the final [n] bytes. *) ]
(** [range_spec] is one requested range. *)

type range = {
  unit : string;  (** [unit] is the range unit, normally ["bytes"]. *)
  ranges : range_spec list;  (** [ranges] is the requested range set. *)
}
(** [range] is a set of representation ranges counted in [unit]s, normally
    bytes. *)

val bytes : range_spec list -> range
(** [bytes rs] is the byte ranges [rs], as in
    [bytes [ `Range (0L, Some 1023L) ]]. *)

val range : range t
(** [range] is the [Range] request-header codec, which asks for part of a
    representation. *)

type if_range = [ `Etag of etag | `Date of string ]
(** [if_range] is the validator on which a ranged request is conditional. *)

val if_range : if_range t
(** [if_range] is the [If-Range] request-header codec
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5}RFC 9110
    §13.1.5}). *)

type content_range = {
  unit : string;  (** [unit] is the range unit, normally ["bytes"]. *)
  range : (int64 * int64) option;
      (** [range] is the inclusive satisfied range, or [None] for [*] in a
          416 response. *)
  complete_length : int64 option;
      (** [complete_length] is the total size, or [None] for [*]. *)
}
(** [content_range] is the description of a representation portion carried by a
    206 response, or the complete length reported by a 416 response. *)

val complete_range :
  first:int64 -> last:int64 -> complete_length:int64 -> content_range
(** [complete_range ~first ~last ~complete_length] is a satisfied
    byte-unit {!type-content_range}.
    @raise Stdlib.Invalid_argument unless [0 <= first <= last < complete_length]. *)

val content_range : content_range t
(** [content_range] is the [Content-Range] header codec for 206 and 416
    responses. *)

type accept_ranges = [ `Bytes | `None | `Other of string ]
(** [accept_ranges] is the description of whether a resource supports ranged
    requests. *)

val accept_ranges : accept_ranges t
(** [accept_ranges] is the [Accept-Ranges] response-header codec. *)

(** {1 Caching}

    Cache directives control storage, freshness, validation, and stale reuse
    according to {{:https://www.rfc-editor.org/rfc/rfc9111}RFC 9111}. *)

type cache_control = {
  max_age : int option;  (** [max_age] is the maximum freshness lifetime. *)
  s_maxage : int option;
      (** [s_maxage] is the freshness override for shared caches. *)
  no_cache : bool;
      (** [no_cache] is [true] when validation is required before reuse. *)
  no_store : bool;  (** [no_store] prohibits storing the request or response. *)
  no_transform : bool;
      (** [no_transform] is [true] when transforming the representation is
          prohibited. *)
  only_if_cached : bool;
      (** [only_if_cached] is [true] when a cache must not contact the origin. *)
  must_revalidate : bool;
      (** [must_revalidate] is [true] when stale reuse requires validation. *)
  proxy_revalidate : bool;
      (** [proxy_revalidate] is [true] when [must_revalidate] applies to shared
          caches. *)
  public : bool;  (** [public] permits storage by a shared cache. *)
  private_ : bool;  (** [private_] restricts storage to a private cache. *)
  immutable : bool;
      (** [immutable] is [true] when the response will not change while fresh. *)
  min_fresh : int option;
      (** [min_fresh] is the required remaining freshness in seconds. *)
  max_stale : int option;
      (** [max_stale] is the permitted staleness in seconds. *)
  stale_while_revalidate : int option;
      (** [stale_while_revalidate] is the period in seconds during which stale
          reuse is permitted while revalidating asynchronously. *)
  extension : (string * string option) list;
      (** [extension] is the list of directives with no field of their own. A bare
          [max-stale] appears here, as do the qualified
          [no-cache="Set-Cookie"] and [private="field"] forms, whose
          flags above are also set. *)
}
(** [cache_control] is a collection of request or response cache directives. *)

val cache_directives :
  ?max_age:int -> ?s_maxage:int -> ?no_cache:bool -> ?no_store:bool ->
  ?no_transform:bool -> ?only_if_cached:bool -> ?must_revalidate:bool ->
  ?proxy_revalidate:bool -> ?public:bool -> ?private_:bool ->
  ?immutable:bool -> ?min_fresh:int -> ?max_stale:int ->
  ?stale_while_revalidate:int -> ?extension:(string * string option) list ->
  unit -> cache_control
(** [cache_directives ()] is the directives given and no others, as in
    [cache_control, cache_directives ~no_cache:true ()]. *)

val cache_control : cache_control t
(** [cache_control] is the [Cache-Control] header codec for request and
    response directives. *)

val age : int64 t
(** [age] is the [Age] response-header codec. Its value is in seconds and is
    saturated at [2147483648L] as required for excessive delta-seconds. *)

val expires : string t
(** [expires] is the [Expires] response-header codec. *)

type vary = [ `Any | `Fields of string list ]
(** [vary] is the set of request headers on which a response's selection
    depended. *)

val vary : vary t
(** [vary] is the [Vary] response-header codec. Field names are lowercased.
    A [*] anywhere in the list decodes to [`Any], which never matches a
    stored response ({{:https://www.rfc-editor.org/rfc/rfc9111#section-4.1}
    RFC 9111 §4.1}). *)

(** {1 Cache-Status}

    A [Cache-Status] response explains whether each cache used a stored
    response or forwarded the request, following
    {{:https://www.rfc-editor.org/rfc/rfc9211}RFC 9211}. *)

type forward =
  [ `Uri_miss | `Vary_miss | `Miss | `Request | `Stale | `Partial | `Bypass
  | `Other of string ]
(** [forward] is the reason a cache forwarded the request. *)

type cache_status = {
  cache : string;  (** [cache] is the cache's identifier. *)
  hit : bool;  (** [hit] is [true] when the cache used a stored response. *)
  fwd : forward option;  (** [fwd] is why the request was forwarded. *)
  fwd_status : int option;
      (** [fwd_status] is the status received from the next hop. *)
  stored : bool;  (** [stored] is [true] when the response was stored. *)
  collapsed : bool;
      (** [collapsed] is [true] when this request was combined with another. *)
  ttl : int option;
      (** [ttl] is the response's remaining freshness lifetime in seconds. *)
  key : string option;  (** [key] identifies the cache lookup key. *)
  detail : string option;  (** [detail] is implementation-specific detail. *)
}
(** [cache_status] is one cache's account of how it handled the request. *)

val cache_status : cache_status list t
(** [cache_status] is the [Cache-Status] header codec. It holds one entry per
    cache the response traversed, closest to the origin server first. It uses
    RFC 8941 item/parameter syntax: identifiers are strings or tokens, [fwd] a
    token, flags booleans, [ttl] and [fwd-status] integers, [key] a string and
    [detail] a string or token. Unknown parameters are syntax-checked then
    ignored; repeated parameters use their last value. Encoding normalizes
    string/token choices without preserving the original wire spelling.
    @raise Stdlib.Invalid_argument on encoding an invalid token, string, integer or
    status code, or an empty list. *)

val cache_hit : cache_status list -> bool
(** [cache_hit entries] is [true] if any cache in [entries] reported a
    hit. *)

(** {1 Authentication}

    These codecs represent credentials, server challenges, and mutual
    authentication metadata defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-11}RFC 9110 §11}. *)

type credentials =
  [ `Basic of string * string
    (** [`Basic (user, password)] is a user and password, Base64-encoded when
        serialized. *)
  | `Bearer of string  (** [`Bearer token] is a bearer token. *)
  | `Other of string * string
    (** [`Other (scheme, parameters)] is an authentication scheme and its
        parameters preserved verbatim. *) ]
(** [credentials] is a parsed authentication credential. *)

val authorization : credentials t
(** [authorization] is the [Authorization] header codec, as in
    [authorization, `Basic ("aladdin", "opensesame")]. The codec can read or
    write a per-request field. {!Fetch.with_headers} refuses credential fields;
    attach reusable credentials with {!Fetch.with_credentials} so that they
    are scoped, redacted, and removed on cross-origin redirects. A [Basic]
    credential whose blob is not canonical Base64, or whose decoding has no
    [:], is rejected rather than read as [`Other]. A [Bearer] credential whose
    token is not a
    {{:https://www.rfc-editor.org/rfc/rfc6750#section-2.1}RFC 6750 §2.1
     b64token} is rejected the same way.
    Encoding a [`Basic] credential whose user-id contains a [:] raises
    [Invalid_argument]: the colon separates the pair
    ({{:https://www.rfc-editor.org/rfc/rfc7617#section-2}RFC 7617 §2}), so such
    a pair would authenticate as a different user and password than it
    names. Basic encoding also rejects bytes outside printable ASCII in either
    part. Empty parts, spaces and password colons are permitted.
    Encoding a [`Bearer] credential whose token is not a b64token
    raises [Invalid_argument] for the same reason: a space or other forbidden
    byte would authenticate as a different, truncated token than it names. *)

val proxy_authorization : credentials t
(** [proxy_authorization] is the [Proxy-Authorization] header codec. *)

type challenge = {
  scheme : string;  (** [scheme] is the authentication scheme, such as ["Basic"]. *)
  params : (string * string) list;
      (** [params] is a list with lowercased keys such as [("realm", …)]. The empty
          key holds a token68 value. *)
}
(** [challenge] is an authentication challenge from a server. *)

val www_authenticate : challenge list t
(** [www_authenticate] is the [WWW-Authenticate] header codec for a 401
    response. A scheme carrying a token68 value rather than parameters
    (["Negotiate SGVsbG8="]) keeps it whole as its single unnamed parameter.
    Anything that is neither a parameter nor a token68, and any parameter
    preceding the first scheme, rejects the whole field: a challenge list
    with a member dropped would read as weaker than the one sent. *)

type authentication_info = {
  nextnonce : string option;  (** [nextnonce] is the nonce for a later request. *)
  qop : string option;  (** [qop] is the applied quality of protection. *)
  rspauth : string option;  (** [rspauth] is the response authentication value. *)
  cnonce : string option;  (** [cnonce] is the client's nonce. *)
  nc : string option;  (** [nc] is the nonce count. *)
}
(** [authentication_info] is a server's reply for mutual authentication. *)

val authentication_info : authentication_info t
(** [authentication_info] is the [Authentication-Info] header codec
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-11.6.3}RFC 9110
    §11.6.3}). *)

(** {1 Integrity digests}

    These fields carry digests for detecting changed or corrupted content as
    specified by {{:https://www.rfc-editor.org/rfc/rfc9530}RFC 9530}.
    Verification needs a hash function, which is left to the caller. *)

type digest = {
  algorithm : [ `Sha256 | `Sha512 | `Other of string ];
      (** [algorithm] is the digest algorithm. *)
  digest : string;  (** [digest] is the Base64 value as received. *)
}
(** [digest] is an integrity digest of a body. *)

val content_digest : digest list t
(** [content_digest] is the [Content-Digest] header codec. It holds digests of the content as it
    was transferred, after any content coding. *)

val repr_digest : digest list t
(** [repr_digest] is the [Repr-Digest] header codec. It holds digests of the representation
    itself, before any content coding. *)

val strongest_digest : digest list -> digest option
(** [strongest_digest ds] is the strongest digest in [ds], preferring
    SHA-512, then SHA-256, then the first entry. *)

(** {1 Strict-Transport-Security}

    HSTS tells a client to use HTTPS for a host and, optionally, its
    subdomains, as specified by
    {{:https://www.rfc-editor.org/rfc/rfc6797}RFC 6797}. *)

type hsts = {
  max_age : int64;  (** [max_age] is the number of seconds to remember the policy. *)
  include_subdomains : bool;
      (** [include_subdomains] is [true] when the policy applies to subdomains. *)
  preload : bool;  (** [preload] records the non-standard preload directive. *)
}
(** [hsts] is a policy requiring HTTPS for the host. *)

val strict_transport_security : hsts t
(** [strict_transport_security] is the [Strict-Transport-Security] header
    codec. *)

(** {1 Link}

    Link fields connect a response to related resources such as the next page
    of a collection, following
    {{:https://www.rfc-editor.org/rfc/rfc8288}RFC 8288}. *)

type link = {
  target : string;  (** [target] is the target URI reference, possibly relative. *)
  rel : string option;  (** [rel] is the link relation. *)
  media_type : string option;
      (** [media_type] is a hint for the target's media type. *)
  title : string option;
      (** [title] is a human-readable label. Decoding accepts RFC 8187 UTF-8
          [title*] and prefers it over a plain [title] fallback. *)
  hreflang : string option;
      (** [hreflang] is a language hint for the target. *)
  params : (string * string) list;
      (** [params] is the list of unrecognized link parameters. *)
}
(** [link] is a typed link to another resource. *)

val link :
  ?rel:string ->
  ?media_type:string ->
  ?title:string ->
  ?hreflang:string ->
  ?params:(string * string) list ->
  string ->
  link
(** [link target] is a link to [target], as in
    [link ~rel:"next" "/page/2"]. *)

val links : link list t
(** [links] is the [Link] header codec, used for API pagination ([rel="next"]), resource
    discovery and relationship navigation. A relative target resolves
    against the response's {!Fetch.val-url}. A comma inside the [<...>]
    target separates nothing. *)

val link_rel : string -> link list -> link option
(** [link_rel r ls] is the first link in [ls] whose relation is [r], as
    in [link_rel "next" ls]. *)

(** {1 Other headers} *)

val allow : Http.Method.t list t
(** [allow] is the [Allow] header codec
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-10.2.1}RFC 9110
    §10.2.1}). *)

type retry_after = [ `Seconds of int | `Date of string ]
(** [retry_after] is a delay or HTTP-date after which a request may be
    retried. *)

val retry_after : retry_after t
(** [retry_after] is the [Retry-After] header codec
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-10.2.3}RFC 9110
    §10.2.3}). {!Fetch.with_retry} honours the [`Seconds] form always, and the
    [`Date] form when it was given a wall clock to read the date against. *)

val location : string t
(** [location] is the [Location] header codec for 3xx and 201 responses. A 3xx is visible to
    the caller with [~redirects:0]. *)

val user_agent : string t
(** [user_agent] is the [User-Agent] request-header codec. *)
