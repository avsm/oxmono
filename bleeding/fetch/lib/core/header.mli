(** Typed header values.

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
(** A typed header. It pairs a field name with an encoder and a decoder
    for values of type ['a]. *)

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
(** [decode h s] parses the field value [s], or is [None] if [s] is
    malformed. *)

val get : 'a t -> Http.Header.t -> 'a option
(** [get h hs] decodes [h] out of the wire-form block [hs].
    {!Fetch.header} is the same for a response. *)

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
      (** A typed header and a value of its type. *)
(** The form request headers are given in. Rebound [[]] and [(::)] give
    it list-literal syntax under a local open, and each value is encoded
    by the codec it is paired with.

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
(** [append a b] concatenates [a] and [b], since [(@)] does not apply
    to {!type-headers}. *)

val of_http : Http.Header.t -> headers
(** [of_http hs] lifts the wire-form block [hs], for instance to
    forward headers received elsewhere. *)

val to_http : headers -> Http.Header.t
(** [to_http hs] compiles [hs] to the wire form, preserving order and
    duplicates. This is what the request functions do with
    [?headers]. *)

val to_list : headers -> (string * string) list
(** [to_list hs] is the encoded name and value pairs of [hs], in
    order. *)

(** {1 Representation metadata}

    {{:https://www.rfc-editor.org/rfc/rfc9110#section-8}RFC 9110 §8}. *)

type media_type = {
  media : string;  (** Lowercase [type/subtype]. *)
  params : (string * string) list;  (** E.g. [("charset", "utf-8")]. *)
}
(** A media type with its parameters. *)

val media : ?params:(string * string) list -> string -> media_type
(** [media ty] is the media type [ty], as in
    [media ~params:[ "charset", "utf-8" ] "text/plain"]. *)

val content_type : media_type t
(** The [Content-Type] header. *)

val content_length : int64 t
(** The [Content-Length] header. This is for reading a response. A
    backend frames the request body itself, so setting it on a request
    raises [Invalid_request] (see {!Fetch.fetch}). *)

val content_encoding : string list t
(** The [Content-Encoding] header, holding the codings applied to the
    body, lowercased, such as [["gzip"]]. *)

val content_language : string list t
(** The [Content-Language] header, holding language tags. *)

(** {1 Content negotiation}

    {{:https://www.rfc-editor.org/rfc/rfc9110#section-12}RFC 9110
    §12}. *)

type pref = {
  value : string;
      (** The offered value. Parameters other than [q] stay attached
          verbatim, so [text/html;level=1] survives a round trip. *)
  q : float option;  (** Quality weight, 0 to 1. *)
}
(** One entry of a weighted preference list. *)

val pref : ?q:float -> string -> pref
(** [pref v] is a preference for [v], optionally weighted, as in
    [pref ~q:0.5 "text/*"]. *)

val accept : pref list t
(** The [Accept] request header, as in
    [accept, [ pref "application/json"; pref ~q:0.5 "text/*" ]]. *)

val accept_encoding : pref list t
(** The [Accept-Encoding] request header. Setting it turns off a
    backend's transparent decompression. *)

val accept_language : pref list t
(** The [Accept-Language] request header. *)

(** {1 Conditional requests}

    {{:https://www.rfc-editor.org/rfc/rfc9110#section-13}RFC 9110 §13},
    with entity tags per
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3}§8.8.3}.
    HTTP-dates are left as opaque strings. A client echoes the exact
    [Last-Modified] value it received back into
    [If-Modified-Since]
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-13.1.3}§13.1.3}),
    so the common flows never need to interpret one. *)

type etag = {
  weak : bool;
  tag : string;  (** The opaque tag, without quotes. *)
}
(** An entity tag. *)

val etag : etag t
(** The [ETag] response header. *)

type etags = [ `Any | `Etags of etag list ]
(** Either [*], meaning any representation, or specific entity tags. *)

val if_match : etags t
(** The [If-Match] request header. The server proceeds only if the
    representation still matches, which guards against a lost update. *)

val if_none_match : etags t
(** The [If-None-Match] request header. The server proceeds only if the
    representation has changed, and answers 304 otherwise. This is how a
    cache revalidates. *)

val last_modified : string t
(** The [Last-Modified] response header, an HTTP-date kept verbatim for
    echoing back. *)

val if_modified_since : string t
(** The [If-Modified-Since] request header, an HTTP-date. *)

val if_unmodified_since : string t
(** The [If-Unmodified-Since] request header, an HTTP-date. *)

val date : string t
(** The [Date] header, an HTTP-date. *)

(** {1 Range requests}

    {{:https://www.rfc-editor.org/rfc/rfc9110#section-14}RFC 9110
    §14}. *)

type range_spec =
  [ `Range of int64 * int64 option
    (** [first] to [last] inclusive; [None] = to the end. *)
  | `Suffix of int64  (** The final [n] bytes. *) ]
(** One requested range. *)

type range = { unit : string; ranges : range_spec list }
(** Ranges of a representation, counted in [unit]s, normally bytes. *)

val bytes : range_spec list -> range
(** [bytes rs] is the byte ranges [rs], as in
    [bytes [ `Range (0L, Some 1023L) ]]. *)

val range : range t
(** The [Range] request header, which asks for part of a
    representation. *)

type if_range = [ `Etag of etag | `Date of string ]
(** The validator a ranged request is conditional on. *)

val if_range : if_range t
(** The [If-Range] request header
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5}RFC 9110
    §13.1.5}). *)

type content_range = {
  unit : string;
  range : (int64 * int64) option;
      (** Satisfied range (inclusive), or [None] for [*] (416). *)
  complete_length : int64 option;  (** Total size, or [None] for [*]. *)
}
(** Which part of a representation a 206 or 416 response carries. *)

val complete_range :
  first:int64 -> last:int64 -> complete_length:int64 -> content_range
(** [complete_range ~first ~last ~complete_length] is a satisfied
    byte-unit {!type-content_range}. *)

val content_range : content_range t
(** The [Content-Range] header of a 206 response. *)

type accept_ranges = [ `Bytes | `None | `Other of string ]
(** Whether a resource supports ranged requests. *)

val accept_ranges : accept_ranges t
(** The [Accept-Ranges] response header. *)

(** {1 Caching}

    {{:https://www.rfc-editor.org/rfc/rfc9111}RFC 9111}. *)

type cache_control = {
  max_age : int option;
  s_maxage : int option;
  no_cache : bool;
  no_store : bool;
  no_transform : bool;
  only_if_cached : bool;
  must_revalidate : bool;
  proxy_revalidate : bool;
  public : bool;
  private_ : bool;
  immutable : bool;
  min_fresh : int option;
  max_stale : int option;
  stale_while_revalidate : int option;
  extension : (string * string option) list;
      (** Directives with no field of their own, kept verbatim. A bare
          [max-stale] appears here, as do the qualified
          [no-cache="Set-Cookie"] and [private="field"] forms, whose
          flags above are also set. *)
}
(** Cache directives, request and response. *)

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
(** The [Cache-Control] header, request and response directives both. *)

val age : int t
(** The [Age] response header, in seconds. *)

val expires : string t
(** The [Expires] response header (an HTTP-date, kept verbatim). *)

type vary = [ `Any | `Fields of string list ]
(** Which request headers a response's selection depended on. *)

val vary : vary t
(** The [Vary] response header. Field names are lowercased. *)

(** {1 Cache-Status}

    {{:https://www.rfc-editor.org/rfc/rfc9211}RFC 9211}. *)

type forward =
  [ `Uri_miss | `Vary_miss | `Miss | `Request | `Stale | `Partial | `Bypass
  | `Other of string ]
(** Why a cache forwarded the request. *)

type cache_status = {
  cache : string;  (** The cache's identifier. *)
  hit : bool;
  fwd : forward option;
  fwd_status : int option;
  stored : bool;
  collapsed : bool;
  ttl : int option;
  key : string option;
  detail : string option;
}
(** One cache's account of how it handled the request. *)

val cache_status : cache_status list t
(** The [Cache-Status] header. It holds one entry per cache the
    response traversed, closest to the origin server first. *)

val cache_hit : cache_status list -> bool
(** [cache_hit entries] is [true] if any cache in [entries] reported a
    hit. *)

(** {1 Authentication}

    {{:https://www.rfc-editor.org/rfc/rfc9110#section-11}RFC 9110
    §11}. *)

type credentials =
  [ `Basic of string * string  (** User and password, base64d on encode. *)
  | `Bearer of string  (** The token. *)
  | `Other of string * string  (** Scheme and the rest, verbatim. *) ]
(** Authentication credentials. *)

val authorization : credentials t
(** The [Authorization] header, as in
    [authorization, `Basic ("aladdin", "opensesame")]. Request functions
    refuse it: a credential belongs to a capability, so attach it with
    {!Fetch.with_credentials}. This codec is for reading one, and for
    backends. *)

val proxy_authorization : credentials t
(** The [Proxy-Authorization] header. *)

type challenge = {
  scheme : string;  (** E.g. ["Basic"], ["Bearer"]. *)
  params : (string * string) list;
      (** Lowercased keys, such as [("realm", …)]. The empty key holds a
          token68 blob. *)
}
(** An authentication challenge from a server. *)

val www_authenticate : challenge list t
(** The [WWW-Authenticate] header of a 401. A scheme carrying a token68
    blob rather than parameters (["Negotiate SGVsbG8="]) keeps it whole
    as its single unnamed parameter. *)

type authentication_info = {
  nextnonce : string option;
  qop : string option;
  rspauth : string option;
  cnonce : string option;
  nc : string option;
}
(** A server's authentication reply, for mutual authentication. *)

val authentication_info : authentication_info t
(** The [Authentication-Info] header
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-11.6.3}RFC 9110
    §11.6.3}). *)

(** {1 Integrity digests}

    {{:https://www.rfc-editor.org/rfc/rfc9530}RFC 9530}. Verifying a
    body needs a hash function, which is left to the caller. *)

type digest = {
  algorithm : [ `Sha256 | `Sha512 | `Other of string ];
  digest : string;  (** Base64, as received. *)
}
(** An integrity digest of a body. *)

val content_digest : digest list t
(** The [Content-Digest] header, holding digests of the content as it
    was transferred, after any content coding. *)

val repr_digest : digest list t
(** The [Repr-Digest] header, holding digests of the representation
    itself, before any content coding. *)

val strongest_digest : digest list -> digest option
(** [strongest_digest ds] is the strongest digest in [ds], preferring
    SHA-512, then SHA-256, then the first entry. *)

(** {1 Strict-Transport-Security}

    {{:https://www.rfc-editor.org/rfc/rfc6797}RFC 6797}. *)

type hsts = {
  max_age : int64;  (** Seconds the policy is to be remembered for. *)
  include_subdomains : bool;
  preload : bool;
}
(** An HSTS policy: this host is to be reached over https only. *)

val strict_transport_security : hsts t
(** The [Strict-Transport-Security] header. *)

(** {1 Link}

    {{:https://www.rfc-editor.org/rfc/rfc8288}RFC 8288}. *)

type link = {
  target : string;  (** The target URI reference, possibly relative. *)
  rel : string option;
  media_type : string option;
  title : string option;
  hreflang : string option;
  params : (string * string) list;  (** Any further parameters. *)
}
(** A typed link to another resource. *)

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
(** The [Link] header, used for API pagination ([rel="next"]), resource
    discovery and relationship navigation. A relative target resolves
    against the response's {!Fetch.val-url}. *)

val link_rel : string -> link list -> link option
(** [link_rel r ls] is the first link in [ls] whose relation is [r], as
    in [link_rel "next" ls]. *)

(** {1 Other headers} *)

val allow : Http.Method.t list t
(** The [Allow] header
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-10.2.1}RFC 9110
    §10.2.1}). *)

type retry_after = [ `Seconds of int | `Date of string ]
(** How long to wait before retrying, as a delay or an HTTP-date. *)

val retry_after : retry_after t
(** The [Retry-After] header
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-10.2.3}RFC 9110
    §10.2.3}). {!Fetch.with_retry} honours the [`Seconds] form. The
    [`Date] form needs a wall clock to interpret, so it is left to the
    caller. *)

val location : string t
(** The [Location] header of a 3xx or 201 response. A 3xx is visible to
    the caller with [~redirects:0]. *)

val user_agent : string t
(** The [User-Agent] request header. *)
