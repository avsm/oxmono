(*
 * Copyright (c) 2012-2026 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012-2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** RFC 3986 URI references.

    A {!t} is a canonical percent-encoded URI-reference string together with
    the offsets of its components.  {!of_string} validates against the RFC
    3986 grammar and applies syntax-based normalization (§6.2.2): it
    lowercases the scheme and host, uppercases percent-triplet hex, and
    decodes triplets that encode unreserved characters.  Invalid input yields
    [Null], never a coerced URI.

    Because the canonical text is the representation, {!to_string} is free,
    and {!equal}, {!compare} and {!hash} are string operations implementing
    syntax-based equivalence.  Scheme-based equivalence (§6.2.3) is not
    applied, so [http://a:80/] and [http://a/] differ, as do [http://a] and
    [http://a/].

    {!of_string} keeps dot segments, because ["../a"] and ["a"] are different
    relative references.  {!normalize} removes them, and {!resolve} does so
    as §5.2 requires.  Parsing [to_string u] yields a URI with the same text,
    and {!normalize} is idempotent.

    Deviations: a port that does not fit in an OCaml [int] is rejected rather
    than truncated, and IPv6 literals are case-normalized but not
    zero-compressed per RFC 5952. *)

type t = Uriz.t
(** A validated, canonical URI reference.  It is a string and a row of
    offsets into it, never written after construction, so it crosses
    portability and contention: a URI may be held at module level and read
    from a portable closure. *)

(** {2 Modes}

    Every function that only reads a URI accepts it at mode [local], so a
    stack-allocated URI can be inspected without heap traffic.  Global callers
    are unaffected.

    The producers are mode-polymorphic and have [__local] variants that return
    the record in the caller's region: {!of_string}, {!of_string_exn},
    {!to_string}, {!make_encoded}, {!resolve}, {!normalize} and the [with_*] family.
    Of these, [resolve__local] and [normalize__local] are wholly heap-free,
    composing their text in the region too, with a checked [@zero_alloc] on
    each.  The others still allocate the canonical string on the heap, since
    they encode component text through intermediate strings.

    A URI in a region dies with it.  Use {!globalize} to keep one.  Every
    export is [portable], and {!t} has the [immutable_data] kind, so a URI
    parsed once at module level is readable from every domain. *)

(** {2 Construction} *)

val%template of_string : string @ m -> t or_null @ m @@ portable
[@@mode m = (global, local)]
(** [of_string s] is the canonical form of [s] parsed as an RFC 3986
    [URI-reference], or [Null] if [s] is not one.  All of [s] must be
    consumed. All syntactically valid schemes and relative references such as
    ["//host/x"], ["?q"] and [""] are accepted. *)

val%template of_string_exn : string @ m -> t @ m @@ portable
[@@mode m = (global, local)]
(** [of_string_exn s] is [of_string s] without the [or_null].

    @raise Stdlib.Invalid_argument if [s] is not a valid URI reference. *)

val%template make_encoded :
  ?scheme:string ->
  ?userinfo:string ->
  ?host:string ->
  ?port:int ->
  ?path:string ->
  ?query:string ->
  ?fragment:string ->
  unit ->
  t @ m
  @@ portable
[@@mode m = (global, local)]
(** [make_encoded ()] assembles a URI by treating every component argument as
    encoded text, then canonicalizes it. Every component defaults to absent,
    except [path], which defaults to [""]. Structural characters keep their
    meaning and well-formed ["%XX"] triplets are left alone, while bytes not
    legal in their component are encoded, so
    [make_encoded ~path:(encoded_path u) ()] round-trips. An unbracketed IPv6
    or IPvFuture [host] is bracketed.

    @raise Stdlib.Invalid_argument if the components cannot form a valid URI
    reference, the scheme is invalid, or [port] is negative. *)

(** {2 Output} *)

val%template to_string : t @ m -> string @ m @@ portable
[@@mode m = (global, local)]
(** [to_string u] is the canonical URI text.  It is the stored
    representation, so it costs nothing. *)

val pp : Format.formatter -> t @ local -> unit @@ portable
[@@ocaml.toplevel_printer]
(** [pp formatter u] prints {!to_string} [u] on [formatter]. *)

(** {2 Identity} *)

val equal : t @ local -> t @ local -> bool @@ portable
(** [equal a b] is [true] when [a] and [b] have identical canonical text. *)

val compare : t @ local -> t @ local -> int @@ portable
(** [compare a b] orders canonical URI text lexicographically. *)

val hash : t @ local -> int @@ portable
(** [hash u] agrees with {!equal}.  It is not [Hashtbl.hash]. *)

(** {2 Component access}

    Encoded component names are explicit: they return substrings of the
    canonical serialization and never silently percent-decode. [Null] means a
    component is absent, which differs from present-but-empty: [http://a] has
    {!encoded_path} [""] and {!encoded_query} [Null], while [http://a/?] has
    {!encoded_path} ["/"] and {!encoded_query} [This ""]. *)

val scheme : t @ local -> string or_null @@ portable
(** [scheme u] is the lower-case scheme without its colon, or [Null]. *)

val encoded_userinfo : t @ local -> string or_null @@ portable
(** [encoded_userinfo u] is the encoded userinfo without its trailing [@], or
    [Null]. *)

val encoded_host : t @ local -> string or_null @@ portable
(** [encoded_host u] is the encoded host without the brackets of an
    IP-literal. *)

type host_kind = [ `Reg_name | `Ipv4 | `Ipv6 | `Ipvfuture ]

val host_kind : t @ local -> host_kind or_null @@ portable
(** [host_kind u] classifies the host, or is [Null] when [u] has no
    authority. *)

val port : t @ local -> int or_null @@ portable
(** [port u] is the port number.  [Null] when absent and for the empty port
    ["h:"]. *)

val has_port : t @ local -> bool @@ portable
(** [has_port u] is whether the authority contains a port delimiter. Together
    with {!port}, this distinguishes an absent port from an empty one. *)

val encoded_path : t @ local -> string @@ portable
(** [encoded_path u] is the encoded path. Always present, possibly [""]. *)

val encoded_query : t @ local -> string or_null @@ portable
(** [encoded_query u] is the encoded query without its leading [?], or [Null]
    when the query component is absent. *)

val encoded_fragment : t @ local -> string or_null @@ portable
(** [encoded_fragment u] is the encoded fragment without its leading [#], or
    [Null] when the fragment component is absent. *)

val has_authority : t @ local -> bool @@ portable
(** [has_authority u] is whether the reference contains an authority, i.e.
    a ["//"].  Distinguishes [http://] from [http:]. *)

val is_absolute : t @ local -> bool @@ portable
(** [is_absolute u] is whether the reference has a scheme. *)

val encoded_path_and_query : t @ local -> string @@ portable
(** [encoded_path_and_query u] is the encoded path followed by its optional [?]
    and query. It excludes the scheme, authority, and fragment. *)

(** {2 Decoded access} *)

val decoded_path : t @ local -> string @@ portable
(** [decoded_path u] is the path with percent-encodings resolved.  This
    conflates an encoded ["%2F"] with a literal ['/']. Use {!encoded_path} when
    segment structure matters. *)

val decoded_fragment : t @ local -> string or_null @@ portable
(** [decoded_fragment u] is {!encoded_fragment} with percent escapes decoded,
    or [Null] when the component is absent. *)

val decoded_userinfo : t @ local -> string or_null @@ portable
(** [decoded_userinfo u] is {!encoded_userinfo} with percent escapes decoded,
    or [Null] when the component is absent. *)
val decoded_host : t @ local -> string or_null @@ portable
(** [decoded_host u] is the host with percent-encodings resolved and without
    IP-literal brackets. It is [Null] when there is no authority. *)

(** {2 Query parameters}

    RFC 3986 treats a query as opaque text. These helpers apply the common
    ['&']-separated, ['=']-bound parameter convention without treating ['+'] as
    a space unless [plus_as_space] is explicitly requested. Keys and values are
    decoded; use {!encoded_query} when their original spelling matters. *)

val query_params :
  ?plus_as_space:bool ->
  t @ local ->
  (string * string option) list
  @@ portable
(** [query_params u] is the decoded parameter sequence in wire order. A value
    is [None] when its parameter has no ['=']; this is distinct from
    [Some ""]. [plus_as_space] defaults to [false]. *)

val iter_query_params :
  ?plus_as_space:bool ->
  t @ local ->
  (key:string -> value:string or_null -> unit) @ local ->
  unit
  @@ portable
(** [iter_query_params u f] applies [f] to each ['&']-separated query parameter in
    order, with keys and values percent-decoded as {!percent_decode} does.
    [value] is [Null] for a parameter with no ['=']. [plus_as_space] defaults
    to [false]. *)

val find_query_param :
  ?plus_as_space:bool -> t @ local -> string @ local -> string or_null
  @@ portable
(** [find_query_param u k] is the decoded value of the first parameter whose
    decoded key is [k].  A parameter with no ['='] yields [This ""].  Keys are
    compared after decoding, so [plus_as_space] applies to them too and defaults
    to [false]. *)

val remove_query_param : ?plus_as_space:bool -> t -> string @ local -> t @@ portable
(** [remove_query_param u key] removes every parameter whose decoded key is
    [key]. [plus_as_space] defaults to [false] and applies the form/query convention to keys as in
    {!find_query_param}. It preserves the encoded spelling and order of all
    other parameters and returns [u] unchanged when no key matches. *)

val add_query_param :
  t @ local -> key:string -> value:string -> t @@ portable
(** [add_query_param u ~key ~value] appends one parameter, percent-encoding the
    key and value so query delimiters in either remain data. *)

(** {2 Percent codecs} *)

type component =
  [ `Userinfo
  | `Host
  | `Path  (** path, keeping ['/'] separators *)
  | `Path_segment  (** one path segment, so ['/'] is encoded *)
  | `Query  (** query, keeping ['&'], ['='] and ['+'] *)
  | `Query_value  (** one key or value, so ['&'], ['='], ['+'] are encoded *)
  | `Fragment
  | `Unreserved
    (** only the unreserved set of RFC 3986 section 2.3, so a sub-delimiter
        is encoded too *)
  ]

val percent_encode : component:component -> string -> string @@ portable
(** [percent_encode ~component s] percent-encodes every character of [s] not
    legal in [component]. Returns [s] itself when there is nothing to encode. *)

val percent_decode : ?plus_as_space:bool -> string -> string or_null @@ portable
(** [percent_decode s] resolves percent-encodings, or is [Null] if a ['%'] is not
    followed by two hex digits.  Returns [s] itself when there is nothing to
    decode.

    [plus_as_space] additionally decodes ['+'] as a space.  That is the
    [application/x-www-form-urlencoded] rule, not an RFC 3986 one, so it is
    off by default and belongs only to query text.  {!decoded_path},
    {!decoded_fragment} and {!decoded_userinfo} never apply it. *)

(** {2 RFC 3986 operations} *)

val%template resolve : base:t @ local -> t @ local -> t @ m @@ portable
[@@mode m = (global, local)]
(** [resolve ~base r] is the RFC 3986 §5.2 resolution of [r] against [base].
    [base] should be an absolute URI.  If it is not, the result is
    well-formed but not meaningful.  The result shares no bytes with either
    argument, so both may be local.  [resolve__local] composes the result text
    in the caller's region and is checked [@zero_alloc]. *)

val%template normalize : t -> t @ m @@ portable
[@@mode m = (global, local)]
(** [normalize u] applies §6.2.2.3 dot-segment removal on top of what
    {!of_string} already did.  Dot segments are removed only when [u] has a
    scheme or an authority, since they carry meaning in a bare relative
    reference.  [normalize__local] is checked [@zero_alloc].  The argument is
    global because a URI with no dot segments is returned unchanged. *)

val globalize : t @ local -> t @@ portable
(** [globalize u] is [u] with its text copied to the heap.  Use it to keep a
    URI built in a region that is about to end. *)

(** {2 Functional update}

    Each function re-serializes the canonical string once. Encoded arguments
    follow the convention of {!make_encoded}. {!with_encoded_host} [Null] drops
    the whole authority, since RFC 3986 has no authority without a host. *)

val%template with_scheme : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_scheme u scheme] replaces [u]'s scheme. [Null] removes it.

    @raise Stdlib.Invalid_argument if [scheme] is present but invalid. *)
val%template with_encoded_userinfo : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_encoded_userinfo u userinfo] replaces the encoded userinfo. [Null]
    removes it. Supplying userinfo to a URI without a host creates an empty-host
    authority. *)
val%template with_encoded_host : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_encoded_host u host] replaces the encoded host. [Null] removes the
    whole authority, including userinfo and port. *)
val%template with_port : t @ local -> int or_null -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_port u port] replaces the numeric port. [Null] removes it.

    @raise Stdlib.Invalid_argument if [port] is negative. *)
val%template with_encoded_path : t @ local -> string -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_encoded_path u path] replaces the encoded path, percent-encoding
    bytes that are not legal path characters. *)
val%template with_encoded_query : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_encoded_query u query] replaces the encoded query. [Null] removes
    the component; [This ""] retains a trailing [?]. *)
val%template with_encoded_fragment : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
(** [with_encoded_fragment u fragment] replaces the encoded fragment. [Null]
    removes the component; [This ""] retains a trailing [#]. *)

(** {2 Scanner}

    The indexing layer beneath {!of_string}, for callers that want spans
    without any allocation. *)

module Scanner = Uriz_scanner
(** [Scanner] is the allocation-free, borrowed-span parser underlying {!t}.
    Its offsets refer to the caller-owned input string. *)

module Raw = Uriz.Raw
