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

type t
(** A validated, canonical URI reference. *)

(** {2 Modes}

    Every function that only reads a URI accepts it at mode [local], so a
    stack-allocated URI can be inspected without heap traffic.  Global callers
    are unaffected.

    The producers are mode-polymorphic and have [__local] variants that return
    the record in the caller's region: {!of_string}, {!of_string_exn},
    {!to_string}, {!make}, {!resolve}, {!normalize} and the [with_*] family.
    Of these, [resolve__local] and [normalize__local] are wholly heap-free,
    composing their text in the region too, with a checked [@zero_alloc] on
    each.  The others still allocate the canonical string on the heap, since
    they encode component text through intermediate strings.

    A URI in a region dies with it.  Use {!globalize} to keep one.  Every
    export is [portable]. *)

(** {2 Construction} *)

val%template of_string : string @ m -> t or_null @ m @@ portable
[@@mode m = (global, local)]
(** [of_string s] is the canonical form of [s] parsed as an RFC 3986
    [URI-reference], or [Null] if [s] is not one.  All of [s] must be
    consumed.  Relative references such as ["//host/x"], ["?q"] and [""] are
    accepted. *)

val%template of_string_exn : string @ m -> t @ m @@ portable
[@@mode m = (global, local)]
(** [of_string_exn s] is [of_string s] without the [or_null].
    @raise Invalid_argument if [s] is not a valid URI reference. *)

val of_string_canonical : string @ local -> t or_null @ local @@ portable
[@@zero_alloc]
(** [of_string_canonical s] is [This u] if [s] is already in canonical form,
    and [Null] otherwise.  The canonical string of [u] is [s] itself and the
    result lives in the caller's frame, so the call never touches the heap.
    Fall back to {!of_string} on [Null]. *)

val%template make :
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
(** [make ()] assembles a URI from component text and canonicalizes it.
    Each argument must already be percent-encoded.  Structural characters
    keep their meaning and well-formed ["%XX"] triplets are left alone, while
    characters not legal in their component are encoded, so
    [make ~path:(path u) ()] round-trips.  A [host] that is a valid IPv6
    address is bracketed.
    @raise Invalid_argument if the components cannot form a valid URI
    reference. *)

(** {2 Output} *)

val%template to_string : t @ m -> string @ m @@ portable
[@@mode m = (global, local)]
(** [to_string u] is the canonical URI text.  It is the stored
    representation, so it costs nothing. *)

val pp : Format.formatter -> t @ local -> unit @@ portable
[@@ocaml.toplevel_printer]

(** {2 Identity} *)

val equal : t @ local -> t @ local -> bool @@ portable
val compare : t @ local -> t @ local -> int @@ portable

val hash : t @ local -> int @@ portable
(** [hash u] agrees with {!equal}.  It is not [Hashtbl.hash]. *)

(** {2 Component access}

    Encoded views are substrings of the canonical text.  [Null] means the
    component is absent, which differs from present-but-empty: [http://a] has
    {!path} [""] and {!query} [Null], while [http://a/?] has {!path} ["/"]
    and {!query} [This ""]. *)

val scheme : t @ local -> string or_null @@ portable
val userinfo : t @ local -> string or_null @@ portable

val host : t @ local -> string or_null @@ portable
(** [host u] is the host, without the brackets of an IP-literal. *)

val host_kind :
  t @ local -> [ `None | `Reg_name | `Ipv4 | `Ipv6 | `Ipvfuture ] @@ portable

val port : t @ local -> int or_null @@ portable
(** [port u] is the port number.  [Null] when absent and for the empty port
    ["h:"]. *)

val path : t @ local -> string @@ portable
(** [path u] is the encoded path.  Always present, possibly [""]. *)

val query : t @ local -> string or_null @@ portable
val fragment : t @ local -> string or_null @@ portable

val has_authority : t @ local -> bool @@ portable
(** [has_authority u] is whether the reference contains an authority, i.e.
    a ["//"].  Distinguishes [http://] from [http:]. *)

val is_absolute : t @ local -> bool @@ portable
(** [is_absolute u] is whether the reference has a scheme. *)

(** {2 Span access}

    Offsets index {!to_string}.  An absent component has offset [-1] and
    length [0].  None of these allocate. *)

val scheme_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]
val userinfo_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]
val host_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]
val port_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]
val path_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]
val query_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]
val fragment_span : t @ local -> #(int * int) @@ portable [@@zero_alloc]

val port_int : t @ local -> int @@ portable [@@zero_alloc]
(** [port_int u] is the port, or [-1] when absent or empty. *)

(** {2 Decoded access} *)

val path_decoded : t @ local -> string @@ portable
(** [path_decoded u] is the path with percent-encodings resolved.  This
    conflates an encoded ["%2F"] with a literal ['/'].  Use {!path} when
    segment structure matters. *)

val fragment_decoded : t @ local -> string or_null @@ portable
val userinfo_decoded : t @ local -> string or_null @@ portable

val query_iter :
  ?plus_as_space:bool ->
  t @ local ->
  (key:string -> value:string or_null -> unit) @ local ->
  unit
  @@ portable
(** [query_iter u f] applies [f] to each ['&']-separated query parameter in
    order, with keys and values percent-decoded as {!pct_decode} does.
    [value] is [Null] for a parameter with no ['=']. *)

val find_query :
  ?plus_as_space:bool -> t @ local -> string @ local -> string or_null
  @@ portable
(** [find_query u k] is the decoded value of the first parameter whose
    decoded key is [k].  A parameter with no ['='] yields [This ""].  Keys are
    compared after decoding, so [plus_as_space] applies to them too. *)

val query_cursor : t @ local -> int @@ portable [@@zero_alloc]
(** [query_cursor u] is the start position for {!query_step}, or [-1] when
    there is no query. *)

val query_step :
  t @ local -> int -> #(int * int * int * int * int) @@ portable [@@zero_alloc]
(** [query_step u pos] is [#(key_off, key_len, value_off, value_len, next)],
    the boundaries of one query parameter.  [key_off = -1] means iteration is
    finished.  [value_off = -1] means the parameter has no ['='].  Offsets
    index {!to_string}.  Pass [next] back in to continue.  Only ['&'] and
    ['='] delimit, so the boundaries do not depend on [plus_as_space]. *)

(** {2 Percent codecs} *)

type component =
  [ `Userinfo
  | `Host
  | `Path  (** path, keeping ['/'] separators *)
  | `Segment  (** one path segment, so ['/'] is encoded *)
  | `Query  (** query, keeping ['&'], ['='] and ['+'] *)
  | `Query_value  (** one key or value, so ['&'], ['='], ['+'] are encoded *)
  | `Fragment ]

val pct_encode : ?component:component -> string -> string @@ portable
(** [pct_encode ~component s] percent-encodes every character of [s] not
    legal in [component] (default [`Path]).  Returns [s] itself when there is
    nothing to encode. *)

val pct_decode : ?plus_as_space:bool -> string -> string or_null @@ portable
(** [pct_decode s] resolves percent-encodings, or is [Null] if a ['%'] is not
    followed by two hex digits.  Returns [s] itself when there is nothing to
    decode.

    [plus_as_space] additionally decodes ['+'] as a space.  That is the
    [application/x-www-form-urlencoded] rule, not an RFC 3986 one, so it is
    off by default and belongs only to query text.  {!path_decoded},
    {!fragment_decoded} and {!userinfo_decoded} never apply it. *)

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

    Each function re-serializes the canonical string once.  Arguments follow
    the encoding convention of {!make}.  {!with_host} [Null] drops the whole
    authority, since RFC 3986 has no authority without a host. *)

val%template with_scheme : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
val%template with_userinfo : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
val%template with_host : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
val%template with_port : t @ local -> int or_null -> t @ m @@ portable
[@@mode m = (global, local)]
val%template with_path : t @ local -> string -> t @ m @@ portable
[@@mode m = (global, local)]
val%template with_query : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]
val%template with_fragment : t @ local -> string or_null -> t @ m @@ portable
[@@mode m = (global, local)]

(** {2 The raw scanner}

    The indexing layer beneath {!of_string}, for callers that want spans
    without any allocation. *)

module Raw = Uriz_raw
