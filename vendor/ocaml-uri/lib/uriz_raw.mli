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

(** RFC 3986 span scanner.

    {!parse} validates a URI reference against the RFC 3986 grammar and
    reports where each component sits in the input.  {!spans} is an unboxed
    record returned in registers.  Every function here carries [@zero_alloc],
    which the compiler checks.

    This layer does not normalize.  {!needs_normalization} and {!shrink}
    report whether normalization would change the input.  {!Uriz.of_string}
    acts on that. *)

type spans = #{
  err : int;
      (** [0] when the input is a valid URI reference, otherwise one plus the
          offset at which validation failed. *)
  flags : int;  (** bit 0: the input is not already in canonical form. *)
  shrink : int;
      (** How many bytes shorter the canonical form is than the input. *)
  scheme_len : int;
      (** The scheme occupies [\[0, scheme_len)].  [-1] when absent.  This is
          a length, not an offset; see {!parse_sub}. *)
  userinfo_off : int;
  userinfo_len : int;
  host_off : int;
      (** For an IP-literal, the span inside the brackets.  [-1] means there
          is no authority at all. *)
  host_len : int;
  host_kind : int;  (** One of the [host_*] constants below. *)
  port_off : int;  (** [-1] when there is no [':'] after the host. *)
  port_len : int;  (** [0] for the empty port ["h:"]. *)
  port_val : int;  (** [-1] when the port is absent or empty. *)
  path_off : int;  (** Always present.  The path may be empty. *)
  path_len : int;
  query_off : int;
  query_len : int;
  frag_off : int;
  frag_len : int;
}

val host_none : int
val host_reg_name : int
val host_ipv4 : int
val host_ipv6 : int
val host_ipvfuture : int
val flag_dirty : int

val parse : string @ local -> spans @@ portable
[@@zero_alloc]
(** [parse s] scans the whole of [s] as a URI reference.  The parse is valid
    only if the entire string is consumed. *)

val parse_sub : string @ local -> pos:int -> len:int -> spans @@ portable
[@@zero_alloc]
(** [parse_sub s ~pos ~len] scans [s\[pos, pos+len)] as a URI reference.  The
    parse is valid only if the whole window is consumed.  No byte outside the
    window is read, so a URI may be scanned in place inside a larger buffer
    holding unrelated bytes on either side.

    Every offset in the result, [err] included, is an index into [s], not into
    the window.  The one exception is [scheme_len], which is a length: the
    scheme occupies [\[pos, pos + scheme_len)].

    A window that does not lie within [s] yields [err = 1] and no spans.

    [parse s] is [parse_sub s ~pos:0 ~len:(String.length s)]. *)

(** {2 Accessors}

    Views of the {!spans} fields for callers that do not unpack the
    record. *)

val err : spans -> int @@ portable [@@zero_alloc]
val is_valid : spans -> bool @@ portable [@@zero_alloc]

val error_offset : spans -> int @@ portable [@@zero_alloc]
(** [error_offset sp] is the offset of the first byte that failed
    validation, or [-1] if valid. *)

val needs_normalization : spans -> bool @@ portable [@@zero_alloc]
val shrink : spans -> int @@ portable [@@zero_alloc]
val scheme_len : spans -> int @@ portable [@@zero_alloc]
val userinfo_off : spans -> int @@ portable [@@zero_alloc]
val userinfo_len : spans -> int @@ portable [@@zero_alloc]
val host_off : spans -> int @@ portable [@@zero_alloc]
val host_len : spans -> int @@ portable [@@zero_alloc]
val host_kind : spans -> int @@ portable [@@zero_alloc]
val port_off : spans -> int @@ portable [@@zero_alloc]
val port_len : spans -> int @@ portable [@@zero_alloc]
val port_val : spans -> int @@ portable [@@zero_alloc]
val path_off : spans -> int @@ portable [@@zero_alloc]
val path_len : spans -> int @@ portable [@@zero_alloc]
val query_off : spans -> int @@ portable [@@zero_alloc]
val query_len : spans -> int @@ portable [@@zero_alloc]
val frag_off : spans -> int @@ portable [@@zero_alloc]
val frag_len : spans -> int @@ portable [@@zero_alloc]

(** {2 Address literals} *)

val is_ipv4 : string @ local -> bool @@ portable [@@zero_alloc]
(** [is_ipv4 s] is whether [s] is a strict RFC 3986 [IPv4address], with no
    leading zeros and no octet above 255. *)

val is_ipv6 : string @ local -> bool @@ portable [@@zero_alloc]
(** [is_ipv6 s] is whether [s] is an RFC 3986 [IPv6address], brackets
    excluded. *)

val ipv4_end : string @ local -> int -> int -> int @@ portable [@@zero_alloc]
(** [ipv4_end s i limit] is the offset just past an [IPv4address] starting at
    [i], or [-1]. *)

val ipv6_end : string @ local -> int -> int -> int @@ portable [@@zero_alloc]
(** [ipv6_end s i limit] is the offset just past an [IPv6address] starting at
    [i], or [-1]. *)

val ipvfuture_end : string @ local -> int -> int -> int @@ portable [@@zero_alloc]
(** [ipvfuture_end s i limit] is the offset just past an [IPvFuture] literal
    starting at [i], or [-1]. *)

val scheme_end : string @ local -> int -> int -> int @@ portable [@@zero_alloc]
(** [scheme_end s i limit] is the offset just past a [scheme] starting at
    [i], or [-1].  The terminating [':'] is not consumed. *)

val port_end : string @ local -> int -> int -> int @@ portable [@@zero_alloc]
(** [port_end s i limit] is the offset just past a [port] starting at [i].
    A port may be empty, so this never fails. *)

(** {2 Percent decoding in place} *)

val needs_decode :
  string @ local -> pos:int -> len:int -> plus_as_space:bool -> bool @@ portable
[@@zero_alloc]
(** [needs_decode s ~pos ~len ~plus_as_space] is whether [s\[pos, pos+len)]
    contains a ['%'], or a ['+'] when [plus_as_space].  When it does not,
    decoding is the identity and the caller can use the window as it stands.
    A window outside [s] is [false]. *)

val pct_decode_into :
  string @ local ->
  pos:int ->
  len:int ->
  dst:bytes @ local ->
  dst_pos:int ->
  plus_as_space:bool ->
  int
  @@ portable
[@@zero_alloc]
(** [pct_decode_into s ~pos ~len ~dst ~dst_pos ~plus_as_space] decodes the
    percent-encodings of [s\[pos, pos+len)] into [dst] from [dst_pos] and is
    the number of bytes written.  When [plus_as_space], a ['+'] decodes to a
    space, which is the [application/x-www-form-urlencoded] rule rather than
    an RFC 3986 one.

    Decoding never lengthens, so [dst] needs [len] bytes of room at [dst_pos];
    with less, or with a window outside [s], the result is [-1] and nothing is
    written.  A ['%'] not followed by two hex digits, including one cut off by
    the end of the window, also gives [-1], leaving whatever was written up to
    that point.

    Neither buffer is copied, so [dst] may be a stack-allocated scratch. *)

(** {2 Character classes}

    The same byte legality the {!Uriz} encoder uses. *)

val is_alpha : char -> bool @@ portable [@@zero_alloc]
val is_digit : char -> bool @@ portable [@@zero_alloc]
val is_hexdig : char -> bool @@ portable [@@zero_alloc]
val is_unreserved : char -> bool @@ portable [@@zero_alloc]
val is_sub_delim : char -> bool @@ portable [@@zero_alloc]

val hex_val : char -> int @@ portable [@@zero_alloc]
(** [hex_val c] is the value of hex digit [c], or [-1]. *)
