(** Streaming gzip decoding for a response body.

    A representation is decoded member by member as it arrives (RFC 1952 §2.2),
    so neither the peer's write boundaries nor a member ending at a read
    boundary change the bytes produced. Each member's header is validated
    before any of it reaches the decoder: the compression method must be
    DEFLATE, reserved [FLG] bits must be clear, [FEXTRA]'s [XLEN] is read
    little-endian, and [FHCRC] is checked over the complete preceding header.
    The decoder's own CRC32 and ISIZE checks are retained.

    Two bounds keep a hostile representation from costing unbounded memory or
    time. A member header, including [FEXTRA], [FNAME] and [FCOMMENT], may not
    exceed 262144 bytes, and a representation may not carry more than 1024
    members. Both reject streams that RFC 1952 itself permits.

    {2 Complete-header invariant}

    This module buffers a member header whole and only then hands it to
    [Gz.Inf] through [Inf.src]. The header parser answers [`Ready] exactly when
    every RFC 1952 header byte is present in the buffer, and nothing reaches the
    decoder before that.

    Two unsafe paths in decompress 1.6.0 depend on it. [gz.ml:454] reads a
    16-bit [XLEN] with a single byte remaining, and [gz.ml:358] advances past
    the end of its input with zero bytes remaining. Neither is reachable while
    the decoder never sees a partial header. A refactor that streamed a header
    to the decoder in pieces would silently reintroduce an out-of-bounds read,
    so the buffering here is a safety property rather than a convenience. *)

val gunzip :
  Eio.Flow.source_ty Eio.Resource.t -> Eio.Flow.source_ty Eio.Resource.t
(** [gunzip src] is [src] decoded as a gzip representation. Reading it raises
    [Fetch.Protocol_error] for a malformed or truncated member, for a member
    header over 262144 bytes, and for a representation of more than 1024
    members. It raises [End_of_file] once the final member is complete, and
    [Invalid_argument] if read into an empty buffer. *)
