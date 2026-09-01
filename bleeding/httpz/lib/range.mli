(** This module provides HTTP byte ranges.

    This module parses Range field values using the [bytes] unit, resolves them
    against a selected representation length, and writes fields used in partial
    responses. Range request semantics are defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-14}RFC 9110, Section
     14}.

    Parsing and evaluation write into arrays supplied by the caller. *)

module Content : sig
  (** Content-Range syntax and numeric relationships. *)

  type kind = Invalid | Satisfied | Unsatisfied
  (** [Satisfied] names an inclusive range, [Unsatisfied] has [*/length], and
      [Invalid] denotes malformed syntax or inconsistent numeric bounds. *)

  val[@zero_alloc] kind : unit:local_ string -> local_ string -> kind @@ portable
  (** [kind ~unit value] checks a complete [unit first-last/length],
      [unit first-last/*], or [unit */length] field. Unit comparison folds ASCII
      case; surrounding SP/HTAB are accepted. Decimal bounds are compared in
      place without integer conversion, including leading zeros and arbitrarily
      long numerals. It does not validate status-specific response policy. *)

  val[@zero_alloc] valid_bounds :
    range:(int64 * int64) option -> complete_length:int64 option -> bool @@ portable
  (** [valid_bounds ~range ~complete_length] checks the corresponding bounds
      of a constructed field: all numbers are nonnegative, [first <= last],
      and [last < complete_length] when known. Both arguments cannot be [None]. *)
end

type byte_range = private
  #{ kind : int
       (** [kind] is an internal discriminator; use the [is_*] predicates. *)
   ; start : int64#
       (** [start] is the first offset or suffix length, according to [kind]. *)
   ; end_ : int64#
       (** [end_] is the inclusive final offset for an explicit range. *)
   }
(** A [byte_range] is a specification before it is resolved against a
    representation length. Use {!is_range}, {!is_suffix}, and {!is_open} to
    interpret it. *)

val max_ranges : int16# @@ portable
(** [max_ranges] is the maximum number of parsed ranges, 16. *)

val empty : byte_range @@ portable
(** [empty] is a placeholder byte range for array initialization. *)

val is_range : byte_range -> bool @@ portable
(** [is_range range] is [true] for an explicit inclusive [start]-[end_] range.
*)

val is_suffix : byte_range -> bool @@ portable
(** [is_suffix range] is [true] for a suffix range such as [-500]. Its suffix
    length is stored in [range.start]. *)

val is_open : byte_range -> bool @@ portable
(** [is_open range] is [true] for an open-ended range such as [9500-]. *)

(** A [parse_status] is the result of parsing a Range field. *)
type parse_status =
  | Valid  (** [Valid] means the range set was valid. *)
  | Invalid
      (** [Invalid] means the field did not contain a valid byte-range set. *)

type resolved =
  #{ start : int64#  (** [start] is the zero-based first byte. *)
   ; end_ : int64#  (** [end_] is the inclusive last byte. *)
   ; length : int64#  (** [length] is the number of selected bytes. *)
   }
(** A [resolved] is a satisfiable byte range expressed as inclusive offsets. *)

(** An [eval_result] is the disposition of a parsed range set. *)
type eval_result =
  | Full_content  (** [Full_content] means no ranges were supplied. *)
  | Single_range
      (** [Single_range] means exactly one supplied range is satisfiable. *)
  | Multiple_ranges
      (** [Multiple_ranges] means more than one supplied range is satisfiable.
      *)
  | Not_satisfiable
      (** [Not_satisfiable] means no supplied range is satisfiable. *)

val parse :
  local_ bytes
  -> Span.t
  -> byte_range array
  -> #(parse_status * int16#)
  @@ portable
(** [parse buf span ranges] is [(status, count)] for a case-insensitive [bytes=]
    range set. The first [count] entries of [ranges] receive parsed
    specifications. [ranges] must have at least {!max_ranges} entries. At most
    {!max_ranges} ranges are stored. A malformed member or input beyond that
    limit rejects the complete field. *)

val parse_string :
  string -> byte_range array -> #(parse_status * int16#) @@ portable
(** [parse_string value ranges] is {!parse} for a standalone string. [ranges]
    must have at least {!max_ranges} entries. *)

val evaluate :
  byte_range array
  -> count:int16#
  -> resource_length:int64#
  -> resolved array
  -> #(eval_result * int16#)
  @@ portable
(** [evaluate ranges ~count ~resource_length resolved] is
    [(disposition, resolved_count)] after resolving the first [count]
    specifications. The first [resolved_count] entries of [resolved] receive
    satisfiable ranges; unsatisfiable entries are discarded. Both arrays must be
    large enough for [count], and [count] must be non-negative. *)

val resolve_range :
  byte_range -> resource_length:int64# -> #(bool * resolved) @@ portable
(** [resolve_range range ~resource_length] is [(true, resolved)] when [range]
    selects at least one byte from a positive-length representation. It is
    [(false, empty_resolved)] otherwise. Explicit end offsets are clamped to the
    representation's last byte. *)

val write_accept_ranges : bytes -> off:int16# -> int16# @@ portable
(** [write_accept_ranges buf ~off] is the next offset after writing
    ["Accept-Ranges: bytes\r\n"]. *)

val write_accept_ranges_none : bytes -> off:int16# -> int16# @@ portable
(** [write_accept_ranges_none buf ~off] is the next offset after writing
    ["Accept-Ranges: none\r\n"]. *)

val write_content_range :
  bytes
  -> off:int16#
  -> start:int64#
  -> end_:int64#
  -> total:int64#
  -> int16#
  @@ portable
(** [write_content_range buf ~off ~start ~end_ ~total] is the next offset after
    writing ["Content-Range: bytes start-end/total\r\n"]. The numeric
    relationship is not validated. *)

val write_content_range_unsatisfiable :
  bytes -> off:int16# -> total:int64# -> int16# @@ portable
(** [write_content_range_unsatisfiable buf ~off ~total] is the next offset after
    writing ["Content-Range: bytes */total\r\n"] for a 416 response. *)

val write_multipart_boundary :
  bytes -> off:int16# -> boundary:string -> int16# @@ portable
(** [write_multipart_boundary buf ~off ~boundary] is the next offset after
    writing ["--"], [boundary], and CRLF. [boundary] is not validated. *)

val write_multipart_final :
  bytes -> off:int16# -> boundary:string -> int16# @@ portable
(** [write_multipart_final buf ~off ~boundary] is the next offset after writing
    the closing multipart delimiter. [boundary] is not validated. *)

val generate_boundary : unit -> string
(** [generate_boundary ()] is a 24-character alphanumeric boundary drawn from a
    self-initialised [Random] state, so the sequence differs from process to
    process rather than repeating from every start. It is not cryptographically
    random and must not be relied on as a secret. The caller must ensure that
    it does not occur in the enclosed representation data.

    This is the one value in this module that is not [portable]: the shared
    [Random] state it draws from is mutable, so a [portable] closure cannot
    reach it. Generate the boundary before entering such a closure. *)

val empty_resolved : resolved @@ portable
(** [empty_resolved] is a zero-length placeholder for array initialization and
    failed resolution. *)
