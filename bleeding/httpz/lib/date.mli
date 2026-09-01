(** This module provides HTTP-date parsing and formatting.

    HTTP senders use the fixed 29-byte IMF-fixdate form, such as
    ["Sun, 06 Nov 1994 08:49:37 GMT"]. Recipients also accept the obsolete RFC
    850 and [asctime] forms for compatibility. See
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.7}RFC 9110,
     Section 5.6.7}.

    Timestamps are seconds since the Unix epoch in UTC. *)

(** A [status] is the result of parsing an HTTP date. *)
type status =
  | Valid  (** [Valid] means the complete date was valid. *)
  | Invalid
      (** [Invalid] means the date was malformed or outside the accepted
          calendar. *)

val[@zero_alloc opt] parse :
  ?now:float -> local_ bytes -> Span.t -> #(status * float#) @@ portable
(** [parse ~now buf span] is [(Valid, timestamp)] for an IMF-fixdate, RFC 850
    date, or [asctime] date in [span], and [(Invalid, 0.)] on failure. The
    weekday spelling is validated but is not checked against the calendar
    date.

    [now] supplies the reference time for RFC 9110's moving 50-year
    interpretation of the two-digit year in an RFC 850 date. When it is
    omitted, the legacy fixed interpretation of 70 to 99 as 1970 to 1999 and
    00 to 69 as 2000 to 2069 is retained. IMF-fixdate and [asctime] parsing do
    not use [now]. It reads only inside [span]: a value too short for the form
    it resembles is [Invalid] rather than a read past the span. *)

val[@zero_alloc] parse_unboxed :
  has_now:bool -> float# -> local_ bytes -> Span.t -> #(status * float#)
  @@ portable
(** [parse_unboxed ~has_now now] is the allocation-free form of {!parse}.
    When [has_now] is [false], [now] is ignored and the legacy fixed RFC 850
    year mapping is used. *)

val format : float# -> string @@ portable
(** [format timestamp] is [timestamp] in IMF-fixdate form. Fractional seconds
    are discarded. Finite values outside calendar years 1 through 9999 are
    clamped to that range. Conversion of a non-finite timestamp is unspecified.
*)

val write_date_header : bytes -> off:int16# -> float# -> int16# @@ portable
(** [write_date_header buf ~off timestamp] is the next offset after writing a
    Date field in IMF-fixdate form. *)

val write_last_modified : bytes -> off:int16# -> float# -> int16# @@ portable
(** [write_last_modified buf ~off timestamp] is the next offset after writing a
    Last-Modified field in IMF-fixdate form. *)

val write_expires : bytes -> off:int16# -> float# -> int16# @@ portable
(** [write_expires buf ~off timestamp] is the next offset after writing an
    Expires field in IMF-fixdate form. *)

val[@zero_alloc opt] write_http_date :
  bytes -> off:int16# -> float# -> int16# @@ portable
(** [write_http_date buf ~off timestamp] is [off + 29] after writing the 29-byte
    IMF-fixdate value without a field name or CRLF. *)

val is_modified_since :
  last_modified:float# -> if_modified_since:float# -> bool @@ portable
(** [is_modified_since ~last_modified ~if_modified_since] is [true] when
    [last_modified] is later than [if_modified_since]. *)

val is_unmodified_since :
  last_modified:float# -> if_unmodified_since:float# -> bool @@ portable
(** [is_unmodified_since ~last_modified ~if_unmodified_since] is [true] when
    [last_modified] is not later than [if_unmodified_since]. *)
