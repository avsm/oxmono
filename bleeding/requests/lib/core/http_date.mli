(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP-date parsing per RFC 9110 Section 5.6.7

    This module provides parsing of HTTP date strings as defined in RFC 9110.
    It supports three date formats:
    - RFC 1123: "Sun, 06 Nov 1994 08:49:37 GMT" (preferred)
    - RFC 850: "Sunday, 06-Nov-94 08:49:37 GMT" (obsolete)
    - ANSI C asctime(): "Sun Nov  6 08:49:37 1994" (obsolete)
*)

(** Log source for HTTP date parsing *)
val src : Logs.Src.t

(** Parse an HTTP-date string to Ptime.t.

    [parse s] attempts to parse the string [s] as an HTTP-date using
    the three supported formats. Returns [None] if parsing fails.

    Examples:
    {[
      parse "Sun, 06 Nov 1994 08:49:37 GMT"  (* RFC 1123 *)
      parse "Sunday, 06-Nov-94 08:49:37 GMT" (* RFC 850 *)
      parse "Sun Nov  6 08:49:37 1994"       (* asctime *)
    ]} *)
val parse : string -> Ptime.t option
