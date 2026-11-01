(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Finger query parsing
    ({{:https://www.rfc-editor.org/rfc/rfc1288#section-2.3}RFC 1288 §2.3}).

    The wire-format query line is:
    {v
    {Q1} ::= [{W}|{W}{S}{U}]{C}
    {Q2} ::= [{W}{S}][{U}]{H}{C}
    v}
    where [{W}] is [/W], [{S}] is a space, [{U}] is a username,
    [{H}] is [@hostname], and [{C}] is CRLF. *)

type t = {
  verbose : bool;
  (** [true] when the [/W] flag was present (verbose output). *)

  user : string;
  (** Username, or [""] for a null query (list users / general info). *)

  host : string option;
  (** [Some hostname] for a forwarding query, [None] for a local query. *)
}
(** A parsed Finger query. *)

val parse : string -> t
(** [parse line] parses a raw query line into a structured query.

    Trailing CRLF (or bare CR / LF) is stripped before parsing.

    {[
      parse ""              (* { verbose = false; user = ""; host = None } *)
      parse "/W\r\n"        (* { verbose = true;  user = ""; host = None } *)
      parse "anil\r\n"      (* { verbose = false; user = "anil"; host = None } *)
      parse "anil@host\r\n" (* { verbose = false; user = "anil"; host = Some "host" } *)
    ]} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt q] pretty-prints [q] in wire format (e.g. [/W anil\@host]). *)
