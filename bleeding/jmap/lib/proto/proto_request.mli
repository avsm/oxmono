@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP requests.

    A Request is the body a client POSTs to the API URL, defined by
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.3} RFC 8620
     Section 3.3}. It names the capabilities the calls need and carries the
    calls themselves.

    @canonical Jmap.Proto.Request *)

type t = {
  using : string list;
      (** The capability URIs the method calls need. The server rejects the
          whole request with [unknownCapability] if it does not support one of
          them. *)
  method_calls : Proto_invocation.t list;
      (** The method calls, executed in order. *)
  created_ids : (Proto_id.t * Proto_id.t) list option;
      (** The record id already known for each creation id, carried over from an
          earlier request so that its creation references keep working. *)
}
(** The type for requests. *)

val create :
  using:string list ->
  method_calls:Proto_invocation.t list ->
  ?created_ids:(Proto_id.t * Proto_id.t) list ->
  unit ->
  t
(** [create ~using ~method_calls ()] is the request calling [method_calls] with
    the capabilities [using]. [created_ids] is [None] unless given. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a request. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf r] prints the JSON {!jsont} encodes [r] as, indented over several
    lines. Members are printed one per line and a short array is kept on one
    line, which is the layout of [Jsont.pp_json] rather than that of an
    indenting encoder. A request the codec cannot encode is printed as the JSON
    string of the encoding error, so the output is always JSON text. *)
