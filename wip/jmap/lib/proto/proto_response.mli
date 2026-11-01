@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP responses.

    A Response is the body a server returns for a Request, defined by
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.4} RFC 8620
     Section 3.4}.

    @canonical Jmap.Proto.Response *)

type t = {
  method_responses : Proto_invocation.t list;
      (** The responses, in the order the server produced them. Each is a method
          name, its response arguments and the method call id of the call that
          produced it. *)
  created_ids : (Proto_id.t * Proto_id.t) list option;
      (** The record id the server assigned to each creation id, present only if
          the request carried [createdIds]. *)
  session_state : string;
      (** The state of the session resource. A change of this value means the
          client should fetch the session resource again. *)
  source : string option;
      (** Original response body when decoded by {!media}. It is provenance of
          the received response, and does not reflect subsequent record edits.
          It is never encoded as a JSON property. *)
}
(** The type for responses. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a response. *)

val media : t Httpz_media.t
(** [media] decodes a validated response and retains its original source.
    Ordinary client requests use this codec. *)

val source : t -> string option
(** [source r] is the original JSON body, if [r] was decoded by {!media}.
    Values decoded with {!jsont} alone have no source. *)

val source_fragment : t -> Jsont.Meta.t -> string option
(** [source_fragment r meta] is the original JSON fragment located by [meta]
    in [r]. Metadata can come from a typed object's [Jsont.Object.map'] codec.
    It must originate from this response and describe the unmodified value.
    Absent or out-of-bounds locations return [None]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf r] prints the JSON {!jsont} encodes [r] as, indented over several
    lines. Members are printed one per line and a short array is kept on one
    line, which is the layout of [Jsont.pp_json] rather than that of an
    indenting encoder. A response the codec cannot encode is printed as the JSON
    string of the encoding error, so the output is always JSON text. *)

(** {1 Finding a response} *)

val find_response : string -> t -> Proto_invocation.t option
(** [find_response id r] is the first response of [r] for the method call [id],
    or [None] if [r] has none. See {!find_responses} for the general case. *)

val find_responses : string -> t -> Proto_invocation.t list
(** [find_responses id r] is every response of [r] for the method call [id], in
    order.
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.2} RFC 8620
     Section 3.2} lets a method return more than one response, and every
    response a call initiates carries that call's method call id. *)

val get_response : string -> t -> Proto_invocation.t
(** [get_response id r] is {!find_response}.

    @raise Not_found if [r] has no response for the method call [id]. *)

val is_error : Proto_invocation.t -> bool
(** [is_error inv] is [true] if [inv] is an error response, that is if its
    method name is ["error"]. *)

val error :
  Proto_invocation.t ->
  (Proto_error.Method_error.t, Jsont.Error.t) result option
(** [error inv] is the method level error [inv] reports, [None] if [inv] is not
    an error response, and an error if [inv] is one whose arguments do not
    decode as a method error object. *)
