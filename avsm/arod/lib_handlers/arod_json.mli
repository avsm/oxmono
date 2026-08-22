(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON responses.

    The JSON routes describe their responses as jsont codecs. A codec encodes
    its members in the order they are declared, and omits a member whose
    [enc_omit] holds, so a response shape reads off the codec.

    Nothing here is portable, and it cannot be: a codec is a module-level
    [Jsont.t], and a portable handler reaches a module-level value only if its
    type carries a crossing kind, which [Jsont.t] cannot be given. The reasons
    are in [TODO.md] under **jsont**, and they are four solved problems and one
    that is not. The routes that write JSON therefore keep a closure in
    {!Arod_env.t}, which is what that record is for. *)

val encode : 'a Jsont.t -> 'a -> string
(** [encode codec v] is [v] encoded through [codec] as minified JSON.

    An encode fails only when a codec maps a value through a function that
    raises, which none of the response codecs do, so the failure branch
    answers a JSON error object rather than raising inside a route. *)

val stream : 'a Jsont.t -> 'a -> Proffer.Body.Sink.t -> unit
(** [stream codec v sink] encodes [v] through [codec] straight onto [sink],
    without the finished string in between.

    jsont writes through a slice at a time, and a slice is bytes with an
    offset and a length, so it goes to the socket through
    {!Proffer.Body.Sink.write_sub} with nothing copied on the way. That is
    what a route answering half a megabyte wants: the encoded body used to
    exist twice, once as jsont's output and once as the response, and now it
    exists as neither.

    The response cannot carry a Content-Length, since the length is not known
    until the encode has run, so a route using this answers chunked. An encode
    that fails part way has already written, so the failure is reported to the
    backend's [on_error] and the body is truncated: a codec that can raise
    does not belong on this path. *)
