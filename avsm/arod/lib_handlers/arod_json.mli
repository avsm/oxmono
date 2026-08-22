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
