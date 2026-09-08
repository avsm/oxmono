@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** URIs.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} types several
    properties a URI as defined by
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3} RFC 3986 Section
     3}, among them the [uri] of every Resource and of a SchedulingAddress.

    @canonical Jscontact.Uri *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] has the shape of a URI. The check is
    syntactic. It asks for a scheme, a colon, and a remainder free of whitespace
    and control characters, and does not parse the authority, path, query or
    fragment. *)

val validate : prop:string -> string -> string Jscontact_valid.t
(** [validate ~prop s] is [Ok s] if {!is_valid} holds of [s], and otherwise an
    error naming the property [prop]. *)
