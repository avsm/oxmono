@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unknown JSON object members.

    JMAP object types with an [unknown] field keep the members their codecs do
    not define, so a decoded value re-encodes with those extension members.
    Other object codecs discard unrecognised members.
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-2} RFC 8620 Section
     2} requires a client to ignore properties it does not understand.

    @canonical Jmap.Proto.Unknown *)

type t = Jsont.json
(** The type for sets of unknown members. A value of this type is always a JSON
    object. The type equality is exposed for pattern matching. It is not itself
    a guarantee that the value is an object. {!val-mems} and the codecs built on
    it only ever produce objects. *)

val empty : t
(** [empty] is the JSON object with no members. *)

val is_empty : t -> bool
(** [is_empty u] is [true] if [u] has no members. A value that is not a JSON
    object has none, as it has for {!val-find}. *)

val find : t -> string -> Jsont.json option
(** [find u name] is the value of the member [name] of [u], or [None] if [u] has
    no such member. A value that is not a JSON object has no member. *)

val mems : (t, t, Jsont.mem list) Jsont.Object.Mems.map
(** [mems] is the member map that collects the members an object codec does not
    define. It is passed to [Jsont.Object.keep_unknown]. *)
