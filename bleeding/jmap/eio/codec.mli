(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON text for JMAP values.

    Every JMAP object of the [jmap] library has a [jsont] codec. These four
    functions turn one into the JSON text a server reads and back. They are
    {!Jmap.Proto.Json.encode} and {!Jmap.Proto.Json.decode} with a raising form
    of each, so both directions apply the I-JSON profile JMAP uses on top of
    what [jsont] itself accepts. *)

val encode :
  ?format:Jsont.format -> 'a Jsont.t -> 'a -> (string, Jsont.Error.t) result
(** [encode ~format jsont v] is {!Jmap.Proto.Json.encode}, the I-JSON text of
    [v] written with [jsont]. [format] defaults to [Jsont.Minify]. The error
    says which value [jsont] refused to write, or which one is not I-JSON, a
    duplicate object member and a non-finite number included. *)

val decode :
  ?locs:bool ->
  ?max_depth:int ->
  'a Jsont.t ->
  string ->
  ('a, Jsont.Error.t) result
(** [decode ~locs ~max_depth jsont s] is {!Jmap.Proto.Json.decode}, the value
    the JSON text [s] holds, read with [jsont]. [locs] defaults to [false]. It
    requests source text locations in the error message, which costs a little on
    every decode and which the I-JSON pass over [s] does not report. [max_depth]
    defaults to {!Httpz_media.Json.default_max_depth}, counts the outermost
    array or object as depth one, and may be zero to accept scalars only. The
    error says which value [jsont] refused to read, or which I-JSON constraint
    [s] breaks, a duplicate member, a surrogate or noncharacter code point and a
    number outside binary64 magnitude included.

    @raise Invalid_argument if [max_depth] is negative. *)

val encode_exn : ?format:Jsont.format -> 'a Jsont.t -> 'a -> string
(** [encode_exn ~format jsont v] is {!encode}.

    @raise Jsont.Error if [jsont] refuses to write [v] or [v] is not I-JSON. *)

val decode_exn : ?locs:bool -> ?max_depth:int -> 'a Jsont.t -> string -> 'a
(** [decode_exn ~locs ~max_depth jsont s] is {!decode}.

    @raise Jsont.Error
      if [s] is not JSON text [jsont] reads, is not I-JSON, or exceeds the depth
      bound.
    @raise Invalid_argument if [max_depth] is negative. *)
