(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr V3 extension points.

    The object with a [name], an optional [configuration] and an
    optional [must_understand] that stands for a data type, a chunk
    grid, a chunk key encoding, a codec or a storage transformer in
    array metadata. Matches the
    {{:https://zarr.dev/zeps/draft/ZEP0009.html}ZEP0009} rules,
    including the bare string form and the [must_understand] default. *)

type t = {
  name : string;  (** The extension identifier. *)
  config : Jsont.json option;
      (** The [configuration] member. Always a JSON object when
          present. *)
  must_understand : bool;
      (** [true] when a reader that does not know [name] must fail.
          Absent in the JSON means [true]. *)
}

val v : ?config:Jsont.json -> ?must_understand:bool -> string -> t
(** [v name] is the extension named [name]. [must_understand] defaults
    to [true] and [config] to absent. [config] should be a JSON object,
    otherwise {!jsont} fails to encode the result. *)

val jsont : t Jsont.t
(** [jsont] is the JSON type of an extension point.

    Decoding accepts a bare JSON string, taken as the name with no
    configuration and [must_understand] [true], or an object with a
    required [name], an optional [configuration] object and an optional
    [must_understand] boolean. An absent [must_understand] is [true].
    Any other member is an error, as is a [configuration] that is not an
    object and a [name] that is empty. A JSON [null] configuration
    decodes as absent.

    Encoding a [must_understand] [true] extension emits a bare string
    when [config] is [None], the object [{"name": n}] when [config] is
    an empty object, and otherwise
    [{"name": n, "configuration": c}]. A [must_understand] [false] one
    always takes the object form and carries the member, because the two
    short forms mean [true]. *)

val config_mem : t -> string -> Jsont.json option
(** [config_mem t n] is the value of the member named [n] of [t]'s
    configuration, or [None] when there is no configuration or no such
    member. *)

val config_mems : t -> Jsont.mem list
(** [config_mems t] are the members of [t]'s configuration, in document
    order. It is the empty list when there is no configuration. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality of [a] and [b], ignoring the
    source layout of the configuration. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as its name followed by its configuration. *)
