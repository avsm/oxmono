(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr V3 array and group metadata.

    The contents of a [zarr.json]. The codecs here stop at the JSON
    level: [fill_value] stays generic until {!Fill_value.of_json} is
    given the data type, and every extension point stays an {!Ext.t}
    until its own module binds it.

    This module maps to and from {!Jsont.json}. The [zarrz] library does
    not depend on a JSON reader or writer, so a caller that starts from
    bytes decodes them with [jsont.bytesrw] and passes the result to
    {!array_of_json}. *)

type array_meta = {
  shape : int array;
  data_type : Ext.t;
  chunk_grid : Ext.t;
  chunk_key_encoding : Ext.t;
  fill_value : Jsont.json;
      (** Uninterpreted. Pass it to {!Fill_value.of_json} with the data
          type. *)
  codecs : Ext.t list;
  attributes : Jsont.json option;
      (** A JSON object when present. A JSON null decodes as absent,
          a read side leniency. *)
  dimension_names : string option list option;
      (** One entry per dimension of [shape] when present. A [None]
          entry is a JSON [null], an unnamed dimension. *)
  storage_transformers : Ext.t list;
  unknown : Jsont.mem list;
      (** Members of the document this version does not model, kept in
          document order so that a decode followed by an encode is
          lossless. Every one of them has [must_understand] [false]. A
          [null] [consolidated_metadata] is dropped rather than kept. *)
}
(** The type for array metadata. *)

type group_meta = {
  group_attributes : Jsont.json option;  (** A JSON object when present. *)
  group_unknown : Jsont.mem list;
      (** Unmodelled members, under the same rules as the [unknown] of
          {!array_meta}. *)
}
(** The type for group metadata. *)

val array_jsont : array_meta Jsont.t
(** [array_jsont] is the JSON type of array metadata.

    Decoding requires [zarr_format] to be [3] and [node_type] to be
    ["array"]. [attributes], [storage_transformers] and
    [dimension_names] may be absent, every other modelled member is
    required. [codecs] must not be empty, since the list holds the array
    to bytes codec, and [dimension_names], when present, must have one
    entry per dimension of [shape]. An unknown top level member is an
    error unless it is a JSON object carrying
    [{"must_understand": false}]. A [consolidated_metadata] member whose
    value is [null] is dropped before that check.

    Encoding writes [zarr_format], [node_type], [shape], [data_type],
    [chunk_grid], [chunk_key_encoding], [fill_value], [codecs],
    [attributes], [storage_transformers], [dimension_names] and then the
    kept unknown members, in that order. Absent optionals and an empty
    [storage_transformers] are omitted. *)

val group_jsont : group_meta Jsont.t
(** [group_jsont] is the JSON type of group metadata. [node_type] must
    be ["group"] and [attributes] is the only modelled member besides
    [zarr_format]. Unknown members follow the rule of {!array_jsont}. *)

val array_of_json : Jsont.json -> (array_meta, string) result
(** [array_of_json j] decodes [j] with {!array_jsont}. *)

val group_of_json : Jsont.json -> (group_meta, string) result
(** [group_of_json j] decodes [j] with {!group_jsont}. *)

val array_to_json : array_meta -> Jsont.json
(** [array_to_json m] encodes [m] with {!array_jsont}.

    @raise Error.E if a member of [m] cannot be encoded, which happens
    when [attributes] or an {!Ext.t} configuration is not a JSON
    object. *)

val group_to_json : group_meta -> Jsont.json
(** [group_to_json m] encodes [m] with {!group_jsont}. Raises as
    {!array_to_json} does. *)
