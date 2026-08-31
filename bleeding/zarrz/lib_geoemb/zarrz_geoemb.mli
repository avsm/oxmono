(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The geoembeddings Zarr convention.

    Typed codecs for the [attributes] object of a Zarr V3 group that
    declares the geoembeddings convention, which describes a store of
    geospatial embedding vectors and the model that produced them. See
    {{:https://github.com/geo-embeddings/embeddings-zarr-convention}the
    convention repository}, commit
    [0655212938f36351245dbd3e5e8868f811d43663]. That repository has no
    [v1] tag, although its schema and specification both cite URLs
    under [refs/tags/v1].

    This module maps to and from {!Jsont.json} and depends on nothing
    else, so it is usable without the Zarr core. A caller that has a
    group at hand reaches the typed attributes with

    {[
      match Zarrz.Group.attributes g with
      | None -> None
      | Some j -> Zarrz_geoemb.of_attributes j
    ]}

    Decoding enforces the schema: the required members, the string
    enumerations, the lower bounds on [geoemb:dimensions] and
    [geoemb:gsd], the registration of the convention in
    [zarr_conventions], and the chip layout that a chip store must
    carry. Encoding does not recheck the bounds, so a record built by
    hand with an out of range field encodes to JSON the schema
    rejects.

    Every object in the convention keeps the members it does not model
    in an [unknown] field, in document order, so that a decode followed
    by an encode loses nothing. Real stores need this: the TESSERA
    store carries a [geoemb:stretch] member the schema never
    defines. *)

(** Convention registration entries.

    An entry of the generic [zarr_conventions] array, which names every
    convention the group declares. The schema constrains only the entry
    that identifies this convention, through [contains], so the other
    entries of the array decode with whatever members they carry. Every
    member is therefore optional here. *)
module Convention : sig
  type t = {
    uuid : string option;  (** Permanent identifier of the convention. *)
    name : string option;  (** Key prefix, ["geoemb:"] for this one. *)
    description : string option;
    spec_url : string option;
    schema_url : string option;
    unknown : Jsont.mem list;  (** Unmodelled members, kept verbatim. *)
  }
  (** The type for a convention registration entry. *)

  val geoemb_uuid : string
  (** [geoemb_uuid] is ["61c12cc5-0e28-4056-999a-480cf3fb7e4c"], the
      identifier of the geoembeddings convention. The schema pins it
      with [const] and the specification repeats it, so it is normative
      rather than a property of any one store. *)

  val geoemb_name : string
  (** [geoemb_name] is ["geoemb:"], the key prefix of the convention,
      also pinned with [const]. *)

  val geoemb_description : string
  (** [geoemb_description] is the [const] description of the
      convention. *)

  val geoemb_spec_url : string
  (** [geoemb_spec_url] is the [const] URL of the specification. *)

  val geoemb_schema_url : string
  (** [geoemb_schema_url] is the [const] URL of the JSON schema. *)

  val geoemb : t
  (** [geoemb] is the registration entry of the geoembeddings
      convention, with all five members set to the constants above. It
      is the entry the specification tells a writer to emit. *)

  val v :
    ?uuid:string ->
    ?name:string ->
    ?description:string ->
    ?spec_url:string ->
    ?schema_url:string ->
    unit ->
    t
  (** [v ()] is an entry with the given members and no unknown ones. *)

  val is_geoemb : t -> bool
  (** [is_geoemb c] is [true] when [c] identifies the geoembeddings
      convention, that is when its [uuid] is {!geoemb_uuid}, or when it
      has no [uuid] and its [name] is {!geoemb_name}. A [uuid] that is
      present and different loses, since the uuid is what permanently
      identifies a convention. *)

  val find : uuid:string -> t list -> t option
  (** [find ~uuid l] is the first entry of [l] whose [uuid] is [uuid],
      or [None]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON type of a registration entry. Every member is
      optional and unknown members are kept. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality, ignoring the source layout of
      the unknown members. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] prints [c] as the JSON {!jsont} encodes it to. *)
end

(** Chip layouts.

    How a chip store divided its source imagery. Required when
    [geoemb:type] is ["chip"]. *)
module Chip_layout : sig
  type layout = Regular_grid | Irregular
  (** The type for the [layout_type] enumeration. *)

  type t = {
    layout_type : layout;
    chip_size : int * int;
        (** Chip height and width in pixels, both at least 1. *)
    stride : (int * int) option;
        (** Stride between chips as [(y, x)]. Absent means [chip_size],
            see {!effective_stride}. *)
    grid_id : string option;
    grid_definition : string option;
    unknown : Jsont.mem list;
  }
  (** The type for a chip layout. *)

  val v :
    ?stride:int * int ->
    ?grid_id:string ->
    ?grid_definition:string ->
    layout_type:layout ->
    chip_size:int * int ->
    unit ->
    t
  (** [v ~layout_type ~chip_size ()] is a chip layout with no unknown
      members. *)

  val effective_stride : t -> int * int
  (** [effective_stride c] is [c.stride] if present and [c.chip_size]
      otherwise, which is the default the specification gives. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON type of a chip layout. Decoding requires
      [layout_type] and [chip_size], requires each array to hold
      exactly two integers that are at least 1, and errors on a
      [layout_type] outside the enumeration. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] prints [c] as the JSON {!jsont} encodes it to. *)
end

(** Quantization.

    How float embeddings were compressed to the stored data type, and
    what a reader needs to recover them. *)
module Quantization : sig
  (** Dequantization scales.

      The schema gives [scale] as a [oneOf] of two objects discriminated
      by a [type] member, and the specification documents both. The set
      is closed and small, so this is a variant rather than typed common
      members beside a raw {!Jsont.json}. A [type] outside the two is a
      decode error, which is what the [oneOf] asks for. Members inside
      each case that the schema does not define are still kept, so an
      extension of a known case survives a round trip. *)
  module Scale : sig
    type nodata = Number of float | String of string
    (** The type for the [nodata] member, which the schema gives as
        either a number or a string. Stores write the string ["+inf"]. *)

    type scalar = {
      scale : float;  (** Scale factor. *)
      offset : float;  (** Offset, [0.] when the member is absent. *)
      unknown : Jsont.mem list;
    }
    (** The type for a global linear scale. Dequantize with
        [value = quantized * scale + offset]. *)

    type array_ref = {
      array_name : string;
          (** Name of the Zarr array holding the per pixel scales. *)
      nodata : nodata option;
      unknown : Jsont.mem list;
    }
    (** The type for a per pixel scale held in a separate array.
        Dequantize with
        [value.(..,y,x) = quantized.(..,y,x) * scales.(..,y,x)]. *)

    type t = Scalar of scalar | Array of array_ref
    (** The type for a scale. *)

    val scalar : ?offset:float -> float -> t
    (** [scalar s] is [Scalar] with scale [s] and offset [offset],
        which defaults to [0.]. *)

    val array_ref : ?nodata:nodata -> string -> t
    (** [array_ref n] is [Array] with array name [n]. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON type of a scale, a case object on the
        [type] member. Encoding writes [offset] even when it is [0.],
        so a scalar scale that omitted it in the source gains it. *)

    val equal : t -> t -> bool
    (** [equal a b] is structural equality. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf s] prints [s] as the JSON {!jsont} encodes it to. *)
  end

  type t = {
    method_ : string;
        (** The [method] member, for example ["linear"] or
            ["per_pixel_scale"]. The schema leaves it open. *)
    original_dtype : string;
    quantized_dtype : string option;
    scale : Scale.t option;
    link : string option;  (** URL of a codebook or lookup table. *)
    unknown : Jsont.mem list;
  }
  (** The type for quantization details. *)

  val v :
    ?quantized_dtype:string ->
    ?scale:Scale.t ->
    ?link:string ->
    method_:string ->
    original_dtype:string ->
    unit ->
    t
  (** [v ~method_ ~original_dtype ()] is quantization details with no
      unknown members. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON type of quantization details. Decoding
      requires [method] and [original_dtype]. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf q] prints [q] as the JSON {!jsont} encodes it to. *)
end

type kind = Pixel | Chip
(** The type for the [geoemb:type] enumeration. [Pixel] is one
    embedding vector per pixel, [Chip] is one per image patch. *)

type spatial_layout = Utm_zones | Global
(** The type for the [geoemb:spatial_layout] enumeration. [Utm_zones]
    is one group per UTM zone, named [utm01] to [utm60]. [Global] is a
    single array in a global CRS. *)

type t = {
  conventions : Convention.t list;
      (** The [zarr_conventions] array, every entry of it. At least one
          satisfies {!Convention.is_geoemb}. *)
  kind : kind;  (** The [geoemb:type] member. *)
  dimensions : int;
      (** The [geoemb:dimensions] member, at least 1. Length of one
          embedding vector. *)
  model : string;  (** URL of the encoder model. *)
  source_data : string list;
      (** URLs of the source datasets, at least one. *)
  data_type : string;
      (** The [geoemb:data_type] member, for example ["float32"] or
          ["int8"]. The schema leaves it an open string rather than an
          enumeration, so it stays one here. *)
  gsd : float option;  (** Ground sample distance in metres, above 0. *)
  chip_layout : Chip_layout.t option;
      (** Present whenever [kind] is [Chip]. *)
  quantization : Quantization.t option;
  spatial_layout : spatial_layout option;
  build_version : string option;
      (** Version of the software that built the store. *)
  benchmark : string list option;
      (** URLs of benchmark evaluation results. [None] and [Some []]
          are distinct, the first being an absent member. *)
  unknown : Jsont.mem list;
      (** Every member of the attributes object this convention does
          not define, in document order, kept so that a decode followed
          by an encode is lossless. Attributes carry the members of the
          other conventions the group declares, such as [proj:code] and
          [spatial:bbox], and these land here. *)
}
(** The type for the attributes of a geoembeddings group. Field names
    drop the [geoemb:] prefix that the JSON member names carry. *)

val jsont : t Jsont.t
(** [jsont] is the JSON type of the attributes object.

    Decoding requires [zarr_conventions], [geoemb:type],
    [geoemb:dimensions], [geoemb:model], [geoemb:source_data] and
    [geoemb:data_type]. It also errors when [zarr_conventions] has no
    entry satisfying {!Convention.is_geoemb}, when [geoemb:type] is
    ["chip"] and [geoemb:chip_layout] is absent, when
    [geoemb:dimensions] is below 1, when [geoemb:source_data] is empty,
    when [geoemb:gsd] is not above 0, and when [geoemb:type] or
    [geoemb:spatial_layout] is outside its enumeration.

    Encoding writes the members in the order above, omits the absent
    optional ones, and appends the kept unknown members. *)

val of_json : Jsont.json -> (t, string) result
(** [of_json j] decodes [j] with {!jsont}. *)

val to_json : t -> Jsont.json
(** [to_json t] encodes [t] with {!jsont}.

    @raise Invalid_argument if [t] cannot be encoded. No value this
    module can decode does that. *)

val of_attributes : Jsont.json -> (t, string) result option
(** [of_attributes j] is [None] when [j] is not an object, or has no
    [zarr_conventions] array holding an entry that names the
    geoembeddings convention, and [Some (of_json j)] otherwise.

    The test walks generic JSON and reads at most two members of each
    array entry, so probing the attributes of a group that belongs to
    another convention costs nothing. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality of [a] and [b], ignoring the
    source layout of the unknown members. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as the JSON {!jsont} encodes it to. *)
