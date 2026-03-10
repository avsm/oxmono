(** {1 Zarr v3 Array Operations}

    Provides chunked array read/write operations over an abstract store.
    Arrays are the primary data-bearing nodes in a Zarr hierarchy: each
    array stores N-dimensional typed data as a grid of compressed chunks.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html}
    Zarr v3.1 spec, "Array" section}.

    {2 Chunk lifecycle}

    - {b Read}: If a chunk key is not present in the store, the chunk is
      filled with the array's fill value (Zarr v3.1 spec requirement).
    - {b Write}: A chunk is read-modify-written on every element or slice
      set.  There is no in-memory chunk cache (callers should add one if
      needed).
    - {b Delete}: Only the metadata key is erased; chunk data keys are
      not automatically cleaned up.

    {2 Performance}

    Slice operations use {!Chunk_data.blit} for contiguous (step-1)
    regions, avoiding per-element polymorphic variant dispatch.  This is
    the fast path for the common case of reading/writing rectangular
    sub-regions. *)

(** Array handle containing store, path, metadata, and codec chain. *)
type 'store t

(** Store operations required by the array functor.

    This is a minimal subset of {!Zarr_store.STORE} -- only the
    operations needed for array I/O. *)
module type STORE_OPS = sig
  type t
  val get : t -> string -> bytes option
  val set : t -> string -> bytes -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
end

(** Functor creating array operations over a specific store type. *)
module Make (S : STORE_OPS) : sig
  type store = S.t
  type nonrec t = S.t t

  (** {3 Lifecycle} *)

  val create :
    store -> path:string -> shape:int array -> chunks:int array ->
    dtype:Zarr_dtype.t -> ?fill_value:Zarr_fill.t ->
    ?codecs:Zarr_codec.codec_spec list -> unit -> t
  (** [create store ~path ~shape ~chunks ~dtype ()] creates a new array,
      writes its [zarr.json] metadata, and builds the codec chain.

      @param fill_value Default value for uninitialised regions.
        Defaults to zero/false for the data type.
      @param codecs Codec pipeline. Defaults to
        [bytes \{ endian = Some Little \}].
      @raise Failure on invalid parameters.
      @raise Zarr_codec.Codec_error on invalid codec chain. *)

  val open_ : store -> path:string -> t
  (** [open_ store ~path] opens an existing array by reading its
      [zarr.json] metadata and building the codec chain.
      @raise Failure if the array is not found or metadata is invalid.
      @raise Zarr_codec.Codec_error on invalid codec chain. *)

  (** {3 Properties} *)

  val metadata : t -> Zarr_metadata.array_metadata
  val shape : t -> int array
  val dtype : t -> Zarr_dtype.t
  val chunks : t -> int array

  (** {3 Element Access} *)

  val get : t -> int array -> Chunk_data.value
  (** [get arr idx] reads a single element at array index [idx].
      Loads and decodes the containing chunk.

      {b Performance:} This loads the entire chunk for every call.
      For bulk reads, prefer {!get_slice}. *)

  val set : t -> int array -> Chunk_data.value -> unit
  (** [set arr idx value] writes a single element. Loads the chunk,
      updates the element, and writes the chunk back.

      {b Performance:} This is a read-modify-write of the entire chunk.
      For bulk writes, prefer {!set_slice}. *)

  (** {3 Slice Access} *)

  val get_slice : t -> Slice.t list -> Chunk_data.t
  (** [get_slice arr slices] reads a rectangular region defined by
      [slices] (one per dimension). Loads all intersecting chunks.

      Uses {!Chunk_data.blit} for contiguous (step-1) regions. *)

  val set_slice : t -> Slice.t list -> Chunk_data.t -> unit
  (** [set_slice arr slices data] writes a rectangular region.
      Updates all intersecting chunks.

      Uses {!Chunk_data.blit} for contiguous (step-1) regions. *)

  (** {3 Attributes} *)

  val attrs : t -> Jsont.json
  (** [attrs arr] returns the user-defined attributes. Returns JSON
      null if the array has no attributes. *)

  val set_attrs : t -> Jsont.json -> unit
  (** [set_attrs arr json] updates the array attributes in the store.

      {b Note:} This writes the updated metadata to the store but does
      not update the in-memory [arr] handle.  Re-open the array to see
      the updated attributes via {!attrs}. *)

  (** {3 Cleanup} *)

  val delete : t -> unit
  (** [delete arr] erases the array metadata from the store. *)
end
