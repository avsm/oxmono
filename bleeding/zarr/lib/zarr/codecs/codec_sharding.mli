(** {1 Sharding Indexed Codec}

    Bundles multiple inner chunks in a single shard (storage object).

    {b Zarr v3.1 spec, "sharding_indexed" codec:}

    This is an array-to-bytes codec that splits an outer chunk (the shard)
    into a regular grid of inner chunks.  Each inner chunk is independently
    encoded through its own codec chain.  A binary index stores
    [(offset, nbytes)] pairs as uint64 little-endian values, allowing
    efficient partial reads.

    {2 Binary format}

    - Successive encoded inner chunks (with allowed gaps)
    - An index at the start or end of the shard (configured)
    - Index shape: [\[chunks_per_dim_0, ..., chunks_per_dim_n, 2\]]
    - Empty chunks: both offset and nbytes set to [2{^64}-1]

    {2 Invariants}

    - Inner chunk shape must {b evenly divide} the outer chunk shape
      (validated at codec construction time).
    - Index codecs must be deterministic (recommended:
      [bytes(little) + crc32c]).
    - The index is encoded through the full index codec chain (including
      the array-to-bytes codec), not just the bytes-to-bytes portion. *)

val empty_marker : int64
(** Sentinel value ([Int64.minus_one], i.e. [0xFFFFFFFFFFFFFFFF])
    marking an uninitialised inner chunk in the shard index. *)

val total_inner_chunks : int array -> int array -> int
(** [total_inner_chunks outer_shape inner_shape] returns the total number
    of inner chunks in a shard (product of per-dimension counts). *)

val create :
  outer_chunk_shape:int array ->
  inner_chunk_shape:int array ->
  inner_chain:Zarr_codec.codec_chain ->
  index_chain:Zarr_codec.codec_chain ->
  index_location:Zarr_codec.index_location ->
  dtype:Zarr_dtype.t ->
  fill_value:Zarr_fill.t ->
  Zarr_codec.array_to_bytes
(** [create ~outer_chunk_shape ~inner_chunk_shape ~inner_chain
    ~index_chain ~index_location ~dtype ~fill_value] builds a sharding
    codec with pre-built codec chains.

    @param fill_value Used for uninitialised inner chunks during decode.
    The Zarr v3.1 spec requires that empty inner chunks (marked with the
    [2{^64}-1] sentinel) are filled with the array's fill value, not
    necessarily zero. *)
