(** {1 Chunk Grid}

    Regular chunk grid for Zarr v3 arrays.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/chunk-grids/regular-grid/index.html}
    Zarr v3.1 spec, "Regular Grid"}.

    All chunks have identical shape.  The grid is aligned to the origin
    (chunk at grid index [(0, 0, ...)] starts at array position
    [(0, 0, ...)]).

    {b Border chunks:} When the array dimension is not evenly divisible
    by the chunk size, edge chunks extend beyond the array bounds.
    {!chunk_size} computes the actual (clipped) shape of a chunk. *)

(** Chunk grid configuration.  Only regular grids are supported. *)
type t = Regular of { chunk_shape : int array }

(** {2 Grid Properties} *)

val chunk_shape : t -> int array
(** [chunk_shape grid] returns the shape of each chunk. *)

val num_chunks : t -> int array -> int array
(** [num_chunks grid array_shape] returns the number of chunks per
    dimension: [ceil(array_shape.(i) / chunk_shape.(i))]. *)

val chunk_for_index : t -> int array -> int array
(** [chunk_for_index grid idx] returns the chunk coordinates containing
    array element [idx]:
    [chunk_coords.(i) = idx.(i) / chunk_shape.(i)]. *)

val chunk_bounds : t -> int array -> int array -> (int * int) array
(** [chunk_bounds grid array_shape chunk_coords] returns the
    [(start, stop)] array coordinates for a chunk, clipped to the array
    shape. *)

val chunk_size : t -> int array -> int array -> int array
(** [chunk_size grid array_shape chunk_coords] returns the actual shape
    of a chunk, accounting for border chunks that may be smaller than
    [chunk_shape]. *)

val is_valid_chunk : t -> int array -> int array -> bool
(** [is_valid_chunk grid array_shape chunk_coords] checks whether the
    chunk coordinates are within the grid bounds. *)

val iter_chunks : t -> int array -> (int array -> unit) -> unit
(** [iter_chunks grid array_shape f] calls [f chunk_coords] for every
    chunk in the grid, in C-order (last dimension varies fastest). *)

val total_chunks : t -> int array -> int
(** [total_chunks grid array_shape] returns the total number of chunks
    (product of {!num_chunks}). *)

(** {2 JSON Serialization}

    JSON format:
    {v {"name": "regular", "configuration": {"chunk_shape": [d0, d1, ...]}} v} *)

val of_json : Jsont.json -> t
(** @raise Failure if the JSON is not a valid chunk grid. *)

val to_json : t -> Jsont.json
