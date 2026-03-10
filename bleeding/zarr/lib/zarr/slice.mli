(** {1 Array Slicing}

    Slice specification and operations for Zarr array sub-regions.

    Slices define a selection within each dimension of an array.
    Python-style negative indices are supported (e.g. [-1] means the
    last element).

    {2 Slice types}

    - {!Index}: single element (collapses dimension)
    - {!Range}: half-open range [\[start, stop)]
    - {!RangeFrom}: [\[start, end_of_dim)]
    - {!RangeTo}: [\[0, stop)]
    - {!All}: entire dimension
    - {!Stepped}: [\[start, stop)] with step *)

(** Slice specification for a single dimension. *)
type t =
  | Index of int                  (** Single index (collapses dimension) *)
  | Range of int * int            (** [\[start, stop)], step 1 *)
  | RangeFrom of int              (** [\[start, end)], step 1 *)
  | RangeTo of int                (** [\[0, stop)], step 1 *)
  | All                           (** Entire dimension *)
  | Stepped of int * int * int    (** [\[start, stop, step)] *)

(** {2 Normalisation} *)

val normalise : int -> t -> (int * int * int, string) result
(** [normalise dim_size slice] resolves a slice specification to a
    concrete [(start, stop, step)] triple, handling negative indices
    and bounds clamping. *)

(** {2 Output Shape} *)

val output_shape : int array -> t list -> int array
(** [output_shape shape slices] computes the shape of the sliced
    result.  Dimensions selected by {!Index} are collapsed. *)

(** {2 Iteration} *)

val iter : int array -> t list -> f:(int array -> int array -> unit) -> unit
(** [iter shape slices ~f] calls [f src_idx dst_idx] for each element
    in the slice.

    {b Performance note:} index arrays are mutable and reused across
    iterations -- [f] must not retain references to them.  No
    per-iteration allocation. *)

(** {2 Chunk Intersection} *)

val chunks_for_slice : int array -> int array -> t list -> int array list
(** [chunks_for_slice shape chunk_shape slices] returns the list of
    chunk coordinate arrays that intersect with the slice. *)

val chunk_intersection : int array -> int array -> int array -> t list
  -> (int * int * int * int) array
(** [chunk_intersection shape chunk_shape chunk_coords slices] returns
    [(chunk_offset, output_offset, length, step)] per dimension,
    describing how a chunk intersects with a slice.

    Used by {!Zarr_array} to determine the copy region between a chunk
    and the output/input buffer.  When all steps are 1, the region is
    contiguous and eligible for {!Chunk_data.blit}. *)
