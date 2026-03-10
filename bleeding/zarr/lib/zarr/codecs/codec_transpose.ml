(** Transpose codec -- permutes array dimensions.

    See Zarr v3.1 spec, "transpose" codec. This is an array-to-array codec.
    The [order] parameter maps new dimension [i] to old dimension [order.(i)]. *)

(** [create order] builds a transpose codec with the given permutation. *)
let create order : Zarr_codec.array_to_array =
  let inverse =
    let inv = Array.make (Array.length order) 0 in
    Array.iteri (fun i v -> inv.(v) <- i) order;
    inv
  in
  { encode = (fun chunk -> Chunk_data.transpose chunk order);
    decode = (fun chunk -> Chunk_data.transpose chunk inverse);
    compute_output_shape = (fun shape -> Array.map (fun i -> shape.(i)) order) }

(** [create_fortran_order ndim] builds a column-major (Fortran) order
    transpose for [ndim] dimensions. *)
let create_fortran_order ndim : Zarr_codec.array_to_array =
  create (Array.init ndim (fun i -> ndim - 1 - i))

(** [create_c_order ndim] builds a row-major (C) order transpose
    (identity for C-layout arrays). *)
let create_c_order ndim : Zarr_codec.array_to_array =
  create (Array.init ndim Fun.id)
