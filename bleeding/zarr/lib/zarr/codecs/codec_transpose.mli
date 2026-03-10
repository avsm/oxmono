(** {1 Transpose Codec}

    Permutes array dimensions.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/codecs/transpose/index.html}
    Zarr v3.1 spec, "transpose" codec}.  This is an {b array-to-array}
    codec.

    The [order] parameter is a permutation of [0..n-1] mapping new
    dimension [i] to old dimension [order.(i)]:
    - [B_shape.(i) = A_shape.(order.(i))]
    - [B[pos] = A[perm_pos]] where [pos.(i) = perm_pos.(order.(i))]

    {b Note:} The Zarr v3.1 spec no longer supports ["C"] or ["F"]
    shorthand constants; an explicit permutation array is always required.
    Use {!create_fortran_order} and {!create_c_order} as convenience
    constructors. *)

val create : int array -> Zarr_codec.array_to_array
(** [create order] builds a transpose codec with the given permutation.
    Precomputes the inverse permutation for efficient decode. *)

val create_fortran_order : int -> Zarr_codec.array_to_array
(** [create_fortran_order ndim] builds a column-major (Fortran) order
    transpose for [ndim] dimensions: [order = \[ndim-1; ...; 1; 0\]]. *)

val create_c_order : int -> Zarr_codec.array_to_array
(** [create_c_order ndim] builds a row-major (C) identity transpose
    for [ndim] dimensions: [order = \[0; 1; ...; ndim-1\]]. *)
