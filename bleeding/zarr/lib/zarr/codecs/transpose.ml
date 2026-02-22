(** Transpose codec - permutes array dimensions *)

(** Create a transpose codec with given dimension order *)
let create order : Codec_intf.array_to_array =
  (* Calculate inverse permutation for decoding *)
  let inverse_order =
    let inv = Array.make (Array.length order) 0 in
    Array.iteri (fun i v -> inv.(v) <- i) order;
    inv
  in
  {
    encode = (fun arr -> Ndarray.transpose arr order);
    decode = (fun arr -> Ndarray.transpose arr inverse_order);
    compute_output_shape = (fun shape ->
      Array.map (fun i -> shape.(i)) order
    );
  }

(** Create a column-major (Fortran) order transpose for a given number of dimensions *)
let create_fortran_order ndim : Codec_intf.array_to_array =
  let order = Array.init ndim (fun i -> ndim - 1 - i) in
  create order

(** Create a row-major (C) order transpose (identity for C-layout arrays) *)
let create_c_order ndim : Codec_intf.array_to_array =
  let order = Array.init ndim (fun i -> i) in
  create order
