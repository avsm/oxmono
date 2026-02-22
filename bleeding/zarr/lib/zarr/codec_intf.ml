(** Codec interface types for Zarr v3 *)

open Ztypes

module D = Ztypes.Dtype

(** Array-to-array codec operations *)
type array_to_array = {
  encode : Ndarray.t -> Ndarray.t;
  decode : Ndarray.t -> Ndarray.t;
  compute_output_shape : int array -> int array;
}

(** Array-to-bytes codec operations *)
type array_to_bytes = {
  encode : Ndarray.t -> bytes;
  decode : int array -> D.t -> bytes -> Ndarray.t;
}

(** Bytes-to-bytes codec operations *)
type bytes_to_bytes = {
  encode : bytes -> bytes;
  decode : bytes -> bytes result;
  compute_encoded_size : int -> int option;  (* None if size is variable *)
}

(** A complete codec chain *)
type codec_chain = {
  array_to_array : array_to_array list;
  array_to_bytes : array_to_bytes;
  bytes_to_bytes : bytes_to_bytes list;
}
