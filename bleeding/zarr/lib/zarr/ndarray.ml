(** N-dimensional array abstraction using Bigarray *)

open Bigarray

module D = Ztypes.Dtype
module FV = Ztypes.Fill_value
module E = Ztypes.Endianness

(** Existential type to wrap different bigarray kinds *)
type t =
  | Int8_signed of (int, int8_signed_elt, c_layout) Genarray.t
  | Int8_unsigned of (int, int8_unsigned_elt, c_layout) Genarray.t
  | Int16_signed of (int, int16_signed_elt, c_layout) Genarray.t
  | Int16_unsigned of (int, int16_unsigned_elt, c_layout) Genarray.t
  | Int32 of (int32, int32_elt, c_layout) Genarray.t
  | Int64 of (int64, int64_elt, c_layout) Genarray.t
  | Float32 of (float, float32_elt, c_layout) Genarray.t
  | Float64 of (float, float64_elt, c_layout) Genarray.t
  | Complex32 of (Complex.t, complex32_elt, c_layout) Genarray.t
  | Complex64 of (Complex.t, complex64_elt, c_layout) Genarray.t
  | Char of (char, int8_unsigned_elt, c_layout) Genarray.t

(** Get the shape of an ndarray *)
let shape = function
  | Int8_signed arr -> Genarray.dims arr
  | Int8_unsigned arr -> Genarray.dims arr
  | Int16_signed arr -> Genarray.dims arr
  | Int16_unsigned arr -> Genarray.dims arr
  | Int32 arr -> Genarray.dims arr
  | Int64 arr -> Genarray.dims arr
  | Float32 arr -> Genarray.dims arr
  | Float64 arr -> Genarray.dims arr
  | Complex32 arr -> Genarray.dims arr
  | Complex64 arr -> Genarray.dims arr
  | Char arr -> Genarray.dims arr

(** Get the number of dimensions *)
let ndim arr = Array.length (shape arr)

(** Get the total number of elements *)
let numel arr =
  Array.fold_left ( * ) 1 (shape arr)

(** Create a new ndarray filled with zeros *)
let create dtype dims =
  let arr = match dtype with
    | D.Bool ->
      let a = Genarray.create int8_unsigned c_layout dims in
      Genarray.fill a 0; Int8_unsigned a
    | D.Int8 ->
      let a = Genarray.create int8_signed c_layout dims in
      Genarray.fill a 0; Int8_signed a
    | D.Int16 ->
      let a = Genarray.create int16_signed c_layout dims in
      Genarray.fill a 0; Int16_signed a
    | D.Int32 ->
      let a = Genarray.create int32 c_layout dims in
      Genarray.fill a 0l; Int32 a
    | D.Int64 ->
      let a = Genarray.create int64 c_layout dims in
      Genarray.fill a 0L; Int64 a
    | D.Uint8 ->
      let a = Genarray.create int8_unsigned c_layout dims in
      Genarray.fill a 0; Int8_unsigned a
    | D.Uint16 ->
      let a = Genarray.create int16_unsigned c_layout dims in
      Genarray.fill a 0; Int16_unsigned a
    | D.Uint32 ->
      let a = Genarray.create int64 c_layout dims in
      Genarray.fill a 0L; Int64 a
    | D.Uint64 ->
      let a = Genarray.create int64 c_layout dims in
      Genarray.fill a 0L; Int64 a
    | D.Float16 ->
      let a = Genarray.create float32 c_layout dims in
      Genarray.fill a 0.0; Float32 a
    | D.Float32 ->
      let a = Genarray.create float32 c_layout dims in
      Genarray.fill a 0.0; Float32 a
    | D.Float64 ->
      let a = Genarray.create float64 c_layout dims in
      Genarray.fill a 0.0; Float64 a
    | D.Complex64 ->
      let a = Genarray.create complex32 c_layout dims in
      Genarray.fill a { Complex.re = 0.0; im = 0.0 }; Complex32 a
    | D.Complex128 ->
      let a = Genarray.create complex64 c_layout dims in
      Genarray.fill a { Complex.re = 0.0; im = 0.0 }; Complex64 a
    | D.Raw _ ->
      let a = Genarray.create char c_layout dims in
      Genarray.fill a '\x00'; Char a
  in
  arr

(** Fill an ndarray with a constant value *)
let fill arr value =
  match arr, value with
  | Int8_signed a, FV.Int i -> Genarray.fill a (Stdlib.Int64.to_int i)
  | Int8_unsigned a, FV.Uint u -> Genarray.fill a (Stdlib.Int64.to_int u)
  | Int8_unsigned a, FV.Bool b -> Genarray.fill a (if b then 1 else 0)
  | Int16_signed a, FV.Int i -> Genarray.fill a (Stdlib.Int64.to_int i)
  | Int16_unsigned a, FV.Uint u -> Genarray.fill a (Stdlib.Int64.to_int u)
  | Int32 a, FV.Int i -> Genarray.fill a (Stdlib.Int64.to_int32 i)
  | Int64 a, FV.Int i -> Genarray.fill a i
  | Int64 a, FV.Uint u -> Genarray.fill a u
  | Float32 a, FV.Float f -> Genarray.fill a f
  | Float32 a, FV.NaN -> Genarray.fill a Float.nan
  | Float32 a, FV.Infinity -> Genarray.fill a Float.infinity
  | Float32 a, FV.NegInfinity -> Genarray.fill a Float.neg_infinity
  | Float64 a, FV.Float f -> Genarray.fill a f
  | Float64 a, FV.NaN -> Genarray.fill a Float.nan
  | Float64 a, FV.Infinity -> Genarray.fill a Float.infinity
  | Float64 a, FV.NegInfinity -> Genarray.fill a Float.neg_infinity
  | Complex32 a, FV.Complex (re, im) -> Genarray.fill a { Complex.re; im }
  | Complex64 a, FV.Complex (re, im) -> Genarray.fill a { Complex.re; im }
  | Char a, FV.Raw b when Bytes.length b >= 1 -> Genarray.fill a (Bytes.get b 0)
  | _ -> ()

(** Convert array index to linear offset (C-layout: row-major) *)
let index_to_offset dims idx =
  let ndim = Array.length dims in
  let offset = ref 0 in
  let stride = ref 1 in
  for i = ndim - 1 downto 0 do
    offset := !offset + idx.(i) * !stride;
    stride := !stride * dims.(i)
  done;
  !offset

(** Convert linear offset to array index (C-layout: row-major) *)
let offset_to_index dims offset =
  let ndim = Array.length dims in
  let idx = Array.make ndim 0 in
  let remaining = ref offset in
  for i = ndim - 1 downto 0 do
    idx.(i) <- !remaining mod dims.(i);
    remaining := !remaining / dims.(i)
  done;
  idx

(** Get element at given index *)
let get arr idx =
  match arr with
  | Int8_signed a -> `Int (Genarray.get a idx)
  | Int8_unsigned a -> `Int (Genarray.get a idx)
  | Int16_signed a -> `Int (Genarray.get a idx)
  | Int16_unsigned a -> `Int (Genarray.get a idx)
  | Int32 a -> `Int32 (Genarray.get a idx)
  | Int64 a -> `Int64 (Genarray.get a idx)
  | Float32 a -> `Float (Genarray.get a idx)
  | Float64 a -> `Float (Genarray.get a idx)
  | Complex32 a -> `Complex (Genarray.get a idx)
  | Complex64 a -> `Complex (Genarray.get a idx)
  | Char a -> `Char (Genarray.get a idx)

(** Set element at given index *)
let set arr idx value =
  match arr, value with
  | Int8_signed a, `Int v -> Genarray.set a idx v
  | Int8_unsigned a, `Int v -> Genarray.set a idx v
  | Int16_signed a, `Int v -> Genarray.set a idx v
  | Int16_unsigned a, `Int v -> Genarray.set a idx v
  | Int32 a, `Int32 v -> Genarray.set a idx v
  | Int64 a, `Int64 v -> Genarray.set a idx v
  | Float32 a, `Float v -> Genarray.set a idx v
  | Float64 a, `Float v -> Genarray.set a idx v
  | Complex32 a, `Complex v -> Genarray.set a idx v
  | Complex64 a, `Complex v -> Genarray.set a idx v
  | Char a, `Char v -> Genarray.set a idx v
  | _ -> ()

(** Convert ndarray to bytes with given endianness *)
let to_bytes endian arr =
  let dims = shape arr in
  let n = numel arr in
  match arr with
  | Int8_signed a ->
    let buf = Bytes.create n in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      Bytes.set buf i (Char.chr (Genarray.get a idx land 0xFF))
    done;
    buf
  | Int8_unsigned a ->
    let buf = Bytes.create n in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      Bytes.set buf i (Char.chr (Genarray.get a idx land 0xFF))
    done;
    buf
  | Int16_signed a ->
    let buf = Bytes.create (n * 2) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      (match endian with
       | E.Little -> Bytes.set_int16_le buf (i * 2) v
       | E.Big -> Bytes.set_int16_be buf (i * 2) v)
    done;
    buf
  | Int16_unsigned a ->
    let buf = Bytes.create (n * 2) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      (match endian with
       | E.Little -> Bytes.set_uint16_le buf (i * 2) v
       | E.Big -> Bytes.set_uint16_be buf (i * 2) v)
    done;
    buf
  | Int32 a ->
    let buf = Bytes.create (n * 4) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      (match endian with
       | E.Little -> Bytes.set_int32_le buf (i * 4) v
       | E.Big -> Bytes.set_int32_be buf (i * 4) v)
    done;
    buf
  | Int64 a ->
    let buf = Bytes.create (n * 8) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      (match endian with
       | E.Little -> Bytes.set_int64_le buf (i * 8) v
       | E.Big -> Bytes.set_int64_be buf (i * 8) v)
    done;
    buf
  | Float32 a ->
    let buf = Bytes.create (n * 4) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      let bits = Stdlib.Int32.bits_of_float v in
      (match endian with
       | E.Little -> Bytes.set_int32_le buf (i * 4) bits
       | E.Big -> Bytes.set_int32_be buf (i * 4) bits)
    done;
    buf
  | Float64 a ->
    let buf = Bytes.create (n * 8) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      let bits = Stdlib.Int64.bits_of_float v in
      (match endian with
       | E.Little -> Bytes.set_int64_le buf (i * 8) bits
       | E.Big -> Bytes.set_int64_be buf (i * 8) bits)
    done;
    buf
  | Complex32 a ->
    let buf = Bytes.create (n * 8) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      let re_bits = Stdlib.Int32.bits_of_float v.Complex.re in
      let im_bits = Stdlib.Int32.bits_of_float v.Complex.im in
      (match endian with
       | E.Little ->
         Bytes.set_int32_le buf (i * 8) re_bits;
         Bytes.set_int32_le buf (i * 8 + 4) im_bits
       | E.Big ->
         Bytes.set_int32_be buf (i * 8) re_bits;
         Bytes.set_int32_be buf (i * 8 + 4) im_bits)
    done;
    buf
  | Complex64 a ->
    let buf = Bytes.create (n * 16) in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      let v = Genarray.get a idx in
      let re_bits = Stdlib.Int64.bits_of_float v.Complex.re in
      let im_bits = Stdlib.Int64.bits_of_float v.Complex.im in
      (match endian with
       | E.Little ->
         Bytes.set_int64_le buf (i * 16) re_bits;
         Bytes.set_int64_le buf (i * 16 + 8) im_bits
       | E.Big ->
         Bytes.set_int64_be buf (i * 16) re_bits;
         Bytes.set_int64_be buf (i * 16 + 8) im_bits)
    done;
    buf
  | Char a ->
    let buf = Bytes.create n in
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      Bytes.set buf i (Genarray.get a idx)
    done;
    buf

(** Create ndarray from bytes with given endianness *)
let of_bytes dtype endian dims bytes =
  let arr = create dtype dims in
  let n = numel arr in
  (match arr with
   | Int8_signed a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let v = Char.code (Bytes.get bytes i) in
       let v = if v > 127 then v - 256 else v in
       Genarray.set a idx v
     done
   | Int8_unsigned a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       Genarray.set a idx (Char.code (Bytes.get bytes i))
     done
   | Int16_signed a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let v = match endian with
         | E.Little -> Bytes.get_int16_le bytes (i * 2)
         | E.Big -> Bytes.get_int16_be bytes (i * 2)
       in
       Genarray.set a idx v
     done
   | Int16_unsigned a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let v = match endian with
         | E.Little -> Bytes.get_uint16_le bytes (i * 2)
         | E.Big -> Bytes.get_uint16_be bytes (i * 2)
       in
       Genarray.set a idx v
     done
   | Int32 a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let v = match endian with
         | E.Little -> Bytes.get_int32_le bytes (i * 4)
         | E.Big -> Bytes.get_int32_be bytes (i * 4)
       in
       Genarray.set a idx v
     done
   | Int64 a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let v = match endian with
         | E.Little -> Bytes.get_int64_le bytes (i * 8)
         | E.Big -> Bytes.get_int64_be bytes (i * 8)
       in
       Genarray.set a idx v
     done
   | Float32 a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let bits = match endian with
         | E.Little -> Bytes.get_int32_le bytes (i * 4)
         | E.Big -> Bytes.get_int32_be bytes (i * 4)
       in
       Genarray.set a idx (Stdlib.Int32.float_of_bits bits)
     done
   | Float64 a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let bits = match endian with
         | E.Little -> Bytes.get_int64_le bytes (i * 8)
         | E.Big -> Bytes.get_int64_be bytes (i * 8)
       in
       Genarray.set a idx (Stdlib.Int64.float_of_bits bits)
     done
   | Complex32 a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let re_bits = match endian with
         | E.Little -> Bytes.get_int32_le bytes (i * 8)
         | E.Big -> Bytes.get_int32_be bytes (i * 8)
       in
       let im_bits = match endian with
         | E.Little -> Bytes.get_int32_le bytes (i * 8 + 4)
         | E.Big -> Bytes.get_int32_be bytes (i * 8 + 4)
       in
       Genarray.set a idx {
         Complex.re = Stdlib.Int32.float_of_bits re_bits;
         im = Stdlib.Int32.float_of_bits im_bits
       }
     done
   | Complex64 a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       let re_bits = match endian with
         | E.Little -> Bytes.get_int64_le bytes (i * 16)
         | E.Big -> Bytes.get_int64_be bytes (i * 16)
       in
       let im_bits = match endian with
         | E.Little -> Bytes.get_int64_le bytes (i * 16 + 8)
         | E.Big -> Bytes.get_int64_be bytes (i * 16 + 8)
       in
       Genarray.set a idx {
         Complex.re = Stdlib.Int64.float_of_bits re_bits;
         im = Stdlib.Int64.float_of_bits im_bits
       }
     done
   | Char a ->
     for i = 0 to n - 1 do
       let idx = offset_to_index dims i in
       Genarray.set a idx (Bytes.get bytes i)
     done);
  arr

(** Create a sub-view of an ndarray (slicing) *)
let sub arr starts lengths =
  match arr with
  | Int8_signed a ->
    Int8_signed (Genarray.sub_left a starts.(0) lengths.(0))
  | Int8_unsigned a ->
    Int8_unsigned (Genarray.sub_left a starts.(0) lengths.(0))
  | Int16_signed a ->
    Int16_signed (Genarray.sub_left a starts.(0) lengths.(0))
  | Int16_unsigned a ->
    Int16_unsigned (Genarray.sub_left a starts.(0) lengths.(0))
  | Int32 a ->
    Int32 (Genarray.sub_left a starts.(0) lengths.(0))
  | Int64 a ->
    Int64 (Genarray.sub_left a starts.(0) lengths.(0))
  | Float32 a ->
    Float32 (Genarray.sub_left a starts.(0) lengths.(0))
  | Float64 a ->
    Float64 (Genarray.sub_left a starts.(0) lengths.(0))
  | Complex32 a ->
    Complex32 (Genarray.sub_left a starts.(0) lengths.(0))
  | Complex64 a ->
    Complex64 (Genarray.sub_left a starts.(0) lengths.(0))
  | Char a ->
    Char (Genarray.sub_left a starts.(0) lengths.(0))

(** Copy data from one ndarray to another at specified offsets *)
let blit ~src ~src_offset ~dst ~dst_offset ~shape:region_shape =
  let src_dims = shape src in
  let _dst_dims = shape dst in
  let ndim = Array.length src_dims in

  (* Iterate over all elements in the region *)
  let rec iter idx dim =
    if dim = ndim then begin
      (* Calculate source and destination indices *)
      let src_idx = Array.mapi (fun i v -> v + src_offset.(i)) idx in
      let dst_idx = Array.mapi (fun i v -> v + dst_offset.(i)) idx in
      (* Copy element *)
      let value = get src src_idx in
      set dst dst_idx value
    end else begin
      for i = 0 to region_shape.(dim) - 1 do
        idx.(dim) <- i;
        iter idx (dim + 1)
      done
    end
  in
  iter (Array.make ndim 0) 0

(** Check if two ndarrays are equal *)
let equal a b =
  let shape_a = shape a in
  let shape_b = shape b in
  if shape_a <> shape_b then false
  else begin
    let n = numel a in
    let dims = shape_a in
    try
      for i = 0 to n - 1 do
        let idx = offset_to_index dims i in
        let va = get a idx in
        let vb = get b idx in
        if va <> vb then raise Exit
      done;
      true
    with Exit -> false
  end

(** Reshape an ndarray (must have same total elements) *)
let reshape arr new_dims =
  let old_n = numel arr in
  let new_n = Array.fold_left ( * ) 1 new_dims in
  if old_n <> new_n then
    invalid_arg "reshape: incompatible dimensions"
  else
    match arr with
    | Int8_signed a -> Int8_signed (Bigarray.reshape a new_dims)
    | Int8_unsigned a -> Int8_unsigned (Bigarray.reshape a new_dims)
    | Int16_signed a -> Int16_signed (Bigarray.reshape a new_dims)
    | Int16_unsigned a -> Int16_unsigned (Bigarray.reshape a new_dims)
    | Int32 a -> Int32 (Bigarray.reshape a new_dims)
    | Int64 a -> Int64 (Bigarray.reshape a new_dims)
    | Float32 a -> Float32 (Bigarray.reshape a new_dims)
    | Float64 a -> Float64 (Bigarray.reshape a new_dims)
    | Complex32 a -> Complex32 (Bigarray.reshape a new_dims)
    | Complex64 a -> Complex64 (Bigarray.reshape a new_dims)
    | Char a -> Char (Bigarray.reshape a new_dims)

(** Transpose array according to given permutation *)
let rec transpose arr perm =
  let dims = shape arr in
  let new_dims = Array.map (fun i -> dims.(i)) perm in
  let n = numel arr in

  (* Create new array and copy with transposed indices *)
  let result = create (dtype_of_ndarray arr) new_dims in
  for i = 0 to n - 1 do
    let old_idx = offset_to_index dims i in
    let new_idx = Array.map (fun p -> old_idx.(p)) perm in
    let value = get arr old_idx in
    set result new_idx value
  done;
  result

(** Get the dtype of an ndarray *)
and dtype_of_ndarray = function
  | Int8_signed _ -> D.Int8
  | Int8_unsigned _ -> D.Uint8
  | Int16_signed _ -> D.Int16
  | Int16_unsigned _ -> D.Uint16
  | Int32 _ -> D.Int32
  | Int64 _ -> D.Int64
  | Float32 _ -> D.Float32
  | Float64 _ -> D.Float64
  | Complex32 _ -> D.Complex64
  | Complex64 _ -> D.Complex128
  | Char _ -> D.Raw 8

(** Initialize an ndarray with a function *)
let init dtype dims f =
  let arr = create dtype dims in
  let n = numel arr in
  for i = 0 to n - 1 do
    let idx = offset_to_index dims i in
    let value = f idx in
    set arr idx value
  done;
  arr

(** Create ndarray from a list of values *)
let of_list dtype dims values =
  let arr = create dtype dims in
  let n = numel arr in
  let values = Array.of_list values in
  if Array.length values <> n then
    invalid_arg "of_list: wrong number of elements"
  else begin
    for i = 0 to n - 1 do
      let idx = offset_to_index dims i in
      set arr idx values.(i)
    done;
    arr
  end
