(** Bytes-backed N-dimensional chunk data.

    Replaces the old Bigarray-based Ndarray with a flat [bytes] buffer
    storing elements in C-order (row-major) at native byte order.

    The bytes codec converts between this internal representation and the
    wire format, handling endianness. For matching endianness the conversion
    is a simple [Bytes.copy]. *)

(** A typed, shaped view over a flat byte buffer. *)
type t = {
  buf : bytes;
  dtype : Zarr_dtype.t;
  shape : int array;
  strides : int array;  (** Byte strides per dimension, C-order *)
  numel : int;           (** Total number of elements *)
}

(** {2 Stride computation} *)

(** [compute_strides dtype shape] returns C-order byte strides.
    For shape [\[|d0; d1; d2|\]] with element size [s], strides are
    [\[|d1*d2*s; d2*s; s|\]]. *)
let compute_strides dtype shape =
  let ndim = Array.length shape in
  let elem_size = Zarr_dtype.byte_size dtype in
  let strides = Array.make ndim elem_size in
  for i = ndim - 2 downto 0 do
    strides.(i) <- strides.(i + 1) * shape.(i + 1)
  done;
  strides

(** [byte_offset strides idx] computes the byte offset for multi-dimensional
    index [idx]. This is the hot inner function -- kept simple for inlining. *)
let[@inline] byte_offset strides idx =
  let mutable off = 0 in
  for i = 0 to Array.length idx - 1 do
    off <- off + idx.(i) * Array.unsafe_get strides i
  done;
  off

(** {2 Creation} *)

(** [create dtype shape fv] allocates a new chunk filled with fill value [fv]. *)
let create dtype shape fv =
  let numel = Array.fold_left ( * ) 1 shape in
  let elem_size = Zarr_dtype.byte_size dtype in
  let buf = Bytes.create (numel * elem_size) in
  Zarr_fill.fill_bytes dtype fv buf;
  let strides = compute_strides dtype shape in
  { buf; dtype; shape; strides; numel }

(** [create_zero dtype shape] allocates a zero-filled chunk. *)
let create_zero dtype shape =
  let numel = Array.fold_left ( * ) 1 shape in
  let elem_size = Zarr_dtype.byte_size dtype in
  let buf = Bytes.make (numel * elem_size) '\x00' in
  let strides = compute_strides dtype shape in
  { buf; dtype; shape; strides; numel }

(** {2 Properties} *)

let shape t = t.shape
let dtype t = t.dtype
let ndim t = Array.length t.shape
let numel t = t.numel
let byte_length t = Bytes.length t.buf

open Stdlib_upstream_compatible

(** {2 Direct buffer access} *)

(** [buf t] returns the underlying byte buffer.  Callers must not modify
    the buffer unless they know what they are doing.  Exposed for
    zero-copy codec paths (e.g. CRC32C over a range). *)
let buf t = t.buf

(** {2 Bytes conversion}

    The internal representation uses native (little-endian on most platforms)
    byte order. When the target endianness matches, conversion is a copy.
    Otherwise we swap bytes element-by-element. *)

(** [to_bytes endian t] serialises the chunk to bytes in the given byte order.

    Raw types are opaque byte sequences and are never byte-swapped,
    regardless of the requested endianness (Zarr v3.1 spec). *)
let to_bytes endian t =
  let len = Bytes.length t.buf in
  if len = 0 then Bytes.empty
  else
    let elem_size = Zarr_dtype.byte_size t.dtype in
    if elem_size <= 1 || endian = Zarr_dtype.Little
       || not (Zarr_dtype.requires_endianness t.dtype) then
      (* Native order, single-byte, or opaque raw type: just copy *)
      Bytes.copy t.buf
    else begin
      (* Big-endian output: copy then swap each element in place *)
      let result = Bytes.copy t.buf in
      let n = t.numel in
      (match elem_size with
       | 2 ->
         for i = 0 to n - 1 do
           let off = i * 2 in
           let v = Bytes.get_int16_le result off in
           Bytes.set_int16_be result off v
         done
       | 4 ->
         for i = 0 to n - 1 do
           let off = i * 4 in
           let v = Bytes.get_int32_le result off in
           Bytes.set_int32_be result off v
         done
       | 8 ->
         for i = 0 to n - 1 do
           let off = i * 8 in
           let v = Bytes.get_int64_le result off in
           Bytes.set_int64_be result off v
         done
       | _ ->
         (* Generic: reverse each element's bytes *)
         let tmp = Bytes.create elem_size in
         for i = 0 to n - 1 do
           let off = i * elem_size in
           for j = 0 to elem_size - 1 do
             Bytes.set tmp j (Bytes.get result (off + elem_size - 1 - j))
           done;
           Bytes.blit tmp 0 result off elem_size
         done);
      result
    end

(** [of_bytes dtype endian shape buf] creates chunk data from serialised bytes.

    If the endianness matches native order, the buffer is used directly
    (copied). Otherwise, byte-swapping is performed. *)
let of_bytes dtype endian shape buf =
  let numel = Array.fold_left ( * ) 1 shape in
  let elem_size = Zarr_dtype.byte_size dtype in
  let expected = numel * elem_size in
  if Bytes.length buf < expected then
    invalid_arg "Chunk_data.of_bytes: buffer too short";
  let strides = compute_strides dtype shape in
  if elem_size <= 1 || endian = Zarr_dtype.Little
     || not (Zarr_dtype.requires_endianness dtype) then begin
    let data = Bytes.create expected in
    Bytes.blit buf 0 data 0 expected;
    { buf = data; dtype; shape; strides; numel }
  end else begin
    (* Big-endian input: swap to native (little-endian) *)
    let data = Bytes.create expected in
    Bytes.blit buf 0 data 0 expected;
    let n = numel in
    (match elem_size with
     | 2 ->
       for i = 0 to n - 1 do
         let off = i * 2 in
         let v = Bytes.get_int16_be data off in
         Bytes.set_int16_le data off v
       done
     | 4 ->
       for i = 0 to n - 1 do
         let off = i * 4 in
         let v = Bytes.get_int32_be data off in
         Bytes.set_int32_le data off v
       done
     | 8 ->
       for i = 0 to n - 1 do
         let off = i * 8 in
         let v = Bytes.get_int64_be data off in
         Bytes.set_int64_le data off v
       done
     | _ ->
       let tmp = Bytes.create elem_size in
       for i = 0 to n - 1 do
         let off = i * elem_size in
         for j = 0 to elem_size - 1 do
           Bytes.set tmp j (Bytes.get data (off + elem_size - 1 - j))
         done;
         Bytes.blit tmp 0 data off elem_size
       done);
    { buf = data; dtype; shape; strides; numel }
  end

(** {2 Element access}

    These read/write individual elements at a multi-dimensional index.
    The dtype determines the access width. *)

let[@inline] get_int8 t idx =
  let off = byte_offset t.strides idx in
  let v = Bytes.get_uint8 t.buf off in
  if v > 127 then v - 256 else v

let[@inline] get_uint8 t idx =
  Bytes.get_uint8 t.buf (byte_offset t.strides idx)

let[@inline] get_int16 t idx =
  Bytes.get_int16_le t.buf (byte_offset t.strides idx)

let[@inline] get_uint16 t idx =
  Bytes.get_uint16_le t.buf (byte_offset t.strides idx)

let[@inline] get_int32 t idx =
  Bytes.get_int32_le t.buf (byte_offset t.strides idx)

let[@inline] get_int64 t idx =
  Bytes.get_int64_le t.buf (byte_offset t.strides idx)

let[@inline] get_float32 t idx =
  Int32.float_of_bits (Bytes.get_int32_le t.buf (byte_offset t.strides idx))

let[@inline] get_float64 t idx =
  Int64.float_of_bits (Bytes.get_int64_le t.buf (byte_offset t.strides idx))

let[@inline] set_int8 t idx v =
  Bytes.set_uint8 t.buf (byte_offset t.strides idx) (v land 0xFF)

let[@inline] set_uint8 t idx v =
  Bytes.set_uint8 t.buf (byte_offset t.strides idx) v

let[@inline] set_int16 t idx v =
  Bytes.set_int16_le t.buf (byte_offset t.strides idx) v

let[@inline] set_uint16 t idx v =
  Bytes.set_uint16_le t.buf (byte_offset t.strides idx) v

let[@inline] set_int32 t idx v =
  Bytes.set_int32_le t.buf (byte_offset t.strides idx) v

let[@inline] set_int64 t idx v =
  Bytes.set_int64_le t.buf (byte_offset t.strides idx) v

let[@inline] set_float32 t idx v =
  Bytes.set_int32_le t.buf (byte_offset t.strides idx) (Int32.bits_of_float v)

let[@inline] set_float64 t idx v =
  Bytes.set_int64_le t.buf (byte_offset t.strides idx) (Int64.bits_of_float v)

(** {2 Unboxed element access}

    Accessors that avoid boxing [int32], [int64], and [float] intermediates.
    These use the OxCaml unboxed types ([int32#], [int64#], [float#]) so that
    the entire get/set path stays in registers for tight inner loops. *)

let[@inline] get_int32_u t idx : int32# =
  Int32_u.of_int32 (Bytes.get_int32_le t.buf (byte_offset t.strides idx))

let[@inline] get_int64_u t idx : int64# =
  Int64_u.of_int64 (Bytes.get_int64_le t.buf (byte_offset t.strides idx))

let[@inline] get_float32_u t idx : float# =
  Float_u.of_float (Int32.float_of_bits (Bytes.get_int32_le t.buf (byte_offset t.strides idx)))

let[@inline] get_float64_u t idx : float# =
  Float_u.of_float (Int64.float_of_bits (Bytes.get_int64_le t.buf (byte_offset t.strides idx)))

let[@inline] set_int32_u t idx (v : int32#) =
  Bytes.set_int32_le t.buf (byte_offset t.strides idx) (Int32_u.to_int32 v)

let[@inline] set_int64_u t idx (v : int64#) =
  Bytes.set_int64_le t.buf (byte_offset t.strides idx) (Int64_u.to_int64 v)

let[@inline] set_float32_u t idx (v : float#) =
  Bytes.set_int32_le t.buf (byte_offset t.strides idx)
    (Int32.bits_of_float (Float_u.to_float v))

let[@inline] set_float64_u t idx (v : float#) =
  Bytes.set_int64_le t.buf (byte_offset t.strides idx)
    (Int64.bits_of_float (Float_u.to_float v))

(** {2 Generic element access}

    Polymorphic variants for dtype-agnostic operations. *)

(** Element value, tagged by dtype family. *)
type value =
  [ `Bool of bool
  | `Int of int
  | `Int32 of int32
  | `Int64 of int64
  | `Float of float
  | `Char of char ]

let get t idx =
  let off = byte_offset t.strides idx in
  match t.dtype with
  | Bool -> `Bool (Bytes.get_uint8 t.buf off <> 0)
  | Int8 ->
    let v = Bytes.get_uint8 t.buf off in
    `Int (if v > 127 then v - 256 else v)
  | Uint8 -> `Int (Bytes.get_uint8 t.buf off)
  | Int16 -> `Int (Bytes.get_int16_le t.buf off)
  | Uint16 -> `Int (Bytes.get_uint16_le t.buf off)
  | Int32 -> `Int32 (Bytes.get_int32_le t.buf off)
  | Uint32 -> `Int32 (Bytes.get_int32_le t.buf off)
  | Int64 -> `Int64 (Bytes.get_int64_le t.buf off)
  | Uint64 -> `Int64 (Bytes.get_int64_le t.buf off)
  | Float32 -> `Float (Int32.float_of_bits (Bytes.get_int32_le t.buf off))
  | Float64 -> `Float (Int64.float_of_bits (Bytes.get_int64_le t.buf off))
  | Raw _ -> `Char (Bytes.get t.buf off)

let set t idx (value : value) =
  let off = byte_offset t.strides idx in
  match t.dtype, value with
  | Bool, `Bool b -> Bytes.set_uint8 t.buf off (if b then 1 else 0)
  | (Int8 | Uint8), `Int v -> Bytes.set_uint8 t.buf off (v land 0xFF)
  | (Int16 | Uint16), `Int v -> Bytes.set_int16_le t.buf off v
  | Int32, `Int32 v -> Bytes.set_int32_le t.buf off v
  | Uint32, `Int32 v -> Bytes.set_int32_le t.buf off v
  | (Int64 | Uint64), `Int64 v -> Bytes.set_int64_le t.buf off v
  | Float32, `Float v -> Bytes.set_int32_le t.buf off (Stdlib.Int32.bits_of_float v)
  | Float64, `Float v -> Bytes.set_int64_le t.buf off (Stdlib.Int64.bits_of_float v)
  | Raw _, `Char c -> Bytes.set t.buf off c
  | _, _ -> ()

(** {2 Bulk operations} *)

(** [blit ~src ~src_off ~dst ~dst_off ~shape] copies a rectangular region
    between two chunks. Uses [Bytes.blit] for the innermost contiguous
    dimension when offsets are aligned. *)
let blit ~src ~src_off ~dst ~dst_off ~shape:region =
  let ndim = Array.length region in
  if ndim = 0 then ()
  else begin
    let elem_size = Zarr_dtype.byte_size src.dtype in
    let inner_bytes = region.(ndim - 1) * elem_size in
    (* Recursive iteration over all but innermost dimension *)
    let src_idx = Array.make ndim 0 in
    let dst_idx = Array.make ndim 0 in
    let rec iter dim =
      if dim = ndim - 1 then begin
        (* Innermost: use Bytes.blit *)
        let s_off = byte_offset src.strides src_idx in
        let d_off = byte_offset dst.strides dst_idx in
        Bytes.blit src.buf s_off dst.buf d_off inner_bytes
      end else begin
        for i = 0 to region.(dim) - 1 do
          src_idx.(dim) <- src_off.(dim) + i;
          dst_idx.(dim) <- dst_off.(dim) + i;
          iter (dim + 1)
        done
      end
    in
    (* Set up innermost dimension offsets *)
    src_idx.(ndim - 1) <- src_off.(ndim - 1);
    dst_idx.(ndim - 1) <- dst_off.(ndim - 1);
    iter 0
  end

(** [transpose t perm] returns a new chunk with permuted dimensions.

    For each element, computes the source index by applying the inverse
    permutation, reads the value from [t], and writes to the new location. *)
let transpose t perm =
  let old_shape = t.shape in
  let ndim = Array.length old_shape in
  let new_shape = Array.map (fun i -> old_shape.(i)) perm in
  let result = create_zero t.dtype new_shape in
  let n = t.numel in
  (* Precompute inverse permutation *)
  let inv_perm = Array.make ndim 0 in
  Array.iteri (fun i v -> inv_perm.(v) <- i) perm;
  (* Iterate over all elements in new array *)
  let new_idx = Array.make ndim 0 in
  let old_idx = Array.make ndim 0 in
  for linear = 0 to n - 1 do
    (* Convert linear index to new multi-index *)
    let mutable rem = linear in
    for d = ndim - 1 downto 0 do
      new_idx.(d) <- rem mod new_shape.(d);
      rem <- rem / new_shape.(d)
    done;
    (* Apply inverse permutation to get source index *)
    for d = 0 to ndim - 1 do
      old_idx.(d) <- new_idx.(inv_perm.(d))
    done;
    (* Copy element bytes directly *)
    let src_off = byte_offset t.strides old_idx in
    let dst_off = byte_offset result.strides new_idx in
    let elem_size = Zarr_dtype.byte_size t.dtype in
    Bytes.blit t.buf src_off result.buf dst_off elem_size
  done;
  result

(** [equal a b] checks structural equality of two chunks. *)
let equal a b =
  a.dtype = b.dtype && a.shape = b.shape && a.buf = b.buf

(** [index_to_offset shape idx] converts multi-dimensional index to linear
    element offset (C-order, not byte offset). *)
let index_to_offset shape idx =
  let ndim = Array.length shape in
  let mutable offset = 0 in
  let mutable stride = 1 in
  for i = ndim - 1 downto 0 do
    offset <- offset + idx.(i) * stride;
    stride <- stride * shape.(i)
  done;
  offset

(** [offset_to_index shape offset] converts linear element offset to
    multi-dimensional index (C-order). *)
let offset_to_index shape offset =
  let ndim = Array.length shape in
  let idx = Array.make ndim 0 in
  let mutable remaining = offset in
  for i = ndim - 1 downto 0 do
    idx.(i) <- remaining mod shape.(i);
    remaining <- remaining / shape.(i)
  done;
  idx
