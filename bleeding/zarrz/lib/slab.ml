(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Fu = Stdlib_upstream_compatible.Float_u
module I64u = Stdlib_upstream_compatible.Int64_u
module I8u = Stdlib_stable.Int8_u
module Ia = Stdlib_stable.Iarray

(* Raw native endian scalar loads and stores on a bigstring, at a byte
   offset. The trailing [#] on the primitive name is what makes the
   result unboxed. The [u] before it selects the unchecked variant: the
   bounds check happens once in the accessor, not twice. *)

external ld_i8 : Base_bigstring.t -> int -> int8#
  = "%caml_bigstring_geti8u#"

external st_i8 : Base_bigstring.t -> int -> int8# -> unit
  = "%caml_bigstring_set8u#"

external ld_i16 : Base_bigstring.t -> int -> int16#
  = "%caml_bigstring_geti16u#"

external st_i16 : Base_bigstring.t -> int -> int16# -> unit
  = "%caml_bigstring_set16u#"

external ld_i32 : Base_bigstring.t -> int -> int32#
  = "%caml_bigstring_get32u#"

external st_i32 : Base_bigstring.t -> int -> int32# -> unit
  = "%caml_bigstring_set32u#"

external ld_i64 : Base_bigstring.t -> int -> int64#
  = "%caml_bigstring_get64u#"

external st_i64 : Base_bigstring.t -> int -> int64# -> unit
  = "%caml_bigstring_set64u#"

external ld_f32 : Base_bigstring.t -> int -> float32#
  = "%caml_bigstring_getf32u#"

external st_f32 : Base_bigstring.t -> int -> float32# -> unit
  = "%caml_bigstring_setf32u#"

(* The half formats do their field arithmetic on a tagged int, so they
   load the sixteen stored bits through the tagged primitive and mask
   them to an unsigned value rather than through the signed [int16#]
   the accessor modules use. *)

external ld_u16 : Base_bigstring.t -> int -> int = "%caml_bigstring_get16u"

external st_u16 : Base_bigstring.t -> int -> int -> unit
  = "%caml_bigstring_set16u"

(* There is no unboxed 64 bit float load. Going through the integer load
   and a bit cast costs one register move, and the boxes the two
   conversions name are erased by the optimiser: the zero-alloc probe
   under the release-check profile is what proves it. *)

let[@inline] ld_f64 b off : float# =
  Fu.of_float (I64u.float_of_bits (ld_i64 b off))

let[@inline] st_f64 b off (v : float#) =
  st_i64 b off (I64u.bits_of_float (Fu.to_float v))

type t = {
  buf : Base_bigstring.t;
  dtype : Dtype.t;
  shape : int iarray;
  n : int; (* product of [shape], cached for the bounds check *)
}

let[@cold] bad_dtype (expected : Dtype.t) (actual : Dtype.t) : 'a =
  invalid_arg
    ("Zarrz.Slab: slab holds " ^ Dtype.name actual ^ ", accessor needs "
   ^ Dtype.name expected)

let[@cold] bad_index i n : 'a =
  invalid_arg
    ("Zarrz.Slab: index " ^ string_of_int i ^ " out of bounds for "
   ^ string_of_int n ^ " elements")

let[@cold] bad_rank got want : 'a =
  invalid_arg
    ("Zarrz.Slab: rank " ^ string_of_int got ^ " slab, need rank "
   ^ string_of_int want)

let[@cold] bad_coord () : 'a =
  invalid_arg "Zarrz.Slab: coordinate out of bounds"

(* Every dtype an accessor serves is a constant constructor, so it is an
   immediate and physical equality decides it exactly. A [Raw] slab is a
   block and compares unequal to all of them, which is the answer we
   want. Structural equality would be a C call and would not be
   zero-alloc. *)
let[@inline] guard_dtype t (expected : Dtype.t) =
  if not (t.dtype == expected) then bad_dtype expected t.dtype

let[@inline] guard t (expected : Dtype.t) i =
  guard_dtype t expected;
  if i < 0 || i >= t.n then bad_index i t.n

let dtype t = t.dtype
let shape t = t.shape
let rank t = Ia.length t.shape
let num_elements t = t.n
let bigstring t = t.buf

let index2 t i j =
  let r = Ia.length t.shape in
  if r <> 2 then bad_rank r 2;
  let d0 = Ia.unsafe_get t.shape 0 in
  let d1 = Ia.unsafe_get t.shape 1 in
  if i < 0 || i >= d0 || j < 0 || j >= d1 then bad_coord ();
  (i * d1) + j

let index3 t i j k =
  let r = Ia.length t.shape in
  if r <> 3 then bad_rank r 3;
  let d0 = Ia.unsafe_get t.shape 0 in
  let d1 = Ia.unsafe_get t.shape 1 in
  let d2 = Ia.unsafe_get t.shape 2 in
  if i < 0 || i >= d0 || j < 0 || j >= d1 || k < 0 || k >= d2 then bad_coord ();
  (((i * d1) + j) * d2) + k

let elements_of_shape shape =
  let r = Ia.length shape in
  let acc = ref 1 in
  for d = 0 to r - 1 do
    let dim = Ia.get shape d in
    if dim < 0 then invalid_arg "Zarrz.Slab: negative dimension";
    if dim <> 0 && !acc > max_int / dim then
      invalid_arg "Zarrz.Slab: shape overflows an int";
    acc := !acc * dim
  done;
  !acc

let byte_size dtype n =
  let size = Dtype.size dtype in
  if size < 0 then invalid_arg "Zarrz.Slab: negative element size";
  if size <> 0 && n > max_int / size then
    invalid_arg "Zarrz.Slab: buffer overflows an int";
  n * size

let create dtype shape =
  let n = elements_of_shape shape in
  { buf = Base_bigstring.create (byte_size dtype n); dtype; shape; n }

let of_bigstring dtype shape buf =
  let n = elements_of_shape shape in
  let want = byte_size dtype n in
  let got = Base_bigstring.length buf in
  if got <> want then
    invalid_arg
      ("Zarrz.Slab: buffer is " ^ string_of_int got ^ " bytes, shape needs "
     ^ string_of_int want);
  { buf; dtype; shape; n }

let fill t elem =
  let size = Dtype.size t.dtype in
  if String.length elem <> size then
    invalid_arg
      ("Zarrz.Slab: fill pattern is " ^ string_of_int (String.length elem)
     ^ " bytes, " ^ Dtype.name t.dtype ^ " needs " ^ string_of_int size);
  let total = t.n * size in
  if total > 0 then
    if size = 1 then Base_bigstring.memset t.buf ~pos:0 ~len:total elem.[0]
    else begin
      Base_bigstring.From_string.blit ~src:elem ~src_pos:0 ~dst:t.buf ~dst_pos:0
        ~len:size;
      (* Doubling blit: each pass copies everything written so far, so
         the buffer fills in log2 (total / size) memcpys. *)
      let done_ = ref size in
      while !done_ < total do
        let len = min !done_ (total - !done_) in
        Base_bigstring.blit ~src:t.buf ~src_pos:0 ~dst:t.buf ~dst_pos:!done_
          ~len;
        done_ := !done_ + len
      done
    end

(* Half precision conversions.

   Both formats are widened and narrowed with integer arithmetic on the
   IEEE 754 fields, which keeps every accessor zero-alloc and makes the
   rounding explicit. [round_shift x n] is [x / 2 ** n] rounded to
   nearest with ties to even, for [1 <= n <= 62] and non-negative [x]. *)

let[@inline] round_shift x n =
  let r = x lsr n in
  let rem = x land ((1 lsl n) - 1) in
  let half = 1 lsl (n - 1) in
  if rem > half || (rem = half && r land 1 = 1) then r + 1 else r

(* [make_float sign exp mant] is the binary64 with those three fields.
   The exponent is shifted into place in [int64#] because a biased
   exponent above 1023 puts a bit past the 62 an OCaml int carries. *)
let[@inline] make_float sign exp mant : float# =
  Fu.of_float
    (I64u.float_of_bits
       (I64u.logor
          (I64u.shift_left (I64u.of_int sign) 63)
          (I64u.logor
             (I64u.shift_left (I64u.of_int exp) 52)
             (I64u.of_int mant))))

(* [f16_to_float h] widens the binary16 in the low 16 bits of [h]. *)
let[@inline] f16_to_float h : float# =
  let sign = (h lsr 15) land 1 in
  let e = (h lsr 10) land 0x1f in
  let m = h land 0x3ff in
  if e = 0 then
    if m = 0 then make_float sign 0 0
    else begin
      (* Subnormal: shift the mantissa up until it is normal, paying for
         it in the exponent. *)
      let mutable m = m in
      let mutable ex = -14 in
      while m < 0x400 do
        m <- m lsl 1;
        ex <- ex - 1
      done;
      make_float sign (ex + 1023) ((m land 0x3ff) lsl 42)
    end
  else if e = 31 then make_float sign 0x7ff (m lsl 42)
  else make_float sign (e - 15 + 1023) (m lsl 42)

(* [float_to_f16 v] narrows [v] to the binary16 in the low 16 bits of the
   result, rounding to nearest with ties to even. *)
let[@inline] float_to_f16 (v : float#) =
  let b = I64u.bits_of_float (Fu.to_float v) in
  let sign = I64u.to_int (I64u.shift_right_logical b 63) land 1 in
  let e = I64u.to_int (I64u.shift_right_logical b 52) land 0x7ff in
  let m = I64u.to_int b land 0xf_ffff_ffff_ffff in
  let s = sign lsl 15 in
  if e = 0x7ff then
    if m = 0 then s lor 0x7c00
    else begin
      (* Keep the leading mantissa bits, and force a non-zero mantissa so
         the result stays a NaN. *)
      let hm = m lsr 42 in
      s lor 0x7c00 lor (if hm = 0 then 0x200 else hm)
    end
  else if e = 0 then s (* a binary64 subnormal is far below 2 ** -25 *)
  else begin
    let ex = e - 1023 in
    if ex < -25 then s
    else if ex < -14 then s lor round_shift ((1 lsl 52) lor m) (28 - ex)
    else begin
      let mutable ex = ex in
      let mutable q = round_shift ((1 lsl 52) lor m) 42 in
      if q >= 0x800 then begin
        ex <- ex + 1;
        q <- q lsr 1
      end;
      if ex + 15 >= 31 then s lor 0x7c00
      else s lor ((ex + 15) lsl 10) lor (q land 0x3ff)
    end
  end

(* [bf16_to_float h] widens the bfloat16 in the low 16 bits of [h]. Its
   fields are those of a binary32 with the low 16 mantissa bits cut. *)
let[@inline] bf16_to_float h : float# =
  let sign = (h lsr 15) land 1 in
  let e = (h lsr 7) land 0xff in
  let m = h land 0x7f in
  if e = 0 then
    if m = 0 then make_float sign 0 0
    else begin
      let mutable m = m in
      let mutable ex = -126 in
      while m < 0x80 do
        m <- m lsl 1;
        ex <- ex - 1
      done;
      make_float sign (ex + 1023) ((m land 0x7f) lsl 45)
    end
  else if e = 0xff then make_float sign 0x7ff (m lsl 45)
  else make_float sign (e - 127 + 1023) (m lsl 45)

(* [float_to_bf16 v] narrows [v] to the bfloat16 in the low 16 bits of
   the result, rounding to nearest with ties to even. *)
let[@inline] float_to_bf16 (v : float#) =
  let b = I64u.bits_of_float (Fu.to_float v) in
  let sign = I64u.to_int (I64u.shift_right_logical b 63) land 1 in
  let e = I64u.to_int (I64u.shift_right_logical b 52) land 0x7ff in
  let m = I64u.to_int b land 0xf_ffff_ffff_ffff in
  let s = sign lsl 15 in
  if e = 0x7ff then
    if m = 0 then s lor 0x7f80
    else begin
      let bm = m lsr 45 in
      s lor 0x7f80 lor (if bm = 0 then 0x40 else bm)
    end
  else if e = 0 then s
  else begin
    let ex = e - 1023 in
    if ex < -134 then s
    else if ex < -126 then s lor round_shift ((1 lsl 52) lor m) (-81 - ex)
    else begin
      let mutable ex = ex in
      let mutable q = round_shift ((1 lsl 52) lor m) 45 in
      if q >= 0x100 then begin
        ex <- ex + 1;
        q <- q lsr 1
      end;
      if ex + 127 >= 255 then s lor 0x7f80
      else s lor ((ex + 127) lsl 7) lor (q land 0x7f)
    end
  end

module type S_f64 = sig
  val get : t -> int -> float# [@@zero_alloc]
  val unsafe_get : t -> int -> float# [@@zero_alloc]
  val set : t -> int -> float# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> float# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> float# [@@zero_alloc]
  val set2 : t -> int -> int -> float# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> float# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> float# -> unit [@@zero_alloc]
end

module type S_f32 = sig
  val get : t -> int -> float32# [@@zero_alloc]
  val unsafe_get : t -> int -> float32# [@@zero_alloc]
  val set : t -> int -> float32# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> float32# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> float32# [@@zero_alloc]
  val set2 : t -> int -> int -> float32# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> float32# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> float32# -> unit [@@zero_alloc]
end

module type S_i64 = sig
  val get : t -> int -> int64# [@@zero_alloc]
  val unsafe_get : t -> int -> int64# [@@zero_alloc]
  val set : t -> int -> int64# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> int64# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> int64# [@@zero_alloc]
  val set2 : t -> int -> int -> int64# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> int64# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> int64# -> unit [@@zero_alloc]
end

module type S_i32 = sig
  val get : t -> int -> int32# [@@zero_alloc]
  val unsafe_get : t -> int -> int32# [@@zero_alloc]
  val set : t -> int -> int32# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> int32# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> int32# [@@zero_alloc]
  val set2 : t -> int -> int -> int32# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> int32# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> int32# -> unit [@@zero_alloc]
end

module type S_i16 = sig
  val get : t -> int -> int16# [@@zero_alloc]
  val unsafe_get : t -> int -> int16# [@@zero_alloc]
  val set : t -> int -> int16# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> int16# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> int16# [@@zero_alloc]
  val set2 : t -> int -> int -> int16# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> int16# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> int16# -> unit [@@zero_alloc]
end

module type S_i8 = sig
  val get : t -> int -> int8# [@@zero_alloc]
  val unsafe_get : t -> int -> int8# [@@zero_alloc]
  val set : t -> int -> int8# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> int8# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> int8# [@@zero_alloc]
  val set2 : t -> int -> int -> int8# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> int8# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> int8# -> unit [@@zero_alloc]
end

module type S_bool = sig
  val get : t -> int -> bool# [@@zero_alloc]
  val unsafe_get : t -> int -> bool# [@@zero_alloc]
  val set : t -> int -> bool# -> unit [@@zero_alloc]
  val unsafe_set : t -> int -> bool# -> unit [@@zero_alloc]
  val get2 : t -> int -> int -> bool# [@@zero_alloc]
  val set2 : t -> int -> int -> bool# -> unit [@@zero_alloc]
  val get3 : t -> int -> int -> int -> bool# [@@zero_alloc]
  val set3 : t -> int -> int -> int -> bool# -> unit [@@zero_alloc]
end

module type S_c64 = sig
  val get_re : t -> int -> float32# [@@zero_alloc]
  val get_im : t -> int -> float32# [@@zero_alloc]
  val unsafe_get_re : t -> int -> float32# [@@zero_alloc]
  val unsafe_get_im : t -> int -> float32# [@@zero_alloc]
  val set_re : t -> int -> float32# -> unit [@@zero_alloc]
  val set_im : t -> int -> float32# -> unit [@@zero_alloc]
  val unsafe_set_re : t -> int -> float32# -> unit [@@zero_alloc]
  val unsafe_set_im : t -> int -> float32# -> unit [@@zero_alloc]
end

module type S_c128 = sig
  val get_re : t -> int -> float# [@@zero_alloc]
  val get_im : t -> int -> float# [@@zero_alloc]
  val unsafe_get_re : t -> int -> float# [@@zero_alloc]
  val unsafe_get_im : t -> int -> float# [@@zero_alloc]
  val set_re : t -> int -> float# -> unit [@@zero_alloc]
  val set_im : t -> int -> float# -> unit [@@zero_alloc]
  val unsafe_set_re : t -> int -> float# -> unit [@@zero_alloc]
  val unsafe_set_im : t -> int -> float# -> unit [@@zero_alloc]
end

module F64 = struct
  let[@inline] unsafe_get t i = ld_f64 t.buf (i * 8)
  let[@inline] unsafe_set t i v = st_f64 t.buf (i * 8) v

  let get t i =
    guard t Dtype.Float64 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Float64 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Float64;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Float64;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Float64;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Float64;
    unsafe_set t (index3 t i j k) v
end

module F32 = struct
  let[@inline] unsafe_get t i = ld_f32 t.buf (i * 4)
  let[@inline] unsafe_set t i v = st_f32 t.buf (i * 4) v

  let get t i =
    guard t Dtype.Float32 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Float32 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Float32;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Float32;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Float32;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Float32;
    unsafe_set t (index3 t i j k) v
end

module F16 = struct
  let[@inline] unsafe_get t i = f16_to_float (ld_u16 t.buf (i * 2) land 0xffff)
  let[@inline] unsafe_set t i v = st_u16 t.buf (i * 2) (float_to_f16 v)

  let get t i =
    guard t Dtype.Float16 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Float16 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Float16;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Float16;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Float16;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Float16;
    unsafe_set t (index3 t i j k) v
end

module BF16 = struct
  let[@inline] unsafe_get t i = bf16_to_float (ld_u16 t.buf (i * 2) land 0xffff)
  let[@inline] unsafe_set t i v = st_u16 t.buf (i * 2) (float_to_bf16 v)

  let get t i =
    guard t Dtype.Bfloat16 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Bfloat16 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Bfloat16;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Bfloat16;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Bfloat16;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Bfloat16;
    unsafe_set t (index3 t i j k) v
end

module I64 = struct
  let[@inline] unsafe_get t i = ld_i64 t.buf (i * 8)
  let[@inline] unsafe_set t i v = st_i64 t.buf (i * 8) v

  let get t i =
    guard t Dtype.Int64 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Int64 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Int64;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Int64;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Int64;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Int64;
    unsafe_set t (index3 t i j k) v
end

module U64 = struct
  let[@inline] unsafe_get t i = ld_i64 t.buf (i * 8)
  let[@inline] unsafe_set t i v = st_i64 t.buf (i * 8) v

  let get t i =
    guard t Dtype.Uint64 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Uint64 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Uint64;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Uint64;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Uint64;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Uint64;
    unsafe_set t (index3 t i j k) v
end

module I32 = struct
  let[@inline] unsafe_get t i = ld_i32 t.buf (i * 4)
  let[@inline] unsafe_set t i v = st_i32 t.buf (i * 4) v

  let get t i =
    guard t Dtype.Int32 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Int32 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Int32;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Int32;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Int32;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Int32;
    unsafe_set t (index3 t i j k) v
end

module U32 = struct
  let[@inline] unsafe_get t i = ld_i32 t.buf (i * 4)
  let[@inline] unsafe_set t i v = st_i32 t.buf (i * 4) v

  let get t i =
    guard t Dtype.Uint32 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Uint32 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Uint32;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Uint32;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Uint32;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Uint32;
    unsafe_set t (index3 t i j k) v
end

module I16 = struct
  let[@inline] unsafe_get t i = ld_i16 t.buf (i * 2)
  let[@inline] unsafe_set t i v = st_i16 t.buf (i * 2) v

  let get t i =
    guard t Dtype.Int16 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Int16 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Int16;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Int16;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Int16;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Int16;
    unsafe_set t (index3 t i j k) v
end

module U16 = struct
  let[@inline] unsafe_get t i = ld_i16 t.buf (i * 2)
  let[@inline] unsafe_set t i v = st_i16 t.buf (i * 2) v

  let get t i =
    guard t Dtype.Uint16 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Uint16 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Uint16;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Uint16;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Uint16;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Uint16;
    unsafe_set t (index3 t i j k) v
end

module I8 = struct
  let[@inline] unsafe_get t i = ld_i8 t.buf i
  let[@inline] unsafe_set t i v = st_i8 t.buf i v

  let get t i =
    guard t Dtype.Int8 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Int8 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Int8;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Int8;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Int8;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Int8;
    unsafe_set t (index3 t i j k) v
end

module U8 = struct
  let[@inline] unsafe_get t i = ld_i8 t.buf i
  let[@inline] unsafe_set t i v = st_i8 t.buf i v

  let get t i =
    guard t Dtype.Uint8 i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Uint8 i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Uint8;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Uint8;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Uint8;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Uint8;
    unsafe_set t (index3 t i j k) v
end

module Bl = struct
  let[@inline] unsafe_get t i : bool# =
    if I8u.to_int (ld_i8 t.buf i) <> 0 then #true else #false

  let[@inline] unsafe_set t i (v : bool#) =
    st_i8 t.buf i (match v with #true -> #1s | #false -> #0s)

  let get t i =
    guard t Dtype.Bool i;
    unsafe_get t i

  let set t i v =
    guard t Dtype.Bool i;
    unsafe_set t i v

  let get2 t i j =
    guard_dtype t Dtype.Bool;
    unsafe_get t (index2 t i j)

  let set2 t i j v =
    guard_dtype t Dtype.Bool;
    unsafe_set t (index2 t i j) v

  let get3 t i j k =
    guard_dtype t Dtype.Bool;
    unsafe_get t (index3 t i j k)

  let set3 t i j k v =
    guard_dtype t Dtype.Bool;
    unsafe_set t (index3 t i j k) v
end

module C64 = struct
  let[@inline] unsafe_get_re t i = ld_f32 t.buf (i * 8)
  let[@inline] unsafe_get_im t i = ld_f32 t.buf ((i * 8) + 4)
  let[@inline] unsafe_set_re t i v = st_f32 t.buf (i * 8) v
  let[@inline] unsafe_set_im t i v = st_f32 t.buf ((i * 8) + 4) v

  let get_re t i =
    guard t Dtype.Complex64 i;
    unsafe_get_re t i

  let get_im t i =
    guard t Dtype.Complex64 i;
    unsafe_get_im t i

  let set_re t i v =
    guard t Dtype.Complex64 i;
    unsafe_set_re t i v

  let set_im t i v =
    guard t Dtype.Complex64 i;
    unsafe_set_im t i v
end

module C128 = struct
  let[@inline] unsafe_get_re t i = ld_f64 t.buf (i * 16)
  let[@inline] unsafe_get_im t i = ld_f64 t.buf ((i * 16) + 8)
  let[@inline] unsafe_set_re t i v = st_f64 t.buf (i * 16) v
  let[@inline] unsafe_set_im t i v = st_f64 t.buf ((i * 16) + 8) v

  let get_re t i =
    guard t Dtype.Complex128 i;
    unsafe_get_re t i

  let get_im t i =
    guard t Dtype.Complex128 i;
    unsafe_get_im t i

  let set_re t i v =
    guard t Dtype.Complex128 i;
    unsafe_set_re t i v

  let set_im t i v =
    guard t Dtype.Complex128 i;
    unsafe_set_im t i v
end

external ba_reinterpret :
  Base_bigstring.t ->
  ('a, 'b) Bigarray.kind ->
  int array ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t = "zarrz_ba_reinterpret"

let to_genarray (type a b) t (kind : (a, b) Bigarray.kind) :
    (a, b, Bigarray.c_layout) Bigarray.Genarray.t =
  let ok =
    match (kind, t.dtype) with
    | Bigarray.Float64, Dtype.Float64 -> true
    | Bigarray.Float32, Dtype.Float32 -> true
    | Bigarray.Float16, Dtype.Float16 -> true
    | Bigarray.Int64, (Dtype.Int64 | Dtype.Uint64) -> true
    | Bigarray.Int32, (Dtype.Int32 | Dtype.Uint32) -> true
    | Bigarray.Int16_signed, Dtype.Int16 -> true
    | Bigarray.Int16_unsigned, Dtype.Uint16 -> true
    | Bigarray.Int8_signed, Dtype.Int8 -> true
    | Bigarray.Int8_unsigned, Dtype.Uint8 -> true
    | Bigarray.Complex32, Dtype.Complex64 -> true
    | Bigarray.Complex64, Dtype.Complex128 -> true
    | _ -> false
  in
  if not ok then
    invalid_arg
      ("Zarrz.Slab.to_genarray: no bigarray kind of this element size for "
     ^ Dtype.name t.dtype);
  ba_reinterpret t.buf kind (Ia.to_array t.shape)
