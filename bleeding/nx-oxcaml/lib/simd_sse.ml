(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  Distributed under the ISC license, see terms at the end of the file.

  Based on ocaml_simd (https://github.com/janestreet/ocaml_simd)
  Copyright (c) 2025-2026 Jane Street Group, LLC
  Released under the MIT license.
  ---------------------------------------------------------------------------*)

(* x86-64 SSE SIMD — Prefetch, Int64x2, Int32x4, Float64x2, Float32x4 *)

type int64_u = Stdlib_upstream_compatible.Int64_u.t
type int32_u = Stdlib_upstream_compatible.Int32_u.t
type float32_u = Stdlib_stable.Float32_u.t

module Prefetch = struct
  external read : 'a -> (int[@untagged]) -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_high_val_offset_untagged"
    [@@noalloc] [@@builtin]
end

module Int64x2 = struct
  type t = int64x2#

  (* ───── Constants ───── *)

  external const1 : int64_u -> t @@ portable
    = "caml_sse2_unreachable" "caml_int64x2_const1"
    [@@noalloc] [@@builtin]

  let[@inline always] zero () = const1 #0L
  let[@inline always] one () = const1 #1L
  let[@inline always] all_ones () = const1 #0xffffffffffffffffL

  (* ───── Arithmetic ───── *)

  external add : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] neg x = sub (zero ()) x

  (* ───── Bitwise ───── *)

  external bitwise_and : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] bitwise_not x = bitwise_xor x (all_ones ())

  let[@inline always] ( land ) x y = bitwise_and x y
  let[@inline always] ( lor ) x y = bitwise_or x y
  let[@inline always] ( lxor ) x y = bitwise_xor x y

  (* ───── Comparison ───── *)

  external cmpgt : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse42_int64x2_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* ───── Blend ───── *)

  let[@inline always] blendv a b mask =
    bitwise_or (bitwise_and b mask) (bitwise_and a (bitwise_not mask))

  (* ───── Lanes ───── *)

  external low_of : int64_u -> t @@ portable
    = "caml_sse2_unreachable" "caml_int64x2_low_of_int64"
    [@@noalloc] [@@builtin]

  external low_to : t -> int64_u @@ portable
    = "caml_sse2_unreachable" "caml_int64x2_low_to_int64"
    [@@noalloc] [@@builtin]

  external dup : t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse3_vec128_dup_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_64_to_high_64 : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_low_64_to_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] set1 a = dup (low_of a)
  let[@inline always] set a b = low_64_to_high_64 (low_of a) (low_of b)

  (* ───── Casts ───── *)

  external of_float64x2 : float64x2# -> t @@ portable
    = "caml_sse2_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@builtin]

  (* ───── Array ───── *)

  module Array = struct
    external unsafe_get : (int64_u array[@local_opt]) @ read -> idx:int -> t
      = "%caml_unboxed_int64_array_get128u#"

    external unsafe_set : (int64_u array[@local_opt]) -> idx:int -> t -> unit
      = "%caml_unboxed_int64_array_set128u#"
  end
end

module Int32x4 = struct
  type t = int32x4#

  (* ───── Constants ───── *)

  external const1 : int32_u -> t @@ portable
    = "caml_sse2_unreachable" "caml_int32x4_const1"
    [@@noalloc] [@@builtin]

  let[@inline always] zero () = const1 #0l
  let[@inline always] one () = const1 #1l
  let[@inline always] all_ones () = const1 #0xffffffffl

  (* ───── Arithmetic ───── *)

  external add : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_int32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_int32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] neg x = sub (zero ()) x

  external abs : t -> t @@ portable
    = "caml_sse2_unreachable" "caml_ssse3_int32x4_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* ───── Min/Max ───── *)

  external min : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse41_int32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse41_int32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* ───── Bitwise ───── *)

  external bitwise_and : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] bitwise_not x = bitwise_xor x (all_ones ())

  let[@inline always] ( land ) x y = bitwise_and x y
  let[@inline always] ( lor ) x y = bitwise_or x y
  let[@inline always] ( lxor ) x y = bitwise_xor x y

  (* ───── Lanes ───── *)

  external low_of : int32_u -> t @@ portable
    = "caml_sse2_unreachable" "caml_int32x4_low_of_int32"
    [@@noalloc] [@@builtin]

  external low_to : t -> int32_u @@ portable
    = "caml_sse2_unreachable" "caml_int32x4_low_to_int32"
    [@@noalloc] [@@builtin]

  (* [shuffle imm x x] builds each lane from lane [imm lsr (2 * i) land 3] of
     [x]; [imm] must be a constant. *)
  external shuffle : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_shuffle_32"
    [@@noalloc] [@@builtin]

  let[@inline always] dup x = shuffle 0x00 x x

  external interleave_low_32 : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_simd_vec128_interleave_low_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_64 : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_simd_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] dup_lane lane x =
    if lane < 0 || lane > 3 then invalid_arg "Int32x4.dup_lane";
    match lane with
    | 1 -> shuffle 0x55 x x
    | 2 -> shuffle 0xaa x x
    | 3 -> shuffle 0xff x x
    | _ -> shuffle 0x00 x x

  let[@inline always] set1 a = dup (low_of a)

  let[@inline always] set a b c d =
    let a = low_of a in
    let b = low_of b in
    let c = low_of c in
    let d = low_of d in
    let ba = interleave_low_32 a b in
    let dc = interleave_low_32 c d in
    interleave_low_64 ba dc

  (* ───── Casts ───── *)

  external of_float32x4 : float32x4# -> t @@ portable
    = "caml_sse2_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@builtin]

  (* ───── Array ───── *)

  module Array = struct
    external unsafe_get : (int32_u array[@local_opt]) @ read -> idx:int -> t
      = "%caml_unboxed_int32_array_get128u#"

    external unsafe_set : (int32_u array[@local_opt]) -> idx:int -> t -> unit
      = "%caml_unboxed_int32_array_set128u#"
  end
end

module Float64x2 = struct
  type t = float64x2#

  (* ───── Arithmetic ───── *)

  external add : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] mul_add a b c = add (mul a b) c

  external hadd : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse3_float64x2_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] horizontal_add x y = hadd x y

  (* ───── Min/Max ───── *)

  external min : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse2_float64x2_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* ───── Constants ───── *)

  external const1 : float# -> t @@ portable
    = "caml_sse2_unreachable" "caml_float64x2_const1"
    [@@noalloc] [@@builtin]

  let[@inline always] zero () = const1 #0.
  let[@inline always] one () = const1 #1.

  (* ───── Bitwise (for neg/abs) ───── *)

  external of_int64x2 : int64x2# -> t @@ portable
    = "caml_sse2_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@builtin]

  let[@inline always] neg x =
    Int64x2.(bitwise_xor (const1 #0x8000000000000000L) (of_float64x2 x))
    |> of_int64x2

  let[@inline always] abs x =
    Int64x2.(bitwise_and (const1 #0x7fffffffffffffffL) (of_float64x2 x))
    |> of_int64x2

  (* ───── Lanes ───── *)

  external low_of : float# -> t @@ portable
    = "caml_sse2_unreachable" "caml_float64x2_low_of_float"
    [@@noalloc] [@@builtin]

  external low_to : t -> float# @@ portable
    = "caml_sse2_unreachable" "caml_float64x2_low_to_float"
    [@@noalloc] [@@builtin]

  external low_64_to_high_64 : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_low_64_to_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external high_64_to_low_64 : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_vec128_high_64_to_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] set1 a =
    let a = low_of a in
    Int64x2.dup (Int64x2.of_float64x2 a) |> of_int64x2

  let[@inline always] set a b = low_64_to_high_64 (low_of a) (low_of b)
  let[@inline always] extract0 x = low_to x
  let[@inline always] splat x = #(low_to x, low_to (high_64_to_low_64 x x))

  (* ───── Array ───── *)

  module Array = struct
    external unsafe_get : (float# array[@local_opt]) @ read -> idx:int -> t
      = "%caml_unboxed_float_array_get128u#"

    external unsafe_set : (float# array[@local_opt]) -> idx:int -> t -> unit
      = "%caml_unboxed_float_array_set128u#"
  end
end

module Float32x4 = struct
  type t = float32x4#

  (* ───── Arithmetic ───── *)

  external add : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] mul_add a b c = add (mul a b) c

  external hadd : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse3_float32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] horizontal_add x y = hadd x y

  (* ───── Min/Max ───── *)

  external min : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t @@ portable
    = "caml_sse2_unreachable" "caml_sse_float32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* ───── Constants ───── *)

  external const1 : float32_u -> t @@ portable
    = "caml_sse2_unreachable" "caml_float32x4_const1"
    [@@noalloc] [@@builtin]

  let[@inline always] zero () = const1 #0.0s
  let[@inline always] one () = const1 #1.0s

  (* ───── Bitwise (for neg/abs) ───── *)

  external of_int32x4 : int32x4# -> t @@ portable
    = "caml_sse2_unreachable" "caml_vec128_cast"
    [@@noalloc] [@@builtin]

  let[@inline always] neg x =
    Int32x4.(bitwise_xor (const1 #0x80000000l) (of_float32x4 x)) |> of_int32x4

  let[@inline always] abs x =
    Int32x4.(bitwise_and (const1 #0x7fffffffl) (of_float32x4 x)) |> of_int32x4

  (* ───── Lanes ───── *)

  external low_of : float32_u -> t @@ portable
    = "caml_sse2_unreachable" "caml_float32x4_low_of_float32"
    [@@noalloc] [@@builtin]

  external low_to : t -> float32_u @@ portable
    = "caml_sse2_unreachable" "caml_float32x4_low_to_float32"
    [@@noalloc] [@@builtin]

  let[@inline always] set1 a =
    let a = low_of a in
    Int32x4.dup (Int32x4.of_float32x4 a) |> of_int32x4

  let[@inline always] set a b c d =
    let a = Int32x4.of_float32x4 (low_of a) in
    let b = Int32x4.of_float32x4 (low_of b) in
    let c = Int32x4.of_float32x4 (low_of c) in
    let d = Int32x4.of_float32x4 (low_of d) in
    let ba = Int32x4.interleave_low_32 a b in
    let dc = Int32x4.interleave_low_32 c d in
    Int32x4.interleave_low_64 ba dc |> of_int32x4

  let[@inline always] extract0 x = low_to x

  let[@inline always] splat x =
    let as_i = Int32x4.of_float32x4 x in
    let lane1 = Int32x4.dup_lane 1 as_i |> of_int32x4 |> low_to in
    let lane2 = Int32x4.dup_lane 2 as_i |> of_int32x4 |> low_to in
    let lane3 = Int32x4.dup_lane 3 as_i |> of_int32x4 |> low_to in
    #(low_to x, lane1, lane2, lane3)

  (* ───── Array ───── *)

  module Array = struct
    external unsafe_get : (float32_u array[@local_opt]) @ read -> idx:int -> t
      = "%caml_unboxed_float32_array_get128u#"

    external unsafe_set : (float32_u array[@local_opt]) -> idx:int -> t -> unit
      = "%caml_unboxed_float32_array_set128u#"
  end
end
