(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Dtype = Zarrz.Dtype
module Slab = Zarrz.Slab
module Subset = Zarrz.Subset
module Ia = Stdlib_stable.Iarray
module Fu = Stdlib_upstream_compatible.Float_u
module I64u = Stdlib_upstream_compatible.Int64_u
module I32u = Stdlib_upstream_compatible.Int32_u
module I16u = Stdlib_stable.Int16_u
module I8u = Stdlib_stable.Int8_u
module F32u = Stdlib_stable.Float32_u

let raises name f =
  match f () with
  | () -> Alcotest.failf "%s: expected Invalid_argument" name
  | exception Invalid_argument _ -> ()

let bits (x : float#) = Int64.bits_of_float (Fu.to_float x)
let unbits b : float# = Fu.of_float (Int64.float_of_bits b)
let check_bits name want got = Alcotest.(check int64) name want (bits got)
let of_bool (b : bool#) = match b with #true -> true | #false -> false

(* [ignore] wants a value layout, so unboxed results need their own. *)
let drop_f (_ : float#) = ()
let drop_i64 (_ : int64#) = ()
let drop_i8 (_ : int8#) = ()

(* The 16 bit pattern a half precision slab stored, read back natively
   through a uint16 view of the same buffer. *)
let raw16 s i =
  let v = Slab.of_bigstring Dtype.Uint16 (Slab.shape s) (Slab.bigstring s) in
  I16u.to_int (Slab.U16.get v i) land 0xffff

(* Accessor round trips *)

let test_f64 () =
  let s = Slab.create Dtype.Float64 [: 5 :] in
  Slab.F64.set s 0 #1.5;
  Slab.F64.set s 1 (unbits 0x7ff8_0000_0000_0000L);
  Slab.F64.set s 2 (unbits 0xfff0_0000_dead_beefL);
  Slab.F64.set s 3 (Fu.of_float infinity);
  Slab.F64.set s 4 (Fu.neg #0.0);
  check_bits "1.5" (Int64.bits_of_float 1.5) (Slab.F64.get s 0);
  check_bits "quiet nan" 0x7ff8_0000_0000_0000L (Slab.F64.get s 1);
  check_bits "nan payload" 0xfff0_0000_dead_beefL (Slab.F64.get s 2);
  check_bits "inf" (Int64.bits_of_float infinity) (Slab.F64.get s 3);
  check_bits "-0." 0x8000_0000_0000_0000L (Slab.F64.get s 4);
  check_bits "unsafe" (Int64.bits_of_float 1.5) (Slab.F64.unsafe_get s 0)

let test_f32 () =
  let s = Slab.create Dtype.Float32 [: 4 :] in
  Slab.F32.set s 0 #1.5s;
  Slab.F32.set s 1 (F32u.of_bits (I32u.of_int32 0x7fc0_0000l));
  Slab.F32.set s 2 (F32u.of_bits (I32u.of_int32 0xff80_0000l));
  Slab.F32.set s 3 (F32u.of_float #1e-45);
  let b i = I32u.to_int32 (F32u.to_bits (Slab.F32.get s i)) in
  Alcotest.(check int32) "1.5s" 0x3fc0_0000l (b 0);
  Alcotest.(check int32) "nan" 0x7fc0_0000l (b 1);
  Alcotest.(check int32) "-inf" 0xff80_0000l (b 2);
  Alcotest.(check int32) "min subnormal" 0x0000_0001l (b 3)

let test_i64 () =
  let s = Slab.create Dtype.Int64 [: 3 :] in
  Slab.I64.set s 0 (I64u.of_int64 Int64.min_int);
  Slab.I64.set s 1 (I64u.of_int64 Int64.max_int);
  Slab.I64.set s 2 (I64u.of_int64 (-1L));
  let g i = I64u.to_int64 (Slab.I64.get s i) in
  Alcotest.(check int64) "min" Int64.min_int (g 0);
  Alcotest.(check int64) "max" Int64.max_int (g 1);
  Alcotest.(check int64) "-1" (-1L) (g 2)

let test_u64 () =
  let s = Slab.create Dtype.Uint64 [: 2 :] in
  (* 2 ** 64 - 1, the sentinel the shard index uses for an absent chunk. *)
  Slab.U64.set s 0 (I64u.of_int64 (-1L));
  Slab.U64.set s 1 (I64u.of_int64 0x8000_0000_0000_0000L);
  Alcotest.(check int64)
    "all ones reinterprets as -1" (-1L)
    (I64u.to_int64 (Slab.U64.get s 0));
  Alcotest.(check int64)
    "2 ** 63" Int64.min_int
    (I64u.to_int64 (Slab.U64.get s 1));
  raises "U64 on an int64 slab" (fun () ->
      let other = Slab.create Dtype.Int64 [: 1 :] in
      ignore (I64u.to_int64 (Slab.U64.get other 0)))

let test_i32 () =
  let s = Slab.create Dtype.Int32 [: 2 :] in
  let u = Slab.create Dtype.Uint32 [: 1 :] in
  Slab.I32.set s 0 (I32u.of_int32 Int32.min_int);
  Slab.I32.set s 1 (I32u.of_int32 Int32.max_int);
  Slab.U32.set u 0 (I32u.of_int32 (-1l));
  Alcotest.(check int32) "min" Int32.min_int (I32u.to_int32 (Slab.I32.get s 0));
  Alcotest.(check int32) "max" Int32.max_int (I32u.to_int32 (Slab.I32.get s 1));
  Alcotest.(check int32)
    "uint32 all ones" (-1l)
    (I32u.to_int32 (Slab.U32.get u 0))

let test_i16 () =
  let s = Slab.create Dtype.Int16 [: 2 :] in
  let u = Slab.create Dtype.Uint16 [: 1 :] in
  Slab.I16.set s 0 (I16u.of_int (-32768));
  Slab.I16.set s 1 (I16u.of_int 32767);
  Slab.U16.set u 0 (I16u.of_int 0xffff);
  Alcotest.(check int) "min" (-32768) (I16u.to_int (Slab.I16.get s 0));
  Alcotest.(check int) "max" 32767 (I16u.to_int (Slab.I16.get s 1));
  Alcotest.(check int)
    "uint16 all ones" 0xffff
    (I16u.to_int (Slab.U16.get u 0) land 0xffff)

let test_i8 () =
  let s = Slab.create Dtype.Int8 [: 2 :] in
  let u = Slab.create Dtype.Uint8 [: 1 :] in
  Slab.I8.set s 0 (I8u.of_int (-128));
  Slab.I8.set s 1 (I8u.of_int 127);
  Slab.U8.set u 0 (I8u.of_int 0xff);
  Alcotest.(check int) "min" (-128) (I8u.to_int (Slab.I8.get s 0));
  Alcotest.(check int) "max" 127 (I8u.to_int (Slab.I8.get s 1));
  Alcotest.(check int)
    "uint8 all ones" 0xff
    (I8u.to_int (Slab.U8.get u 0) land 0xff)

let test_bool () =
  let s = Slab.create Dtype.Bool [: 3 :] in
  Slab.Bl.set s 0 #true;
  Slab.Bl.set s 1 #false;
  Slab.Bl.set s 2 #true;
  Alcotest.(check bool) "true" true (of_bool (Slab.Bl.get s 0));
  Alcotest.(check bool) "false" false (of_bool (Slab.Bl.get s 1));
  Alcotest.(check bool) "true again" true (of_bool (Slab.Bl.get s 2));
  Alcotest.(check char)
    "stored byte is 1" '\001'
    (Base_bigstring.get (Slab.bigstring s) 0);
  (* Any non-zero byte reads as true. *)
  Base_bigstring.set (Slab.bigstring s) 1 '\042';
  Alcotest.(check bool) "non zero" true (of_bool (Slab.Bl.get s 1))

let test_complex () =
  let c64 = Slab.create Dtype.Complex64 [: 2 :] in
  Slab.C64.set_re c64 0 #1.5s;
  Slab.C64.set_im c64 0 (F32u.of_float (-#2.25));
  Slab.C64.set_re c64 1 #0.0s;
  Slab.C64.set_im c64 1 #3.0s;
  Alcotest.(check (float 0.))
    "c64 re" 1.5
    (Fu.to_float (F32u.to_float (Slab.C64.get_re c64 0)));
  Alcotest.(check (float 0.))
    "c64 im" (-2.25)
    (Fu.to_float (F32u.to_float (Slab.C64.get_im c64 0)));
  Alcotest.(check (float 0.))
    "c64 im 1" 3.0
    (Fu.to_float (F32u.to_float (Slab.C64.get_im c64 1)));
  let c128 = Slab.create Dtype.Complex128 [: 2 :] in
  Slab.C128.set_re c128 1 #6.5;
  Slab.C128.set_im c128 1 (unbits 0x7ff8_0000_0000_0000L);
  Alcotest.(check (float 0.))
    "c128 re" 6.5
    (Fu.to_float (Slab.C128.get_re c128 1));
  check_bits "c128 im nan" 0x7ff8_0000_0000_0000L (Slab.C128.get_im c128 1)

(* Half precision *)

let test_f16 () =
  let s = Slab.create Dtype.Float16 [: 12 :] in
  let cases =
    [|
      (0.5, 0x3800); (1.5, 0x3e00); (-1.5, 0xbe00); (65504., 0x7bff);
      (65520., 0x7c00) (* rounds up to infinity *); (infinity, 0x7c00);
      (neg_infinity, 0xfc00); (0., 0x0000); (-0., 0x8000);
      (5.9604644775390625e-08, 0x0001) (* min subnormal *);
      (2.98023223876953125e-08, 0x0000) (* half of it, ties to even *);
      (8.940696716308594e-08, 0x0002) (* 1.5 ulp, ties to even rounds up *);
    |]
  in
  Array.iteri
    (fun i (v, want) ->
      Slab.F16.set s i (Fu.of_float v);
      Alcotest.(check int) (Printf.sprintf "f16 bits of %h" v) want (raw16 s i))
    cases;
  (* Exact round trips for values representable in binary16. *)
  List.iter
    (fun v ->
      Slab.F16.set s 0 (Fu.of_float v);
      Alcotest.(check (float 0.))
        (Printf.sprintf "f16 round trip %h" v)
        v
        (Fu.to_float (Slab.F16.get s 0)))
    [ 0.5; 1.5; -1.5; 65504.; 0.; infinity; neg_infinity;
      5.9604644775390625e-08 ];
  (* A NaN stays a NaN and keeps its leading mantissa bit. *)
  Slab.F16.set s 0 (unbits 0x7ff8_0000_0000_0000L);
  Alcotest.(check int) "f16 canonical nan" 0x7e00 (raw16 s 0);
  check_bits "f16 nan widens canonically" 0x7ff8_0000_0000_0000L
    (Slab.F16.get s 0)

let test_bf16 () =
  let s = Slab.create Dtype.Bfloat16 [: 8 :] in
  let cases =
    [|
      (1.0, 0x3f80); (-2.5, 0xc020); (0.5, 0x3f00); (infinity, 0x7f80);
      (neg_infinity, 0xff80); (0., 0x0000); (-0., 0x8000);
      (3.3895313892515355e+38, 0x7f7f) (* max finite bfloat16 *);
    |]
  in
  Array.iteri
    (fun i (v, want) ->
      Slab.BF16.set s i (Fu.of_float v);
      Alcotest.(check int)
        (Printf.sprintf "bf16 bits of %h" v)
        want (raw16 s i);
      Alcotest.(check (float 0.))
        (Printf.sprintf "bf16 round trip %h" v)
        v
        (Fu.to_float (Slab.BF16.get s i)))
    cases;
  (* Ties to even: 1 + 2 ** -8 sits exactly between two bfloat16s and the
     even neighbour is 1.0. *)
  Slab.BF16.set s 0 (Fu.of_float (1.0 +. Float.ldexp 1.0 (-8)));
  Alcotest.(check int) "bf16 tie to even" 0x3f80 (raw16 s 0);
  Slab.BF16.set s 0 (Fu.of_float (1.0 +. Float.ldexp 3.0 (-9)));
  Alcotest.(check int) "bf16 round up" 0x3f81 (raw16 s 0);
  (* Overflow of the binary32 exponent range. *)
  Slab.BF16.set s 0 (Fu.of_float 1e39);
  Alcotest.(check int) "bf16 overflow" 0x7f80 (raw16 s 0);
  Slab.BF16.set s 0 (unbits 0x7ff8_0000_0000_0000L);
  Alcotest.(check int) "bf16 canonical nan" 0x7fc0 (raw16 s 0);
  check_bits "bf16 nan widens canonically" 0x7ff8_0000_0000_0000L
    (Slab.BF16.get s 0)

(* Coordinates *)

let test_coords () =
  let s = Slab.create Dtype.Float64 [: 2; 3; 4 :] in
  for i = 0 to 1 do
    for j = 0 to 2 do
      for k = 0 to 3 do
        let linear = (((i * 3) + j) * 4) + k in
        Alcotest.(check int) "index3" linear (Slab.index3 s i j k);
        Slab.F64.set3 s i j k (Fu.of_float (float_of_int linear))
      done
    done
  done;
  for l = 0 to 23 do
    Alcotest.(check (float 0.))
      "linear matches" (float_of_int l)
      (Fu.to_float (Slab.F64.get s l))
  done;
  Alcotest.(check (float 0.))
    "get3" 17.
    (Fu.to_float (Slab.F64.get3 s 1 1 1));
  let two = Slab.create Dtype.Int32 [: 3; 5 :] in
  Slab.I32.set2 two 2 4 (I32u.of_int32 99l);
  Alcotest.(check int) "index2" 14 (Slab.index2 two 2 4);
  Alcotest.(check int32)
    "get2" 99l
    (I32u.to_int32 (Slab.I32.get2 two 2 4));
  raises "index2 on rank 3" (fun () -> ignore (Slab.index2 s 0 0));
  raises "index3 on rank 2" (fun () -> ignore (Slab.index3 two 0 0 0));
  raises "coordinate out of range" (fun () -> ignore (Slab.index2 two 3 0))

(* Checks *)

let test_checks () =
  let s = Slab.create Dtype.Float64 [: 4 :] in
  Alcotest.(check int) "num_elements" 4 (Slab.num_elements s);
  Alcotest.(check int) "rank" 1 (Slab.rank s);
  Alcotest.(check (array int)) "shape" [| 4 |] (Ia.to_array (Slab.shape s));
  Alcotest.(check bool) "dtype" true (Dtype.equal Dtype.Float64 (Slab.dtype s));
  raises "index too large" (fun () -> drop_f (Slab.F64.get s 4));
  raises "negative index" (fun () -> drop_f (Slab.F64.get s (-1)));
  raises "set out of bounds" (fun () -> Slab.F64.set s 4 #0.0);
  raises "wrong dtype" (fun () -> drop_i64 (Slab.I64.get s 0));
  raises "wrong dtype on set" (fun () -> Slab.I32.set s 0 (I32u.of_int32 0l));
  (* An unsafe accessor checks nothing. Reading an int64 slab as float64
     stays inside the buffer, so this is defined. *)
  let i = Slab.create Dtype.Int64 [: 2 :] in
  Slab.I64.set i 0 (I64u.of_int64 (Int64.bits_of_float 2.5));
  Alcotest.(check (float 0.))
    "unsafe ignores the dtype" 2.5
    (Fu.to_float (Slab.F64.unsafe_get i 0));
  (* A raw dtype matches no accessor. *)
  let r = Slab.create (Dtype.Raw 3) [: 2 :] in
  Alcotest.(check int)
    "raw length" 6
    (Base_bigstring.length (Slab.bigstring r));
  raises "raw has no accessor" (fun () -> drop_i8 (Slab.U8.get r 0))

let test_of_bigstring () =
  let buf = Base_bigstring.create 24 in
  let s = Slab.of_bigstring Dtype.Float64 [: 3 :] buf in
  Slab.F64.set s 1 #7.5;
  Alcotest.(check (float 0.))
    "shares the buffer" 7.5
    (Fu.to_float
       (Slab.F64.get (Slab.of_bigstring Dtype.Float64 [: 3 :] buf) 1));
  Alcotest.(check bool)
    "physically the same buffer" true
    (Slab.bigstring s == buf);
  raises "too short" (fun () ->
      ignore (Slab.of_bigstring Dtype.Float64 [: 4 :] buf));
  raises "too long" (fun () ->
      ignore (Slab.of_bigstring Dtype.Float64 [: 2 :] buf));
  raises "negative dimension" (fun () ->
      ignore (Slab.create Dtype.Uint8 [: -1 :]));
  (* Rank 0 holds exactly one element. *)
  let z = Slab.create Dtype.Int32 [: :] in
  Alcotest.(check int) "rank 0 elements" 1 (Slab.num_elements z);
  Slab.I32.set z 0 (I32u.of_int32 5l);
  Alcotest.(check int32) "rank 0 value" 5l (I32u.to_int32 (Slab.I32.get z 0))

let test_fill () =
  let s = Slab.create Dtype.Int32 [: 3; 5 :] in
  Slab.fill s "\x01\x02\x03\x04";
  for i = 0 to 14 do
    Alcotest.(check int32)
      (Printf.sprintf "element %d" i)
      0x04030201l
      (I32u.to_int32 (Slab.I32.get s i))
  done;
  let b = Slab.create Dtype.Uint8 [: 7 :] in
  Slab.fill b "\xab";
  for i = 0 to 6 do
    Alcotest.(check int)
      "byte fill" 0xab
      (I8u.to_int (Slab.U8.get b i) land 0xff)
  done;
  (* An odd length that the doubling blit cannot halve evenly. *)
  let r = Slab.create (Dtype.Raw 3) [: 5 :] in
  Slab.fill r "\x01\x02\x03";
  let raw = Slab.bigstring r in
  for i = 0 to 14 do
    Alcotest.(check char)
      (Printf.sprintf "raw byte %d" i)
      (Char.chr ((i mod 3) + 1))
      (Base_bigstring.get raw i)
  done;
  raises "wrong pattern length" (fun () -> Slab.fill s "\x01")

(* Bigarray view *)

let test_genarray () =
  let s = Slab.create Dtype.Float64 [: 2; 3 :] in
  for i = 0 to 5 do
    Slab.F64.set s i (Fu.of_float (float_of_int i))
  done;
  let g = Slab.to_genarray s Bigarray.float64 in
  Alcotest.(check int) "num_dims" 2 (Bigarray.Genarray.num_dims g);
  Alcotest.(check (array int))
    "dims" [| 2; 3 |] (Bigarray.Genarray.dims g);
  Alcotest.(check (float 0.))
    "reads through" 4.
    (Bigarray.Genarray.get g [| 1; 1 |]);
  Bigarray.Genarray.set g [| 1; 2 |] 99.;
  Alcotest.(check (float 0.))
    "writes through" 99.
    (Fu.to_float (Slab.F64.get2 s 1 2));
  Slab.F64.set2 s 0 0 (-#1.);
  Alcotest.(check (float 0.))
    "and back" (-1.)
    (Bigarray.Genarray.get g [| 0; 0 |]);
  (* Every accepted pairing. *)
  let pair dtype k =
    let s = Slab.create dtype [: 2 :] in
    ignore (Slab.to_genarray s k)
  in
  pair Dtype.Float32 Bigarray.float32;
  pair Dtype.Float16 Bigarray.float16;
  pair Dtype.Int64 Bigarray.int64;
  pair Dtype.Uint64 Bigarray.int64;
  pair Dtype.Int32 Bigarray.int32;
  pair Dtype.Uint32 Bigarray.int32;
  pair Dtype.Int16 Bigarray.int16_signed;
  pair Dtype.Uint16 Bigarray.int16_unsigned;
  pair Dtype.Int8 Bigarray.int8_signed;
  pair Dtype.Uint8 Bigarray.int8_unsigned;
  pair Dtype.Complex64 Bigarray.complex32;
  pair Dtype.Complex128 Bigarray.complex64;
  raises "kind mismatch" (fun () -> pair Dtype.Float64 Bigarray.int32);
  raises "same size is not enough" (fun () ->
      pair Dtype.Int64 Bigarray.float64);
  raises "bool has no kind" (fun () -> pair Dtype.Bool Bigarray.int8_unsigned);
  raises "bfloat16 has no kind" (fun () ->
      pair Dtype.Bfloat16 Bigarray.float16);
  raises "raw has no kind" (fun () ->
      pair (Dtype.Raw 1) Bigarray.int8_unsigned);
  raises "rank above 16" (fun () ->
      let shape = Ia.of_list (List.init 17 (fun _ -> 1)) in
      let s = Slab.create Dtype.Uint8 shape in
      ignore (Slab.to_genarray s Bigarray.int8_unsigned))

let test_genarray_keeps_alive () =
  (* The slab goes out of scope, so only the view holds the buffer. If the
     stub failed to attach a proxy the collector would free it here. *)
  let view =
    let s = Slab.create Dtype.Float64 [: 3 :] in
    Slab.F64.set s 2 #12.5;
    Slab.to_genarray s Bigarray.float64
  in
  Gc.full_major ();
  Gc.compact ();
  Alcotest.(check (float 0.))
    "still readable" 12.5
    (Bigarray.Genarray.get view [| 2 |]);
  Bigarray.Genarray.set view [| 0 |] 1.;
  Gc.full_major ();
  Alcotest.(check (float 0.))
    "still writable" 1.
    (Bigarray.Genarray.get view [| 0 |])

(* Subset *)

let runs ~outer sub =
  let acc = ref [] in
  Subset.iter_runs ~outer sub ~f:(fun ~src ~dst ~len ->
      acc := (src, dst, len) :: !acc);
  List.rev !acc

let run_list = Alcotest.(list (triple int int int))

let test_runs_1d () =
  Alcotest.check run_list "whole"
    [ (0, 0, 10) ]
    (runs ~outer:[: 10 :] { start = [: 0 :]; shape = [: 10 :] });
  Alcotest.check run_list "interior"
    [ (3, 0, 4) ]
    (runs ~outer:[: 10 :] { start = [: 3 :]; shape = [: 4 :] });
  Alcotest.check run_list "empty" []
    (runs ~outer:[: 10 :] { start = [: 3 :]; shape = [: 0 :] })

let test_runs_2d () =
  Alcotest.check run_list "interior rectangle"
    [ (23, 0, 5); (33, 5, 5); (43, 10, 5) ]
    (runs ~outer:[: 6; 10 :] { start = [: 2; 3 :]; shape = [: 3; 5 :] });
  Alcotest.check run_list "full rows coalesce"
    [ (20, 0, 30) ]
    (runs ~outer:[: 6; 10 :] { start = [: 2; 0 :]; shape = [: 3; 10 :] });
  Alcotest.check run_list "one column"
    [ (3, 0, 1); (13, 1, 1); (23, 2, 1) ]
    (runs ~outer:[: 3; 10 :] { start = [: 0; 3 :]; shape = [: 3; 1 :] });
  Alcotest.check run_list "whole array is one run"
    [ (0, 0, 60) ]
    (runs ~outer:[: 6; 10 :] { start = [: 0; 0 :]; shape = [: 6; 10 :] })

let test_runs_3d () =
  (* Full trailing dimensions coalesce into one run per outer slice. *)
  (* Two full trailing dimensions make the slices adjacent, so the two
     outer slices coalesce into a single run. *)
  Alcotest.check run_list "full trailing pair"
    [ (30, 0, 60) ]
    (runs ~outer:[: 4; 5; 6 :]
       { start = [: 1; 0; 0 :]; shape = [: 2; 5; 6 :] });
  Alcotest.check run_list "innermost full only"
    [ (36, 0, 18); (66, 18, 18) ]
    (runs ~outer:[: 4; 5; 6 :]
       { start = [: 1; 1; 0 :]; shape = [: 2; 3; 6 :] });
  Alcotest.check run_list "nothing full"
    [ (37, 0, 2); (43, 2, 2); (67, 4, 2); (73, 6, 2) ]
    (runs ~outer:[: 4; 5; 6 :]
       { start = [: 1; 1; 1 :]; shape = [: 2; 2; 2 :] });
  Alcotest.check run_list "whole array"
    [ (0, 0, 120) ]
    (runs ~outer:[: 4; 5; 6 :]
       { start = [: 0; 0; 0 :]; shape = [: 4; 5; 6 :] });
  Alcotest.check run_list "rank 0"
    [ (0, 0, 1) ]
    (runs ~outer:[: :] { start = [: :]; shape = [: :] })

let test_runs_cover () =
  (* Every element of the subset is visited exactly once, and the source
     indices match a direct coordinate walk. *)
  let outer = [: 4; 5; 6 :] in
  let sub = { Subset.start = [: 1; 2; 3 :]; shape = [: 2; 3; 2 :] } in
  let seen = Array.make (Subset.num_elements sub) (-1) in
  Subset.iter_runs ~outer sub ~f:(fun ~src ~dst ~len ->
      for o = 0 to len - 1 do
        Alcotest.(check int) "dst in range" (-1) seen.(dst + o);
        seen.(dst + o) <- src + o
      done);
  let d = ref 0 in
  for i = 0 to 1 do
    for j = 0 to 2 do
      for k = 0 to 1 do
        let src = ((((1 + i) * 5) + (2 + j)) * 6) + (3 + k) in
        Alcotest.(check int) "src matches" src seen.(!d);
        incr d
      done
    done
  done

let test_validate () =
  let outer = [: 4; 5 :] in
  Subset.validate ~outer { start = [: 0; 0 :]; shape = [: 4; 5 :] };
  raises "rank mismatch" (fun () ->
      Subset.validate ~outer { start = [: 0 :]; shape = [: 4 :] });
  raises "past the end" (fun () ->
      Subset.validate ~outer { start = [: 2; 0 :]; shape = [: 3; 5 :] });
  raises "negative start" (fun () ->
      Subset.validate ~outer { start = [: -1; 0 :]; shape = [: 1; 5 :] })

let test_gather_scatter () =
  let outer = [: 4; 6 :] in
  let elem_size = 4 in
  let big = Slab.create Dtype.Int32 outer in
  for i = 0 to 23 do
    Slab.I32.set big i (I32u.of_int32 (Int32.of_int (100 + i)))
  done;
  let sub = { Subset.start = [: 1; 2 :]; shape = [: 2; 3 :] } in
  let dense = Slab.create Dtype.Int32 sub.shape in
  Subset.gather ~elem_size ~src:(Slab.bigstring big) ~outer sub
    ~dst:(Slab.bigstring dense);
  let want = [| 108; 109; 110; 114; 115; 116 |] in
  Array.iteri
    (fun i w ->
      Alcotest.(check int32)
        (Printf.sprintf "gathered %d" i)
        (Int32.of_int w)
        (I32u.to_int32 (Slab.I32.get dense i)))
    want;
  let back = Slab.create Dtype.Int32 outer in
  Slab.fill back "\x00\x00\x00\x00";
  Subset.scatter ~elem_size ~src:(Slab.bigstring dense)
    ~dst:(Slab.bigstring back) ~outer sub;
  for i = 0 to 23 do
    let r = i / 6 and c = i mod 6 in
    let expect =
      if r >= 1 && r <= 2 && c >= 2 && c <= 4 then Int32.of_int (100 + i)
      else 0l
    in
    Alcotest.(check int32)
      (Printf.sprintf "scattered %d" i)
      expect
      (I32u.to_int32 (Slab.I32.get back i))
  done;
  raises "dense buffer too small" (fun () ->
      let small = Slab.create Dtype.Int32 [: 2 :] in
      Subset.gather ~elem_size ~src:(Slab.bigstring big) ~outer sub
        ~dst:(Slab.bigstring small))

(* A hot loop over the unboxed accessor. The allocation guarantee itself
   is the release-check build, this only pins the arithmetic down. *)
let test_sum_loop () =
  let n = 1_000_000 in
  let s = Slab.create Dtype.Float64 (Ia.of_list [ n ]) in
  for i = 0 to n - 1 do
    Slab.F64.unsafe_set s i (Fu.of_float 0.5)
  done;
  let mutable acc : float# = #0.0 in
  for i = 0 to n - 1 do
    acc <- Fu.add acc (Slab.F64.unsafe_get s i)
  done;
  Alcotest.(check (float 0.))
    "sum" 500_000. (Fu.to_float acc)

let () =
  Alcotest.run "slab"
    [
      ( "accessors",
        [
          Alcotest.test_case "float64" `Quick test_f64;
          Alcotest.test_case "float32" `Quick test_f32;
          Alcotest.test_case "int64" `Quick test_i64;
          Alcotest.test_case "uint64" `Quick test_u64;
          Alcotest.test_case "int32" `Quick test_i32;
          Alcotest.test_case "int16" `Quick test_i16;
          Alcotest.test_case "int8" `Quick test_i8;
          Alcotest.test_case "bool" `Quick test_bool;
          Alcotest.test_case "complex" `Quick test_complex;
          Alcotest.test_case "float16" `Quick test_f16;
          Alcotest.test_case "bfloat16" `Quick test_bf16;
        ] );
      ( "slab",
        [
          Alcotest.test_case "coordinates" `Quick test_coords;
          Alcotest.test_case "checks" `Quick test_checks;
          Alcotest.test_case "of_bigstring" `Quick test_of_bigstring;
          Alcotest.test_case "fill" `Quick test_fill;
          Alcotest.test_case "genarray" `Quick test_genarray;
          Alcotest.test_case "genarray keeps alive" `Quick
            test_genarray_keeps_alive;
          Alcotest.test_case "sum loop" `Quick test_sum_loop;
        ] );
      ( "subset",
        [
          Alcotest.test_case "runs 1d" `Quick test_runs_1d;
          Alcotest.test_case "runs 2d" `Quick test_runs_2d;
          Alcotest.test_case "runs 3d" `Quick test_runs_3d;
          Alcotest.test_case "runs cover" `Quick test_runs_cover;
          Alcotest.test_case "validate" `Quick test_validate;
          Alcotest.test_case "gather and scatter" `Quick test_gather_scatter;
        ] );
    ]
