(** QCheck generators for property-based testing *)

open QCheck

(* Save Array before Zarr shadows it *)
module StdArray = Array

open Zarr

(* Module aliases for nested types *)
module D = Zarr.Ztypes.Dtype
module E = Zarr.Ztypes.Endianness
module FV = Zarr.Ztypes.Fill_value

(** Generate a random data type *)
let dtype_gen = Gen.oneofl [
  D.Bool; D.Int8; D.Int16; D.Int32; D.Int64;
  D.Uint8; D.Uint16; D.Uint32; D.Uint64;
  D.Float32; D.Float64
  (* Omit Float16, Complex64, Complex128 for simpler testing *)
] [@alert "-deprecated"]

(** Generate a shape array (1-4 dimensions, reasonable sizes) *)
let shape_gen = Gen.(
  array_size (1 -- 4) (1 -- 50)
)

(** Generate a small shape for quick tests *)
let small_shape_gen = Gen.(
  array_size (1 -- 3) (1 -- 20)
)

(** Generate chunk shape compatible with array shape - simplified version *)
let chunk_shape_gen shape =
  let open Gen in
  let dims = StdArray.to_list (StdArray.map (fun dim ->
    1 -- dim
  ) shape) in
  match dims with
  | [] -> return [||]
  | [d1] -> map (fun c1 -> [|c1|]) d1
  | [d1; d2] -> map2 (fun c1 c2 -> [|c1; c2|]) d1 d2
  | [d1; d2; d3] -> map3 (fun c1 c2 c3 -> [|c1; c2; c3|]) d1 d2 d3
  | d1 :: d2 :: d3 :: d4 :: _ ->
    map4 (fun c1 c2 c3 c4 -> [|c1; c2; c3; c4|]) d1 d2 d3 d4

(** Generate a fill value for a given dtype *)
let fill_value_gen dtype = Gen.(
  match dtype with
  | D.Bool -> map (fun b -> FV.Bool b) bool
  | D.Int8 | D.Int16 | D.Int32 | D.Int64 ->
    map (fun i -> FV.Int (Int64.of_int i)) ((-100) -- 100)
  | D.Uint8 | D.Uint16 | D.Uint32 | D.Uint64 ->
    map (fun i -> FV.Uint (Int64.of_int i)) (0 -- 200)
  | D.Float32 | D.Float64 ->
    map (fun f -> FV.Float f) (float_range (-1000.0) 1000.0)
  | _ -> return (FV.Float 0.0)
)

(** Generate random bytes *)
let bytes_gen = Gen.(
  let* len = 1 -- 1000 in
  let* chars = list_size (return len) (char_range '\x00' '\xff') in
  return (Bytes.of_seq (List.to_seq chars))
)

(** Generate an endianness *)
let endian_gen = Gen.oneofl [E.Little; E.Big] [@alert "-deprecated"]

(** Generate a gzip level *)
let gzip_level_gen = Gen.(1 -- 9)

(** QCheck arbitrary for dtype *)
let dtype_arb = make dtype_gen

(** QCheck arbitrary for shape *)
let shape_arb = make shape_gen

(** QCheck arbitrary for small shape *)
let small_shape_arb = make small_shape_gen

(** QCheck arbitrary for bytes *)
let bytes_arb = make bytes_gen

(** QCheck arbitrary for endianness *)
let endian_arb = make endian_gen

(** QCheck arbitrary for gzip level *)
let gzip_level_arb = make gzip_level_gen

(** Print functions for better error messages *)
let pp_dtype fmt dt = Format.pp_print_string fmt (Data_type.to_string dt)

let pp_shape fmt shape =
  Format.fprintf fmt "[|%s|]"
    (String.concat "; " (StdArray.to_list (StdArray.map string_of_int shape)))

let pp_endian fmt = function
  | E.Little -> Format.pp_print_string fmt "Little"
  | E.Big -> Format.pp_print_string fmt "Big"

(** Arbitrary with better printing *)
let dtype_arb_pp = set_print (fun dt -> Data_type.to_string dt) dtype_arb

let shape_arb_pp = set_print (fun shape ->
  "[|" ^ String.concat "; " (StdArray.to_list (StdArray.map string_of_int shape)) ^ "|]"
) shape_arb

let small_shape_arb_pp = set_print (fun shape ->
  "[|" ^ String.concat "; " (StdArray.to_list (StdArray.map string_of_int shape)) ^ "|]"
) small_shape_arb
