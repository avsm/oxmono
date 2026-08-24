(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The [zarrs_conformance] contract, so that the corpus at
   <https://github.com/Bisaloo/zarr-conformance-tests> runs against this
   library unchanged. Invoked as

     zarrz_conformance --array_path <dir>

   it opens <dir> as a store rooted at the array, reads the whole array
   and writes one line per element in C order, each the element's fill
   value metadata as compact JSON. The oracle is
   zarrs/zarrs_conformance/src/main.rs. *)

module Arr = Zarrz.Arr
module Dtype = Zarrz.Dtype
module Fill_value = Zarrz.Fill_value
module Slab = Zarrz.Slab
module Subset = Zarrz.Subset
module Ia = Stdlib_stable.Iarray

(* {1 Numbers}

   [Fill_value.to_json] hands back a [Jsont.json], where every number is
   a float whatever the data type. The oracle prints an integer data
   type through [serde_json::Number::from], which writes the digits
   alone, and a float data type through [Number::from_f64], which writes
   the shortest decimal that reads back as the same double and never a
   bare integer, so 0.0 prints as "0.0". [jsont]'s own encoder cannot be
   asked for that spelling, hence this printer over the small subset of
   JSON that [Fill_value.to_json] builds: booleans, numbers, strings and
   arrays of those.

   Shortest is found by trying the [%g] precisions in turn and taking
   the first that reads back equal. That agrees with the oracle's [ryu]
   for every magnitude both spell positionally, which is every fill
   value of a fixed size data type in the corpus. The two part company
   where [%g] switches to an exponent and [ryu] has not, above 1e15 and
   below 1e-5, so a fixture holding such a value would need this printer
   revisited rather than trusted. *)

let normalise s =
  match String.index_opt s 'e' with
  | None -> if String.contains s '.' then s else s ^ ".0"
  | Some i ->
      let mant = String.sub s 0 i in
      let e = String.sub s (i + 1) (String.length s - i - 1) in
      let neg = e.[0] = '-' in
      let d = if e.[0] = '+' || e.[0] = '-' then 1 else 0 in
      let j = ref d in
      while !j < String.length e - 1 && e.[!j] = '0' do
        incr j
      done;
      let digits = String.sub e !j (String.length e - !j) in
      mant ^ "e" ^ (if neg then "-" else "") ^ digits

let float_string f =
  let rec pick = function
    | [] -> Printf.sprintf "%.17g" f
    | p :: ps ->
        let s = Printf.sprintf "%.*g" p f in
        if Float.equal (float_of_string s) f then s else pick ps
  in
  normalise (pick [ 15; 16; 17 ])

let int_string f = Printf.sprintf "%.0f" f

(* {1 JSON} *)

let add_string b s =
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"'

let rec add_json b ~floats (j : Jsont.json) =
  match j with
  | Jsont.Null _ -> Buffer.add_string b "null"
  | Jsont.Bool (v, _) -> Buffer.add_string b (if v then "true" else "false")
  | Jsont.Number (f, _) ->
      Buffer.add_string b (if floats then float_string f else int_string f)
  | Jsont.String (s, _) -> add_string b s
  | Jsont.Array (l, _) ->
      Buffer.add_char b '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char b ',';
          add_json b ~floats x)
        l;
      Buffer.add_char b ']'
  | Jsont.Object _ -> failwith "a fill value is never an object"

(* The whole distinction the JSON value has lost: a number of a float
   data type is spelled as a float, one of an integer data type as an
   integer. Complex arrays hold floats, [r*] arrays hold bytes. *)
let floats_of_dtype = function
  | Dtype.Float16 | Dtype.Bfloat16 | Dtype.Float32 | Dtype.Float64
  | Dtype.Complex64 | Dtype.Complex128 ->
      true
  | _ -> false

(* {1 The array} *)

let print_elements arr =
  let dtype = Arr.dtype arr in
  let floats = floats_of_dtype dtype in
  let esz = Dtype.size dtype in
  let shape = Arr.shape arr in
  let sub =
    {
      Subset.start = Ia.of_array (Array.map (fun _ -> 0) shape);
      shape = Ia.of_array shape;
    }
  in
  let slab = Arr.read arr sub in
  let buf = Slab.bigstring slab in
  let n = Slab.num_elements slab in
  let out = Buffer.create (16 * n) in
  for i = 0 to n - 1 do
    let elem = Base_bigstring.get_string buf ~pos:(i * esz) ~len:esz in
    add_json out ~floats (Fill_value.to_json dtype (Fill_value.of_bytes elem));
    Buffer.add_char out '\n'
  done;
  print_string (Buffer.contents out)

let run env dir =
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  print_elements (Arr.open_ (Zarrz_eio.store root) ~path:"/")

(* {1 Command line}

   [clap] accepts both spellings of a long option's value, so both are
   accepted here. Nothing else is a valid invocation. *)

let usage = "usage: zarrz_conformance --array_path <dir>"
let flag = "--array_path"

let array_path argv =
  let n = Array.length argv in
  let eq = flag ^ "=" in
  let elen = String.length eq in
  let rec go i =
    if i >= n then None
    else
      let a = argv.(i) in
      if String.equal a flag then if i + 1 < n then Some argv.(i + 1) else None
      else if String.length a > elen && String.equal (String.sub a 0 elen) eq
      then Some (String.sub a elen (String.length a - elen))
      else go (i + 1)
  in
  go 1

let () =
  match array_path Sys.argv with
  | None ->
      prerr_endline usage;
      exit 2
  | Some dir -> (
      try Eio_main.run (fun env -> run env dir) with
      | Zarrz.Error.E e ->
          prerr_endline ("zarrz_conformance: " ^ Zarrz.Error.to_string e);
          exit 1
      | e ->
          prerr_endline ("zarrz_conformance: " ^ Printexc.to_string e);
          exit 1)
