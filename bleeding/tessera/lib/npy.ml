(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Dtype = Zarrz.Dtype
module Ga = Bigarray.Genarray
module Slab = Zarrz.Slab

let magic = "\147NUMPY"

(* numpy pads the header to a multiple of 64 bytes, counting the ten
   bytes of magic, version and length that precede it. *)
let align = 64

let dims s =
  if not (Dtype.equal (Slab.dtype s) Dtype.Float32) then
    invalid_arg
      (Printf.sprintf "Tessera.Npy: data type is %s, not float32"
         (Dtype.name (Slab.dtype s)));
  Ga.dims (Slab.to_genarray s Bigarray.float32)

(* A Python tuple literal, so a one-dimensional shape keeps its comma. *)
let tuple d =
  let l = Array.to_list (Array.map string_of_int d) in
  match l with
  | [ x ] -> Printf.sprintf "(%s,)" x
  | l -> Printf.sprintf "(%s)" (String.concat ", " l)

let header s =
  if Sys.big_endian then
    invalid_arg
      "Tessera.Npy: the elements are native endian and the descriptor \
       says little endian, so this host cannot write a valid file";
  let d = dims s in
  let body =
    Printf.sprintf "{'descr': '<f4', 'fortran_order': False, 'shape': %s, }"
      (tuple d)
  in
  let pad = (align - ((10 + String.length body + 1) mod align)) mod align in
  let len = String.length body + pad + 1 in
  if len > 0xffff then
    invalid_arg
      (Printf.sprintf "Tessera.Npy: a %d byte header does not fit version 1.0"
         len);
  let b = Buffer.create (10 + len) in
  Buffer.add_string b magic;
  Buffer.add_char b '\001';
  Buffer.add_char b '\000';
  Buffer.add_char b (Char.chr (len land 0xff));
  Buffer.add_char b (Char.chr ((len lsr 8) land 0xff));
  Buffer.add_string b body;
  Buffer.add_string b (String.make pad ' ');
  Buffer.add_char b '\n';
  Buffer.contents b

let to_string s =
  header s ^ Base_bigstring.to_string (Slab.bigstring s)
