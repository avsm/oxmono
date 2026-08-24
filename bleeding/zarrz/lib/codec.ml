(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Skeleton pinning the interface. The codec milestone replaces the
   [todo] bodies with the built-in codecs and the chain logic. *)

type size = Fixed of int | Bounded of int | Unbounded
type repr = { dtype : Dtype.t; shape : int array }

type a2a = {
  name : string;
  encoded_repr : repr -> repr;
  encode : Slab.t -> Slab.t;
  decode : Slab.t -> repr -> Slab.t;
}

type a2b = {
  name : string;
  encoded_size : repr -> size;
  encode : Slab.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> repr -> Slab.t;
  partial_decode : (Byte_source.t -> repr -> Subset.t -> Slab.t) option;
}

type b2b = {
  name : string;
  encoded_size : size -> size;
  encode : Base_bigstring.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> decoded_size:size -> Base_bigstring.t;
}

type bound = A2a of a2a | A2b of a2b | B2b of b2b

type resolver = Ext.t -> dtype:Dtype.t -> fill_value:Fill_value.t ->
  (bound, string) result option

type chain = { a2a : a2a list; a2b : a2b; b2b : b2b list; exts : Ext.t list }

let todo name = failwith ("Zarrz.Codec." ^ name ^ ": unimplemented")

let chain_of_exts ?resolver ~dtype ~fill_value exts =
  ignore resolver;
  ignore dtype;
  ignore fill_value;
  ignore exts;
  todo "chain_of_exts"

let chain_exts c = c.exts
let encoded_size _ _ = todo "encoded_size"
let decode_chunk _ _ _ = todo "decode_chunk"
let encode_chunk _ _ = todo "encode_chunk"
let supports_partial c = c.b2b = [] && c.a2b.partial_decode <> None

let partial_decode c r src sub =
  if supports_partial c then
    match c.a2b.partial_decode with
    | Some f -> Some (f src r sub)
    | None -> None
  else None
