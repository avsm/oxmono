(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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

(* The built-in codecs. The codec milestone replaces this stub with the
   bytes, transpose, gzip, zstd, crc32c and sharding_indexed codecs. *)
let builtins : resolver = fun _ext ~dtype:_ ~fill_value:_ -> None

let chain_of_exts ?resolver ~dtype ~fill_value exts =
  let resolve ext =
    let user =
      match resolver with
      | Some r -> r ext ~dtype ~fill_value
      | None -> None
    in
    match user with Some _ -> user | None -> builtins ext ~dtype ~fill_value
  in
  let rec go a2a a2b b2b kept = function
    | [] -> (
        match a2b with
        | None -> Error "missing array to bytes codec"
        | Some a2b ->
            Ok
              {
                a2a = List.rev a2a;
                a2b;
                b2b = List.rev b2b;
                exts = List.rev kept;
              })
    | (ext : Ext.t) :: tl -> (
        match resolve ext with
        | None ->
            if ext.must_understand then
              Error (Printf.sprintf "unknown codec %S" ext.name)
            else go a2a a2b b2b kept tl
        | Some (Error e) -> Error (Printf.sprintf "codec %S: %s" ext.name e)
        | Some (Ok (A2a c)) -> go (c :: a2a) a2b b2b (ext :: kept) tl
        | Some (Ok (A2b c)) -> (
            match a2b with
            | Some _ -> Error "multiple array to bytes codecs"
            | None -> go a2a (Some c) b2b (ext :: kept) tl)
        | Some (Ok (B2b c)) -> go a2a a2b (c :: b2b) (ext :: kept) tl)
  in
  go [] None [] [] exts

let chain_exts c = c.exts

(* [reprs_through c r] pairs each array to array codec with the decoded
   representation entering it, and gives the representation reaching the
   array to bytes codec. *)
let reprs_through c repr0 =
  let rec go acc r = function
    | [] -> (List.rev acc, r)
    | (a : a2a) :: tl -> go ((a, r) :: acc) (a.encoded_repr r) tl
  in
  go [] repr0 c.a2a

let encoded_size c repr0 =
  let _, r = reprs_through c repr0 in
  List.fold_left
    (fun s (b : b2b) -> b.encoded_size s)
    (c.a2b.encoded_size r) c.b2b

let decode_chunk c repr0 bytes =
  let stages, r_a2b = reprs_through c repr0 in
  (* The size entering each bytes to bytes codec, in encode order, so
     each decode step knows the exact size it must produce. *)
  let sizes_in =
    let rec go acc s = function
      | [] -> List.rev acc
      | (b : b2b) :: tl -> go (s :: acc) (b.encoded_size s) tl
    in
    go [] (c.a2b.encoded_size r_a2b) c.b2b
  in
  let bytes =
    List.fold_left2
      (fun bs (b : b2b) ds -> b.decode bs ~decoded_size:ds)
      bytes (List.rev c.b2b) (List.rev sizes_in)
  in
  let slab = c.a2b.decode bytes r_a2b in
  List.fold_left (fun s ((a : a2a), r) -> a.decode s r) slab (List.rev stages)

let encode_chunk c slab =
  let slab = List.fold_left (fun s (a : a2a) -> a.encode s) slab c.a2a in
  List.fold_left (fun bs (b : b2b) -> b.encode bs) (c.a2b.encode slab) c.b2b

let supports_partial c =
  c.a2a = [] && c.b2b = [] && c.a2b.partial_decode <> None

let partial_decode c r src sub =
  if supports_partial c then
    match c.a2b.partial_decode with
    | Some f -> Some (f src r sub)
    | None -> None
  else None
