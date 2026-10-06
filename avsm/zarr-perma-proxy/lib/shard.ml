(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Z = Zarrz
module R = Coverage

type t = { chain : Z.Codec.chain; repr : Z.Codec.repr; bytes : int; at_start : bool }

let fail s = invalid_arg ("Shard: " ^ s)
let get = function Ok x -> x | Error e -> fail e

let of_json json =
  match Z.Metadata.array_of_json json with
  | Error _ -> None
  | Ok meta ->
      match meta.codecs with
      | [ ext ] when ext.Z.Ext.name = "sharding_indexed" ->
          let grid = get (Z.Chunk_grid.of_ext meta.chunk_grid ~array_shape:meta.shape) in
          let shape = Z.Chunk_grid.chunk_shape grid in
          let inner = match Z.Ext.config_mem ext "chunk_shape" with
            | Some j -> Array.of_list (get (Jsont.Json.decode (Jsont.list Jsont.int) j))
            | None -> fail "missing inner chunk_shape" in
          if Array.length shape <> Array.length inner then fail "rank mismatch";
          let count = ref 1 in
          let grid = Array.mapi (fun i n ->
              if inner.(i) < 1 || n mod inner.(i) <> 0 then fail "invalid chunk_shape";
              let n = n / inner.(i) in
              if n < 1 || !count > 1048576 / n then fail "index too large";
              count := !count * n; n) shape in
          let codecs = match Z.Ext.config_mem ext "index_codecs" with
            | Some j -> get (Jsont.Json.decode (Jsont.list Z.Ext.jsont) j)
            | None -> fail "missing index_codecs" in
          let fill_value = get (Z.Fill_value.of_json Z.Dtype.Uint64
              (Jsont.Json.number 0.)) in
          let chain = get (Z.Codec.chain_of_exts ~dtype:Z.Dtype.Uint64 ~fill_value codecs) in
          let repr = Z.Codec.{ dtype = Z.Dtype.Uint64; shape = Array.append grid [|2|] } in
          let bytes = match Z.Codec.encoded_size chain repr with
            | Fixed n -> n | _ -> fail "index codecs have variable size" in
          let at_start = match Z.Ext.config_mem ext "index_location" with
            | None | Some (Jsont.String ("end", _)) -> false
            | Some (Jsont.String ("start", _)) -> true
            | _ -> fail "invalid index_location" in
          Some { chain; repr; bytes; at_start }
      | _ -> None

let index_range t ~size =
  if size < t.bytes then fail "object shorter than shard index";
  if t.at_start then R.v 0 t.bytes else R.v (size - t.bytes) size

let ranges t ~size bytes =
  let expected = index_range t ~size in
  if String.length bytes <> t.bytes then fail "truncated index";
  let slab = Z.Codec.decode_chunk t.chain t.repr (Base_bigstring.of_string bytes) in
  let view = Bigarray.reshape_1 (Z.Slab.to_genarray slab Bigarray.int64)
      (Z.Slab.num_elements slab) in
  let result = ref [expected] in
  for i = 0 to Bigarray.Array1.dim view / 2 - 1 do
    let off = Bigarray.Array1.get view (2 * i)
    and len = Bigarray.Array1.get view (2 * i + 1) in
    if off = -1L && len = -1L then () else begin
      if off < 0L || len < 0L || off > Int64.of_int size
          || len > Int64.sub (Int64.of_int size) off then
        fail "inner chunk outside shard";
      let start = Int64.to_int off in
      let stop = start + Int64.to_int len in
      if start < expected.stop && stop > expected.start then
        fail "inner chunk overlaps index";
      result := R.v start stop :: !result
    end
  done;
  List.sort (fun a b -> compare a.R.start b.R.start) !result

let expand ranges requested =
  List.fold_left (fun acc r ->
      if r.R.start < requested.R.stop && requested.R.start < r.stop then
        R.merge acc r else acc) [requested] ranges
