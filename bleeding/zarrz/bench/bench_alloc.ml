(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Run with dune exec --profile release-check
   bleeding/zarrz/bench/bench_alloc.exe. Setup is outside each measurement. *)

open Zarrz

let measure name count f =
  for _ = 1 to 10 do
    ignore (Sys.opaque_identity (f ()))
  done;
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to count do
    ignore (Sys.opaque_identity (f ()))
  done;
  let words = (Gc.minor_words () -. before) /. float count in
  Printf.printf "%s: %.3f minor words/op\n%!" name words

let chain json =
  let exts =
    match Jsont_bytesrw.decode_string (Jsont.list Ext.jsont) json with
    | Ok exts -> exts
    | Error message -> failwith message
  in
  match
    Codec.chain_of_exts ~dtype:Dtype.Uint8
      ~fill_value:(Fill_value.of_bytes "\000")
      exts
  with
  | Ok chain -> chain
  | Error message -> failwith message

let () =
  let outer = [: 4; 4; 4; 6 :] in
  let start = [: 1; 1; 1; 1 :] in
  let shape = [: 2; 2; 2; 3 :] in
  let src = Base_bigstring.of_string (String.make 384 'x') in
  let dst = Base_bigstring.create 384 in
  measure "Subset.copy rank 4" 10_000 (fun () ->
      Subset.copy ~elem_size:1 ~src ~src_outer:outer ~src_start:start ~dst
        ~dst_outer:outer ~dst_start:start ~shape);
  let shard =
    chain
      {|[{"name":"sharding_indexed","configuration":{
           "chunk_shape":[1],
           "codecs":[{"name":"bytes","configuration":{"endian":"little"}}],
           "index_codecs":[{"name":"bytes","configuration":{"endian":"little"}}],
           "index_location":"end"}}]|}
  in
  let repr = { Codec.dtype = Dtype.Uint8; shape = [| 1024 |] } in
  List.iter
    (fun (name, byte) ->
      let slab = Slab.create Dtype.Uint8 [: 1024 :] in
      Slab.fill slab byte;
      let encoded = Codec.encode_chunk shard slab in
      measure
        ("shard decode 1024 " ^ name ^ " chunks")
        100
        (fun () -> Codec.decode_chunk shard repr encoded);
      measure
        ("shard encode 1024 " ^ name ^ " chunks")
        100
        (fun () -> Codec.encode_chunk shard slab))
    [ ("present", "x"); ("absent", "\000") ];
  let zstd =
    chain
      {|[{"name":"bytes","configuration":{"endian":"little"}},
         {"name":"zstd","configuration":{"level":3,"checksum":false}}]|}
  in
  let slab = Slab.create Dtype.Uint8 [: 1024 :] in
  Slab.fill slab "x";
  let encoded = Codec.encode_chunk zstd slab in
  measure "zstd decode 1024 bytes" 1000 (fun () ->
      Codec.decode_chunk zstd repr encoded)
