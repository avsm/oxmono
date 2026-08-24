(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The performance contract of DESIGN.md, measured. Run it with:

     dune exec --profile release-check bleeding/zarrz/bench/bench_zarrz.exe

   The profile is not decoration. There is no unboxed 64 bit float load,
   so [Slab.F64] goes through an integer load and a bit cast, and only
   an optimising profile erases the two boxes that names. Under [dev]
   the sum loop below reads five times slower than it does under
   [release-check], which is a fact about the profile and not about the
   accessor.

   It is a plain executable, not a test: the numbers are hardware, and
   a threshold on them would fail for reasons that are not regressions.
   Three measurements, in the order the contract states them.

   1. Chunk decode throughput for a 64 MiB float64 array in 1 MiB
      chunks, through the zero-copy [bytes] chain and through the same
      chain behind zstd and gzip.
   2. A sum loop over 8M elements through [Slab.F64] against the same
      loop over the [Bigarray.Array1] that [Slab.to_genarray] hands out
      over the very same memory.
   3. A sharded partial read against a whole-shard decode, counting the
      store calls and the bytes each one moves. *)

module Arr = Zarrz.Arr
module Codec = Zarrz.Codec
module Dtype = Zarrz.Dtype
module Ext = Zarrz.Ext
module Fill_value = Zarrz.Fill_value
module Slab = Zarrz.Slab
module Store = Zarrz.Store
module Subset = Zarrz.Subset
module Ia = Stdlib_stable.Iarray
module Fu = Stdlib_upstream_compatible.Float_u

let mib = 1024. *. 1024.

(* [best n f] is the shortest wall clock time of [n] runs of [f], in
   seconds. The shortest run is the one least disturbed by the rest of
   the machine. *)
let best n f =
  let t = ref infinity in
  for _ = 1 to n do
    let t0 = Unix.gettimeofday () in
    f ();
    t := Float.min !t (Unix.gettimeofday () -. t0)
  done;
  !t

(* [scaled f] runs [f] as many times as it takes to spend a tenth of a
   second, and is the count and the seconds it took. Without this the
   zero-copy chain, which does no work at all, is timed entirely
   against the clock's own resolution. *)
let scaled f =
  let runs = ref 1 and secs = ref 0. in
  let settled = ref false in
  while not !settled do
    let t0 = Unix.gettimeofday () in
    for _ = 1 to !runs do
      f ()
    done;
    secs := Unix.gettimeofday () -. t0;
    if !secs >= 0.1 then settled := true else runs := !runs * 2
  done;
  (!runs, !secs)

let exts_of_string s =
  match Jsont_bytesrw.decode_string (Jsont.list Ext.jsont) s with
  | Ok l -> l
  | Error m -> failwith ("bench codec metadata: " ^ m)

let chain_of_string ~dtype s =
  let zero = String.make (Dtype.size dtype) '\000' in
  let fill_value = Fill_value.of_bytes zero in
  match Codec.chain_of_exts ~dtype ~fill_value (exts_of_string s) with
  | Ok c -> c
  | Error m -> failwith ("bench chain: " ^ m)

let rule () = print_endline (String.make 72 '-')

(* {1 Chunk decode throughput} *)

let bytes_le = {|{"name":"bytes","configuration":{"endian":"little"}}|}

let chains =
  [
    ("bytes le", Printf.sprintf "[%s]" bytes_le);
    ( "bytes le, zstd",
      Printf.sprintf {|[%s,{"name":"zstd","configuration":{"level":3,
        "checksum":false}}]|} bytes_le );
    ( "bytes le, gzip",
      Printf.sprintf {|[%s,{"name":"gzip","configuration":{"level":5}}]|}
        bytes_le );
  ]

(* One 1 MiB chunk of float64. The values are a smooth wave rather than
   zeros, so a compressor has real work to do and its throughput means
   something. *)
let chunk_elements = 1 lsl 17
let chunk_bytes = chunk_elements * 8
let chunk_count = 64

let sample_chunk () =
  let s = Slab.create Dtype.Float64 (Ia.of_list [ chunk_elements ]) in
  for i = 0 to chunk_elements - 1 do
    Slab.F64.unsafe_set s i (Fu.of_float (sin (float_of_int i *. 0.001)))
  done;
  s

(* Reading one element of every decoded slab keeps the decode alive
   through an optimiser that would otherwise see the result discarded,
   which matters for the chain whose decode is a no-op. *)
let sink = ref 0.0

let bench_decode () =
  let repr = { Codec.dtype = Dtype.Float64; shape = [| chunk_elements |] } in
  let chunk = sample_chunk () in
  let array = float_of_int (chunk_bytes * chunk_count) /. mib in
  Printf.printf "Chunk decode, %.0f MiB of float64 in %d chunks of %d KiB\n"
    array chunk_count (chunk_bytes / 1024);
  rule ();
  Printf.printf "%-16s %10s %8s %12s %12s\n" "chain" "stored" "ratio"
    "per array" "throughput";
  List.iter
    (fun (name, json) ->
      let c = chain_of_string ~dtype:Dtype.Float64 json in
      let enc = Codec.encode_chunk c chunk in
      let stored = Base_bigstring.length enc in
      let one_array () =
        for _ = 1 to chunk_count do
          let s = Codec.decode_chunk c repr enc in
          sink := !sink +. Fu.to_float (Slab.F64.unsafe_get s 0)
        done
      in
      one_array ();
      let runs, secs = scaled one_array in
      Printf.printf "%-16s %8.2f MiB %7.2fx %9.2f ms %7.0f MiB/s\n" name
        (float_of_int stored /. mib)
        (float_of_int chunk_bytes /. float_of_int stored)
        (secs /. float_of_int runs *. 1000.)
        (array *. float_of_int runs /. secs))
    chains;
  print_endline
    "(the bytes chain copies nothing, so its rate measures slab\n\
    \ construction rather than any bandwidth)";
  print_newline ()

(* {1 Element access} *)

let elements = 8_000_000

let bench_access () =
  let s = Slab.create Dtype.Float64 (Ia.of_list [ elements ]) in
  for i = 0 to elements - 1 do
    Slab.F64.unsafe_set s i (Fu.of_float 0.5)
  done;
  let ga = Slab.to_genarray s Bigarray.Float64 in
  let ba = Bigarray.array1_of_genarray ga in
  let checked = ref 0.0 in
  let slab_sum () =
    let mutable acc : float# = #0.0 in
    for i = 0 to elements - 1 do
      acc <- Fu.add acc (Slab.F64.unsafe_get s i)
    done;
    checked := Fu.to_float acc
  in
  let ba_sum () =
    let mutable acc : float# = #0.0 in
    for i = 0 to elements - 1 do
      acc <- Fu.add acc (Fu.of_float (Bigarray.Array1.unsafe_get ba i))
    done;
    checked := Fu.to_float acc
  in
  slab_sum ();
  ba_sum ();
  let a = best 5 slab_sum and b = best 5 ba_sum in
  let per t = t *. 1e9 /. float_of_int elements in
  Printf.printf "Sum loop over %d float64 elements (sum %.0f)\n" elements
    !checked;
  rule ();
  Printf.printf "%-28s %10s %12s\n" "view" "total" "per element";
  Printf.printf "%-28s %7.1f ms %9.2f ns\n" "Slab.F64.unsafe_get" (a *. 1000.)
    (per a);
  Printf.printf "%-28s %7.1f ms %9.2f ns\n" "Bigarray.Array1.unsafe_get"
    (b *. 1000.) (per b);
  print_newline ()

(* {1 Sharded partial reads} *)

type counts = {
  mutable get : int;
  mutable get_range : int;
  mutable get_ranges : int;
  mutable size : int;
  mutable bytes : int;
}

let counting (t : Store.t) =
  let c = { get = 0; get_range = 0; get_ranges = 0; size = 0; bytes = 0 } in
  let seen b =
    c.bytes <- c.bytes + Base_bigstring.length b;
    b
  in
  let t =
    {
      t with
      Store.get =
        (fun ~key ->
          c.get <- c.get + 1;
          Option.map seen (t.Store.get ~key));
      get_range =
        (fun ~key r ->
          c.get_range <- c.get_range + 1;
          Option.map seen (t.Store.get_range ~key r));
      get_ranges =
        (fun ~key rs ->
          c.get_ranges <- c.get_ranges + 1;
          Option.map (List.map seen) (t.Store.get_ranges ~key rs));
      size =
        (fun ~key ->
          c.size <- c.size + 1;
          t.Store.size ~key);
    }
  in
  (t, c)

let reset c =
  c.get <- 0;
  c.get_range <- 0;
  c.get_ranges <- 0;
  c.size <- 0;
  c.bytes <- 0

let shard_side = 64
let inner_side = 8
let array_side = 128

let shard_json =
  Printf.sprintf
    {|[{"name":"sharding_indexed","configuration":{
         "chunk_shape":[%d,%d],
         "codecs":[%s,{"name":"crc32c"}],
         "index_codecs":[%s,{"name":"crc32c"}],
         "index_location":"end"}}]|}
    inner_side inner_side bytes_le bytes_le

let sub_of ~start ~shape =
  { Subset.start = Ia.of_array start; shape = Ia.of_array shape }

let filled shape =
  let s = Slab.create Dtype.Float64 (Ia.of_array shape) in
  for i = 0 to Slab.num_elements s - 1 do
    Slab.F64.unsafe_set s i (Fu.of_float (float_of_int i))
  done;
  s

let bench_shard () =
  let build () =
    let m = Store.memory () in
    let s, c = counting m in
    let a =
      Arr.create
        ~codecs:(exts_of_string shard_json)
        ~shape:[| array_side; array_side |]
        ~chunk_shape:[| shard_side; shard_side |]
        ~dtype:Dtype.Float64
        ~fill_value:(Fill_value.of_bytes (String.make 8 '\000'))
        s ~path:"/a"
    in
    Arr.write a
      (sub_of ~start:[| 0; 0 |] ~shape:[| array_side; array_side |])
      (filled [| array_side; array_side |]);
    (c, a)
  in
  let ranged_c, ranged_a = build () in
  let whole_c, whole_s = build () in
  (* The same hierarchy behind a store that declares no ranged reads,
     which sends the core down the whole-shard path. *)
  let unranged = { (Arr.store whole_s) with Store.ranged = false } in
  let whole_a = Arr.open_ unranged ~path:"/a" in
  let one = sub_of ~start:[| 0; 0 |] ~shape:[| inner_side; inner_side |] in
  let run name a c =
    (* Count one read, then time a batch of them. Counting and timing
       the same runs would report the batch's calls, not a read's. *)
    reset c;
    ignore (Arr.read a one);
    let get = c.get and range = c.get_range and ranges = c.get_ranges in
    let size = c.size and bytes = c.bytes in
    let secs = best 5 (fun () -> ignore (Arr.read a one)) in
    Printf.printf "%-14s %5d %6d %7d %5d %10d B %9.1f us\n" name get range
      ranges size bytes (secs *. 1e6)
  in
  Printf.printf "One %dx%d float64 inner chunk of a %dx%d shard\n" inner_side
    inner_side shard_side shard_side;
  rule ();
  Printf.printf "%-14s %5s %6s %7s %5s %12s %12s\n" "store" "get" "range"
    "ranges" "size" "fetched" "read";
  run "ranged" ranged_a ranged_c;
  run "not ranged" whole_a whole_c;
  print_newline ()

(* {1 Driver} *)

let () =
  bench_decode ();
  bench_access ();
  bench_shard ()
