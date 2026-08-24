(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for the C-Blosc1 bindings. Everything is checked against the
   installed library rather than against a recorded frame: blosc does not
   promise byte-identical output across versions, only that it reads back
   what it wrote. *)

let bs = Base_bigstring.of_string
let str = Base_bigstring.to_string
let sub b ~len = Base_bigstring.sub b ~pos:0 ~len

(* The compressors this build has. Read once: the C call walks a static
   table, but the list is used by every round trip below. *)
let available = lazy (Bloscz.compressors ())

let has name = List.mem name (Lazy.force available)

(* Compress into a destination large enough that blosc cannot run out of
   room, and return the frame alone. *)
let compress ?level ?shuffle ?blocksize ~typesize ~cname data =
  let src = bs data in
  let src_len = String.length data in
  let dst_len = src_len + Bloscz.max_overhead in
  let dst = Base_bigstring.create dst_len in
  let n =
    Bloscz.compress ?level ?shuffle ?blocksize cname ~typesize ~src
      ~src_off:0 ~src_len ~dst ~dst_off:0 ~dst_len
  in
  Alcotest.(check bool)
    (Printf.sprintf "%s frame within the bound" cname)
    true (n > 0 && n <= dst_len);
  sub dst ~len:n

let decompress frame ~len =
  let dst = Base_bigstring.create (max len 1) in
  let n =
    Bloscz.decompress ~src:frame ~src_off:0
      ~src_len:(Base_bigstring.length frame) ~dst ~dst_off:0 ~dst_len:len
  in
  Alcotest.(check int) "decompressed length" len n;
  str (sub dst ~len)

let payloads =
  lazy
    [
      ("empty", "");
      ("one byte", "z");
      ("100 bytes", String.init 100 (fun i -> Char.chr (i land 0xff)));
      ("one mib random", String.init (1024 * 1024) (fun _ ->
                             Char.chr (Random.int 256)));
      ("one mib runs", String.init (1024 * 1024) (fun i ->
                           Char.chr (i / 4096 land 0xff)));
    ]

let shuffles = [ ("noshuffle", `No); ("shuffle", `Byte); ("bitshuffle", `Bit) ]

(* {1 The compressor list} *)

(* zstd is not optional for this tree: the Tessera store zarrz reads is
   written with it, so a build of the C library without it would pass
   every round trip below and still fail on real data. *)
let test_compressors () =
  let l = Lazy.force available in
  Alcotest.(check bool) "some compressor exists" true (l <> []);
  Alcotest.(check bool)
    (Printf.sprintf "zstd is in [%s]" (String.concat ", " l))
    true (List.mem "zstd" l);
  Alcotest.(check bool)
    "every name is non-empty" true
    (List.for_all (fun n -> n <> "") l);
  (* The names blosc can report are a closed set. A name outside it means
     the split or the C string went wrong. *)
  let known = [ "blosclz"; "lz4"; "lz4hc"; "snappy"; "zlib"; "zstd" ] in
  List.iter
    (fun n ->
      Alcotest.(check bool) (Printf.sprintf "%S is a blosc name" n) true
        (List.mem n known))
    l

let test_max_overhead () =
  Alcotest.(check int) "header length" 16 Bloscz.max_overhead

(* {1 Round trips} *)

let round_trip ~cname ~size_filter =
  List.iter
    (fun (sname, shuffle) ->
      List.iter
        (fun typesize ->
          List.iter
            (fun (pname, data) ->
              if size_filter (String.length data) then begin
                let what =
                  Printf.sprintf "%s %s typesize %d %s" cname sname typesize
                    pname
                in
                let frame = compress ~shuffle ~typesize ~cname data in
                Alcotest.(check string) what data
                  (decompress frame ~len:(String.length data))
              end)
            (Lazy.force payloads))
        [ 1; 4; 8 ])
    shuffles

let for_each_compressor f =
  List.iter
    (fun cname -> if has cname then f cname)
    [ "blosclz"; "lz4"; "lz4hc"; "snappy"; "zlib"; "zstd" ]

let test_round_trip_small () =
  for_each_compressor (fun cname ->
      round_trip ~cname ~size_filter:(fun n -> n <= 100))

let test_round_trip_large () =
  for_each_compressor (fun cname ->
      round_trip ~cname ~size_filter:(fun n -> n > 100))

let test_levels () =
  List.iter
    (fun level ->
      let data = String.init 4096 (fun i -> Char.chr (i / 16 land 0xff)) in
      let frame = compress ~level ~shuffle:`Byte ~typesize:4 ~cname:"zstd"
                    data in
      Alcotest.(check string)
        (Printf.sprintf "zstd level %d" level)
        data
        (decompress frame ~len:(String.length data)))
    [ 0; 1; 5; 9 ]

(* A blocksize blosc is told to use rather than one it picks. *)
let test_blocksize () =
  let data = String.init 65536 (fun i -> Char.chr (i land 0xff)) in
  List.iter
    (fun blocksize ->
      let frame =
        compress ~blocksize ~shuffle:`Byte ~typesize:4 ~cname:"zstd" data
      in
      let ~nbytes:_, ~cbytes:_, ~blocksize:got =
        Bloscz.buffer_sizes frame ~off:0
          ~len:(Base_bigstring.length frame)
      in
      if blocksize > 0 then
        Alcotest.(check int) "the requested blocksize was used" blocksize got;
      Alcotest.(check string)
        (Printf.sprintf "blocksize %d" blocksize)
        data
        (decompress frame ~len:(String.length data)))
    [ 0; 4096; 16384 ]

(* Blosc reads what it wrote whatever the offsets, so a frame written
   into the middle of one buffer decompresses out of the middle of it. *)
let test_offsets () =
  let data = String.init 1000 (fun i -> Char.chr (i land 0xff)) in
  let src = bs (String.make 7 'x' ^ data ^ String.make 3 'y') in
  let dst = Base_bigstring.create (11 + 1000 + Bloscz.max_overhead) in
  let n =
    Bloscz.compress "zstd" ~shuffle:`Byte ~typesize:4 ~src ~src_off:7
      ~src_len:1000 ~dst ~dst_off:11
      ~dst_len:(1000 + Bloscz.max_overhead)
  in
  let out = Base_bigstring.create 1005 in
  let m =
    Bloscz.decompress ~src:dst ~src_off:11 ~src_len:n ~dst:out ~dst_off:5
      ~dst_len:1000
  in
  Alcotest.(check int) "length" 1000 m;
  Alcotest.(check string) "payload" data
    (str (Base_bigstring.sub out ~pos:5 ~len:1000))

(* {1 Frame inspection} *)

let test_buffer_sizes () =
  let data = String.init 8192 (fun i -> Char.chr (i / 4 land 0xff)) in
  let frame = compress ~shuffle:`Byte ~typesize:4 ~cname:"zstd" data in
  let len = Base_bigstring.length frame in
  let ~nbytes, ~cbytes, ~blocksize = Bloscz.buffer_sizes frame ~off:0 ~len in
  Alcotest.(check int) "nbytes" 8192 nbytes;
  Alcotest.(check int) "cbytes is the whole frame" len cbytes;
  Alcotest.(check bool) "blocksize is positive" true (blocksize > 0);
  (* Only the header is read, so a range that stops at it works. *)
  let ~nbytes, ~cbytes, ~blocksize:_ =
    Bloscz.buffer_sizes frame ~off:0 ~len:Bloscz.max_overhead
  in
  Alcotest.(check int) "nbytes from the header alone" 8192 nbytes;
  Alcotest.(check int) "cbytes from the header alone" len cbytes;
  (* An empty payload is a header and nothing else. *)
  let empty = compress ~typesize:1 ~cname:"zstd" "" in
  let ~nbytes, ~cbytes, ~blocksize:_ =
    Bloscz.buffer_sizes empty ~off:0
      ~len:(Base_bigstring.length empty)
  in
  Alcotest.(check int) "empty nbytes" 0 nbytes;
  Alcotest.(check int) "empty cbytes" 16 cbytes

let test_buffer_sizes_garbage () =
  (* A header blosc does not recognise reads as three zeroes rather than
     as a crash, which is what makes the length guard the only check the
     OCaml side has to make. *)
  let junk = bs (String.make 64 '\xab') in
  let ~nbytes, ~cbytes, ~blocksize =
    Bloscz.buffer_sizes junk ~off:0 ~len:64
  in
  Alcotest.(check int) "nbytes" 0 nbytes;
  Alcotest.(check int) "cbytes" 0 cbytes;
  Alcotest.(check int) "blocksize" 0 blocksize

let test_buffer_sizes_short () =
  let frame = compress ~typesize:1 ~cname:"zstd" "hello" in
  Alcotest.check_raises "fifteen bytes"
    (Invalid_argument
       "Bloscz.buffer_sizes: 15 bytes cannot hold a 16 byte header")
    (fun () -> ignore (Bloscz.buffer_sizes frame ~off:0 ~len:15));
  Alcotest.check_raises "outside the buffer"
    (Invalid_argument
       (Printf.sprintf
          "Bloscz.buffer_sizes: 32 bytes at offset 0 outside a buffer of %d"
          (Base_bigstring.length frame)))
    (fun () -> ignore (Bloscz.buffer_sizes frame ~off:0 ~len:32))

let test_validate () =
  let data = String.init 4096 (fun i -> Char.chr (i land 0xff)) in
  let frame = compress ~shuffle:`Bit ~typesize:4 ~cname:"zstd" data in
  let len = Base_bigstring.length frame in
  Alcotest.(check (option int))
    "a whole frame" (Some 4096)
    (Bloscz.validate frame ~off:0 ~len);
  (* The length must be the frame's own, not merely enough of it. *)
  Alcotest.(check (option int))
    "a truncated frame" None
    (Bloscz.validate frame ~off:0 ~len:(len - 1));
  Alcotest.(check (option int))
    "a header alone" None
    (Bloscz.validate frame ~off:0 ~len:8);
  let padded = Base_bigstring.create (len + 10) in
  Base_bigstring.blit ~src:frame ~src_pos:0 ~dst:padded ~dst_pos:0 ~len;
  Alcotest.(check (option int))
    "trailing bytes" None
    (Bloscz.validate padded ~off:0 ~len:(len + 10));
  Alcotest.(check (option int))
    "the frame inside the padding" (Some 4096)
    (Bloscz.validate padded ~off:0 ~len);
  Alcotest.(check (option int))
    "garbage" None
    (Bloscz.validate (bs (String.make 64 '\xab')) ~off:0 ~len:64);
  Alcotest.(check (option int))
    "nothing at all" None
    (Bloscz.validate (bs "") ~off:0 ~len:0)

(* {1 Failure paths} *)

let raises what f =
  match f () with
  | _ -> Alcotest.failf "%s did not raise" what
  | exception Bloscz.Error (code, msg) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s reports %d: %s" what code msg)
        true (msg <> "")

let test_dst_too_small () =
  let data = String.init 4096 (fun _ -> Char.chr (Random.int 256)) in
  let src = bs data in
  let dst = Base_bigstring.create 32 in
  raises "a destination of 32 bytes" (fun () ->
      Bloscz.compress "zstd" ~typesize:1 ~src ~src_off:0 ~src_len:4096
        ~dst ~dst_off:0 ~dst_len:32)

(* Random bytes do not compress, so the frame is the input plus the
   overhead exactly and this is the tightest destination that works. *)
let test_exact_bound () =
  let data = String.init 8192 (fun _ -> Char.chr (Random.int 256)) in
  let src = bs data in
  let dst_len = 8192 + Bloscz.max_overhead in
  let dst = Base_bigstring.create dst_len in
  let n =
    Bloscz.compress "zstd" ~typesize:1 ~src ~src_off:0 ~src_len:8192
      ~dst ~dst_off:0 ~dst_len
  in
  Alcotest.(check bool) "the frame fits the bound" true (n <= dst_len);
  Alcotest.(check string) "round trip" data
    (decompress (sub dst ~len:n) ~len:8192);
  (* One byte short of the bound is where blosc gives up on an
     incompressible input. *)
  let dst = Base_bigstring.create (dst_len - 1) in
  raises "one byte short of the bound" (fun () ->
      Bloscz.compress "zstd" ~typesize:1 ~src ~src_off:0 ~src_len:8192
        ~dst ~dst_off:0 ~dst_len:(dst_len - 1))

let test_unknown_compressor () =
  let src = bs "hello" in
  let dst = Base_bigstring.create 64 in
  match
    Bloscz.compress "nosuchthing" ~typesize:1 ~src ~src_off:0
      ~src_len:5 ~dst ~dst_off:0 ~dst_len:64
  with
  | _ -> Alcotest.fail "an unknown compressor was accepted"
  | exception Bloscz.Error (code, _) ->
      Alcotest.(check int) "not supported" (-5) code

(* A frame the caller did not write must fail rather than run off the
   end of the buffer. Every one of these reaches blosc through the same
   path a store's bytes would. *)
let test_corrupt () =
  let data = String.init 4096 (fun i -> Char.chr (i land 0xff)) in
  let frame = compress ~shuffle:`Byte ~typesize:4 ~cname:"zstd" data in
  let len = Base_bigstring.length frame in
  let attempt what src src_len =
    let dst = Base_bigstring.create 4096 in
    match
      Bloscz.decompress ~src ~src_off:0 ~src_len ~dst ~dst_off:0
        ~dst_len:4096
    with
    | n ->
        Alcotest.(check bool)
          (Printf.sprintf "%s did not produce the payload" what)
          true
          (n <> 4096 || str dst <> data)
    | exception Bloscz.Error _ -> ()
  in
  attempt "garbage" (bs (String.make 200 '\xab')) 200;
  attempt "a truncated frame" (sub frame ~len:(len / 2)) (len / 2);
  attempt "a header alone" (sub frame ~len:16) 16;
  (* One flipped byte in the compressed body. *)
  let flipped = Base_bigstring.create len in
  Base_bigstring.blit ~src:frame ~src_pos:0 ~dst:flipped ~dst_pos:0 ~len;
  let at = len - 3 in
  Base_bigstring.set flipped at
    (Char.chr (Char.code (Base_bigstring.get flipped at) lxor 0xff));
  attempt "a flipped byte" flipped len

let test_decompress_dst_too_small () =
  let data = String.init 4096 (fun i -> Char.chr (i land 0xff)) in
  let frame = compress ~shuffle:`Byte ~typesize:4 ~cname:"zstd" data in
  let dst = Base_bigstring.create 100 in
  raises "a destination of 100 bytes" (fun () ->
      Bloscz.decompress ~src:frame ~src_off:0
        ~src_len:(Base_bigstring.length frame) ~dst ~dst_off:0 ~dst_len:100)

(* The guards the C library does not make for itself. A typesize of zero
   is the important one: blosc divides by it and raises SIGFPE. *)
let test_argument_guards () =
  let src = bs "hello" in
  let dst = Base_bigstring.create 64 in
  let call ?level ?shuffle ?blocksize ?(typesize = 1) ?(cname = "zstd")
      ?(src_off = 0) ?(src_len = 5) ?(dst_off = 0) ?(dst_len = 64) () =
    ignore
      (Bloscz.compress ?level ?shuffle ?blocksize cname ~typesize ~src
         ~src_off ~src_len ~dst ~dst_off ~dst_len)
  in
  Alcotest.check_raises "typesize 0"
    (Invalid_argument "Bloscz.compress: typesize 0 is below 1") (fun () ->
      call ~typesize:0 ());
  Alcotest.check_raises "level 10"
    (Invalid_argument "Bloscz.compress: level 10 is outside [0, 9]") (fun () ->
      call ~level:10 ());
  Alcotest.check_raises "level -1"
    (Invalid_argument "Bloscz.compress: level -1 is outside [0, 9]") (fun () ->
      call ~level:(-1) ());
  Alcotest.check_raises "a negative blocksize"
    (Invalid_argument "Bloscz.compress: blocksize -1 is negative") (fun () ->
      call ~blocksize:(-1) ());
  Alcotest.check_raises "an empty compressor name"
    (Invalid_argument "Bloscz.compress: compressor name of 0 bytes")
    (fun () -> call ~cname:"" ());
  Alcotest.check_raises "a compressor name of 32 bytes"
    (Invalid_argument "Bloscz.compress: compressor name of 32 bytes")
    (fun () -> call ~cname:(String.make 32 'z') ());
  Alcotest.check_raises "a source past the end"
    (Invalid_argument
       "Bloscz.compress: 6 bytes at offset 0 outside a buffer of 5") (fun () ->
      call ~src_len:6 ());
  Alcotest.check_raises "a negative source offset"
    (Invalid_argument
       "Bloscz.compress: 5 bytes at offset -1 outside a buffer of 5")
    (fun () -> call ~src_off:(-1) ());
  Alcotest.check_raises "a destination past the end"
    (Invalid_argument
       "Bloscz.compress: 64 bytes at offset 1 outside a buffer of 64")
    (fun () -> call ~dst_off:1 ());
  Alcotest.check_raises "a decompression source past the end"
    (Invalid_argument
       "Bloscz.decompress: 9 bytes at offset 0 outside a buffer of 5")
    (fun () ->
      ignore
        (Bloscz.decompress ~src ~src_off:0 ~src_len:9 ~dst ~dst_off:0
           ~dst_len:64))

(* {1 Portability}

   A compiler probe for the [@@ portable] claim in bloscz.mli rather than
   a runtime check: this closure is ascribed [portable], so it compiles
   only if every value it names is portable too. *)
let portable_probe : (Base_bigstring.t -> int) @ portable =
 fun src ->
  let src_len = Base_bigstring.length src in
  let dst_len = src_len + Bloscz.max_overhead in
  let dst = Base_bigstring.create dst_len in
  let n =
    Bloscz.compress "blosclz" ~shuffle:`Byte ~typesize:4 ~src
      ~src_off:0 ~src_len ~dst ~dst_off:0 ~dst_len
  in
  let ~nbytes, ~cbytes:_, ~blocksize:_ =
    Bloscz.buffer_sizes dst ~off:0 ~len:n
  in
  match Bloscz.validate dst ~off:0 ~len:n with
  | Some m when m = nbytes ->
      Bloscz.decompress ~src:dst ~src_off:0 ~src_len:n ~dst:src ~dst_off:0
        ~dst_len:src_len
  | _ -> -1

let test_portable () =
  let data = String.init 4096 (fun i -> Char.chr (i land 0xff)) in
  let buf = bs data in
  Alcotest.(check int) "a portable round trip" 4096 (portable_probe buf);
  Alcotest.(check string) "the payload survived" data (str buf)

let () =
  Random.init 20260824;
  Alcotest.run "bloscz"
    [
      ( "library",
        [
          ("compressors", `Quick, test_compressors);
          ("max overhead", `Quick, test_max_overhead);
          ("portable", `Quick, test_portable);
        ] );
      ( "round trips",
        [
          ("small", `Quick, test_round_trip_small);
          ("one mib", `Slow, test_round_trip_large);
          ("levels", `Quick, test_levels);
          ("blocksize", `Quick, test_blocksize);
          ("offsets", `Quick, test_offsets);
        ] );
      ( "frames",
        [
          ("buffer_sizes", `Quick, test_buffer_sizes);
          ("buffer_sizes on garbage", `Quick, test_buffer_sizes_garbage);
          ("buffer_sizes length guard", `Quick, test_buffer_sizes_short);
          ("validate", `Quick, test_validate);
        ] );
      ( "failures",
        [
          ("destination too small", `Quick, test_dst_too_small);
          ("the exact bound", `Quick, test_exact_bound);
          ("an unknown compressor", `Quick, test_unknown_compressor);
          ("corrupt frames", `Quick, test_corrupt);
          ("decompression destination", `Quick, test_decompress_dst_too_small);
          ("argument guards", `Quick, test_argument_guards);
        ] );
    ]
