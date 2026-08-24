(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module I64 = Stdlib_upstream_compatible.Int64_u

let cctx = Zstdz.create_cctx ()
let dctx = Zstdz.create_dctx ()

(* Compressible enough that a 1 MiB frame is not just stored, varied
   enough that a broken window would show up as a mismatch. *)
let sample n =
  String.init n (fun i -> Char.chr (((i / 13) + (i land 31)) land 0xff))

let bs = Base_bigstring.of_string
let str b ~len = Base_bigstring.to_string ~pos:0 ~len b

let compress ?level ?checksum s =
  let src_len = String.length s in
  let dst_len = Zstdz.compress_bound src_len in
  let dst = Base_bigstring.create dst_len in
  let n =
    Zstdz.compress ?level ?checksum cctx ~src:(bs s) ~src_off:0 ~src_len ~dst
      ~dst_off:0 ~dst_len
  in
  (dst, n)

let decompress frame frame_len ~size =
  let dst = Base_bigstring.create size in
  let n =
    Zstdz.decompress dctx ~src:frame ~src_off:0 ~src_len:frame_len ~dst
      ~dst_off:0 ~dst_len:size
  in
  str dst ~len:n

(* error_name returns a stack-allocated string, so a name reaches the
   heap only through a copy. *)
let name_of code =
  Bytes.unsafe_to_string (Bytes.of_string (Zstdz.error_name code))

let sizes = [ 0; 1; 100; 1024 * 1024 ]
let levels = [ 1; 3; 19 ]

let test_round_trip () =
  List.iter
    (fun size ->
      let s = sample size in
      List.iter
        (fun level ->
          List.iter
            (fun checksum ->
              let name =
                Printf.sprintf "size %d level %d checksum %b" size level
                  checksum
              in
              let frame, flen = compress ~level ~checksum s in
              Alcotest.(check bool)
                (name ^ ": frame non-empty")
                true (flen > 0);
              Alcotest.(check string) name s (decompress frame flen ~size);
              let cs = Zstdz.content_size frame ~off:0 ~len:flen in
              Alcotest.(check int64)
                (name ^ ": content size")
                (Int64.of_int size) (I64.to_int64 cs))
            [ false; true ])
        levels)
    sizes

(* The default level must behave as level 3 does, since that is what the
   documentation promises. *)
let test_default_level () =
  let s = sample 4096 in
  let a, alen = compress s in
  let b, blen = compress ~level:3 s in
  Alcotest.(check int) "same length" blen alen;
  Alcotest.(check string) "same bytes" (str b ~len:blen) (str a ~len:alen)

let test_exact_bound () =
  (* Incompressible input is the case where compress_bound is not slack. *)
  let s = String.init 8192 (fun i -> Char.chr ((i * 2654435761) land 0xff)) in
  let src_len = String.length s in
  let dst_len = Zstdz.compress_bound src_len in
  let dst = Base_bigstring.create dst_len in
  let n =
    Zstdz.compress cctx ~src:(bs s) ~src_off:0 ~src_len ~dst ~dst_off:0
      ~dst_len
  in
  Alcotest.(check bool) "fits the bound" true (n <= dst_len);
  let out = Base_bigstring.create src_len in
  let m =
    Zstdz.decompress dctx ~src:dst ~src_off:0 ~src_len:n ~dst:out ~dst_off:0
      ~dst_len:src_len
  in
  Alcotest.(check int) "exact size" src_len m;
  Alcotest.(check string) "round trip" s (str out ~len:m)

(* Both ends of both buffers must be respected, since a codec pipeline
   compresses into the middle of a shard. *)
let test_offsets () =
  let s = sample 5000 in
  let src_len = String.length s in
  let src = Base_bigstring.create (src_len + 64) in
  Base_bigstring.memset src ~pos:0 ~len:(src_len + 64) '\xaa';
  Base_bigstring.From_string.blit ~src:s ~src_pos:0 ~dst:src ~dst_pos:32
    ~len:src_len;
  let bound = Zstdz.compress_bound src_len in
  let dst = Base_bigstring.create (bound + 64) in
  Base_bigstring.memset dst ~pos:0 ~len:(bound + 64) '\xbb';
  let n =
    Zstdz.compress cctx ~src ~src_off:32 ~src_len ~dst ~dst_off:16
      ~dst_len:bound
  in
  Alcotest.(check char) "before dst untouched" '\xbb'
    (Base_bigstring.get dst 15);
  (* Nothing is asserted about dst beyond the frame: libzstd uses the
     rest of the destination as scratch and leaves it undefined. *)
  let out = Base_bigstring.create (src_len + 8) in
  Base_bigstring.memset out ~pos:0 ~len:(src_len + 8) '\xcc';
  let m =
    Zstdz.decompress dctx ~src:dst ~src_off:16 ~src_len:n ~dst:out ~dst_off:4
      ~dst_len:src_len
  in
  Alcotest.(check int) "size" src_len m;
  Alcotest.(check char) "before out untouched" '\xcc'
    (Base_bigstring.get out 3);
  Alcotest.(check char) "after out untouched" '\xcc'
    (Base_bigstring.get out (4 + src_len));
  Alcotest.(check string) "content" s
    (Base_bigstring.to_string ~pos:4 ~len:src_len out)

let test_content_size () =
  let frame, flen = compress (sample 1234) in
  Alcotest.(check int64)
    "known frame" 1234L
    (I64.to_int64 (Zstdz.content_size frame ~off:0 ~len:flen));
  let garbage = bs "this is definitely not a zstd frame" in
  Alcotest.(check int64)
    "garbage" (-2L)
    (I64.to_int64
       (Zstdz.content_size garbage ~off:0
          ~len:(Base_bigstring.length garbage)));
  (* A header cut short is indistinguishable from garbage here. *)
  Alcotest.(check int64)
    "short header" (-2L)
    (I64.to_int64 (Zstdz.content_size frame ~off:0 ~len:2))

let test_frame_info () =
  let frame, flen = compress ~level:3 ~checksum:true (sample 1234) in
  let content, window, dict, checksum =
    let i = Zstdz.frame_info frame ~off:0 ~len:flen in
    (i.content_size, i.window_size, i.dict_id, i.has_checksum)
  in
  Alcotest.(check int) "content size" 1234 content;
  Alcotest.(check bool) "window size positive" true (window > 0);
  Alcotest.(check int) "no dictionary" 0 dict;
  Alcotest.(check bool) "checksum present" true checksum;
  let frame, flen = compress ~level:3 ~checksum:false (sample 1234) in
  let checksum =
    let i = Zstdz.frame_info frame ~off:0 ~len:flen in
    i.has_checksum
  in
  Alcotest.(check bool) "checksum absent" false checksum;
  let garbage = bs "this is definitely not a zstd frame" in
  let content =
    let i =
      Zstdz.frame_info garbage ~off:0 ~len:(Base_bigstring.length garbage)
    in
    i.content_size
  in
  Alcotest.(check int) "garbage" (-2) content

let test_error_name () =
  let check expect code =
    Alcotest.(check string)
      (Printf.sprintf "code %d" code)
      expect (name_of code)
  in
  check "No error detected" 0;
  check "Destination buffer is too small" 70;
  check "Data corruption detected" 20;
  check "Restored data doesn't match checksum" 22;
  check "Unspecified error code" 119

let error = function
  | Zstdz.Error (code, name) -> Some (code, name)
  | _ -> None

let expect_error what f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Zstdz.Error" what
  | exception e -> (
      match error e with
      | Some (code, name) ->
          Alcotest.(check bool) (what ^ ": positive code") true (code > 0);
          Alcotest.(check bool)
            (what ^ ": named")
            true
            (String.length name > 0);
          code
      | None -> Alcotest.failf "%s: unexpected %s" what (Printexc.to_string e))

let test_truncated () =
  let s = sample 4096 in
  let frame, flen = compress s in
  let code =
    expect_error "truncated frame" (fun () ->
        decompress frame (flen - 1) ~size:(String.length s))
  in
  Alcotest.(check string)
    "srcSize_wrong" "Src size is incorrect"
    (name_of code);
  (* A frame cut short with a checksum fails on the checksum instead. *)
  let frame, flen = compress ~checksum:true s in
  let code =
    expect_error "truncated frame with checksum" (fun () ->
        decompress frame (flen - 1) ~size:(String.length s))
  in
  Alcotest.(check string)
    "checksum_wrong" "Restored data doesn't match checksum"
    (name_of code);
  (* A corrupt payload under a valid header. *)
  let corrupt = Base_bigstring.create flen in
  Base_bigstring.blit ~src:frame ~src_pos:0 ~dst:corrupt ~dst_pos:0 ~len:flen;
  let mid = flen / 2 in
  Base_bigstring.set corrupt mid
    (Char.chr (Char.code (Base_bigstring.get corrupt mid) lxor 0xff));
  ignore
    (expect_error "corrupt frame" (fun () ->
         decompress corrupt flen ~size:(String.length s)))

let test_dst_too_small () =
  let s = sample 4096 in
  let frame, flen = compress s in
  let code =
    expect_error "decompress dst too small" (fun () ->
        decompress frame flen ~size:(String.length s - 1))
  in
  Alcotest.(check string)
    "dstSize_tooSmall" "Destination buffer is too small"
    (name_of code);
  let src_len = String.length s in
  ignore
    (expect_error "compress dst too small" (fun () ->
         let dst = Base_bigstring.create 8 in
         Zstdz.compress cctx ~src:(bs s) ~src_off:0 ~src_len ~dst ~dst_off:0
           ~dst_len:8))

let expect_invalid what f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Invalid_argument" what
  | exception Invalid_argument _ -> ()
  | exception e ->
      Alcotest.failf "%s: unexpected %s" what (Printexc.to_string e)

let test_bad_ranges () =
  let b = Base_bigstring.create 16 in
  expect_invalid "negative src_off" (fun () ->
      Zstdz.compress cctx ~src:b ~src_off:(-1) ~src_len:4 ~dst:b ~dst_off:0
        ~dst_len:16);
  expect_invalid "negative src_len" (fun () ->
      Zstdz.compress cctx ~src:b ~src_off:0 ~src_len:(-1) ~dst:b ~dst_off:0
        ~dst_len:16);
  expect_invalid "src past the end" (fun () ->
      Zstdz.compress cctx ~src:b ~src_off:8 ~src_len:9 ~dst:b ~dst_off:0
        ~dst_len:16);
  expect_invalid "dst past the end" (fun () ->
      Zstdz.compress cctx ~src:b ~src_off:0 ~src_len:4 ~dst:b ~dst_off:1
        ~dst_len:16);
  expect_invalid "decompress src past the end" (fun () ->
      Zstdz.decompress dctx ~src:b ~src_off:0 ~src_len:17 ~dst:b ~dst_off:0
        ~dst_len:16);
  expect_invalid "content_size past the end" (fun () ->
      I64.to_int64 (Zstdz.content_size b ~off:12 ~len:8));
  expect_invalid "frame_info past the end" (fun () ->
      let i = Zstdz.frame_info b ~off:12 ~len:8 in
      i.content_size)

let test_compress_bound () =
  Alcotest.(check bool) "empty" true (Zstdz.compress_bound 0 > 0);
  Alcotest.(check bool) "monotone" true
    (Zstdz.compress_bound 1000 < Zstdz.compress_bound 100000)

let read_file path =
  In_channel.with_open_bin path In_channel.input_all

let write_file path s =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc s)

let have_zstd = lazy (Sys.command "command -v zstd >/dev/null 2>&1" = 0)

(* The system CLI is the differential oracle: a frame it wrote must
   decode here, and a frame written here must decode there. *)
let test_cli () =
  if not (Lazy.force have_zstd) then
    Alcotest.skip ()
  else begin
    let s = sample 200_000 in
    write_file "cli_in.bin" s;
    Alcotest.(check int)
      "zstd compress" 0
      (Sys.command "zstd -q -f -19 -o cli_in.zst cli_in.bin");
    let frame_s = read_file "cli_in.zst" in
    let frame = bs frame_s in
    let flen = String.length frame_s in
    Alcotest.(check int64)
      "cli content size"
      (Int64.of_int (String.length s))
      (I64.to_int64 (Zstdz.content_size frame ~off:0 ~len:flen));
    (* Streaming from a pipe leaves the header without a content size,
       which is the only way to reach the -1 case. *)
    Alcotest.(check int)
      "zstd stream" 0
      (Sys.command "zstd -q -f < cli_in.bin > cli_stream.zst");
    let stream_s = read_file "cli_stream.zst" in
    let stream = bs stream_s in
    let slen = String.length stream_s in
    Alcotest.(check int64)
      "streamed content size" (-1L)
      (I64.to_int64 (Zstdz.content_size stream ~off:0 ~len:slen));
    let content =
      let i = Zstdz.frame_info stream ~off:0 ~len:slen in
      i.content_size
    in
    Alcotest.(check int) "streamed frame_info" (-1) content;
    Alcotest.(check string)
      "decode streamed frame" s
      (decompress stream slen ~size:(String.length s));
    Alcotest.(check string)
      "decode cli frame" s
      (decompress frame flen ~size:(String.length s));
    let ours, ourlen = compress ~level:7 ~checksum:true s in
    write_file "ours.zst" (str ours ~len:ourlen);
    Alcotest.(check int)
      "zstd decompress" 0
      (Sys.command "zstd -q -d -f -o ours.bin ours.zst");
    Alcotest.(check string) "cli decodes ours" s (read_file "ours.bin")
  end

let () =
  Alcotest.run "zstdz"
    [
      ( "round trip",
        [
          Alcotest.test_case "sizes and levels" `Slow test_round_trip;
          Alcotest.test_case "default level" `Quick test_default_level;
          Alcotest.test_case "exact bound" `Quick test_exact_bound;
          Alcotest.test_case "offsets" `Quick test_offsets;
          Alcotest.test_case "compress_bound" `Quick test_compress_bound;
        ] );
      ( "frames",
        [
          Alcotest.test_case "content_size" `Quick test_content_size;
          Alcotest.test_case "frame_info" `Quick test_frame_info;
        ] );
      ( "errors",
        [
          Alcotest.test_case "error_name" `Quick test_error_name;
          Alcotest.test_case "truncated" `Quick test_truncated;
          Alcotest.test_case "dst too small" `Quick test_dst_too_small;
          Alcotest.test_case "bad ranges" `Quick test_bad_ranges;
        ] );
      ("cli", [ Alcotest.test_case "differential" `Quick test_cli ]);
    ]
