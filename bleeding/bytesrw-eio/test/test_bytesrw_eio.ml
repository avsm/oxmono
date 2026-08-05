(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Test reading from a mock flow *)
let test_reader_basic () =
  Eio_main.run @@ fun _env ->
  let test_data = "Hello, World!" in
  let flow = Eio.Flow.string_source test_data in
  let reader = Bytesrw_eio.bytes_reader_of_flow flow in

  (* Read first slice *)
  let slice1 = Bytesrw.Bytes.Reader.read reader in
  Alcotest.(check bool)
    "slice is not eod" false
    (Bytesrw.Bytes.Slice.is_eod slice1);

  let read_data =
    Bytes.sub_string
      (Bytesrw.Bytes.Slice.bytes slice1)
      (Bytesrw.Bytes.Slice.first slice1)
      (Bytesrw.Bytes.Slice.length slice1)
  in
  Alcotest.(check string) "data matches" test_data read_data;

  (* Next read should be eod *)
  let slice2 = Bytesrw.Bytes.Reader.read reader in
  Alcotest.(check bool)
    "second read is eod" true
    (Bytesrw.Bytes.Slice.is_eod slice2)

(* Test reading with custom slice length *)
let test_reader_custom_slice_length () =
  Eio_main.run @@ fun _env ->
  let test_data = "Hello, World!" in
  let flow = Eio.Flow.string_source test_data in
  let slice_length = 5 in
  let reader = Bytesrw_eio.bytes_reader_of_flow ~slice_length flow in

  (* Read should respect slice_length as maximum *)
  let slice = Bytesrw.Bytes.Reader.read reader in
  Alcotest.(check bool)
    "slice length <= custom size" true
    (Bytesrw.Bytes.Slice.length slice <= slice_length)

(* Test reading empty flow *)
let test_reader_empty () =
  Eio_main.run @@ fun _env ->
  let flow = Eio.Flow.string_source "" in
  let reader = Bytesrw_eio.bytes_reader_of_flow flow in

  let slice = Bytesrw.Bytes.Reader.read reader in
  Alcotest.(check bool)
    "empty flow returns eod" true
    (Bytesrw.Bytes.Slice.is_eod slice)

(* Test writing to a mock flow *)
let test_writer_basic () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 100 in
  let flow = Eio.Flow.buffer_sink buf in
  let writer = Bytesrw_eio.bytes_writer_of_flow flow in

  let test_data = "Hello, World!" in
  let bytes = Bytes.of_string test_data in
  let slice =
    Bytesrw.Bytes.Slice.make bytes ~first:0 ~length:(Bytes.length bytes)
  in

  Bytesrw.Bytes.Writer.write writer slice;

  let written = Buffer.contents buf in
  Alcotest.(check string) "written data matches" test_data written

(* Test writing with custom slice length *)
let test_writer_custom_slice_length () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 100 in
  let flow = Eio.Flow.buffer_sink buf in
  let slice_length = 8 in
  let writer = Bytesrw_eio.bytes_writer_of_flow ~slice_length flow in

  let test_data = "Hello, World!" in
  let bytes = Bytes.of_string test_data in
  let slice =
    Bytesrw.Bytes.Slice.make bytes ~first:0 ~length:(Bytes.length bytes)
  in

  Bytesrw.Bytes.Writer.write writer slice;

  let written = Buffer.contents buf in
  Alcotest.(check string)
    "written data matches regardless of slice_length" test_data written

(* Test writing eod slice (should be no-op) *)
let test_writer_eod () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 100 in
  let flow = Eio.Flow.buffer_sink buf in
  let writer = Bytesrw_eio.bytes_writer_of_flow flow in

  Bytesrw.Bytes.Writer.write writer Bytesrw.Bytes.Slice.eod;

  let written = Buffer.contents buf in
  Alcotest.(check string) "eod writes nothing" "" written

(* Test writing partial slice *)
let test_writer_partial_slice () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 100 in
  let flow = Eio.Flow.buffer_sink buf in
  let writer = Bytesrw_eio.bytes_writer_of_flow flow in

  let test_data = "Hello, World!" in
  let bytes = Bytes.of_string test_data in
  (* Write only "World" *)
  let slice = Bytesrw.Bytes.Slice.make bytes ~first:7 ~length:5 in

  Bytesrw.Bytes.Writer.write writer slice;

  let written = Buffer.contents buf in
  Alcotest.(check string) "partial slice written" "World" written

(* Test multiple reads to ensure data isolation - buffers from previous reads
   should not be corrupted by subsequent reads *)
let test_reader_multiple_reads () =
  Eio_main.run @@ fun _env ->
  let test_data = "ABCDEFGHIJ" in
  (* 10 bytes *)
  let flow = Eio.Flow.string_source test_data in
  let reader = Bytesrw_eio.bytes_reader_of_flow ~slice_length:5 flow in

  (* Read first 5 bytes *)
  let slice1 = Bytesrw.Bytes.Reader.read reader in
  let bytes1 = Bytesrw.Bytes.Slice.bytes slice1 in
  let data1 =
    Bytes.sub_string bytes1
      (Bytesrw.Bytes.Slice.first slice1)
      (Bytesrw.Bytes.Slice.length slice1)
  in

  (* Read next 5 bytes *)
  let slice2 = Bytesrw.Bytes.Reader.read reader in
  let data2 =
    Bytes.sub_string
      (Bytesrw.Bytes.Slice.bytes slice2)
      (Bytesrw.Bytes.Slice.first slice2)
      (Bytesrw.Bytes.Slice.length slice2)
  in

  (* Critical test: verify first read's data is STILL intact after second read
     This would fail if we were reusing buffers or if Cstruct.to_bytes created a view *)
  let data1_check =
    Bytes.sub_string bytes1
      (Bytesrw.Bytes.Slice.first slice1)
      (Bytesrw.Bytes.Slice.length slice1)
  in

  Alcotest.(check string) "first read" "ABCDE" data1;
  Alcotest.(check string) "second read" "FGHIJ" data2;
  Alcotest.(check string)
    "first read still intact after second" "ABCDE" data1_check

(* Test round-trip: write then read *)
let test_roundtrip () =
  Eio_main.run @@ fun _env ->
  let test_data = "Round-trip test data" in

  (* Write to buffer *)
  let buf = Buffer.create 100 in
  let write_flow = Eio.Flow.buffer_sink buf in
  let writer = Bytesrw_eio.bytes_writer_of_flow write_flow in

  let bytes = Bytes.of_string test_data in
  let slice =
    Bytesrw.Bytes.Slice.make bytes ~first:0 ~length:(Bytes.length bytes)
  in
  Bytesrw.Bytes.Writer.write writer slice;

  (* Read back from buffer *)
  let read_flow = Eio.Flow.string_source (Buffer.contents buf) in
  let reader = Bytesrw_eio.bytes_reader_of_flow read_flow in

  let read_slice = Bytesrw.Bytes.Reader.read reader in
  let read_data =
    Bytes.sub_string
      (Bytesrw.Bytes.Slice.bytes read_slice)
      (Bytesrw.Bytes.Slice.first read_slice)
      (Bytesrw.Bytes.Slice.length read_slice)
  in

  Alcotest.(check string) "round-trip data matches" test_data read_data

let () =
  Alcotest.run "Bytesrw_eio"
    [
      ( "reader",
        [
          Alcotest.test_case "basic read" `Quick test_reader_basic;
          Alcotest.test_case "custom slice length" `Quick
            test_reader_custom_slice_length;
          Alcotest.test_case "empty flow" `Quick test_reader_empty;
          Alcotest.test_case "multiple reads data isolation" `Quick
            test_reader_multiple_reads;
        ] );
      ( "writer",
        [
          Alcotest.test_case "basic write" `Quick test_writer_basic;
          Alcotest.test_case "custom slice length" `Quick
            test_writer_custom_slice_length;
          Alcotest.test_case "eod write" `Quick test_writer_eod;
          Alcotest.test_case "partial slice" `Quick test_writer_partial_slice;
        ] );
      ("integration", [ Alcotest.test_case "round-trip" `Quick test_roundtrip ]);
    ]
