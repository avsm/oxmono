module R = Cbort.Rw
module B = Bytesrw.Bytes

let check label condition = if not condition then failwith label

let input ~slice_length s =
  let buffer = Bytes.create (slice_length + 3) in
  let offset = ref 0 in
  let reader = B.Reader.make (fun () ->
    let length = min slice_length (String.length s - !offset) in
    if length = 0 then B.Slice.eod
    else begin
      Bytes.fill buffer 0 (Bytes.length buffer) '\xff';
      Bytes.blit_string s !offset buffer 3 length;
      offset := !offset + length;
      B.Slice.make buffer ~first:3 ~length
    end) in
  R.make_decoder reader

let bytes64 n =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 n;
  Bytes.unsafe_to_string b

let reference_arg info payload =
  match info with
  | _ when info < 24 -> Int64.of_int info
  | 24 -> Int64.of_int (Char.code payload.[0])
  | 25 -> Int64.of_int (String.get_uint16_be payload 0)
  | 26 ->
      Int64.logand (Int64.of_int32 (String.get_int32_be payload 0)) 0xffffffffL
  | 27 -> String.get_int64_be payload 0
  | 31 -> -1L
  | _ -> failwith (Printf.sprintf "Invalid additional info: %d" info)

let outcome f = try Ok (f ()) with Failure message -> Error message

let expect_failure f =
  match f () with
  | _ -> failwith "invalid CBOR accepted"
  | exception Failure _ -> ()

let test_headers slice_length =
  let payload = "\xff\xfe\xfd\xfc\xfb\xfa\xf9\xf8" in
  for byte = 0 to 255 do
    let d = input ~slice_length (String.make 1 (Char.chr byte) ^ payload) in
    let h = R.read_header d in
    check "public header fields" (h.major = byte lsr 5 && h.info = byte land 31);
    check "argument bits and failure messages"
      (outcome (fun () -> R.read_argument d h)
      = outcome (fun () -> reference_arg h.info payload))
  done

let test_integers slice_length =
  List.iter
    (fun n ->
      check "uint64 bits across slices"
        (R.read_u64_be (input ~slice_length (bytes64 n)) = n);
      let wire = "\x1b" ^ bytes64 n in
      if n >= 0L then
        check "signed int64 reader" (R.read_int (input ~slice_length wire) = n)
      else expect_failure (fun () -> R.read_int (input ~slice_length wire));
      let unsigned =
        if n >= 0L then Z.of_int64 n
        else Z.add (Z.of_int64 n) (Z.shift_left Z.one 64) in
      check "generic reader retains full unsigned range"
        (Cbort.Cbor.equal (R.read_cbor (input ~slice_length wire))
           (Cbort.Cbor.Int unsigned));
      let wire = "\x3b" ^ bytes64 n in
      if n >= 0L then
        check "signed negative integer"
          (R.read_int (input ~slice_length wire) = Int64.neg (Int64.succ n))
      else expect_failure (fun () -> R.read_int (input ~slice_length wire));
      check "generic reader retains full negative range"
        (Cbort.Cbor.equal (R.read_cbor (input ~slice_length wire))
           (Cbort.Cbor.Int (Z.neg (Z.succ unsigned)))))
    [
      0L;
      23L;
      24L;
      255L;
      256L;
      65535L;
      65536L;
      0xffffffffL;
      0x100000000L;
      Int64.max_int;
      Int64.min_int;
      -1L;
    ];
  for len = 0 to 7 do
    let d = input ~slice_length (String.make len '\xff') in
    (match R.read_u64_be d with
    | _ -> failwith "truncated integer accepted"
    | exception End_of_file -> ());
    check "truncated integer consumes available bytes"
      (R.decoder_position d = len)
  done

let test_values slice_length =
  let module C = Cbort.Cbor in
  let values =
    [
      C.Null;
      C.Bool true;
      C.Simple 19;
      C.Int (Z.of_string "18446744073709551615");
      C.Text (String.make 300 'x');
      C.Bytes "\x00\xff";
      C.Array [ C.int 23; C.int 24; C.Float (-0.) ];
      C.Map [ (C.Text "x", C.Array [ C.Bool false; C.int64 Int64.min_int ]) ];
    ]
  in
  let buf = Buffer.create 256 in
  let enc = R.make_encoder (B.Writer.of_buffer buf) in
  List.iter (R.write_cbor enc) values;
  R.flush_encoder enc;
  let dec = input ~slice_length (Buffer.contents buf) in
  List.iter
    (fun expected ->
      check "value stream roundtrip" (C.equal expected (R.read_cbor dec)))
    values;
  check "value stream fully consumed" (R.decoder_at_end dec)

let test_skip slice_length =
  List.iter (fun wire ->
    let d = input ~slice_length (wire ^ "\xf5") in
    R.skip d;
    check "skip stops before next item"
      (R.decoder_position d = String.length wire);
    check "successor survives skip" (R.read_bool d))
    ["\x40"; "\x61x"; "\x43abc"; "\x7f\x61x\x60\xff";
     "\x5f\x41x\x40\xff"; "\x82\x61x\x40";
     "\xa1\x61x\x80"; "\x9f\x61x\xff"; "\xc0\x61x";
     "\xdb" ^ bytes64 (-1L) ^ "\xf6"]

let test_malformed slice_length =
  List.iter (fun wire ->
    expect_failure (fun () -> R.read_cbor (input ~slice_length wire));
    expect_failure (fun () -> R.skip (input ~slice_length wire)))
    ["\x1f"; "\x3f"; "\xdf\x00"; "\xf8\x14"; "\xf8\x1f";
     "\xff"; "\xfc"; "\x7f\x01\xff"; "\x5f\x60\xff";
     "\x7f\x7f\xff\xff"; "\x5f\x5f\xff\xff"];
  List.iter (fun n ->
    List.iter (fun major ->
      let wire = String.make 1 (Char.chr ((major lsl 5) lor 27)) ^
        bytes64 n ^ "\xff" in
      expect_failure (fun () -> R.read_cbor (input ~slice_length wire));
      expect_failure (fun () -> R.skip (input ~slice_length wire));
      let read = match major with
        | 2 -> (fun d -> ignore (R.read_bytes_data d))
        | 3 -> (fun d -> ignore (R.read_text d))
        | 4 -> (fun d -> ignore (R.read_array_start d))
        | 5 -> (fun d -> ignore (R.read_map_start d))
        | _ -> (fun d -> ignore (R.read_tag d)) in
      expect_failure (fun () -> read (input ~slice_length wire))) [2;3;4;5;6])
    [Int64.succ (Int64.of_int max_int); Int64.min_int; -1L]

let test_invalid_writes () =
  let buf = Buffer.create 16 in
  let enc = R.make_encoder (B.Writer.of_buffer buf) in
  List.iter (fun write ->
    match write () with
    | _ -> failwith "invalid CBOR writer argument accepted"
    | exception Invalid_argument _ -> ())
    [ (fun () -> R.write_simple enc (-1));
      (fun () -> R.write_simple enc 24);
      (fun () -> R.write_simple enc 31);
      (fun () -> R.write_simple enc 256);
      (fun () -> R.write_array_start enc (-1));
      (fun () -> R.write_map_start enc (-1));
      (fun () -> R.write_bytes_header enc (-1));
      (fun () -> R.write_tag enc (-1)) ];
  R.flush_encoder enc;
  check "rejected writes emit no bytes" (Buffer.length buf = 0)

let () =
  test_invalid_writes ();
  List.iter
    (fun slice_length ->
      test_headers slice_length;
      test_integers slice_length;
      test_values slice_length;
      test_skip slice_length;
      test_malformed slice_length)
    [ 1; 2; 3; 7; 8; 9; 4096 ];
  print_endline "CBOR binary boundary and fragmented-input tests passed"
