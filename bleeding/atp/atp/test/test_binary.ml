module D = Atp.Dagcbor
module C = Atp.Car
module Reader = Bytesrw.Bytes.Reader

module R = struct
  include Reader

  let of_string ~slice_length s =
    let buffer = Bytes.create (slice_length + 3) in
    let offset = ref 0 in
    Reader.make (fun () ->
      let length = min slice_length (String.length s - !offset) in
      if length = 0 then Bytesrw.Bytes.Slice.eod
      else begin
        Bytes.fill buffer 0 (Bytes.length buffer) '\xff';
        Bytes.blit_string s !offset buffer 3 length;
        offset := !offset + length;
        Bytesrw.Bytes.Slice.make buffer ~first:3 ~length
      end)
end

let check label condition = if not condition then failwith label

let expect_io f =
  match f () with
  | _ -> failwith "invalid binary input accepted"
  | exception Eio.Io _ -> ()

let test_dagcbor slice_length =
  let ints =
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
    ]
  in
  List.iter
    (fun n ->
      let wire = D.encode_string (`Int n) in
      check "fragmented integer roundtrip"
        (D.decode (R.of_string ~slice_length wire) = `Int n);
      for length = 0 to String.length wire - 1 do
        expect_io (fun () ->
            D.decode (R.of_string ~slice_length (String.sub wire 0 length)))
      done)
    ints;
  List.iter
    (fun n ->
      let wire = D.encode_string (`Float n) in
      match D.decode (R.of_string ~slice_length wire) with
      | `Float actual ->
          check "float bit pattern"
            (Int64.bits_of_float n = Int64.bits_of_float actual)
      | _ -> failwith "float type changed")
    [ 0.; -0.; 1.; -1.; 1e300; 1e-300 ];
  List.iter
    (fun wire ->
      expect_io (fun () -> D.decode (R.of_string ~slice_length wire)))
    [
      "\x18\x17";
      "\x19\x00\xff";
      "\x1a\x00\x00\xff\xff";
      "\x1b\x00\x00\x00\x00\xff\xff\xff\xff";
      "\x1b\x80\x00\x00\x00\x00\x00\x00\x00";
      "\x5b\x80\x00\x00\x00\x00\x00\x00\x00";
    ]

let test_car slice_length =
  let blocks =
    List.map
      (fun size ->
        let body = String.make size 'x' in
        (Atp.Cid.create `Raw body, body))
      [ 0; 1; 90; 91; 92; 127; 128; 16348; 16349 ]
  in
  let header : C.header = { version = 1; roots = [ fst (List.hd blocks) ] } in
  let wire = C.to_string header (List.to_seq blocks) in
  let reader = R.of_string ~slice_length wire in
  check "CAR header survives fragmented varint" (C.read_header reader = header);
  List.iter
    (fun expected ->
      check "CAR frame survives fragmented varint"
        (C.read_block reader = Some expected))
    blocks;
  check "CAR EOF" (C.read_block reader = None);
  List.iter
    (fun wire ->
      expect_io (fun () -> C.read_block (R.of_string ~slice_length wire)))
    [
      "\x00";
      "\x80";
      String.make 9 '\xff';
      String.make 8 '\xff' ^ "\x40";
      "\x81\x01";
      Atp.Varint.encode ((64 * 1024 * 1024) + 1);
    ]

let test_prefix_ownership slice_length =
  let values = [`String (String.make 129 'x');
    `Map ["key", `Bytes (String.make 257 'y')]; `Int Int64.max_int] in
  let reader = R.of_string ~slice_length
      (String.concat "" (List.map D.encode_string values)) in
  let actual = List.map (fun _ -> D.decode_prefix reader) values in
  check "prefixes survive buffer reuse" (List.equal D.equal values actual);
  check "prefixes consume stream" (Bytesrw.Bytes.Slice.is_eod (R.read reader));
  let wire = D.encode_string (List.hd values) in
  check "exact byte limit" (D.equal (List.hd values)
    (D.decode ~max_bytes:(String.length wire)
       (R.of_string ~slice_length wire)));
  expect_io (fun () -> D.decode ~max_bytes:(String.length wire - 1)
    (R.of_string ~slice_length wire))

let () =
  List.iter
    (fun slice_length ->
      test_dagcbor slice_length;
      test_car slice_length;
      test_prefix_ownership slice_length)
    [ 1; 2; 3; 7; 8; 9; 4096 ];
  print_endline "ATP binary boundary and fragmented-input tests passed"
