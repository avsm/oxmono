let check label p = if not p then failwith label
let expect_io f = match f () with _ -> failwith "expected protocol error" | exception Eio.Io _ -> ()
let () =
  let json = match Jsont_bytesrw.decode_string Jsont.json {|{"$bytes":"AP8="}|} with Ok j -> j | Error e -> failwith e in
  check "AT Protocol JSON bytes" (Jsont.Json.decode Atp.Lex.bytes_jsont json = Ok "\x00\xff");
  check "AT Protocol JSON bytes encoding" (match Jsont.Json.encode Atp.Lex.bytes_jsont "\x00\xff" with
    | Ok encoded -> Jsont.Json.decode Atp.Lex.bytes_jsont encoded = Ok "\x00\xff" | Error _ -> false);
  List.iter (fun n -> check "varint boundary roundtrip" (fst (Atp.Varint.decode_string (Atp.Varint.encode n) 0) = n)) [0;127;128;16384;max_int];
  List.iter (fun s -> expect_io (fun () -> Atp.Varint.decode_string s 0);
    expect_io (fun () -> Atp.Varint.decode_bytes (Bytes.of_string s) 0))
    [String.make 9 '\xff' ^ "\x01"; String.make 9 '\x80' ^ "\x01"; String.make 8 '\xff' ^ "\x40"];
  (match Atp.Varint.decode_string "\x01" (-1) with _ -> failwith "negative offset accepted" | exception Invalid_argument _ -> ());
  List.iter (fun n -> check "canonical CBOR integer boundary" (Atp.Dagcbor.decode_string (Atp.Dagcbor.encode_string (`Int n)) = `Int n))
    [65535L;65536L;4294967295L;4294967296L;Int64.max_int;Int64.min_int];
  expect_io (fun () -> Atp.Dagcbor.decode_string ~strict:false "\x1b\x80\x00\x00\x00\x00\x00\x00\x00");
  expect_io (fun () -> Atp.Dagcbor.decode_string "\x61\xff");
  expect_io (fun () -> Atp.Dagcbor.encode_string (`Map ["x", `Int 1L; "x", `Int 2L]));
  expect_io (fun () -> Atp.Dagcbor.decode_string ~max_depth:4 (String.make 5 '\x81' ^ "\xf6"));
  expect_io (fun () -> Atp.Dagcbor.decode_string ~max_bytes:2 "\x43abc");
  let data = "test data" in
  let cid = Atp.Cid.create `Raw data in
  let header : Atp.Car.header = { version = 1; roots = [cid] } in
  let car = Atp.Car.to_string header (List.to_seq [cid, data; cid, data]) in
  let reader = Bytesrw.Bytes.Reader.of_string car in
  check "CAR header" (Atp.Car.read_header reader = header);
  check "CAR first block preserves read-ahead" (Atp.Car.read_block reader = Some (cid, data));
  check "CAR second block preserves read-ahead" (Atp.Car.read_block reader = Some (cid, data));
  check "CAR EOF" (Atp.Car.read_block reader = None);
  let invalid_header = Atp.Dagcbor.encode_string (`Map ["roots", `List [`Int 1L]; "version", `Int 1L]) in
  expect_io (fun () -> Atp.Car.of_string (Atp.Varint.encode (String.length invalid_header) ^ invalid_header));
  let corrupt = Atp.Car.to_string header (List.to_seq [cid, "wrong data"]) in
  let store = Atp.Blockstore.memory () in
  expect_io (fun () -> Atp.Car.import store (Bytesrw.Bytes.Reader.of_string corrupt));
  check "corrupt import not stored" (not (store#has cid));
  expect_io (fun () -> Atp.Car.export ~root:cid (store :> Atp.Blockstore.readable) (List.to_seq [cid]));
  let leaf : Atp.Mst.Raw.node = {l = None; e = [{p = 0; k = "com.example.record/z"; v = cid; t = None}]} in
  let bytes = Atp.Mst.Raw.encode_bytes leaf in
  let leaf_cid = Atp.Cid.create `Dag_cbor bytes in
  store#put leaf_cid bytes;
  let root : Atp.Mst.Raw.node = {l = None; e = [{p = 0; k = "com.example.record/a"; v = cid; t = Some leaf_cid}]} in
  let bytes = Atp.Mst.Raw.encode_bytes root in
  let root_cid = Atp.Cid.create `Dag_cbor bytes in
  store#put root_cid bytes;
  let tree = Atp.Mst.of_cid root_cid ~store:(store :> Atp.Blockstore.readable) in
  check "MST final right subtree lookup" (Atp.Mst.get "com.example.record/z" tree ~store:(store :> Atp.Blockstore.readable) = Some cid);
  let upper = Atp.Handle.of_string_exn "Alice.Example" and lower = Atp.Handle.of_string_exn "alice.example" in
  check "handle equality agrees with ordering" (Atp.Handle.equal upper lower && Atp.Handle.compare upper lower = 0)
