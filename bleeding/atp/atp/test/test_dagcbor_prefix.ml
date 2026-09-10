let () =
  let module D = Atp.Dagcbor in
  let module R = Bytesrw.Bytes.Reader in
  let values =
    [
      `Map [ ("t", `String "#data"); ("op", `Int 1L) ];
      `Map [ ("content", `String (String.make 70000 'x')) ];
      `List [ `Bool true; `Null ];
    ]
  in
  let wire = String.concat "" (List.map D.encode_string values) in
  List.iter
    (fun slice_length ->
      let reader = R.of_string ~slice_length wire in
      List.iter
        (fun expected ->
          if not (D.equal expected (D.decode_prefix reader)) then
            failwith "DAG-CBOR prefix lost bytes")
        values;
      if not (Bytesrw.Bytes.Slice.is_eod (R.read reader)) then
        failwith "DAG-CBOR prefix left unexpected bytes")
    [ 1; 7; 4096; String.length wire ];
  (match D.decode_string wire with
  | _ -> failwith "Strict document decoder accepted trailing values"
  | exception Eio.Io _ -> ());
  let noncanonical =
    R.of_string
      (String.init 3 (function
        | 0 -> Char.chr 24
        | 1 -> Char.chr 1
        | _ -> Char.chr 2))
  in
  if
    D.decode_prefix ~strict:false noncanonical <> `Int 1L
    || D.decode_prefix noncanonical <> `Int 2L
  then failwith "Noncanonical prefix consumed its successor";
  print_endline "PASS: concatenated DAG-CBOR values preserve reader slices"
