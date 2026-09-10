(* SPDX-License-Identifier: ISC *)
open Bytesrw

let header major size =
  let buffer = Stdlib.Bytes.create (if size <= 65535 then 3 else 5) in
  Stdlib.Bytes.set_uint8 buffer 0
    ((major lsl 5) lor if size <= 65535 then 25 else 26);
  if size <= 65535 then Stdlib.Bytes.set_uint16_be buffer 1 size
  else Stdlib.Bytes.set_int32_be buffer 1 (Int32.of_int size);
  Stdlib.Bytes.to_string buffer

let () =
  List.iter
    (fun size ->
      let payload = String.make size 'x' in
      List.iter
        (fun (major, item) ->
          (* A value before and after the large field checks flush ordering. *)
          let value = `List [ `Int 1L; item; `Int 2L ] in
          let expected = "\x83\x01" ^ header major size ^ payload ^ "\x02" in
          assert (Atp.Dagcbor.encode_string value = expected);
          assert (Atp.Dagcbor.decode_string expected = value);
          let output = Buffer.create (String.length expected) in
          let writer =
            Bytes.Writer.make ~slice_length:113 (fun slice ->
                if not (Bytes.Slice.is_eod slice) then
                  Buffer.add_subbytes output (Bytes.Slice.bytes slice)
                    (Bytes.Slice.first slice) (Bytes.Slice.length slice))
          in
          Atp.Dagcbor.encode value ~eod:true writer;
          assert (Buffer.contents output = expected))
        [ (2, `Bytes payload); (3, `String payload) ])
    [ 4095; 4096; 4097; 65535; 65536; 70000 ];
  print_endline
    "DAG-CBOR: large text/byte strings and streaming boundaries passed"
