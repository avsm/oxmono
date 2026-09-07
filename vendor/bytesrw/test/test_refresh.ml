module B = Bytesrw.Bytes

let () =
  let empty = B.Reader.empty ~pos:7 ~slice_length:3 () in
  assert (B.Reader.pos empty = 7);
  assert (B.Reader.slice_length empty = 3);
  let source = B.Reader.of_string ~pos:4 ~slice_length:2 "data" in
  let empty_sub = B.Reader.sub 0 source in
  assert (B.Reader.pos empty_sub = 4);
  assert (B.Reader.slice_length empty_sub = 2);
  assert (B.Slice.is_eod (B.Reader.read empty_sub));
  let bigbytes = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout 4 in
  assert (B.Slice.is_eod (B.Slice.of_bigbytes_or_eod ~first:8 bigbytes));
  let output = Buffer.create 8 in
  let calls = ref 0 in
  let action _ _ = incr calls in
  let limited = B.Writer.limit ~action 0 ~eod:true (B.Writer.of_buffer output) in
  B.Writer.write_string limited "data";
  B.Writer.write_eod limited;
  assert (!calls = 1);
  assert (Buffer.contents output = "");
  print_endline "Bytesrw upstream boundary regressions passed"
