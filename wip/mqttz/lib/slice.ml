type t = { global_ bytes : bytes; off : int; len : int }
let make ?(off = 0) ?len bytes =
  let len = Option.value len ~default:(Bytes.length bytes - off) in
  if off < 0 || len < 0 || off > Bytes.length bytes - len then
    invalid_arg "Slice.make";
  { bytes; off; len }
let[@zero_alloc] make_local bytes ~off ~len = exclave_
  if off < 0 || len < 0 || off > Bytes.length bytes - len then
    invalid_arg "Slice.make_local";
  { bytes; off; len }
let of_string s = make (Bytes.of_string s)
let empty = make Bytes.empty
let[@zero_alloc] length (s @ local) = s.len
let[@zero_alloc] get_uint8 (s @ local) i =
  if i < 0 || i >= s.len then invalid_arg "Slice.get_uint8";
  Bytes.get_uint8 s.bytes (s.off + i)
let sub s off len =
  if off < 0 || len < 0 || off > s.len - len then invalid_arg "Slice.sub";
  { bytes = s.bytes; off = s.off + off; len }
let[@zero_alloc] sub_local (s @ local) off len = exclave_
  if off < 0 || len < 0 || off > s.len - len then invalid_arg "Slice.sub_local";
  { bytes = s.bytes; off = s.off + off; len }
let to_string (s @ local) = Bytes.sub_string s.bytes s.off s.len
let copy (s @ local) = make (Bytes.sub s.bytes s.off s.len)
