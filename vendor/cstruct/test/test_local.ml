let expect label expected actual =
  if actual <> expected then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let[@zero_alloc] (local_sum @ portable) (cs @ local) =
  let local_ view = Cstruct.sub_local cs 1 4 in
  Cstruct.get_uint8 view 0 + Cstruct.get_byte view 3

let[@zero_alloc] (split_length @ portable) (cs @ local) =
  match Cstruct.split_local ~start:1 cs 2 with
  | left, right -> Cstruct.length left + Cstruct.length right

let[@zero_alloc] (copy_u32 @ portable) (src @ local) (dst @ local) =
  let local_ value = Cstruct.BE.get_uint32 src 0 in
  Cstruct.LE.set_uint32 dst 0 value

let[@zero_alloc] (copy_u64 @ portable) (src @ local) (dst @ local) =
  let local_ value = Cstruct.BE.get_uint64 src 0 in
  Cstruct.LE.set_uint64 dst 0 value

let[@zero_alloc] (cap_length @ portable) (cs @ local) =
  let local_ view = Cstruct_cap.sub_local cs ~off:1 ~len:3 in
  let local_ view = Cstruct_cap.ro view in
  Cstruct_cap.length view

let split_text cs =
  match Cstruct.split_local ~start:1 cs 2 with
  | left, right -> Cstruct.to_string left, Cstruct.to_string right

let shifted_text cs =
  let local_ first = Cstruct.sub_local cs 0 3 in
  let local_ second = Cstruct.sub_local cs 3 3 in
  let local_ remaining =
    Cstruct.shiftv_local (stack_ [ first; second ]) 2
  in
  let result = Cstruct.copyv remaining in
  result

let persistent_views () =
  let cs = Cstruct.of_string "abcdef" in
  let local_ first = Cstruct.sub_local cs 0 2 in
  let local_ second = Cstruct.sub_local cs 4 2 in
  let local_ local_views = stack_ [ first; second ] in
  let views = Cstruct.globalize_list local_views in
  views

let bigarray_view () =
  let buffer =
    Bigarray.(Array1.create char c_layout 5)
  in
  for i = 0 to 4 do
    Bigarray.Array1.set buffer i (Char.chr (Char.code '0' + i))
  done;
  let local_ view = Cstruct.of_bigarray_local ~off:1 ~len:3 buffer in
  let view = Cstruct.globalize view in
  view

let local_copies () =
  let local_ from_string = Cstruct.of_string_local ~off:1 ~len:3 "abcde" in
  let local_ from_bytes =
    Cstruct.of_bytes_local ~off:2 ~len:2 (Bytes.of_string "01234")
  in
  Cstruct.to_string from_string ^ Cstruct.to_string from_bytes

let () =
  let cs = Cstruct.of_string "abcdef" in
  assert (local_sum cs = Char.code 'b' + Char.code 'e');
  assert (split_length cs = 5);
  assert (cap_length (Cstruct_cap.create 5) = 3);
  let left, right = split_text cs in
  expect "split left" "bc" left;
  expect "split right" "def" right;
  expect "shiftv_local" "cdef" (shifted_text cs);

  let local_ middle = Cstruct.sub_local cs 1 4 in
  Cstruct.BE.set_uint16 middle 1 0x5859;
  assert (Cstruct.BE.get_uint16 middle 1 = 0x5859);
  expect "shared backing" "abcXYf" (Cstruct.to_string cs);

  let numbers = Cstruct.of_hex "0102030405060708" in
  let copied = Cstruct.create 8 in
  copy_u32 numbers copied;
  copy_u64 numbers copied;
  expect "local boxed endian values" "0807060504030201"
    (Cstruct.to_hex_string copied);

  let views = persistent_views () in
  Gc.full_major ();
  expect "globalize_list" "abef" (Cstruct.copyv views);

  let view = bigarray_view () in
  Gc.full_major ();
  expect "of_bigarray_local" "123" (Cstruct.to_string view);
  expect "local copying constructors" "bcd23" (local_copies ())
