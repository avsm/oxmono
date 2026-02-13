type buf = Base_bigstring.t

let[@inline always] get_uint8 buf ~pos =
  Base_bigstring.get_uint8 buf ~pos

let[@inline always] get_int16_le buf ~pos =
  Base_bigstring.get_int16_le buf ~pos

let[@inline always] get_uint16_le buf ~pos =
  Base_bigstring.get_uint16_le buf ~pos

let[@inline always] get_int32_le buf ~pos =
  Base_bigstring.get_int32_le buf ~pos

let[@inline always] get_uint32_le buf ~pos =
  Base_bigstring.get_uint32_le buf ~pos

let[@inline always] get_int32_t_le buf ~pos =
  Base_bigstring.get_int32_t_le buf ~pos

let[@inline always] get_int64_t_le buf ~pos =
  Base_bigstring.get_int64_t_le buf ~pos

let[@inline always] get_int64_le_trunc buf ~pos =
  Base_bigstring.get_int64_le_trunc buf ~pos

let[@inline always] get_float64_le buf ~pos =
  Int64.float_of_bits (Base_bigstring.get_int64_t_le buf ~pos)

let[@inline always] unsafe_get_uint8 buf ~pos =
  Base_bigstring.unsafe_get_uint8 buf ~pos

let[@inline always] unsafe_get_int16_le buf ~pos =
  Base_bigstring.unsafe_get_int16_le buf ~pos

let[@inline always] unsafe_get_uint16_le buf ~pos =
  Base_bigstring.unsafe_get_uint16_le buf ~pos

let[@inline always] unsafe_get_int32_le buf ~pos =
  Base_bigstring.unsafe_get_int32_le buf ~pos

let[@inline always] unsafe_get_uint32_le buf ~pos =
  Base_bigstring.unsafe_get_uint32_le buf ~pos

let[@inline always] unsafe_get_int32_t_le buf ~pos =
  Base_bigstring.unsafe_get_int32_t_le buf ~pos

let[@inline always] unsafe_get_int64_t_le buf ~pos =
  Base_bigstring.unsafe_get_int64_t_le buf ~pos

let[@inline always] unsafe_get_int64_le_trunc buf ~pos =
  Base_bigstring.unsafe_get_int64_le_trunc buf ~pos

let[@inline always] unsafe_get_float64_le buf ~pos =
  Int64.float_of_bits (Base_bigstring.unsafe_get_int64_t_le buf ~pos)

let get_varint buf ~pos =
  let len = Base_bigstring.length buf in
  let result = ref 0 in
  let shift = ref 0 in
  let i = ref pos in
  let continue = ref true in
  while !continue do
    if !i >= len then
      invalid_arg "Chdb_binary.get_varint: unexpected end of buffer";
    let byte = Base_bigstring.get_uint8 buf ~pos:!i in
    result := !result lor ((byte land 0x7f) lsl !shift);
    shift := !shift + 7;
    Base.Int.incr i;
    if byte land 0x80 = 0 then continue := false
  done;
  (!result, !i - pos)

let get_string buf ~pos =
  let str_len, varint_size = get_varint buf ~pos in
  let str_pos = pos + varint_size in
  let s = Base_bigstring.to_string ~pos:str_pos ~len:str_len buf in
  (s, varint_size + str_len)

let iter_uint64 buf ~f =
  let len = Base_bigstring.length buf in
  let n = len / 8 in
  for i = 0 to n - 1 do
    let v = Base_bigstring.unsafe_get_int64_t_le buf ~pos:(i * 8) in
    f i v
  done

let fold_uint64 buf ~init ~f =
  let len = Base_bigstring.length buf in
  let n = len / 8 in
  let acc = ref init in
  for i = 0 to n - 1 do
    let v = Base_bigstring.unsafe_get_int64_t_le buf ~pos:(i * 8) in
    acc := f !acc i v
  done;
  !acc

let sum_uint64 buf =
  fold_uint64 buf ~init:0L ~f:(fun acc _i v -> Int64.add acc v)

let iter_uint32 buf ~f =
  let len = Base_bigstring.length buf in
  let n = len / 4 in
  for i = 0 to n - 1 do
    let v = Base_bigstring.unsafe_get_uint32_le buf ~pos:(i * 4) in
    f i v
  done

let fold_uint32 buf ~init ~f =
  let len = Base_bigstring.length buf in
  let n = len / 4 in
  let acc = ref init in
  for i = 0 to n - 1 do
    let v = Base_bigstring.unsafe_get_uint32_le buf ~pos:(i * 4) in
    acc := f !acc i v
  done;
  !acc

let iter_float64 buf ~f =
  let len = Base_bigstring.length buf in
  let n = len / 8 in
  for i = 0 to n - 1 do
    let v = Int64.float_of_bits
              (Base_bigstring.unsafe_get_int64_t_le buf ~pos:(i * 8)) in
    f i v
  done

let[@inline always] length buf = Base_bigstring.length buf
