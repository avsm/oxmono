(* SPDX-License-Identifier: ISC *)
module B = Cbort.Cbor
exception Invalid of string
let fail message = raise (Invalid message)
let limit = 65536
let max_items = 4096
let max_depth = 32
let uint64_max = Z.(pred (shift_left one 64))

type decoder = {
  global_ input : string;
  mutable pos : int;
  mutable items : int;
}
let byte (d @ local) =
  if d.pos = String.length d.input then fail "truncated CBOR";
  let b = Char.code (String.get d.input d.pos) in
  d.pos <- d.pos + 1;
  b
let unsigned (d @ local) n =
  let mutable value = 0L in
  for _ = 1 to n do
    value <- Int64.logor (Int64.shift_left value 8) (Int64.of_int (byte d))
  done;
  value
let argument (d @ local) info =
  match info with
  | n when n < 24 -> Int64.of_int n
  | 24 -> unsigned d 1 | 25 -> unsigned d 2
  | 26 -> unsigned d 4 | 27 -> unsigned d 8
  | _ -> fail "invalid CBOR argument"
let bounded_length n =
  if n < 0L || n > Int64.of_int limit then fail "CBOR length limit";
  Int64.to_int n
let bytes (d @ local) size =
  if size > String.length d.input - d.pos then fail "truncated CBOR string";
  let value = String.sub d.input d.pos size in
  d.pos <- d.pos + size;
  value
let text value =
  if not (String.is_valid_utf_8 value) then fail "invalid CBOR UTF-8";
  value
let unsigned_z n =
  if n >= 0L then Z.of_int64 n
  else Z.(add (of_int64 (Int64.logand n Int64.max_int)) (shift_left one 63))
let label_compare a b = match a, b with
  | B.Int a, B.Int b -> Z.compare a b
  | B.Text a, B.Text b -> String.compare a b
  | B.Int _, B.Text _ -> -1 | B.Text _, B.Int _ -> 1
  | _ -> fail "map labels must be integers or text"
let unique pairs =
  List.iter (fun (key, _) -> ignore (label_compare key key)) pairs;
  let sorted = List.sort label_compare (List.map fst pairs) in
  let rec loop = function
    | a :: (b :: _ as rest) ->
        if label_compare a b = 0 then fail "duplicate CBOR map label";
        loop rest
    | _ -> () in
  loop sorted
let half n =
  let sign = if n land 0x8000 = 0 then 1. else -1. in
  let exponent = (n lsr 10) land 31 and mantissa = n land 1023 in
  let value = match exponent with
    | 0 -> Float.ldexp (float_of_int mantissa) (-24)
    | 31 -> if mantissa = 0 then infinity else nan
    | e -> Float.ldexp (float_of_int (1024 + mantissa)) (e - 25) in
  sign *. value
let break (d @ local) =
  if d.pos < String.length d.input && d.input.[d.pos] = '\255' then
    (d.pos <- d.pos + 1; true)
  else false
let rec item (d @ local) depth =
  if depth > max_depth then fail "CBOR depth limit";
  d.items <- d.items + 1;
  if d.items > max_items then fail "CBOR item limit";
  let head = byte d in
  let major = head lsr 5 and info = head land 31 in
  match major with
  | 0 | 1 ->
      let n = unsigned_z (argument d info) in
      B.Int (if major = 0 then n else Z.neg (Z.succ n))
  | 2 | 3 ->
      let chunk size =
        let s = bytes d size in if major = 3 then text s else s in
      let value = if info <> 31 then chunk (bounded_length (argument d info))
        else
          let rec chunks acc =
            if break d then String.concat "" (List.rev acc)
            else begin
              d.items <- d.items + 1;
              if d.items > max_items then fail "CBOR item limit";
              let head = byte d in
              if head lsr 5 <> major || head land 31 = 31 then
                fail "invalid indefinite string chunk";
              let s = chunk (bounded_length (argument d (head land 31))) in
              chunks (s :: acc)
            end in
          chunks [] in
      if major = 2 then B.Bytes value else B.Text value
  | 4 | 5 ->
      let length = if info = 31 then None
        else Some (bounded_length (argument d info)) in
      (match length with Some n when n > max_items -> fail "CBOR item limit"
       | _ -> ());
      let rec elements remaining acc =
        if remaining = Some 0 || (remaining = None && break d) then
          List.rev acc
        else
          let key = item d (depth + 1) in
          let value = if major = 5 then item d (depth + 1) else B.Null in
          let remaining = Option.map (fun n -> n - 1) remaining in
          elements remaining ((key, value) :: acc) in
      let pairs = elements length [] in
      if major = 4 then B.Array (List.map fst pairs)
      else (unique pairs; B.Map pairs)
  | 6 ->
      let n = argument d info in
      if n < 0L || n > Int64.of_int max_int then fail "CBOR tag range";
      B.Tag (Int64.to_int n, item d (depth + 1))
  | 7 ->
      (match info with
       | 20 -> B.Bool false | 21 -> B.Bool true | 22 -> B.Null
       | 23 -> B.Undefined | n when n < 20 -> B.Simple n
       | 24 ->
           let n = byte d in
           if n < 32 then fail "invalid CBOR simple encoding";
           B.Simple n
       | 25 -> B.Float (half (Int64.to_int (unsigned d 2)))
       | 26 -> B.Float (Int32.float_of_bits (Int64.to_int32 (unsigned d 4)))
       | 27 -> B.Float (Int64.float_of_bits (unsigned d 8))
       | _ -> fail "invalid CBOR simple value")
  | _ -> assert false
let decode ?(max_size = limit) input =
  if max_size < 0 || String.length input > min max_size limit then
    Error "CBOR size limit"
  else try
    let local_ d = { input; pos = 0; items = 0 } in
    let value = item d 0 in
    if d.pos <> String.length input then fail "trailing CBOR bytes";
    Ok value
  with Invalid message -> Error message

let encode value =
  let buffer = Buffer.create 128 in
  let count = ref 0 in
  let byte n =
    if Buffer.length buffer >= limit then fail "CBOR size limit";
    Buffer.add_char buffer (Char.chr (n land 255)) in
  let unsigned bytes value =
    for i = bytes - 1 downto 0 do
      byte (Int64.to_int (Int64.shift_right_logical value (8 * i)))
    done in
  let argument major value =
    let head = major lsl 5 in
    if value >= 0L && value < 24L then byte (head lor Int64.to_int value)
    else if value >= 0L && value <= 255L then
      (byte (head lor 24); unsigned 1 value)
    else if value >= 0L && value <= 65535L then
      (byte (head lor 25); unsigned 2 value)
    else if value >= 0L && value <= 0xffffffffL then
      (byte (head lor 26); unsigned 4 value)
    else (byte (head lor 27); unsigned 8 value) in
  let string major s =
    argument major (Int64.of_int (String.length s));
    if String.length s > limit - Buffer.length buffer then
      fail "CBOR size limit";
    Buffer.add_string buffer s in
  let rec item depth = function
    | _ when depth > max_depth -> fail "CBOR depth limit"
    | value ->
        incr count;
        if !count > max_items then fail "CBOR item limit";
        match value with
        | B.Int z ->
            let negative = Z.sign z < 0 in
            let n = if negative then Z.pred (Z.neg z) else z in
            if Z.compare n uint64_max > 0 then fail "CBOR integer range";
            let low = Z.to_int64 (Z.logand n (Z.of_int64 Int64.max_int)) in
            let bits = if Z.testbit n 63 then Int64.logor low Int64.min_int
              else low in
            argument (if negative then 1 else 0) bits
        | B.Bytes s -> string 2 s
        | B.Text s -> string 3 (text s)
        | B.Array values ->
            argument 4 (Int64.of_int (List.length values));
            List.iter (item (depth + 1)) values
        | B.Map pairs ->
            if List.length pairs > max_items then fail "CBOR item limit";
            unique pairs;
            argument 5 (Int64.of_int (List.length pairs));
            List.iter (fun (k, v) -> item (depth + 1) k;
              item (depth + 1) v) pairs
        | B.Tag (n, value) ->
            if n < 0 then fail "negative CBOR tag";
            argument 6 (Int64.of_int n); item (depth + 1) value
        | B.Bool b -> byte (if b then 0xf5 else 0xf4)
        | B.Null -> byte 0xf6 | B.Undefined -> byte 0xf7
        | B.Simple n ->
            if n < 0 || n > 255 || (n >= 20 && n < 32) then
              fail "invalid CBOR simple value";
            if n < 20 then byte (0xe0 lor n)
            else (byte 0xf8; byte n)
        | B.Float f -> byte 0xfb; unsigned 8 (Int64.bits_of_float f) in
  try item 0 value; Buffer.contents buffer
  with Invalid message -> invalid_arg message
