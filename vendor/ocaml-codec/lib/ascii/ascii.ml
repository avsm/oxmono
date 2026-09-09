let is_digit c = c >= '0' && c <= '9'

let is_hex_digit c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let hex_value_int = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let hex_value c = match hex_value_int c with -1 -> None | v -> Some v

let hex_char v =
  if v < 0 || v > 15 then invalid_arg "Ascii.hex_char: value out of range"
  else if v < 10 then Char.chr (Char.code '0' + v)
  else Char.chr (Char.code 'a' + v - 10)

let is_printable c = c >= ' ' && c < '\x7f'
