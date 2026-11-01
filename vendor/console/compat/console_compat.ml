module Char = struct
  include Stdlib.Char

  module Ascii = struct
    let is_digit c = c >= '0' && c <= '9'
    let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    let is_alphanum c = is_digit c || is_letter c
    let is_print c = c >= ' ' && c <= '~'

    let is_hex_digit c =
      is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    let hex_digit_to_int c =
      if is_digit c then code c - code '0'
      else if c >= 'a' && c <= 'f' then code c - code 'a' + 10
      else if c >= 'A' && c <= 'F' then code c - code 'A' + 10
      else invalid_arg "Char.Ascii.hex_digit_to_int"
  end
end

module List = struct
  include Stdlib.List

  let take n xs =
    let rec loop n acc = function
      | [] -> rev acc
      | _ when n <= 0 -> rev acc
      | x :: xs -> loop (n - 1) (x :: acc) xs
    in
    loop n [] xs

  let rec drop n xs =
    if n <= 0 then xs else match xs with [] -> [] | _ :: xs -> drop (n - 1) xs

  let rec drop_while p = function
    | x :: xs when p x -> drop_while p xs
    | xs -> xs
end
