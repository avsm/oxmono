let ceil_pow_2_minus_1 n =
  let open Stdlib in
  let n = Nativeint.of_int n in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 1) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 2) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 4) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 8) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 16) in
  Nativeint.to_int
    (if Sys.int_size > 32
     then Nativeint.logor n (Nativeint.shift_right_logical n 32)
     else n)
;;
