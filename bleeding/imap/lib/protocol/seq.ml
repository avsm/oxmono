type t = int64

let of_int64 n =
  if n < 1L || n > 4_294_967_295L then
    Error "sequence number must be in 1..4294967295"
  else Ok n
let to_int64 n = n
let to_string = Int64.to_string
let equal = Int64.equal
let compare = Int64.compare
let pp ppf n = Format.fprintf ppf "%Ld" n
