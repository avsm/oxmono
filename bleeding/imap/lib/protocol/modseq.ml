type t = int64

let of_int64 n = if n < 1L then Error "MODSEQ must be positive" else Ok n
let to_int64 n = n
let to_string = Int64.to_string
let equal = Int64.equal
let compare = Int64.compare
let pp ppf n = Format.fprintf ppf "%Ld" n
