type t = int64

let max_uid = 4_294_967_295L

let of_int64 n =
  if n < 1L || n > max_uid then Error "UID must be in 1..4294967295"
  else Ok n
let to_int64 n = n
let to_string = Int64.to_string
let succ n = if n = max_uid then None else Some (Int64.succ n)
let pred n = if n = 1L then None else Some (Int64.pred n)
let equal = Int64.equal
let compare = Int64.compare
let pp ppf n = Format.fprintf ppf "%Ld" n
