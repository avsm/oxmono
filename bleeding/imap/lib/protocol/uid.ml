type t = int

let max_uid = 4_294_967_295

let of_int64 n =
  if n < 1L || n > 4_294_967_295L then Error "UID must be in 1..4294967295"
  else Ok (Int64.to_int n)
let to_int64 = Int64.of_int
let to_string = Int.to_string
let succ n = if n = max_uid then None else Some (n + 1)
let pred n = if n = 1 then None else Some (n - 1)
let equal (a : t) b = a = b
let compare (a : t) b = Stdlib.compare a b
let pp ppf n = Format.fprintf ppf "%d" n
