type t = int

let max_uid = 4_294_967_295

let invalid = Error "UID must be in 1..4294967295"

let of_int n = if n < 1 || n > max_uid then invalid else Ok n
let of_int64 n =
  if n < 1L || n > 4_294_967_295L then invalid else Ok (Int64.to_int n)
let to_int (n : t) : int = n
let to_int64 = Int64.of_int
let to_string = Int.to_string
let succ n = if n = max_uid then None else Some (n + 1)
let pred n = if n = 1 then None else Some (n - 1)
let equal (a : t) b = a = b
let compare (a : t) b = Stdlib.compare a b
let pp ppf n = Format.fprintf ppf "%d" n

include (val Base.Comparator.make__portable ~compare
    ~sexp_of_t:(fun u -> Base.Sexp.Atom (to_string u)))
