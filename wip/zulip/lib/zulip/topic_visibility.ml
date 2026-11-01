type t = Inherit | Muted | Unmuted | Followed | Other of int

let of_int = function
  | 0 -> Inherit
  | 1 -> Muted
  | 2 -> Unmuted
  | 3 -> Followed
  | n -> Other n

let to_int = function
  | Inherit -> 0
  | Muted -> 1
  | Unmuted -> 2
  | Followed -> 3
  | Other n -> n

let equal a b = Int.equal (to_int a) (to_int b)
let compare a b = Int.compare (to_int a) (to_int b)

let pp ppf = function
  | Inherit -> Format.pp_print_string ppf "inherit"
  | Muted -> Format.pp_print_string ppf "muted"
  | Unmuted -> Format.pp_print_string ppf "unmuted"
  | Followed -> Format.pp_print_string ppf "followed"
  | Other n -> Format.fprintf ppf "other(%d)" n

let jsont =
  Jsont.map ~kind:"Zulip topic visibility" ~dec:of_int ~enc:to_int
    Json_integer.jsont
