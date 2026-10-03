type t = string

let of_string s =
  if s = "" then invalid_arg "Layer_hash.of_string: empty";
  s

let to_string h = h
let equal = String.equal
let compare = String.compare
let pp ppf h = Fmt.string ppf h
