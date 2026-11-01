type algorithm = Orderedsubject | References | Other of string

let of_wire s =
  match String.uppercase_ascii s with
  | "ORDEREDSUBJECT" -> Orderedsubject
  | "REFERENCES" -> References
  | name -> Other name

let to_wire = function
  | Orderedsubject -> "ORDEREDSUBJECT"
  | References -> "REFERENCES"
  | Other name -> String.uppercase_ascii name

let equal a b = String.equal (to_wire a) (to_wire b)
let pp ppf a = Format.pp_print_string ppf (to_wire a)
