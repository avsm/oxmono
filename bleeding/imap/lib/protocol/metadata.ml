type depth = Zero | One | Infinity

let depth_to_wire = function
  | Zero -> "0" | One -> "1" | Infinity -> "infinity"
