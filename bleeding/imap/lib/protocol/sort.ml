type key = Arrival | Cc | Date | From | Size | Subject | To
type order = Ascending | Descending
type return = Min | Max | Count | All | Partial of (int64 * int64)

let key_to_wire = function
  | Arrival -> "ARRIVAL" | Cc -> "CC" | Date -> "DATE" | From -> "FROM"
  | Size -> "SIZE" | Subject -> "SUBJECT" | To -> "TO"

let criterion_to_wire (key, order) =
  (match order with Ascending -> "" | Descending -> "REVERSE ") ^
  key_to_wire key

let return_to_wire = function
  | Min -> "MIN" | Max -> "MAX" | Count -> "COUNT" | All -> "ALL"
  | Partial (first, last) -> Printf.sprintf "PARTIAL %Ld:%Ld" first last

let equal_return (a : return) b = a = b
