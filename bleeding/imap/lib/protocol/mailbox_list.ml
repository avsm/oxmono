type selection = Subscribed | Remote | Recursive_match | Special_use
type return = Subscribed | Children | Special_use

let selection_to_wire : selection -> string = function
  | Subscribed -> "SUBSCRIBED" | Remote -> "REMOTE"
  | Recursive_match -> "RECURSIVEMATCH" | Special_use -> "SPECIAL-USE"

let return_to_wire : return -> string = function
  | Subscribed -> "SUBSCRIBED" | Children -> "CHILDREN"
  | Special_use -> "SPECIAL-USE"

let equal_selection (a : selection) b = a = b
let equal_return (a : return) b = a = b
