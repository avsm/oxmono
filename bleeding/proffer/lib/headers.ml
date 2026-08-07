(* Header field names are case-insensitive on the wire. They are lowercased on
   construction so every lookup is a plain string compare. *)

type t = (string * string) list

let lower s = String.lowercase_ascii s
let empty : t = []
let of_list l = List.map (fun (n, v) -> (lower n, v)) l
let to_list (t : t) = t
let add t name value = t @ [ (lower name, value) ]
let find t name = List.assoc_opt (lower name) t
let mem t name = List.mem_assoc (lower name) t
