(* Header field names are case-insensitive on the wire, so lookup folds case.
   A name is kept exactly as the caller wrote it, because the block is what a
   backend sends: lowercasing here would put "etag" and "content-type" on the
   wire instead of their conventional spelling. *)

type t = (string * string) list

let empty : t = []
let of_list (l : (string * string) list) : t = l
let to_list (t : t) = t
let add (t : t) name value : t = t @ [ (name, value) ]

let same a b =
  String.length a = String.length b
  && String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)

let find (t : t) name =
  let rec go = function
    | [] -> None
    | (n, v) :: tl -> if same n name then Some v else go tl
  in
  go t

let mem (t : t) name = List.exists (fun (n, _) -> same n name) t
