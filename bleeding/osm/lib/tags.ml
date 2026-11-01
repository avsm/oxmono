type t = string Jsont.String_map.t

let empty : t = Jsont.String_map.empty
let find = Jsont.String_map.find_opt

let of_list entries =
  List.fold_left
    (fun tags (key, value) -> Jsont.String_map.add key value tags)
    empty entries

let to_list tags =
  Jsont.String_map.fold (fun k v acc -> (k, v) :: acc) tags []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let jsont = Jsont.Object.as_string_map Jsont.string
