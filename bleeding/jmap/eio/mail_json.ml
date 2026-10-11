let invalid () = invalid_arg "Invalid JMAP mail data."

let fields = function
  | Jsont.Object (fs, _) -> List.map (fun ((k, _), v) -> (k, v)) fs
  | _ -> invalid ()

let member k j = List.assoc_opt k (fields j)
let get k j = match member k j with Some x -> x | None -> invalid ()
let string = function Jsont.String (s, _) -> s | _ -> invalid ()
let array = function Jsont.Array (xs, _) -> xs | _ -> invalid ()

let int j =
  match Jsont.Json.decode Jmap.Proto.Int53.Unsigned.jsont j with
  | Ok n when n <= Int64.of_int max_int -> Int64.to_int n
  | _ -> invalid ()

let id s =
  match Jmap.Proto.Id.of_string_received s with
  | Ok _ when String.is_valid_utf_8 s -> s
  | _ -> invalid ()

let ids j =
  let xs = List.map (fun j -> id (string j)) (array j) in
  if List.length xs <> List.length (List.sort_uniq String.compare xs) then
    invalid ();
  xs

let only allowed j =
  let names = List.map fst (fields j) in
  if
    List.length names <> List.length (List.sort_uniq String.compare names)
    || not (List.for_all (fun s -> List.mem s allowed) names)
  then invalid ()
