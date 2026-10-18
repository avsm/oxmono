let fail json message = Jsont.Error.msgf (Jsont.Json.meta json) "%s" message

let read codec json =
  match Jsont.Json.decode codec json with
  | Ok value -> value
  | Error message -> fail json message

let object_members = function
  | Jsont.Object (members, _) -> members
  | json -> fail json "Expected an object"

let optional name codec json =
  let members = object_members json in
  match List.find_opt (fun ((key, _), _) -> key = name) members with
  | None -> None
  | Some (_, value) -> Some (read codec value)

let required name codec json =
  match optional name codec json with
  | Some value -> value
  | None -> fail json ("Missing member: " ^ name)

let default name codec fallback json =
  Option.value ~default:fallback (optional name codec json)

let integer =
  let dec json =
    (match json with
    | Jsont.Number (n, _) when Float.trunc n <> n ->
        fail json "Expected an integer"
    | _ -> ());
    let n = read Jsont.int64 json in
    if n < 0L then fail json "Expected a nonnegative integer";
    n
  in
  Jsont.map ~dec ~enc:Jsont.Json.int64 Jsont.json

let number_string =
  Jsont.map Jsont.string
    ~enc:(fun f -> Printf.sprintf "%.17g" f)
    ~dec:(fun s ->
      match float_of_string_opt s with
      | Some f when Float.is_finite f -> f
      | _ -> Jsont.Error.msgf Jsont.Meta.none "Invalid coordinate: %s" s)

let checked make =
  try make ()
  with Invalid_argument message ->
    Jsont.Error.msgf Jsont.Meta.none "%s" message

let id =
  Jsont.iter integer ~dec:(fun n ->
      if n = 0L then Jsont.Error.msgf Jsont.Meta.none "OSM IDs must be positive")
