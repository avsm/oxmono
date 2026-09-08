let json string =
  match Jsont_bytesrw.decode_string Jsont.json string with
  | Ok json -> json
  | Error message -> failwith message

let json_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok string -> string
  | Error message -> failwith message

let decode codec json =
  match Jsont.Json.decode' codec json with
  | Ok value -> value
  | Error error -> raise (Jsont.Error error)

let () =
  let response =
    json
      {|{"list":[{"id":"a","tags":["x","y"]},
                  {"id":"b","tags":["z"]}],
         "*":"literal member"}|}
  in
  let ids =
    Json_pointer.Jmap.get (Json_pointer.of_string "/list/*/id") response
  in
  let tags =
    Json_pointer.Jmap.get (Json_pointer.of_string "/list/*/tags") response
  in
  Printf.printf "ids: %s\n" (json_string ids);
  Printf.printf "flattened tags: %s\n" (json_string tags);

  let typed_ids =
    decode
      (Json_pointer.Jmap.path_list
         (Json_pointer.of_string "/list/*/id")
         Jsont.string)
      response
  in
  Printf.printf "typed ids: [%s]\n" (String.concat "; " typed_ids);

  let literal = Json_pointer.Jmap.get (Json_pointer.of_string "/*") response in
  Printf.printf "object wildcard: %s\n" (json_string literal)
