let json string =
  match Jsont_bytesrw.decode_string Jsont.json string with
  | Ok json -> json
  | Error message -> failwith message

let json_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok string -> string
  | Error message -> failwith message

let print_found document path =
  let pointer = Json_pointer.of_string path in
  match Json_pointer.find pointer document with
  | Some value -> Printf.printf "%s -> %s\n" path (json_string value)
  | None -> Printf.printf "%s -> not found\n" path

let () =
  let document =
    json
      {|{"users":[{"name":"Ada"},{"name":"Grace"}],
         "0":"zero is an object member","-":"hyphen is too"}|}
  in
  let grace =
    Json_pointer.get (Json_pointer.of_string "/users/1/name") document
  in
  Printf.printf "required value: %s\n" (json_string grace);
  List.iter (print_found document) [ "/users/0/name"; "/0"; "/-"; "/users/9" ]
