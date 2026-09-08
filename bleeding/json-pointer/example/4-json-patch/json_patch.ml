let json string =
  match Jsont_bytesrw.decode_string Jsont.json string with
  | Ok json -> json
  | Error message -> failwith message

let json_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok string -> string
  | Error message -> failwith message

let pointer = Json_pointer.of_string
let string = Jsont.Json.string

let () =
  let document = json {|{"tasks":["draft","review"],"status":"open"}|} in
  Printf.printf "before: %s\n" (json_string document);

  let document =
    Json_pointer.add (pointer "/tasks/-") document ~value:(string "ship")
  in
  let document =
    Json_pointer.replace (pointer "/status") document ~value:(string "done")
  in
  let document = Json_pointer.remove (pointer "/tasks/0") document in
  let document =
    Json_pointer.copy ~from:(pointer "/status")
      ~path:(pointer "/previousStatus")
      document
  in
  let document =
    Json_pointer.move ~from:(pointer "/tasks/0") ~path:(pointer "/highlight")
      document
  in

  Printf.printf "after:  %s\n" (json_string document);
  Printf.printf "status is done: %b\n"
    (Json_pointer.test (pointer "/status") document ~expected:(string "done"))
