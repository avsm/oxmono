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

let recode codec json =
  match Jsont.Json.recode' codec json with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let pointer = Json_pointer.of_string

let () =
  let config =
    json {|{"server":{"host":"localhost","port":8080},"debug":false}|}
  in
  let host =
    decode (Json_pointer.path (pointer "/server/host") Jsont.string) config
  in
  let timeout =
    decode
      (Json_pointer.path ~absent:30 (pointer "/server/timeout") Jsont.int)
      config
  in
  Printf.printf "host: %s\n" host;
  Printf.printf "timeout: %d\n" timeout;

  let increment = Jsont.map ~kind:"increment" ~dec:succ ~enc:Fun.id Jsont.int in
  let config =
    recode (Json_pointer.update_path (pointer "/server/port") increment) config
  in
  let config =
    recode
      (Json_pointer.set_path ~allow_absent:true Jsont.int
         (pointer "/server/timeout")
         30)
      config
  in
  let config = recode (Json_pointer.delete_path (pointer "/debug")) config in
  Printf.printf "updated: %s\n" (json_string config)
