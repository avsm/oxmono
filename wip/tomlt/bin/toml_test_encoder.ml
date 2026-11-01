(* TOML test encoder - reads tagged JSON from stdin, outputs TOML to stdout *)

let () =
  let input = In_channel.input_all In_channel.stdin in
  match Tomlt_bytesrw.Tagged_json.decode_and_encode_toml input with
  | Ok toml ->
      print_string toml
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
