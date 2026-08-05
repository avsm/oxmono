(* TOML test decoder - reads TOML from stdin, outputs tagged JSON to stdout *)

let () =
  let input = In_channel.input_all In_channel.stdin in
  match Tomlt_bytesrw.of_string input with
  | Ok toml ->
      let json = Tomlt_bytesrw.Tagged_json.encode toml in
      print_string json;
      print_newline ()
  | Error e ->
      Printf.eprintf "Error: %s\n" (Tomlt.Toml.Error.to_string e);
      exit 1
