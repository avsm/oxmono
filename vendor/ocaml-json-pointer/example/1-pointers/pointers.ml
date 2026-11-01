let print_pointer label pointer =
  Printf.printf "%s: %s\n" label (Json_pointer.to_string pointer)

let print_tokens pointer =
  let tokens =
    Json_pointer.tokens pointer
    |> List.map (Printf.sprintf "%S")
    |> String.concat "; "
  in
  Printf.printf "tokens: [%s]\n" tokens

let () =
  let parsed = Json_pointer.of_string "/users/0/display~1name" in
  print_pointer "parsed" parsed;
  print_tokens parsed;

  let built = Json_pointer.(root / "users" / "0" / "display/name") in
  print_pointer "built" built;
  Printf.printf "same pointer: %b\n" (Json_pointer.equal parsed built);

  let special = Json_pointer.of_tokens [ "a/b"; "m~n" ] in
  print_pointer "escaped tokens" special;

  match Json_pointer.of_string_result "users/0" with
  | Ok _ -> assert false
  | Error message -> Printf.printf "invalid input: %s\n" message
