let read_file path =
  let input = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in input)
    (fun () -> really_input_string input (in_channel_length input))

let parse_json s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok json -> json
  | Error error -> failwith error

let json_to_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok s -> s
  | Error error -> failwith error

let print_error error =
  Printf.printf "ERROR: %s\n" (Jsont.Error.to_string error)

let protect f =
  try f () with
  | Jsont.Error error -> print_error error
  | Failure error -> Printf.printf "FAIL: %s\n" error

let quote_token token =
  let b = Buffer.create (String.length token + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c -> Buffer.add_char b c)
    token;
  Buffer.add_char b '"';
  Buffer.contents b

let show_tokens tokens = tokens |> List.map quote_token |> String.concat "; "

let print_tokens p =
  Printf.printf "[%s]\n" (show_tokens (Json_pointer.tokens p))

let test_parse pointer =
  protect @@ fun () ->
  let p = Json_pointer.of_string pointer in
  Printf.printf "OK: ";
  print_tokens p

let test_roundtrip pointer =
  protect @@ fun () ->
  let output = Json_pointer.(pointer |> of_string |> to_string) in
  if String.equal pointer output then Printf.printf "OK: %s\n" output
  else Printf.printf "MISMATCH: input=%s output=%s\n" pointer output

let test_eval json_path pointer =
  protect @@ fun () ->
  let json = parse_json (read_file json_path) in
  let result = Json_pointer.get (Json_pointer.of_string pointer) json in
  Printf.printf "OK: %s\n" (json_to_string result)

let test_escape token = Printf.printf "%s\n" (Json_pointer.Token.escape token)

let test_unescape token =
  protect @@ fun () ->
  Printf.printf "OK: %s\n" (Json_pointer.Token.unescape token)

let test_uri_fragment pointer =
  protect @@ fun () ->
  let p = Json_pointer.of_string pointer in
  let fragment = Json_pointer.to_uri_fragment p in
  let output = Json_pointer.(fragment |> of_uri_fragment |> to_string) in
  if String.equal pointer output then
    Printf.printf "OK: %s -> %s\n" pointer fragment
  else Printf.printf "MISMATCH: %s -> %s -> %s\n" pointer fragment output

let test_add json pointer value =
  protect @@ fun () ->
  let result =
    Json_pointer.add
      (Json_pointer.of_string pointer)
      (parse_json json) ~value:(parse_json value)
  in
  Printf.printf "%s\n" (json_to_string result)

let test_remove json pointer =
  protect @@ fun () ->
  let result =
    Json_pointer.remove (Json_pointer.of_string pointer) (parse_json json)
  in
  Printf.printf "%s\n" (json_to_string result)

let test_replace json pointer value =
  protect @@ fun () ->
  let result =
    Json_pointer.replace
      (Json_pointer.of_string pointer)
      (parse_json json) ~value:(parse_json value)
  in
  Printf.printf "%s\n" (json_to_string result)

let test_move json from path =
  protect @@ fun () ->
  let result =
    Json_pointer.move
      ~from:(Json_pointer.of_string from)
      ~path:(Json_pointer.of_string path)
      (parse_json json)
  in
  Printf.printf "%s\n" (json_to_string result)

let test_copy json from path =
  protect @@ fun () ->
  let result =
    Json_pointer.copy
      ~from:(Json_pointer.of_string from)
      ~path:(Json_pointer.of_string path)
      (parse_json json)
  in
  Printf.printf "%s\n" (json_to_string result)

let test_test json pointer expected =
  protect @@ fun () ->
  let result =
    Json_pointer.test
      (Json_pointer.of_string pointer)
      (parse_json json) ~expected:(parse_json expected)
  in
  Printf.printf "%b\n" result

let test_has json pointer =
  protect @@ fun () ->
  let result =
    Json_pointer.find (Json_pointer.of_string pointer) (parse_json json)
  in
  Printf.printf "%b\n" (Option.is_some result)

let duplicate_object () =
  let member value =
    Jsont.Json.mem (Jsont.Json.name "duplicate") (Jsont.Json.int value)
  in
  Jsont.Json.object' [ member 1; member 2 ]

let test_duplicate operation =
  protect @@ fun () ->
  let pointer = Json_pointer.of_string "/duplicate" in
  let json = duplicate_object () in
  let result =
    match operation with
    | "get" -> Json_pointer.get pointer json
    | "add" -> Json_pointer.add pointer json ~value:(Jsont.Json.int 3)
    | "delete" ->
        Jsont.Json.update
          (Json_pointer.delete_path ~allow_absent:true pointer)
          json
    | _ -> failwith "unknown duplicate-member operation"
  in
  Printf.printf "%s\n" (json_to_string result)

let test_jmap_parse pointer =
  protect @@ fun () ->
  let p = Json_pointer.of_string pointer in
  match Json_pointer.to_string p with
  | "" -> Printf.printf "OK: (root)\n"
  | pointer -> Printf.printf "OK: %s\n" pointer

let test_jmap_eval json pointer =
  protect @@ fun () ->
  let result =
    Json_pointer.Jmap.get (Json_pointer.of_string pointer) (parse_json json)
  in
  Printf.printf "OK: %s\n" (json_to_string result)

let test_jmap_eval_file path pointer = test_jmap_eval (read_file path) pointer

let test_jmap_eval_bounded json pointer max_results =
  protect @@ fun () ->
  let result =
    Json_pointer.Jmap.get
      ~max_results:(int_of_string max_results)
      (Json_pointer.of_string pointer)
      (parse_json json)
  in
  Printf.printf "OK: %s\n" (json_to_string result)

let test_jmap_shared_bounded copies width max_results =
  protect @@ fun () ->
  let copies = int_of_string copies in
  let width = int_of_string width in
  let max_results = int_of_string max_results in
  let leaf = Jsont.Json.array (Array.init width (fun n -> Jsont.Json.int n)) in
  let json = Jsont.Json.array (Array.init copies (Fun.const leaf)) in
  let result =
    Json_pointer.Jmap.get ~max_results (Json_pointer.of_string "/*/*") json
  in
  match result with
  | Jsont.Array (values, _) ->
      Printf.printf "OK: %d results\n" (List.length values)
  | _ -> assert false

let decode codec json =
  match Jsont.Json.decode' codec json with
  | Ok value -> value
  | Error error -> raise (Jsont.Error error)

let recode codec json =
  match Jsont.Json.recode' codec json with
  | Ok value -> value
  | Error error -> raise (Jsont.Error error)

let test_jmap_path_strings json pointer =
  protect @@ fun () ->
  let codec =
    Json_pointer.Jmap.path_list (Json_pointer.of_string pointer) Jsont.string
  in
  let result = decode codec (parse_json json) in
  Printf.printf "OK: [%s]\n" (String.concat ", " result)

let test_jmap_path_ints json pointer =
  protect @@ fun () ->
  let codec =
    Json_pointer.Jmap.path_list (Json_pointer.of_string pointer) Jsont.int
  in
  let result = decode codec (parse_json json) in
  Printf.printf "OK: [%s]\n"
    (result |> List.map string_of_int |> String.concat ", ")

let test_jmap_path_single json pointer =
  protect @@ fun () ->
  let codec =
    Json_pointer.Jmap.path (Json_pointer.of_string pointer) Jsont.string
  in
  Printf.printf "OK: %s\n" (decode codec (parse_json json))

let test_jmap_path_absent json pointer absent =
  protect @@ fun () ->
  let codec =
    Json_pointer.Jmap.path ~absent (Json_pointer.of_string pointer) Jsont.string
  in
  Printf.printf "OK: %s\n" (decode codec (parse_json json))

let test_root () =
  Printf.printf "root = %s\n" (Json_pointer.to_string Json_pointer.root);
  Printf.printf "is_root(root) = %b\n" (Json_pointer.is_root Json_pointer.root)

let test_is_root pointer =
  protect @@ fun () ->
  Printf.printf "%b\n" (Json_pointer.is_root (Json_pointer.of_string pointer))

let test_of_tokens tokens =
  let tokens =
    if String.equal tokens "" then [] else String.split_on_char ',' tokens
  in
  let pointer = Json_pointer.of_tokens tokens in
  Printf.printf "%s\n" (Json_pointer.to_string pointer)

let test_append base token =
  protect @@ fun () ->
  Printf.printf "%s\n" Json_pointer.(append (of_string base) token |> to_string)

let test_concat left right =
  protect @@ fun () ->
  Printf.printf "%s\n"
    Json_pointer.(concat (of_string left) (of_string right) |> to_string)

let test_parent pointer =
  protect @@ fun () ->
  match Json_pointer.(pointer |> of_string |> parent) with
  | None -> Printf.printf "None\n"
  | Some p -> Printf.printf "Some(%s)\n" (Json_pointer.to_string p)

let test_last pointer =
  protect @@ fun () ->
  match Json_pointer.(pointer |> of_string |> last) with
  | None -> Printf.printf "None\n"
  | Some token -> Printf.printf "Some(%S)\n" token

let test_of_string_result pointer =
  match Json_pointer.of_string_result pointer with
  | Ok p -> Printf.printf "Ok(%s)\n" (Json_pointer.to_string p)
  | Error error -> Printf.printf "Error(%s)\n" error

let test_of_uri_fragment_result fragment =
  match Json_pointer.of_uri_fragment_result fragment with
  | Ok p -> Printf.printf "Ok(%s)\n" (Json_pointer.to_string p)
  | Error error -> Printf.printf "Error(%s)\n" error

let test_pp pointer =
  protect @@ fun () ->
  Format.printf "%a\n" Json_pointer.pp (Json_pointer.of_string pointer)

let test_equal left right =
  protect @@ fun () ->
  Printf.printf "%b\n"
    (Json_pointer.equal
       (Json_pointer.of_string left)
       (Json_pointer.of_string right))

let test_compare left right =
  protect @@ fun () ->
  let comparison =
    Json_pointer.compare
      (Json_pointer.of_string left)
      (Json_pointer.of_string right)
  in
  Printf.printf "%s\n"
    (if comparison < 0 then "LT" else if comparison > 0 then "GT" else "EQ")

let test_of_path () =
  let path = Jsont.Path.(root |> nth 0 |> mem "foo" |> nth 1) in
  let pointer = Json_pointer.of_path path in
  Printf.printf "%s\n" (Json_pointer.to_string pointer)

let test_get_result json pointer =
  protect @@ fun () ->
  match
    Json_pointer.get_result (Json_pointer.of_string pointer) (parse_json json)
  with
  | Ok value -> Printf.printf "Ok(%s)\n" (json_to_string value)
  | Error error -> Printf.printf "Error(%s)\n" (Jsont.Error.to_string error)

let test_jsont_codec uri pointer =
  protect @@ fun () ->
  let codec =
    if uri then Json_pointer.jsont_uri_fragment else Json_pointer.jsont
  in
  let pointer =
    if uri then Json_pointer.to_uri_fragment (Json_pointer.of_string pointer)
    else pointer
  in
  let decoded = decode codec (Jsont.Json.string pointer) in
  Printf.printf "%s\n"
    (json_to_string
       (Jsont.Json.string
          (if uri then Json_pointer.to_uri_fragment decoded
           else Json_pointer.to_string decoded)))

let test_decode_pointer_codec uri json =
  protect @@ fun () ->
  let codec =
    if uri then Json_pointer.jsont_uri_fragment else Json_pointer.jsont
  in
  let pointer = decode codec (parse_json json) in
  Printf.printf "%s\n"
    (if uri then Json_pointer.to_uri_fragment pointer
     else Json_pointer.to_string pointer)

let test_query_path json pointer =
  protect @@ fun () ->
  let codec = Json_pointer.path (Json_pointer.of_string pointer) Jsont.string in
  Printf.printf "OK: %s\n" (decode codec (parse_json json))

let test_query_path_absent json pointer absent =
  protect @@ fun () ->
  let codec =
    Json_pointer.path ~absent (Json_pointer.of_string pointer) Jsont.string
  in
  Printf.printf "OK: %s\n" (decode codec (parse_json json))

let test_set_path allow_absent json pointer value =
  protect @@ fun () ->
  let codec =
    Json_pointer.set_path ~allow_absent Jsont.string
      (Json_pointer.of_string pointer)
      value
  in
  Printf.printf "%s\n" (json_to_string (recode codec (parse_json json)))

let test_update_path absent json pointer =
  protect @@ fun () ->
  let p = Json_pointer.of_string pointer in
  let codec =
    match absent with
    | None -> Json_pointer.update_path p Jsont.string
    | Some value -> Json_pointer.update_path ~absent:value p Jsont.string
  in
  Printf.printf "%s\n" (json_to_string (recode codec (parse_json json)))

let test_delete_path allow_absent json pointer =
  protect @@ fun () ->
  let codec =
    Json_pointer.delete_path ~allow_absent (Json_pointer.of_string pointer)
  in
  Printf.printf "%s\n" (json_to_string (recode codec (parse_json json)))

let test_jmap_get_result json pointer =
  protect @@ fun () ->
  match
    Json_pointer.Jmap.get_result
      (Json_pointer.of_string pointer)
      (parse_json json)
  with
  | Ok value -> Printf.printf "Ok(%s)\n" (json_to_string value)
  | Error error -> Printf.printf "Error(%s)\n" (Jsont.Error.to_string error)

let test_jmap_find json pointer =
  protect @@ fun () ->
  match
    Json_pointer.Jmap.find (Json_pointer.of_string pointer) (parse_json json)
  with
  | Some value -> Printf.printf "Some(%s)\n" (json_to_string value)
  | None -> Printf.printf "None\n"

let usage () =
  prerr_endline "usage: test_pointer COMMAND ...";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | [ _; "parse"; pointer ] -> test_parse pointer
  | [ _; "roundtrip"; pointer ] -> test_roundtrip pointer
  | [ _; "eval"; json; pointer ] -> test_eval json pointer
  | [ _; "escape"; token ] -> test_escape token
  | [ _; "unescape"; token ] -> test_unescape token
  | [ _; "uri-fragment"; pointer ] -> test_uri_fragment pointer
  | [ _; "add"; json; pointer; value ] -> test_add json pointer value
  | [ _; "remove"; json; pointer ] -> test_remove json pointer
  | [ _; "replace"; json; pointer; value ] -> test_replace json pointer value
  | [ _; "move"; json; from; path ] -> test_move json from path
  | [ _; "copy"; json; from; path ] -> test_copy json from path
  | [ _; "test"; json; pointer; expected ] -> test_test json pointer expected
  | [ _; "has"; json; pointer ] -> test_has json pointer
  | [ _; "duplicate"; operation ] -> test_duplicate operation
  | [ _; "jmap-parse"; pointer ] -> test_jmap_parse pointer
  | [ _; "jmap-eval"; json; pointer ] -> test_jmap_eval json pointer
  | [ _; "jmap-eval-bounded"; json; pointer; max_results ] ->
      test_jmap_eval_bounded json pointer max_results
  | [ _; "jmap-eval-file"; path; pointer ] -> test_jmap_eval_file path pointer
  | [ _; "jmap-shared-bounded"; copies; width; max_results ] ->
      test_jmap_shared_bounded copies width max_results
  | [ _; "jmap-path-strings"; json; pointer ] ->
      test_jmap_path_strings json pointer
  | [ _; "jmap-path-ints"; json; pointer ] -> test_jmap_path_ints json pointer
  | [ _; "jmap-path-single"; json; pointer ] ->
      test_jmap_path_single json pointer
  | [ _; "jmap-path-absent"; json; pointer; absent ] ->
      test_jmap_path_absent json pointer absent
  | [ _; "root" ] -> test_root ()
  | [ _; "is-root"; pointer ] -> test_is_root pointer
  | [ _; "of-tokens"; tokens ] -> test_of_tokens tokens
  | [ _; "tokens"; pointer ] ->
      protect (fun () -> print_tokens (Json_pointer.of_string pointer))
  | [ _; "append"; base; token ] -> test_append base token
  | [ _; "concat"; left; right ] -> test_concat left right
  | [ _; "parent"; pointer ] -> test_parent pointer
  | [ _; "last"; pointer ] -> test_last pointer
  | [ _; "of-string-result"; pointer ] -> test_of_string_result pointer
  | [ _; "of-uri-fragment-result"; fragment ] ->
      test_of_uri_fragment_result fragment
  | [ _; "pp"; pointer ] -> test_pp pointer
  | [ _; "equal"; left; right ] -> test_equal left right
  | [ _; "compare"; left; right ] -> test_compare left right
  | [ _; "of-path" ] -> test_of_path ()
  | [ _; "get-result"; json; pointer ] -> test_get_result json pointer
  | [ _; "jsont-codec"; pointer ] -> test_jsont_codec false pointer
  | [ _; "jsont-uri-fragment"; pointer ] -> test_jsont_codec true pointer
  | [ _; "decode-jsont-codec"; json ] -> test_decode_pointer_codec false json
  | [ _; "decode-jsont-uri-fragment"; json ] ->
      test_decode_pointer_codec true json
  | [ _; "query-path"; json; pointer ] -> test_query_path json pointer
  | [ _; "query-path-absent"; json; pointer; absent ] ->
      test_query_path_absent json pointer absent
  | [ _; "set-path"; json; pointer; value ] ->
      test_set_path false json pointer value
  | [ _; "set-path-absent"; json; pointer; value ] ->
      test_set_path true json pointer value
  | [ _; "update-path"; json; pointer ] -> test_update_path None json pointer
  | [ _; "update-path-absent"; json; pointer; absent ] ->
      test_update_path (Some absent) json pointer
  | [ _; "delete-path"; json; pointer ] -> test_delete_path false json pointer
  | [ _; "delete-path-absent"; json; pointer ] ->
      test_delete_path true json pointer
  | [ _; "jmap-get-result"; json; pointer ] -> test_jmap_get_result json pointer
  | [ _; "jmap-find"; json; pointer ] -> test_jmap_find json pointer
  | _ -> usage ()
