let () =
  let pointer =
    Json_pointer.of_tokens [ "catalog"; "snow man"; "100%"; "caf\u{00e9}" ]
  in
  let fragment = Json_pointer.to_uri_fragment pointer in
  Printf.printf "pointer: %s\n" (Json_pointer.to_string pointer);
  Printf.printf "fragment content: %s\n" fragment;
  Printf.printf "complete URI: document.json#%s\n" fragment;

  let decoded = Json_pointer.of_uri_fragment fragment in
  Printf.printf "round trip: %b\n" (Json_pointer.equal pointer decoded)
