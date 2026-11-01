let parse_local :
  (string @ local -> Httpz_uri.t or_null @ local) @ portable =
  Httpz_uri.of_string__local

let () =
  let source = "https://example.test/api/" in
  let local_ bytes = Base.Bytes.create_local (String.length source) in
  Bytes.blit_string source 0 bytes 0 (String.length source);
  let local_ base =
    match parse_local (Bytes.unsafe_to_string bytes) with
    | This uri -> uri
    | Null -> failwith "valid URI rejected"
  in
  let template = Httpz_uri.Template.of_string_exn "{name}" in
  let uri : Httpz_uri.t =
    Result.get_ok
      (Httpz_uri.Template.expand_resolve_assoc ~base template
         [ "name", `String "inbox" ])
  in
  assert (Httpz_uri.to_string uri = "https://example.test/api/inbox");
  let spans = Httpz_uri.Scanner.parse (Httpz_uri.to_string uri) in
  assert (Httpz_uri.Scanner.is_valid spans);
  assert (Httpz_uri.Scanner.host_off spans = 8);
  assert (spans.#host_off = 8);
  let local_ reference = Httpz_uri.of_string_exn__local "inbox" in
  let local_ resolved = Httpz_uri.resolve__local ~base reference in
  let #(path_offset, path_length) = Httpz_uri.encoded_path_span resolved in
  assert (path_offset = 20 && path_length = 10);
  assert (Httpz_uri.equal resolved uri);
  assert (Httpz_uri.Ip.is_literal "127.0.0.1")
