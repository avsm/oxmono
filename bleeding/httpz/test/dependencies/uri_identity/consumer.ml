let () =
  let external_uri = Uriz.of_string_exn "https://example.test/a?x=1" in
  let facade_uri : Httpz_uri.t = external_uri in
  let updated : Uriz.t =
    Httpz_uri.add_query_params facade_uri [ "x", "2"; "space", "a b" ]
  in
  assert (Uriz.to_string updated = "https://example.test/a?x=1&x=2&space=a%20b");
  let template = Httpz_uri.Template.of_string_exn "../{name}" in
  let expanded : Uriz.t =
    Result.get_ok
      (Httpz_uri.Template.expand_resolve_assoc ~base:external_uri template
         [ "name", `String "inbox" ])
  in
  assert (Uriz.to_string expanded = "https://example.test/inbox");
  assert (Uriz.Raw.error_offset (Uriz.Raw.parse "%") = 0);
  assert (Uriz.Raw.is_ipv4 "127.0.0.1")
