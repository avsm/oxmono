let () =
  assert (Punycode.encode_label "münchen" = "xn--mnchen-3ya");
  assert (Punycode_idna.to_ascii "münchen.example" = "xn--mnchen-3ya.example");
  assert (Pubsuffix.public_suffix "www.example.com" = Ok "com");
  let cookie =
    Cookie.v
      ~domain:"example.com"
      ~path:"/"
      ~name:"session"
      ~value:"value"
      ~expiry:`Session
      ~now:Ptime.epoch
      ()
  in
  assert (Cookie.name cookie = "session");
  let (_ : Cookie_jar.t option) = None in
  print_endline "test_namespaced_libraries: public modules available"
;;
