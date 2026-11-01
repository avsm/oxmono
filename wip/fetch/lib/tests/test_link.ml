let check name b = if not b then failwith name
let shared = Fetch.Header.link ~rel:"FIRST memento" "/old"
let portable : (unit -> bool) @ portable = fun () ->
  let encoded = Fetch.Header.encode_links [shared] in
  match Fetch.Header.decode_links encoded with
  | Some [link] -> Fetch.Header.link_has_rel "memento" link &&
      Fetch.Header.link_has_rel "first" link
  | _ -> false
let () =
  check "portable Link codec" (portable ());
  check "multi-token relation lookup"
    (Fetch.Header.link_rel "memento" [shared] = Some shared);
  check "relation token boundary" (not (Fetch.Header.link_has_rel "ment" shared));
  check "extension URI case retained"
    (not (Fetch.Header.link_has_rel "https://example/REL"
      (Fetch.Header.link ~rel:"https://example/rel" "/old")));
  check "ordinary codec matches portable encoder"
    (Fetch.Header.encode Fetch.Header.links [shared] = Fetch.Header.encode_links [shared]);
  check "ordinary codec matches portable decoder"
    (Fetch.Header.decode Fetch.Header.links {|</old>; rel="first memento"|}
      = Fetch.Header.decode_links {|</old>; rel="first memento"|});
  check "invalid syntax rejected" (Fetch.Header.decode_links "bad" = None);
  check "Content-Location relative URI"
    (Fetch.Header.decode Fetch.Header.content_location "/version/1" = Some "/version/1");
  check "Content-Location malformed URI"
    (Fetch.Header.decode Fetch.Header.content_location "https://example/%zz" = None);
  check "Content-Location singleton"
    (Fetch.Header.get Fetch.Header.content_location (Http.Header.of_list
      ["Content-Location", "/1"; "Content-Location", "/2"]) = None);
  Printf.printf "Fetch Link: 10 checks passed\n"
