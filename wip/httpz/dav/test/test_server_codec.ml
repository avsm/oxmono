open Httpz_dav
let checks = ref 0
let check name ok = incr checks; if not ok then failwith name
let xml text = match parse_xml text with Ok x -> x | Error e -> failwith e
let rec clean e = {e with
  attrs=List.filter (fun ((ns, _), _) -> ns <> ns_xmlns) e.attrs;
  children=List.map (function Element e -> Element (clean e) | x -> x) e.children}
let clean_update = function Set es -> Set (List.map clean es) | x -> x
let () =
  List.iter (fun query ->
    check "PROPFIND encoder/decoder roundtrip"
      (Server.propfind (xml (propfind query)) = Ok query))
    [Allprop []; Allprop ["urn:test", "x"]; Propname;
     Prop [dav "getetag"; "urn:test", "x"]];
  let updates = [Set [element ("urn:test", "x") [Text "a"]];
    Remove ["urn:test", "x"]; Set [element ("urn:test", "x") [Text "b"]]] in
  check "PROPPATCH preserves update order"
    (Result.map (List.map clean_update)
      (Server.proppatch (xml (proppatch updates))) = Ok updates);
  List.iter (fun body -> check "reject ambiguous PROPFIND"
    (Result.is_error (Server.propfind (xml body))))
    ["<propfind xmlns='DAV:'/>";
     "<propfind xmlns='DAV:'><propname/><allprop/></propfind>";
     "<propfind xmlns='DAV:'><prop/><prop/></propfind>";
     "<propfind xmlns='DAV:'><propname/><include/></propfind>"];
  List.iter (fun body -> check "reject invalid PROPPATCH"
    (Result.is_error (Server.proppatch (xml body))))
    ["<propertyupdate xmlns='DAV:'/>";
     "<propertyupdate xmlns='DAV:'><set/></propertyupdate>"];
  let token = match Token.of_string "urn:test:token" with
    | Ok t -> t | Error e -> failwith e in
  List.iter (fun conditions ->
    check "If encoder/decoder roundtrip"
      (Server.if_condition (encode_if conditions) = Ok conditions))
    [Untagged [[Is (Token token); Not (Etag "W/\"x\"")]; [Is (Etag "\"y\"")]];
     Tagged ["https://example.test/a", [[Is (Token token)]];
             "https://example.test/b", [[Not (Token token)]]]];
  List.iter (fun value -> check "reject malformed If"
    (Result.is_error (Server.if_condition value)))
    [""; "()"; "(<urn:x>"; "(Not<urn:x>)"; "([x])";
     "(<urn:x>) <https://example.test/a> (<urn:y>)";
     "<https://example.test/a>"; String.make 16385 ' '];
  let source = {responses=[{hrefs=["/dav/a%20b"];
    outcome=Properties [{status=200;
      properties=[element ("urn:test", "x") [Text "<&>"]];
      errors=[]; description=None};
      {status=404; properties=[element ("urn:test", "missing") []];
       errors=[]; description=None}];
    errors=[]; description=None; location=None}]; description=None} in
  check "multistatus encoder/decoder roundtrip"
    (match multistatus (xml (Server.multistatus source)) with
    | Ok m -> List.map (fun r -> {r with outcome=(match r.outcome with
        | Properties ps -> Properties (List.map (fun p ->
            {p with properties=List.map clean p.properties}) ps)
        | other -> other)}) m.responses = source.responses
    | Error _ -> false);
  check "server error decoder roundtrip"
    (let e = xml (Server.error [dav "lock-token-submitted"]) in
     e.name = dav "error" && (clean e).children =
       [Element (element (dav "lock-token-submitted") [])]);
  let wide = element (dav "prop") [Text (String.make 10000 '&')] in
  check "encoded XML output bound" (try
    ignore (encode_xml ~max_bytes:20000 wide); false
    with Output_too_large -> true);
  check "namespace prepass bound" (try
    ignore (encode_xml ~max_bytes:10 wide); false
    with Output_too_large -> true);
  Printf.printf "%d DAV server codec checks passed\n" !checks
