open Davz
let count = ref 0
let check name b = incr count; if not b then failwith name
let ok = function Ok v -> v | Error e -> failwith e
let invalid name f = check name (try ignore (f ()); false with Invalid_argument _ -> true)
let parse s = ok (parse_xml s)
let multi s = ok (multistatus (parse s))
let wrap body = "<d:multistatus xmlns:d='DAV:' xmlns:p='urn:test'>" ^ body ^ "</d:multistatus>"
let mixed = wrap "<d:response><d:href>/dav/a%2fb</d:href><d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype><p:colour xml:lang='fr'> bleu </p:colour></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><p:absent/></d:prop><d:status>HTTP/1.1 404 Missing</d:status><d:error><d:cannot-modify-protected-property/></d:error></d:propstat></d:response>"
let () =
  let m = multi mixed in
  let r = List.hd m.responses in
  check "reported failure" (property ("urn:test", "absent") r = Some (Error 404));
  check "unreported distinct" (property ("urn:test", "unreported") r = None);
  let p = match property ("urn:test", "colour") r with Some (Ok p) -> p | _ -> failwith "colour" in
  check "preserve property whitespace" (text p = Ok " bleu ");
  check "namespace scope retained" (List.mem ((Xmlm.ns_xmlns, "p"), "urn:test") p.attrs);
  check "empty multistatus" ((multi (wrap "")).responses = []);
  check "empty property" (text (element (dav "x") []) = Ok "");
  check "reject text extraction of structured property" (Result.is_error (text (element (dav "x") [Element p])));
  let r = List.hd (multi (wrap "<d:response><d:href>/a</d:href><d:href>/b</d:href><d:status>HTTP/1.1 423 Locked</d:status><d:location><d:href>/c</d:href></d:location><d:responsedescription>locked</d:responsedescription></d:response>")).responses in
  check "multiple href status" (r.hrefs = ["/a"; "/b"] && r.outcome = Status 423 && r.location = Some "/c");
  List.iter (fun s -> check ("reject XML " ^ s) (Result.is_error (parse_xml s)))
    [""; "<x>"; "<x/>junk"; "<x/><x/>"; "<!DOCTYPE x><x/>";
     "<!DOCTYPE x [<!ENTITY a 'bad'>]><x>&a;</x>"; "<x>&unknown;</x>";
     "<unbound:x/>"; "<x a='1' a='2'/>"; "<x>\000</x>";
     "<x xmlns:a='u' xmlns:b='u' a:c='1' b:c='2'/>";
     "<xmlns:multistatus xmlns:xmlns='DAV:'/>"; "<x xmlns:xml='wrong'/>";
     "<x xmlns='http://www.w3.org/XML/1998/namespace'/>"];
  List.iter (fun body -> check ("reject DAV " ^ body)
    (Result.is_error (multistatus (parse (wrap body)))) )
    ["<d:response/>"; "<d:response><d:href>/a</d:href></d:response>";
     "<d:response><d:href>relative</d:href><d:status>HTTP/1.1 200 OK</d:status></d:response>";
     "<d:response><d:href>/a#fragment</d:href><d:status>HTTP/1.1 200 OK</d:status></d:response>";
     "<d:response><d:href>/a</d:href><d:status>HTTP/1.1 200 OK</d:status><d:status>HTTP/1.1 404 Missing</d:status></d:response>";
     "<d:response><d:href>/a</d:href><d:status>HTTP/1.1 200 OK</d:status><d:propstat/></d:response>";
     "<d:response><d:href>/a</d:href><d:status>HTTP/1.1 +20 bad</d:status></d:response>";
     "<d:response><d:href>/a</d:href><d:status>HTTP/1.1 100 Continue</d:status></d:response>";
     "<d:response><d:href>/a</d:href><d:href>https://h/b</d:href><d:status>HTTP/1.1 404 Missing</d:status></d:response>"];
  List.iter (fun (limits, source) -> check "bounds" (Result.is_error (parse_xml ~limits source)))
    [{default_limits with max_bytes=3}, "<x/>";
     {default_limits with max_depth=1}, "<x><x/></x>";
     {default_limits with max_nodes=2}, "<x a='1' b='2'/>";
     {default_limits with max_nodes=3}, "<x xmlns:a='u'><a:x/></x>"];
  check "exact byte bound" (Result.is_ok (parse_xml ~limits:{default_limits with max_bytes=4} "<x/>"));
  invalid "zero limits" (fun () -> parse_xml ~limits:{default_limits with max_depth=0} "<x/>");
  let source = "<root xmlns='urn:default' xmlns:p='urn:p' xml:lang='en'><p:x a='  a  b &#9;&#10;&#13; '>p:QName<inner xmlns=''><b/>tail</inner> done </p:x></root>" in
  let p = List.hd (children ("urn:p", "x") (parse source)) in
  let reread = parse (encode_xml p) in
  check "attribute whitespace round trip" (List.assoc ("", "a") reread.attrs = "  a  b \t\n\r ");
  check "inherited language" (List.assoc (Xmlm.ns_xml, "lang") reread.attrs = "en");
  check "QName context" (List.assoc (Xmlm.ns_xmlns, "p") reread.attrs = "urn:p");
  check "unqualified nested name" (List.length (children ("", "inner") reread) = 1);
  let p = parse "<p:x xmlns:p='urn:p' xmlns='urn:p' p:a='b'/>" in
  check "qualified attribute with same default namespace"
    (List.mem (("urn:p", "a"), "b") (parse (encode_xml p)).attrs);
  invalid "invalid output name" (fun () -> encode_xml (element ("", "bad name") []));
  invalid "output name injection" (fun () -> encode_xml (element ("", "x a='b'") []));
  invalid "output control character" (fun () -> encode_xml (element ("", "x") [Text "\000"]));
  invalid "output invalid UTF-8" (fun () -> encode_xml (element ("", "x") [Text "\255"]));
  check "href escape identity" (resolve_href ~base:"https://h/dav/" "/dav/a%2fb" = Ok "https://h/dav/a%2Fb");
  List.iter (fun s -> check "bad href" (Result.is_error (resolve_href ~base:"https://h/" s)))
    ["relative"; "//other/x"; "ftp://h/a"; "https://user@h/x"; "/bad%"; "/x#f"; "https:///x"];
  check "ordered patch" (let p = parse (proppatch [Set [element ("urn:p", "x") [Text "first"]]; Remove ["urn:p", "x"]]) in
    List.filter_map (function Element e -> Some e.name | Text _ -> None) p.children = [dav "set"; dav "remove"]);
  let repeated = multi (wrap "<d:response><d:href>/a</d:href><d:propstat><d:prop><p:x/><p:x/></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat><d:propstat><d:prop><p:x/></d:prop><d:status>HTTP/1.1 424 Failed Dependency</d:status></d:propstat></d:response>") in
  let r = List.hd repeated.responses in
  check "all repeated results preserved" (match property_results ("urn:test", "x") r with
    | [Ok _; Ok _; Error 424] -> true | _ -> false);
  invalid "singular accessor rejects ambiguity" (fun () -> property ("urn:test", "x") r);
  check "plural accessor unreported" (property_results ("urn:test", "missing") r = []);
  invalid "empty patch" (fun () -> proppatch []);
  check "allprop include" (List.length (children (dav "include") (parse (propfind (Allprop [dav "getetag"])))) = 1);
  let tok = ok (Token.of_string "urn:example:opaque,token") in
  check "token opaque" (Token.decode (Token.encode tok) = Some tok);
  check "If grammar" (encode_if (Untagged [[Is (Token tok); Not (Etag "\"old\"")]; [Is (Etag "\"new\"")]]) =
    "(<urn:example:opaque,token> Not [\"old\"]) ([\"new\"])" );
  invalid "empty If" (fun () -> encode_if (Untagged [[]]));
  check "weak validator" (valid_etag "W/\"x\"" && not (strong_etag "W/\"x\""));
  List.iter (fun s -> check "invalid ETag" (not (valid_etag s))) ["x"; "\"a b\""; "\"x\"junk"; "w/\"x\""];
  List.iter (fun t -> check "timeout round trip" (decode_timeout (encode_timeout t) = Some t))
    [Infinite; Seconds 0L; Seconds 0xffff_ffffL];
  List.iter (fun s -> check "invalid timeout" (decode_timeout s = None)) ["Second--1"; "Second-4294967296"; "Second-+2"; "Second-0x1"];
  let source = "<prop xmlns='DAV:'><lockdiscovery><activelock><lockscope><exclusive/></lockscope><locktype><write/></locktype><depth>0</depth><timeout>Second-60</timeout><locktoken><href>urn:example:token</href></locktoken><lockroot><href>/dav/a</href></lockroot></activelock></lockdiscovery></prop>" in
  let ls = ok (locks (parse source)) in
  check "lock decoding" (List.length ls = 1 && (List.hd ls).timeout = Some (Seconds 60L));
  check "empty discovery" (locks (parse "<prop xmlns='DAV:'><lockdiscovery/></prop>") = Ok []);
  Printf.printf "davz: %d protocol checks passed\n" !count
