open Httpz_dav
let count = ref 0
let check name b = incr count; if not b then failwith name
let ok = function Ok x -> x | Error e -> failwith e
let parse s = ok (parse_xml s)
let ns p uri = (Httpz_dav.ns_xmlns, p), uri
let binding e p = List.assoc (Httpz_dav.ns_xmlns, p) e.attrs
let child e name = List.hd (children name e)

let () =
  (* Construct new children under an existing binding: they do not yet carry
     the inherited attributes that parse_xml adds to returned fragments. *)
  List.iter (fun prefix ->
    List.iter (fun default ->
      let attrs = [ns prefix "urn:value"] @ if default then [ns "xmlns" "urn:default"] else [] in
      let value = prefix ^ ":Thing" in
      let leaf = element ~attrs:[("", "type"), value]
        ("urn:child", "child") [Text value; Element (element ("urn:child", "empty") []); Text " tail "] in
      let root = element ~attrs ("urn:root", "root") [Element leaf] in
      let encoded = encode_xml root in
      let parsed = parse encoded in
      let leaf = child parsed leaf.name in
      check "QName binding preserved" (binding leaf prefix = "urn:value");
      check "QName attribute preserved" (List.assoc ("", "type") leaf.attrs = value);
      check "mixed text preserved" (match leaf.children with [Text s; Element _; Text " tail "] -> s = value | _ -> false);
      let detached = parse (encode_xml leaf) in
      check "detached QName binding" (binding detached prefix = "urn:value");
      let again = encode_xml parsed in
      check "reencoding does not accumulate declarations"
        (String.length (encode_xml (parse again)) = String.length again)
    ) [false; true]
  ) ("p" :: List.init 64 (fun i -> "davz" ^ string_of_int (i+1)));
  (* Shadow a prefix, use its old URI in an expanded name, then leave scope. *)
  let shadow = element ~attrs:[ns "p" "urn:inner"; ("urn:outer", "attr"), "p:Value"]
    ("urn:outer", "child") [Text "p:Value"] in
  let sibling = element ("urn:outer", "sibling") [Text "p:Value"] in
  let root = element ~attrs:[ns "p" "urn:outer"] ("urn:outer", "root") [Element shadow; Element sibling] in
  let root = parse (encode_xml root) in
  check "shadowed prefix keeps local meaning" (binding (child root shadow.name) "p" = "urn:inner");
  check "expanded attribute keeps old URI" (List.mem (("urn:outer", "attr"), "p:Value") (child root shadow.name).attrs);
  check "sibling restores outer meaning" (binding (child root sibling.name) "p" = "urn:outer");
  let source = "<x xmlns:p='urn:outer' xml:lang='en'><y xmlns:p='urn:inner' xml:lang='fr'/><z/></x>" in
  let root = parse source in
  check "parser inheritance overrides" (binding (child root ("", "y")) "p" = "urn:inner");
  check "parser inheritance restores" (binding (child root ("", "z")) "p" = "urn:outer");
  check "inherited language" (List.assoc (Httpz_dav.ns_xml, "lang") (child root ("", "z")).attrs = "en");
  (* Exercise a wide scope and its synthesized attributes at the exact node
     bound. Timing is measured separately by bench_namespaces. *)
  let n = 32000 in
  let b = Buffer.create (n*24) in
  Buffer.add_string b "<x";
  for i = 1 to n do Printf.bprintf b " xmlns:n%d='urn:n%d'" i i done;
  Buffer.add_string b "><y/></x>";
  let source = Buffer.contents b in
  let limit = 2*n + 2 in
  let root = ok (parse_xml ~limits:{default_limits with max_nodes=limit} source) in
  check "wide inherited scope" (List.length (child root ("", "y")).attrs = n);
  check "wide inherited scope boundary" (Result.is_error
    (parse_xml ~limits:{default_limits with max_nodes=limit-1} source));
  Printf.printf "httpz.dav namespaces: %d checks passed\n" !count
