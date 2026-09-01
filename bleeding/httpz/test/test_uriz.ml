open Base

module Uriz = Httpz.Uriz

let checks = ref 0

let check name condition =
  Int.incr checks;
  if not condition then failwith ("test_uriz: " ^ name)
;;

let uri s =
  match Uriz.of_string s with
  | This uri -> uri
  | Null -> failwith ("test_uriz: invalid fixture " ^ s)
;;

let test_components () =
  let u = uri "HTTP://B%C3%9CCHER.Example:443/a/../b?q=a%2Fb#frag" in
  check "syntax canonicalization"
    (String.equal
       (Uriz.to_string u)
       "http://b%C3%9Ccher.example:443/a/../b?q=a%2Fb#frag");
  check "decoded host"
    (match Uriz.decoded_host u with
     | This host -> String.equal host "b\195\156cher.example"
     | Null -> false);
  check "path and query"
    (String.equal (Uriz.encoded_path_and_query u) "/a/../b?q=a%2Fb");
  check "normalized path"
    (String.equal
       (Uriz.to_string (Uriz.normalize u))
       "http://b%C3%9Ccher.example:443/b?q=a%2Fb#frag")
;;

let test_public_api () =
  let u = uri "foo://user%3Aname@example.test/a%2Fb/c?q=a+b%2Bc#f%20g" in
  check "encoded userinfo"
    (match Uriz.encoded_userinfo u with
     | This value -> String.equal value "user%3Aname"
     | Null -> false);
  check "decoded userinfo"
    (match Uriz.decoded_userinfo u with
     | This value -> String.equal value "user:name"
     | Null -> false);
  check "encoded path" (String.equal (Uriz.encoded_path u) "/a%2Fb/c");
  check "decoded path" (String.equal (Uriz.decoded_path u) "/a/b/c");
  check "encoded query"
    (match Uriz.encoded_query u with
     | This value -> String.equal value "q=a+b%2Bc"
     | Null -> false);
  check "encoded fragment"
    (match Uriz.encoded_fragment u with
     | This value -> String.equal value "f%20g"
     | Null -> false);
  check "decoded fragment"
    (match Uriz.decoded_fragment u with
     | This value -> String.equal value "f g"
     | Null -> false);
  check "registered-name host"
    (match Uriz.host_kind u with
     | This `Reg_name -> true
     | This (`Ipv4 | `Ipv6 | `Ipvfuture) | Null -> false);
  check "IPv6 host"
    (match Uriz.host_kind (uri "https://[::1]/") with
     | This `Ipv6 -> true
     | This (`Reg_name | `Ipv4 | `Ipvfuture) | Null -> false);
  check "component-aware percent encoding"
    (String.equal
       (Uriz.percent_encode ~component:`Path_segment "a/b c")
       "a%2Fb%20c");
  check "query value keeps non-delimiter semicolon"
    (String.equal
       (Uriz.percent_encode ~component:`Query_value "a;b&c=d+e")
       "a;b%26c%3Dd%2Be");
  check "invalid percent encoding"
    (match Uriz.percent_decode "bad%2" with Null -> true | This _ -> false);
  let made =
    Uriz.make_encoded
      ~scheme:"HTTPS"
      ~host:"EXAMPLE.test"
      ~path:"a%2Fb"
      ~query:"x=1"
      ~fragment:"f"
      ()
  in
  check "encoded construction"
    (String.equal
       (Uriz.to_string made)
       "https://example.test/a%2Fb?x=1#f");
  check "encoded functional update"
    (String.equal
       (Uriz.to_string (Uriz.with_encoded_fragment made Null))
       "https://example.test/a%2Fb?x=1");
  let empty_port = uri "http://example.test:/a" in
  check "empty port delimiter is observable"
    (Uriz.has_port empty_port
     && match Uriz.port empty_port with Null -> true | This _ -> false);
  check "unrelated update preserves empty port"
    (String.equal
       (Uriz.to_string (Uriz.with_encoded_query empty_port (This "x=1")))
       "http://example.test:/a?x=1");
  let ipvfuture = uri "HTTP://[Vf.FOO:BAR]/a" in
  check "IPvFuture host canonicalization"
    (String.equal (Uriz.to_string ipvfuture) "http://[vf.foo:bar]/a");
  check "unrelated update preserves IPvFuture literal"
    (String.equal
       (Uriz.to_string (Uriz.with_encoded_fragment ipvfuture (This "f")))
       "http://[vf.foo:bar]/a#f");
  check "encoded IPvFuture host is valid setter input"
    (String.equal
       (Uriz.to_string
          (Uriz.with_encoded_host
             (uri "http://example.test/a")
             (Uriz.encoded_host ipvfuture)))
       "http://[vf.foo:bar]/a")
;;

let test_query_decoding () =
  let u = uri "?flag&empty=&x=a+b%2Bc" in
  let expected = [ "flag", None; "empty", Some ""; "x", Some "a+b+c" ] in
  check "query flag and empty value stay distinct"
    (List.equal
       (fun (ak, av) (bk, bv) -> String.equal ak bk && Poly.equal av bv)
       (Uriz.query_params u)
       expected);
  let form_expected = [ "flag", None; "empty", Some ""; "x", Some "a b+c" ] in
  check "query plus decoding is opt-in"
    (List.equal
       (fun (ak, av) (bk, bv) -> String.equal ak bk && Poly.equal av bv)
       (Uriz.query_params ~plus_as_space:true u)
       form_expected);
  check "find flag follows value convenience semantics"
    (match Uriz.find_query_param u "flag" with
     | This value -> String.equal value ""
     | Null -> false)
;;

let test_query_updates () =
  let u = uri "https://example.test/p?a=old&keep=x%2Fy&a=again&flag" in
  let u = Uriz.remove_query_param u "a" in
  check "remove every decoded key"
    (String.equal (Uriz.to_string u) "https://example.test/p?keep=x%2Fy&flag");
  let same = Uriz.remove_query_param u "missing" in
  check "no-op removal preserves identity" (phys_equal same u);
  let plus = uri "https://example.test/p?a+b=old&keep=1&a%20b=again" in
  let plus = Uriz.remove_query_param ~plus_as_space:true plus "a b" in
  check "form-style removal treats plus as space"
    (String.equal (Uriz.to_string plus) "https://example.test/p?keep=1");
  let u = Uriz.add_query_param u ~key:"a&b" ~value:"x+y=z" in
  check "append and encode query parameter"
    (String.equal
       (Uriz.to_string u)
       "https://example.test/p?keep=x%2Fy&flag&a%26b=x%2By%3Dz");
  check "decoded query parameters"
    (List.equal
       (fun (ak, av) (bk, bv) ->
          String.equal ak bk && Poly.equal av bv)
       (Uriz.query_params u)
       [ "keep", Some "x/y"; "flag", None; "a&b", Some "x+y=z" ]);
  check "find added parameter"
    (match Uriz.find_query_param u "a&b" with
     | This value -> String.equal value "x+y=z"
     | Null -> false)
;;

let test_scanner () =
  let text = "prefix:https://example.test/p?q:suffix" in
  let pos = 7 in
  let len = 24 in
  let spans = Uriz.Scanner.parse_sub text ~pos ~len in
  check "sub scanner valid" (Uriz.Scanner.is_valid spans);
  check "sub scanner absolute offsets" (Uriz.Scanner.host_off spans = 15);
  check "sub scanner query" (Uriz.Scanner.query_off spans = 30)
;;

let test_exn_quotes_input () =
  let message f =
    match f () with
    | (_ : Uriz.t) -> ""
    | exception Invalid_argument msg -> msg
  in
  let bad_uri = message (fun () -> Uriz.of_string_exn "http://h/\r\nforged") in
  check "URI exception cannot inject CR"
    (String.equal bad_uri "Uriz.of_string_exn: not a URI reference"
     && not (String.contains bad_uri '\r'));
  let bad_scheme =
    message (fun () -> Uriz.make_encoded ~scheme:"bad\r\nscheme" ~path:"/" ())
  in
  check "scheme exception cannot inject CR"
    (String.equal bad_scheme "Uriz: invalid scheme"
     && not (String.contains bad_scheme '\r'))
;;

let () =
  test_components ();
  test_public_api ();
  test_query_decoding ();
  test_query_updates ();
  test_scanner ();
  test_exn_quotes_input ();
  Stdio.printf "test_uriz: %d checks ok\n" !checks
;;
