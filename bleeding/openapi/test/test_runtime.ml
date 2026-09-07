open Openapi
let check name ok = Alcotest.(check bool) name true ok
let invalid f = match f () with
  | _ -> Alcotest.fail "expected invalid argument"
  | exception Invalid_argument _ -> ()
let json s = Result.get_ok (Jsont_bytesrw.decode_string ~locs:true Jsont.json s)
let decode codec s = Runtime.Json.decode codec s
let encode codec value = Runtime.Json.encode codec value

let paths () =
  Alcotest.(check string) "escaped components, one pass" "/%7Bb%7D/x%2Fy/%7Bb%7D"
    (Runtime.Path.render ~params:["a", "{b}"; "b", "x/y"] "/{a}/{b}/{a}");
  Alcotest.(check string) "UTF-8 bytes" "/%C3%A9%3F%23%25"
    (Runtime.Path.render ~params:["x", "é?#%"] "/{x}");
  invalid (fun () -> Runtime.Path.render ~params:[] "/{missing}");
  invalid (fun () -> Runtime.Path.render ~params:[] "/{broken");
  List.iter (fun x -> invalid (fun () -> Runtime.Path.render ~params:["x", x] "/{x}")) ["."; ".."];
  Alcotest.(check string) "query names and values escaped"
    "?a%26b%3Dc=x%2By%20z&a%26b%3Dc=%C3%A9"
    (Runtime.Query.encode ["a&b=c", "x+y z"; "a&b=c", "é"])

let json_trees () =
  let rejecting = Jsont.map Jsont.string ~dec:Fun.id
    ~enc:(fun _ -> Jsont.Error.msg Jsont.Meta.none "rejected") in
  invalid (fun () -> Runtime.Json.encode_json rejecting "test");
  check "reference codec propagates encode failure"
    (Result.is_error (encode (Spec.or_ref_jsont rejecting) (Spec.Value "test")));
  check "reference field must be a string"
    (Result.is_error (decode (Spec.or_ref_jsont Jsont.json) {|{"$ref":23}|}));
  check "JSON tree direct decode" (Runtime.Json.decode_json Jsont.(list int) (json "[1,2]") = Ok [1;2]);
  check "nullable combinator" (decode (Runtime.nullable_any Jsont.string) "null" = Ok None)

let strings () =
  let codec = Runtime.validated_string ~min_length:1 ~max_length:1 Jsont.string in
  List.iter (fun text ->
    check "Unicode code point count" (Result.is_ok (encode codec text));
    check "Unicode decoding" (decode codec (Result.get_ok (encode Jsont.string text)) = Ok text)) ["é"; "🦀"];
  check "encoding constraints" (Result.is_error (encode codec "ab"));
  check "decoding constraints" (Result.is_error (decode codec {|"ab"|}));
  check "invalid UTF-8" (Result.is_error (encode codec "\xff"));
  let pattern = Runtime.validated_string ~pattern:"^[a-z]+$" Jsont.string in
  check "pattern encode" (Result.is_error (encode pattern "1"));
  check "invalid regex is not ignored"
    (try ignore (Runtime.validated_string ~pattern:"[" Jsont.string); false with _ -> true)

let numbers () =
  let codec = Runtime.validated_int64 ~maximum:9007199254740992. Jsont.int64 in
  check "exact boundary accepted" (Result.is_ok (encode codec 9007199254740992L));
  check "bound does not round integers" (Result.is_error (encode codec 9007199254740993L));
  let min = Runtime.validated_int64 ~minimum:(-1.5) Jsont.int64 in
  check "fractional negative minimum" (Result.is_error (encode min (-2L)) && Result.is_ok (encode min (-1L)));
  let max = Runtime.validated_int64 ~maximum:1.5 Jsont.int64 in
  check "fractional positive maximum" (Result.is_error (encode max 2L) && Result.is_ok (encode max 1L));
  let exclusive = Runtime.validated_int ~exclusive_minimum:1. ~exclusive_maximum:3. Jsont.int in
  check "exclusive bounds" (Result.is_error (encode exclusive 1) && Result.is_ok (encode exclusive 2) && Result.is_error (encode exclusive 3));
  List.iter (fun text -> check "strict integer" (Result.is_error (decode Runtime.int_jsont text))) ["1.5"; "null"; "1e999"; {|"1"|}];
  List.iter (fun value -> check "finite number encoding" (Result.is_error (encode Runtime.number_jsont value))) [nan; infinity; neg_infinity];
  List.iter (fun text -> check "finite number decoding" (Result.is_error (decode Runtime.number_jsont text))) ["null"; "1e999"];
  check "large integers are not emitted as strings" (Result.is_error (encode Runtime.int64_jsont Int64.max_int))

let lists () =
  let codec = Runtime.validated_list ~unique_items:true Jsont.json in
  let duplicate = json {|[{"a":1,"b":2},{"b":2,"a":1}]|} in
  check "JSON equality ignores object order and locations" (Result.is_error (Runtime.Json.decode_json codec duplicate));
  let values = match duplicate with Jsont.Array (values, _) -> values | _ -> assert false in
  check "uniqueness validated on encode" (Result.is_error (encode codec values));
  check "distinct items" (Result.is_ok (decode codec "[1,2]"));
  check "list length encode" (Result.is_error (encode (Runtime.validated_list ~min_items:1 Jsont.string) []))

let generation_errors () =
  let generate text =
    let spec = Result.get_ok (Spec.of_string text) in
    Codegen.generate ~config:{output_dir=".";package_name="test";spec_path=None} spec in
  invalid (fun () -> generate {|{"openapi":"3.0.3","info":{"title":"test","version":"1"},"paths":{},"components":{"schemas":{"Cycle":{"allOf":[{"$ref":"#/components/schemas/Cycle"}]}}}}|});
  invalid (fun () -> generate {|{"openapi":"3.0.3","info":{"title":"test","version":"1"},"paths":{"/":{"get":{"parameters":[{"$ref":"#/components/parameters/Missing"}],"responses":{"204":{"description":"ok"}}}}}}|});
  let spec = Result.get_ok (Spec.of_string {|{"openapi":"3.0.3","info":{"title":"test","version":"1"},"paths":{}}|}) in
  invalid (fun () -> Codegen.generate ~config:{output_dir=".";package_name="../outside";spec_path=None} spec)

let () = Alcotest.run "OpenAPI runtime review" ["regressions", List.map (fun (n, f) -> n, `Quick, f) [
  "URI escaping", paths;
  "JSON trees and failures", json_trees;
  "Unicode and regex constraints", strings;
  "numeric constraints", numbers;
  "JSON array uniqueness", lists;
  "generation failures", generation_errors;
]]
