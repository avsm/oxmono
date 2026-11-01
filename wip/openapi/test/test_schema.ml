open Openapi
module Api = Schema_api
let check name ok = Alcotest.(check bool) name true ok
let json s = Result.get_ok (Runtime.Json.decode Jsont.json s)
let decode = Runtime.Json.decode
let encode = Runtime.Json.encode
let invalid f = match f () with
  | _ -> Alcotest.fail "expected invalid argument" | exception Invalid_argument _ -> ()
let rejects codec value =
  check ("reject decode " ^ value) (Result.is_error (decode codec value));
  check ("reject encode " ^ value) (Result.is_error (encode codec (json value)))
let accepts codec value =
  check ("accept decode " ^ value) (Result.is_ok (decode codec value));
  check ("accept encode " ^ value) (Result.is_ok (encode codec (json value)))
let context version definitions = Schema.of_string ~version definitions
let codec ?(version="3.1.2") schema = Schema.guard_string (context version "{}") schema Jsont.json

let dialects () =
  List.iter (fun schema ->
    let parsed = Result.get_ok (decode Spec.schema_or_ref_jsont schema) in
    let reparsed = Result.get_ok (decode Spec.schema_or_ref_jsont (Result.get_ok (encode Spec.schema_or_ref_jsont parsed))) in
    check "schema model round trip" (parsed = reparsed))
    ["true";"false";{|{"type":["string","null"]}|};
     {|{"minimum":0,"exclusiveMinimum":true,"maximum":10,"exclusiveMaximum":false}|};
     {|{"$ref":"#/components/schemas/X","minimum":1}|};
     {|{"type":"object","dependentRequired":{"a":["b"]}}|}];
  let bounded = codec ~version:"3.0.3" {|{"type":"number","minimum":0,"exclusiveMinimum":true,"maximum":1,"exclusiveMaximum":false}|} in
  rejects bounded "0"; accepts bounded "1";
  let nullable = codec {|{"type":["string","null"],"minLength":2}|} in
  accepts nullable "null"; accepts nullable {|"ok"|}; rejects nullable {|"x"|}; rejects nullable "42";
  rejects (codec "false") "null"; accepts (codec "true") "null";
  List.iter (fun schema -> invalid (fun () -> codec ~version:"3.0.3" schema))
    ["false";{|{"type":["string","null"]}|};{|{"exclusiveMinimum":1}|}];
  invalid (fun () -> codec {|{"exclusiveMinimum":true}|});
  invalid (fun () -> context "2.0" "{}");
  let c = context "3.0.3" {|{"N":{"type":"integer"}}|} in
  let alias = Schema.guard_string c {|{"$ref":"#/components/schemas/N","minimum":10,"type":["null"]}|} Jsont.json in
  accepts alias "1"; rejects alias "null";
  let c = context "3.1.2" {|{"N":{"type":"integer"}}|} in
  let alias = Schema.guard_string c {|{"$ref":"#/components/schemas/N","minimum":10}|} Jsont.json in
  rejects alias "1"; accepts alias "10";
  rejects (codec {|{"type":"string","nullable":true}|}) "null"

let constraints () =
  let multiples = codec {|{"type":"number","multipleOf":0.1}|} in
  List.iter (accepts multiples) ["0.3";"-0.3";"1e-1"];
  List.iter (rejects multiples) ["0.30000000000000004";"0.31"];
  let obj = codec {|{"type":"object","minProperties":1,"maxProperties":2,"required":["id"],"properties":{"id":{"const":1}},"additionalProperties":{"type":"string"}}|} in
  accepts obj {|{"id":1,"note":"ok"}|};
  List.iter (rejects obj) ["{}";{|{"id":2}|};{|{"id":1,"extra":2}|};{|{"id":1,"x":"a","y":"b"}|};{|{"id":1,"id":1}|}];
  rejects (codec {|{"not":{"enum":[1,2]}}|}) "1";
  accepts (codec {|{"anyOf":[{"type":"integer"},{"type":"string"}]}|}) {|"x"|};
  List.iter (fun s -> invalid (fun () -> codec s))
    [{|{"multipleOf":0}|};{|{"minLength":1.5}|};{|{"minItems":-1}|};{|{"type":[]}|};{|{"type":["string","string"]}|};{|{"type":"wat"}|};{|{"if":true}|}]

let references () =
  let c = context "3.1.2" {|{"a/b~c":{"type":"object","properties":{"id":{"type":"integer","minimum":1}}},"Alias":{"$ref":"#/components/schemas/a~1b~0c"}}|} in
  let nested = Schema.guard_string c {|{"$ref":"#/components/schemas/a~1b~0c/properties/id"}|} Jsont.json in
  accepts nested "1"; rejects nested "0";
  let encoded = Schema.guard_string c {|{"$ref":"#/components/schemas/a~1b%7E0c/properties/id"}|} Jsont.json in
  accepts encoded "2";
  invalid (fun () -> Schema.guard_string c {|{"$ref":"#/components/schemas/a%zz"}|} Jsont.json);
  invalid (fun () -> Schema.guard_string c {|{"$ref":"#/components/schemas/Missing"}|} Jsont.json);
  invalid (fun () -> context "3.1.2" {|{"Bad":{"$ref":"#/components/schemas/Missing"}}|});
  let cyclic = context "3.1.2" {|{"Cycle":{"$ref":"#/components/schemas/Cycle"}}|} in
  rejects (Schema.guard_ref cyclic "Cycle" Jsont.json) "null"

let presence () =
  let values = [None, {|{"required":null}|}; Some None, {|{"required":null,"optional":null}|};
    Some (Some "set"), {|{"required":null,"optional":"set"}|}] in
  List.iter (fun (optional, text) ->
    let v = Result.get_ok (decode Api.Presence.T.jsont text) in
    check "presence decoded" (Api.Presence.T.optional v = optional);
    let made = Api.Presence.T.v ?optional () in
    let encoded = Result.get_ok (encode Api.Presence.T.jsont made) in
    check "presence encoded" (Jsont.Json.equal (json text) (json encoded))) values;
  check "required nullable must be present" (Result.is_error (decode Api.Presence.T.jsont "{}"))

let generated_scalars () =
  check "numeric component" (decode Api.Scalar.T.jsont "3" = Ok 3);
  check "alias component" (decode Api.Alias.T.jsont "5" = Ok 5);
  check "numeric enum decode" (Result.is_error (decode Api.Scalar.T.jsont "2"));
  check "numeric enum encode" (Result.is_error (encode Api.Scalar.T.jsont 2));
  check "array component, sibling ordering" (decode Api.Batch.Response.jsont "[1,2]" = Ok [1;2]);
  check "array constraints" (Result.is_error (encode Api.Batch.Response.jsont []));
  check "array item constraints" (Result.is_error (encode Api.Batch.Response.jsont [0]));
  List.iter (accepts Api.Mixed.T.jsont) ["1";{|"one"|};"null"];
  rejects Api.Mixed.T.jsont "2";
  check "nullable object" (decode Api.NullableObject.T.jsont "null" = Ok None);
  check "nullable object fields" (Result.is_error (decode Api.NullableObject.T.jsont "{}"));
  rejects Api.Never.T.jsont "null"; accepts Api.Anything.T.jsont "42";
  check "reference sibling bounds" (Result.is_error (decode Api.RefSibling.T.jsont "1"));
  check "reference sibling value" (Result.is_ok (decode Api.RefSibling.T.jsont "3"))

let defaults_and_names () =
  let v = Result.get_ok (decode Api.Defaults.T.jsont "{}") in
  check "float default round-trip precision" (Api.Defaults.T.precise v = 0.30000000000000004);
  check "invalid numeric default not truncated" (Api.Defaults.T.integer v = None);
  check "nonnullable null default does not generate invalid literal" (Api.Defaults.T.not_null v = None);
  check "defaults remain encodable" (Result.is_ok (encode Api.Defaults.T.jsont v));
  let v = Api.KeyNames.T.v ~v_:"value" ~jsont_:"codec" () in
  check "reserved accessor names" (Api.KeyNames.T.v_ v = "value" && Api.KeyNames.T.jsont_ v = "codec");
  check "escaped enum constructors" (Result.is_ok (decode Api.KeyEnum.T.jsont {|"__strange"|}))

let generated_constraints () =
  let valid = {|{"mode":"safe","value":0.3,"stamp":"fixed"}|} in
  let v = Result.get_ok (decode Api.Restricted.T.jsont valid) in
  check "restricted round trip" (Result.is_ok (encode Api.Restricted.T.jsont v));
  List.iter (fun value -> check "field and object constraints" (Result.is_error (decode Api.Restricted.T.jsont value)))
    [{|{"mode":"unsafe","value":0.3}|};{|{"mode":"safe","value":0.31}|};{|{"mode":"safe","value":0.3,"stamp":"wrong"}|};{|{"mode":"safe","value":0.3,"extra":1}|}];
  check "record encoding validation" (Result.is_error (encode Api.Restricted.T.jsont (Api.Restricted.T.v ~mode:"unsafe" ~value:0.3 ())));
  List.iter (fun n -> check "allOf intersection" (Result.is_error (decode Api.Intersection.T.jsont n)))
    [{|{"n":0}|};{|{"n":4}|}];
  check "allOf valid" (Result.is_ok (decode Api.Intersection.T.jsont {|{"n":2}|}));
  check "inline mixed union retains objects" (Result.is_ok (decode Api.UnionField.T.jsont {|{"choice":{"ok":true}}|}));
  check "inline mixed union validates objects" (Result.is_error (decode Api.UnionField.T.jsont {|{"choice":{"ok":false}}|}))

let recursive () =
  let valid = {|{"value":1,"children":[{"value":2,"children":[{"value":3}]}]}|} in
  let v = Result.get_ok (decode Api.Node.T.jsont valid) in
  check "recursive value" (Result.is_ok (encode Api.Node.T.jsont v));
  check "recursive constraints" (Result.is_error (decode Api.Node.T.jsont {|{"value":1,"children":[{"value":0}]}|}));
  check "recursive array shape" (Result.is_error (decode Api.Node.T.jsont {|{"value":1,"children":{}}|}));
  accepts Api.Loop.Item.jsont {|{"n":1,"next":{"n":2,"next":{"n":3}}}|};
  rejects Api.Loop.Item.jsont {|{"n":1,"next":{"n":0}}|};
  accepts Api.Loop.Response.jsont {|{"n":1,"next":{"n":2}}|};
  let bad = Api.Node.T.v ~value:1 ~children:[json {|{"value":0}|}] () in
  check "opaque recursive field encode" (Result.is_error (encode Api.Node.T.jsont bad))

let unions () =
  let v = Result.get_ok (decode Api.Pet.T.jsont {|{"kind":"PetCat","meows":true}|}) in
  check "implicit discriminator encode" (Result.is_ok (encode Api.Pet.T.jsont v));
  check "implicit discriminator case sensitive" (Result.is_error (decode Api.Pet.T.jsont {|{"kind":"pet_cat","meows":true}|}));
  check "oneOf valid" (Result.is_ok (decode Api.Branch.T.jsont "-1"));
  check "oneOf ambiguous decode" (Result.is_error (decode Api.Branch.T.jsont "5"));
  let bad = Api.Branch.T.Low 5. in
  check "oneOf ambiguous encode" (Result.is_error (encode Api.Branch.T.jsont bad))

let operations () = Eio_mock.Backend.run_full (fun _ ->
  let client status body = Api.of_fetch ~base_url:"https://example.test"
    (Fetch_mock.client (fun req -> Fetch_mock.respond ~status ~headers:(Http.Header.of_list ["Content-Type","application/json"]) body req)) in
  let rejects f = match f () with
    | _ -> Alcotest.fail "expected Fetch decoding error"
    | exception Eio.Io (Fetch.E (Fetch.Decode_failure _), _) -> () in
  ignore (Api.Client.get_choice (client 200 {|"ok"|}) ());
  ignore (Api.Client.get_choice (client 201 "2") ());
  rejects (fun () -> Api.Client.get_choice (client 200 "2") ());
  rejects (fun () -> Api.Client.get_choice (client 201 {|"ok"|}) ());
  rejects (fun () -> Api.Client.get_nested (client 200 "0") ());
  ignore (Api.Client.get_nested (client 200 "1") ());
  let called = ref false in
  let c = Api.of_fetch ~base_url:"https://example.test" (Fetch_mock.client (fun req ->
    called := true; Fetch_mock.respond ~status:204 "" req)) in
  invalid (fun () -> Api.Client.send_input ~body:(json {|{"amount":0}|}) c ());
  check "request rejected before I/O" (not !called);
  Api.Client.send_input ~body:(json {|{"amount":1}|}) c ();
  check "valid request sent" !called;
  Api.Client.get_items c (); Api.Client.get_items_by_id ~id:"a" c ())

let generate text = Codegen.generate ~config:{output_dir=".";package_name="test";spec_path=None}
  (Result.get_ok (Spec.of_string text))
let document ?(paths="{}") schemas =
  Printf.sprintf {|{"openapi":"3.1.2","info":{"title":"test","version":"1"},"paths":%s,"components":{"schemas":%s}}|} paths schemas
let preflight () =
  List.iter (fun schemas -> invalid (fun () -> generate (document schemas)))
    [{|{"Bad":{"type":"object","properties":{"x":{"type":"integer","minimum":10,"default":1}}}}|};{|{"a-b":{},"a_b":{}}|};{|{"Client":{}}|};
     {|{"Obj":{"type":"object","properties":{"a-b":{},"a_b":{}}}}|};
     {|{"Enum":{"type":"string","enum":["a-b","a_b"]}}|};
     {|{"Bad":{"$ref":"#/components/schemas/Missing"}}|}];
  List.iter (fun paths -> invalid (fun () -> generate (document ~paths "{}")))
    [{|{"/{id":{"get":{"responses":{}}}}|};{|{"/{id}":{"get":{"responses":{"204":{"description":"ok"}}}}}|};
     {|{"/":{"get":{"responses":{"600":{"description":"bad"}}}}}|};
     {|{"/":{"get":{"operationId":"same","responses":{}},"post":{"operationId":"same","responses":{}}}}|};
     {|{"/{id}":{"get":{"parameters":[{"in":"path","name":"id"}],"responses":{}}}}|}];
  check "duplicate schemas rejected" (Result.is_error (Spec.of_string (document {|{"X":{},"X":{}}|})));
  check "duplicate responses rejected" (Result.is_error (Spec.of_string (document ~paths:{|{"/":{"get":{"responses":{"200":{"description":"a"},"200":{"description":"b"}}}}}|} "{}")));
  check "response extensions are allowed" (Result.is_ok (decode Spec.responses_jsont {|{"x-note":"anything","200":{"description":"ok"}}|}));
  check "fractional schema counts rejected" (Result.is_error (decode Spec.schema_jsont {|{"minLength":1.5}|}));
  let first = document {|{"Value":{"type":"string"}}|} and second = document {|{"Thing":{"type":"array","items":{"type":"integer"}}}|} in
  let result = generate first in ignore (generate second);
  check "generation context does not leak" (generate first = result)

let writes () =
  let dir = Filename.temp_file "openapi-writes" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let path = Filename.concat dir "api.ml" in
  let read () = let ic = open_in path in Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic)) in
  Fun.protect ~finally:(fun () -> Array.iter (fun n ->
    let p = Filename.concat dir n in if Sys.is_directory p then Unix.rmdir p else Sys.remove p) (Sys.readdir dir); Unix.rmdir dir)
    (fun () ->
      Codegen.write_files ~output_dir:dir ["api.ml","old"];
      invalid (fun () -> Codegen.write_files ~output_dir:dir ["api.ml","new";"../outside","bad"]);
      check "preflight protects existing output" (read () = "old");
      Unix.mkdir (Filename.concat dir "blocked.ml") 0o700;
      (match Codegen.write_files ~output_dir:dir ["blocked.ml","new"] with
       | () -> Alcotest.fail "expected rename failure" | exception Sys_error _ -> ());
      check "failed writes clean temporaries" (Array.length (Sys.readdir dir) = 2);
      Codegen.write_files ~output_dir:dir ["api.ml","new"];
      check "successful replacement" (read () = "new"))

let () = Alcotest.run "OpenAPI schema correctness" ["regressions", List.map (fun (n,f) -> n, `Quick, f) [
  "dialects and schema round trips", dialects;
  "complete constraint guards", constraints;
  "reference graph and JSON pointers", references;
  "nullable presence", presence;
  "generated scalars and arrays", generated_scalars;
  "defaults and generated names", defaults_and_names;
  "generated field and intersection constraints", generated_constraints;
  "recursive fields", recursive;
  "union encode and discriminator", unions;
  "operation schemas", operations;
  "generation preflight and isolation", preflight;
  "atomic output replacement", writes;
]]
