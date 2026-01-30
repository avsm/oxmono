(** Tests for ocaml-openapi *)

module Spec = Openapi.Spec
module Codegen = Openapi.Codegen
module Runtime = Openapi.Runtime

(** {1 Path Template Tests} *)

let test_path_render_simple () =
  let result = Runtime.Path.render ~params:[] "/users" in
  Alcotest.(check string) "no params" "/users" result

let test_path_render_one_param () =
  let result = Runtime.Path.render ~params:[("id", "123")] "/users/{id}" in
  Alcotest.(check string) "one param" "/users/123" result

let test_path_render_multiple_params () =
  let result = Runtime.Path.render
    ~params:[("userId", "42"); ("postId", "99")]
    "/users/{userId}/posts/{postId}" in
  Alcotest.(check string) "multiple params" "/users/42/posts/99" result

let test_path_parameters () =
  let params = Runtime.Path.parameters "/users/{userId}/posts/{postId}" in
  Alcotest.(check (list string)) "extract params" ["userId"; "postId"] params

(** {1 Query Parameter Tests} *)

let test_query_singleton () =
  let params = Runtime.Query.singleton ~key:"name" ~value:"alice" in
  Alcotest.(check (list (pair string string))) "singleton" [("name", "alice")] params

let test_query_optional_some () =
  let params = Runtime.Query.optional ~key:"name" ~value:(Some "alice") in
  Alcotest.(check (list (pair string string))) "optional some" [("name", "alice")] params

let test_query_optional_none () =
  let params = Runtime.Query.optional ~key:"name" ~value:None in
  Alcotest.(check (list (pair string string))) "optional none" [] params

let test_query_encode_empty () =
  let result = Runtime.Query.encode [] in
  Alcotest.(check string) "empty query" "" result

let test_query_encode_single () =
  let result = Runtime.Query.encode [("name", "alice")] in
  Alcotest.(check string) "single query" "?name=alice" result

let test_query_encode_multiple () =
  let result = Runtime.Query.encode [("name", "alice"); ("age", "30")] in
  Alcotest.(check string) "multiple query" "?name=alice&age=30" result

let test_query_encode_special_chars () =
  let result = Runtime.Query.encode [("q", "hello world")] in
  Alcotest.(check string) "special chars" "?q=hello%20world" result

(** {1 Name Conversion Tests} *)

let test_snake_case_simple () =
  let result = Codegen.Name.to_snake_case "getUserById" in
  Alcotest.(check string) "camel to snake" "get_user_by_id" result

let test_snake_case_with_dashes () =
  let result = Codegen.Name.to_snake_case "user-name" in
  Alcotest.(check string) "dashes to underscore" "user_name" result

let test_snake_case_reserved () =
  let result = Codegen.Name.to_snake_case "type" in
  Alcotest.(check string) "reserved word" "type_" result

let test_module_name () =
  let result = Codegen.Name.to_module_name "user_profile" in
  Alcotest.(check string) "module name" "UserProfile" result

let test_variant_name () =
  let result = Codegen.Name.to_variant_name "active_user" in
  Alcotest.(check string) "variant name" "Active_user" result

(** {1 Spec Parsing Tests} *)

let minimal_spec = {|{
  "openapi": "3.0.0",
  "info": {
    "title": "Test API",
    "version": "1.0.0"
  },
  "paths": {}
}|}

let test_parse_minimal_spec () =
  match Spec.of_string minimal_spec with
  | Error e -> Alcotest.fail e
  | Ok spec ->
      Alcotest.(check string) "openapi version" "3.0.0" spec.openapi;
      Alcotest.(check string) "title" "Test API" spec.info.title;
      Alcotest.(check string) "version" "1.0.0" spec.info.version

let spec_with_schema = {|{
  "openapi": "3.0.0",
  "info": {
    "title": "Test API",
    "version": "1.0.0"
  },
  "paths": {},
  "components": {
    "schemas": {
      "User": {
        "type": "object",
        "properties": {
          "id": { "type": "integer" },
          "name": { "type": "string" },
          "email": { "type": "string", "format": "email" }
        },
        "required": ["id", "name"]
      }
    }
  }
}|}

let test_parse_schema () =
  match Spec.of_string spec_with_schema with
  | Error e -> Alcotest.fail e
  | Ok spec ->
      match spec.components with
      | None -> Alcotest.fail "expected components"
      | Some c ->
          Alcotest.(check int) "schema count" 1 (List.length c.schemas);
          match List.assoc_opt "User" c.schemas with
          | None -> Alcotest.fail "expected User schema"
          | Some (Spec.Ref _) -> Alcotest.fail "expected value not ref"
          | Some (Spec.Value s) ->
              Alcotest.(check (option string)) "type" (Some "object") s.type_;
              Alcotest.(check int) "properties" 3 (List.length s.properties);
              Alcotest.(check (list string)) "required" ["id"; "name"] s.required

let spec_with_enum = {|{
  "openapi": "3.0.0",
  "info": {
    "title": "Test API",
    "version": "1.0.0"
  },
  "paths": {},
  "components": {
    "schemas": {
      "Status": {
        "type": "string",
        "enum": ["active", "inactive", "pending"]
      }
    }
  }
}|}

let test_parse_enum () =
  match Spec.of_string spec_with_enum with
  | Error e -> Alcotest.fail e
  | Ok spec ->
      match spec.components with
      | None -> Alcotest.fail "expected components"
      | Some c ->
          match List.assoc_opt "Status" c.schemas with
          | None -> Alcotest.fail "expected Status schema"
          | Some (Spec.Ref _) -> Alcotest.fail "expected value not ref"
          | Some (Spec.Value s) ->
              match s.enum with
              | None -> Alcotest.fail "expected enum"
              | Some values ->
                  Alcotest.(check int) "enum count" 3 (List.length values)

let spec_with_paths = {|{
  "openapi": "3.0.0",
  "info": {
    "title": "Test API",
    "version": "1.0.0"
  },
  "paths": {
    "/users": {
      "get": {
        "operationId": "listUsers",
        "summary": "List all users",
        "responses": {
          "200": {
            "description": "Success"
          }
        }
      },
      "post": {
        "operationId": "createUser",
        "summary": "Create a user",
        "responses": {
          "201": {
            "description": "Created"
          }
        }
      }
    },
    "/users/{id}": {
      "get": {
        "operationId": "getUser",
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "schema": { "type": "integer" }
          }
        ],
        "responses": {
          "200": {
            "description": "Success"
          }
        }
      }
    }
  }
}|}

let test_parse_paths () =
  match Spec.of_string spec_with_paths with
  | Error e -> Alcotest.fail e
  | Ok spec ->
      Alcotest.(check int) "path count" 2 (List.length spec.paths);
      match List.assoc_opt "/users" spec.paths with
      | None -> Alcotest.fail "expected /users path"
      | Some path_item ->
          (match path_item.get with
           | None -> Alcotest.fail "expected GET"
           | Some op ->
               Alcotest.(check (option string)) "operation id" (Some "listUsers") op.operation_id);
          (match path_item.post with
           | None -> Alcotest.fail "expected POST"
           | Some op ->
               Alcotest.(check (option string)) "operation id" (Some "createUser") op.operation_id)

(** {1 Code Generation Tests} *)

let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub > len_s then false
  else
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else check (i + 1)
    in
    check 0

let test_split_schema_name () =
  let p, s = Codegen.Name.split_schema_name "AlbumResponseDto" in
  Alcotest.(check string) "prefix" "Album" p;
  Alcotest.(check string) "suffix" "ResponseDto" s

let test_split_schema_name_no_suffix () =
  let p, s = Codegen.Name.split_schema_name "User" in
  Alcotest.(check string) "prefix" "User" p;
  Alcotest.(check string) "suffix" "T" s

let test_generate_files () =
  match Spec.of_string spec_with_schema with
  | Error e -> Alcotest.fail e
  | Ok spec ->
      let config = Codegen.{
        output_dir = ".";
        package_name = "test_api";
        spec_path = None;
      } in
      let files = Codegen.generate ~config spec in
      Alcotest.(check int) "file count" 4 (List.length files);
      let ml = List.assoc_opt "test_api.ml" files in
      (match ml with
       | None -> Alcotest.fail "missing .ml file"
       | Some content ->
           Alcotest.(check bool) "contains module User" true
             (contains_substring content "module User"))

let test_generate_enum_schema () =
  match Spec.of_string spec_with_enum with
  | Error e -> Alcotest.fail e
  | Ok spec ->
      let config = Codegen.{
        output_dir = ".";
        package_name = "test_enum";
        spec_path = None;
      } in
      let files = Codegen.generate ~config spec in
      let ml = List.assoc_opt "test_enum.ml" files in
      (match ml with
       | None -> Alcotest.fail "missing .ml file"
       | Some content ->
           Alcotest.(check bool) "contains Active variant" true
             (contains_substring content "Active"))

(** {1 Test Suites} *)

let path_tests = [
  "render simple", `Quick, test_path_render_simple;
  "render one param", `Quick, test_path_render_one_param;
  "render multiple params", `Quick, test_path_render_multiple_params;
  "extract parameters", `Quick, test_path_parameters;
]

let query_tests = [
  "singleton", `Quick, test_query_singleton;
  "optional some", `Quick, test_query_optional_some;
  "optional none", `Quick, test_query_optional_none;
  "encode empty", `Quick, test_query_encode_empty;
  "encode single", `Quick, test_query_encode_single;
  "encode multiple", `Quick, test_query_encode_multiple;
  "encode special chars", `Quick, test_query_encode_special_chars;
]

let name_tests = [
  "snake case simple", `Quick, test_snake_case_simple;
  "snake case dashes", `Quick, test_snake_case_with_dashes;
  "snake case reserved", `Quick, test_snake_case_reserved;
  "module name", `Quick, test_module_name;
  "variant name", `Quick, test_variant_name;
]

let spec_tests = [
  "parse minimal", `Quick, test_parse_minimal_spec;
  "parse schema", `Quick, test_parse_schema;
  "parse enum", `Quick, test_parse_enum;
  "parse paths", `Quick, test_parse_paths;
]

let codegen_tests = [
  "split schema name", `Quick, test_split_schema_name;
  "split schema name no suffix", `Quick, test_split_schema_name_no_suffix;
  "generate files", `Quick, test_generate_files;
  "generate enum schema", `Quick, test_generate_enum_schema;
]

let () =
  Alcotest.run "openapi" [
    "Path", path_tests;
    "Query", query_tests;
    "Name", name_tests;
    "Spec", spec_tests;
    "Codegen", codegen_tests;
  ]
