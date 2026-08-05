(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Consolidated unit tests for the Claude OCaml SDK.

    This test suite covers:
    - Protocol message encoding/decoding
    - Tool module for custom tool definitions
    - Mcp_server module for in-process MCP servers
    - Structured error handling *)

module J = Jsont.Json

(* ============================================
   Protocol Tests - Incoming message codec
   ============================================ *)

let test_decode_user_message () =
  (* User messages from CLI come wrapped in a "message" envelope *)
  let json_str = {|{"type":"user","message":{"content":"Hello"}}|} in
  match Jsont_bytesrw.decode_string' Claude.Proto.Incoming.jsont json_str with
  | Ok (Claude.Proto.Incoming.Message (Claude.Proto.Message.User _)) -> ()
  | Ok _ -> Alcotest.fail "Wrong message type decoded"
  | Error err -> Alcotest.fail (Jsont.Error.to_string err)

let test_decode_assistant_message () =
  (* Assistant messages from CLI come wrapped in a "message" envelope *)
  let json_str =
    {|{"type":"assistant","message":{"model":"claude-sonnet-4","content":[{"type":"text","text":"Hi"}]}}|}
  in
  match Jsont_bytesrw.decode_string' Claude.Proto.Incoming.jsont json_str with
  | Ok (Claude.Proto.Incoming.Message (Claude.Proto.Message.Assistant _)) -> ()
  | Ok _ -> Alcotest.fail "Wrong message type decoded"
  | Error err -> Alcotest.fail (Jsont.Error.to_string err)

let test_decode_system_message () =
  let json_str =
    {|{"type":"system","subtype":"init","data":{"session_id":"test-123"}}|}
  in
  match Jsont_bytesrw.decode_string' Claude.Proto.Incoming.jsont json_str with
  | Ok (Claude.Proto.Incoming.Message (Claude.Proto.Message.System _)) -> ()
  | Ok _ -> Alcotest.fail "Wrong message type decoded"
  | Error err -> Alcotest.fail (Jsont.Error.to_string err)

let test_decode_control_response_success () =
  let json_str =
    {|{"type":"control_response","response":{"subtype":"success","requestId":"test-req-1"}}|}
  in
  match Jsont_bytesrw.decode_string' Claude.Proto.Incoming.jsont json_str with
  | Ok (Claude.Proto.Incoming.Control_response resp) -> (
      match resp.response with
      | Claude.Proto.Control.Response.Success s ->
          Alcotest.(check string) "request_id" "test-req-1" s.request_id
      | Claude.Proto.Control.Response.Error _ ->
          Alcotest.fail "Got error response instead of success")
  | Ok _ -> Alcotest.fail "Wrong message type decoded"
  | Error err -> Alcotest.fail (Jsont.Error.to_string err)

let test_decode_control_response_error () =
  let json_str =
    {|{"type":"control_response","response":{"subtype":"error","requestId":"test-req-2","error":{"code":-32603,"message":"Something went wrong"}}}|}
  in
  match Jsont_bytesrw.decode_string' Claude.Proto.Incoming.jsont json_str with
  | Ok (Claude.Proto.Incoming.Control_response resp) -> (
      match resp.response with
      | Claude.Proto.Control.Response.Error e ->
          Alcotest.(check string) "request_id" "test-req-2" e.request_id;
          Alcotest.(check int) "error code" (-32603) e.error.code;
          Alcotest.(check string)
            "error message" "Something went wrong" e.error.message
      | Claude.Proto.Control.Response.Success _ ->
          Alcotest.fail "Got success response instead of error")
  | Ok _ -> Alcotest.fail "Wrong message type decoded"
  | Error err -> Alcotest.fail (Jsont.Error.to_string err)

let protocol_tests =
  [
    Alcotest.test_case "decode user message" `Quick test_decode_user_message;
    Alcotest.test_case "decode assistant message" `Quick
      test_decode_assistant_message;
    Alcotest.test_case "decode system message" `Quick test_decode_system_message;
    Alcotest.test_case "decode control response success" `Quick
      test_decode_control_response_success;
    Alcotest.test_case "decode control response error" `Quick
      test_decode_control_response_error;
  ]

(* ============================================
   Tool Module Tests
   ============================================ *)

let json_testable =
  Alcotest.testable
    (fun fmt json ->
      match Jsont_bytesrw.encode_string' Jsont.json json with
      | Ok s -> Format.pp_print_string fmt s
      | Error e -> Format.pp_print_string fmt (Jsont.Error.to_string e))
    (fun a b ->
      match
        ( Jsont_bytesrw.encode_string' Jsont.json a,
          Jsont_bytesrw.encode_string' Jsont.json b )
      with
      | Ok sa, Ok sb -> String.equal sa sb
      | _ -> false)

let test_tool_schema_string () =
  let schema = Claude.Tool.schema_string in
  let expected = J.object' [ J.mem (J.name "type") (J.string "string") ] in
  Alcotest.check json_testable "schema_string" expected schema

let test_tool_schema_int () =
  let schema = Claude.Tool.schema_int in
  let expected = J.object' [ J.mem (J.name "type") (J.string "integer") ] in
  Alcotest.check json_testable "schema_int" expected schema

let test_tool_schema_number () =
  let schema = Claude.Tool.schema_number in
  let expected = J.object' [ J.mem (J.name "type") (J.string "number") ] in
  Alcotest.check json_testable "schema_number" expected schema

let test_tool_schema_bool () =
  let schema = Claude.Tool.schema_bool in
  let expected = J.object' [ J.mem (J.name "type") (J.string "boolean") ] in
  Alcotest.check json_testable "schema_bool" expected schema

let test_tool_schema_array () =
  let schema = Claude.Tool.schema_array Claude.Tool.schema_string in
  let expected =
    J.object'
      [
        J.mem (J.name "type") (J.string "array");
        J.mem (J.name "items")
          (J.object' [ J.mem (J.name "type") (J.string "string") ]);
      ]
  in
  Alcotest.check json_testable "schema_array" expected schema

let test_tool_schema_string_enum () =
  let schema = Claude.Tool.schema_string_enum [ "foo"; "bar"; "baz" ] in
  let expected =
    J.object'
      [
        J.mem (J.name "type") (J.string "string");
        J.mem (J.name "enum")
          (J.list [ J.string "foo"; J.string "bar"; J.string "baz" ]);
      ]
  in
  Alcotest.check json_testable "schema_string_enum" expected schema

let test_tool_schema_object () =
  let schema =
    Claude.Tool.schema_object
      [ ("name", Claude.Tool.schema_string); ("age", Claude.Tool.schema_int) ]
      ~required:[ "name" ]
  in
  let expected =
    J.object'
      [
        J.mem (J.name "type") (J.string "object");
        J.mem (J.name "properties")
          (J.object'
             [
               J.mem (J.name "name")
                 (J.object' [ J.mem (J.name "type") (J.string "string") ]);
               J.mem (J.name "age")
                 (J.object' [ J.mem (J.name "type") (J.string "integer") ]);
             ]);
        J.mem (J.name "required") (J.list [ J.string "name" ]);
      ]
  in
  Alcotest.check json_testable "schema_object" expected schema

let test_tool_text_result () =
  let result = Claude.Tool.text_result "Hello, world!" in
  let expected =
    J.list
      [
        J.object'
          [
            J.mem (J.name "type") (J.string "text");
            J.mem (J.name "text") (J.string "Hello, world!");
          ];
      ]
  in
  Alcotest.check json_testable "text_result" expected result

let test_tool_error_result () =
  let result = Claude.Tool.error_result "Something went wrong" in
  let expected =
    J.list
      [
        J.object'
          [
            J.mem (J.name "type") (J.string "text");
            J.mem (J.name "text") (J.string "Something went wrong");
            J.mem (J.name "is_error") (J.bool true);
          ];
      ]
  in
  Alcotest.check json_testable "error_result" expected result

let test_tool_create_and_call () =
  let greet =
    Claude.Tool.create ~name:"greet" ~description:"Greet a user"
      ~input_schema:
        (Claude.Tool.schema_object
           [ ("name", Claude.Tool.schema_string) ]
           ~required:[ "name" ])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "name" with
        | Some name -> Ok (Claude.Tool.text_result ("Hello, " ^ name ^ "!"))
        | None -> Error "Missing name parameter")
  in
  Alcotest.(check string) "tool name" "greet" (Claude.Tool.name greet);
  Alcotest.(check string)
    "tool description" "Greet a user"
    (Claude.Tool.description greet);

  (* Test successful call *)
  let input_json = J.object' [ J.mem (J.name "name") (J.string "Alice") ] in
  let input = Claude.Tool_input.of_json input_json in
  match Claude.Tool.call greet input with
  | Ok result ->
      let expected = Claude.Tool.text_result "Hello, Alice!" in
      Alcotest.check json_testable "call result" expected result
  | Error msg -> Alcotest.fail msg

let test_tool_call_error () =
  let tool =
    Claude.Tool.create ~name:"fail" ~description:"Always fails"
      ~input_schema:(Claude.Tool.schema_object [] ~required:[])
      ~handler:(fun _ -> Error "Intentional failure")
  in
  let input = Claude.Tool_input.of_json (J.object' []) in
  match Claude.Tool.call tool input with
  | Ok _ -> Alcotest.fail "Expected error"
  | Error msg ->
      Alcotest.(check string) "error message" "Intentional failure" msg

let tool_tests =
  [
    Alcotest.test_case "schema_string" `Quick test_tool_schema_string;
    Alcotest.test_case "schema_int" `Quick test_tool_schema_int;
    Alcotest.test_case "schema_number" `Quick test_tool_schema_number;
    Alcotest.test_case "schema_bool" `Quick test_tool_schema_bool;
    Alcotest.test_case "schema_array" `Quick test_tool_schema_array;
    Alcotest.test_case "schema_string_enum" `Quick test_tool_schema_string_enum;
    Alcotest.test_case "schema_object" `Quick test_tool_schema_object;
    Alcotest.test_case "text_result" `Quick test_tool_text_result;
    Alcotest.test_case "error_result" `Quick test_tool_error_result;
    Alcotest.test_case "create and call" `Quick test_tool_create_and_call;
    Alcotest.test_case "call error" `Quick test_tool_call_error;
  ]

(* ============================================
   Mcp_server Module Tests
   ============================================ *)

let test_mcp_server_create () =
  let tool =
    Claude.Tool.create ~name:"echo" ~description:"Echo input"
      ~input_schema:
        (Claude.Tool.schema_object
           [ ("text", Claude.Tool.schema_string) ]
           ~required:[ "text" ])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "text" with
        | Some text -> Ok (Claude.Tool.text_result text)
        | None -> Error "Missing text")
  in
  let server =
    Claude.Mcp_server.create ~name:"test-server" ~version:"2.0.0"
      ~tools:[ tool ] ()
  in
  Alcotest.(check string)
    "server name" "test-server"
    (Claude.Mcp_server.name server);
  Alcotest.(check string)
    "server version" "2.0.0"
    (Claude.Mcp_server.version server);
  Alcotest.(check int)
    "tools count" 1
    (List.length (Claude.Mcp_server.tools server))

let test_mcp_server_initialize () =
  let server = Claude.Mcp_server.create ~name:"init-test" ~tools:[] () in
  let request =
    J.object'
      [
        J.mem (J.name "jsonrpc") (J.string "2.0");
        J.mem (J.name "id") (J.number 1.0);
        J.mem (J.name "method") (J.string "initialize");
        J.mem (J.name "params") (J.object' []);
      ]
  in
  let response = Claude.Mcp_server.handle_json_message server request in
  (* Check it's a success response with serverInfo *)
  match response with
  | Jsont.Object (mems, _) ->
      let has_result = List.exists (fun ((k, _), _) -> k = "result") mems in
      Alcotest.(check bool) "has result" true has_result
  | _ -> Alcotest.fail "Expected object response"

let test_mcp_server_tools_list () =
  let tool =
    Claude.Tool.create ~name:"my_tool" ~description:"My test tool"
      ~input_schema:(Claude.Tool.schema_object [] ~required:[])
      ~handler:(fun _ -> Ok (Claude.Tool.text_result "ok"))
  in
  let server = Claude.Mcp_server.create ~name:"list-test" ~tools:[ tool ] () in
  let request =
    J.object'
      [
        J.mem (J.name "jsonrpc") (J.string "2.0");
        J.mem (J.name "id") (J.number 2.0);
        J.mem (J.name "method") (J.string "tools/list");
        J.mem (J.name "params") (J.object' []);
      ]
  in
  let response = Claude.Mcp_server.handle_json_message server request in
  match response with
  | Jsont.Object (mems, _) -> (
      match List.find_opt (fun ((k, _), _) -> k = "result") mems with
      | Some (_, Jsont.Object (result_mems, _)) -> (
          match List.find_opt (fun ((k, _), _) -> k = "tools") result_mems with
          | Some (_, Jsont.Array (tools, _)) ->
              Alcotest.(check int) "tools count" 1 (List.length tools)
          | _ -> Alcotest.fail "Missing tools in result")
      | _ -> Alcotest.fail "Missing result in response")
  | _ -> Alcotest.fail "Expected object response"

let test_mcp_server_tools_call () =
  let tool =
    Claude.Tool.create ~name:"uppercase" ~description:"Convert to uppercase"
      ~input_schema:
        (Claude.Tool.schema_object
           [ ("text", Claude.Tool.schema_string) ]
           ~required:[ "text" ])
      ~handler:(fun args ->
        match Claude.Tool_input.get_string args "text" with
        | Some text ->
            Ok (Claude.Tool.text_result (String.uppercase_ascii text))
        | None -> Error "Missing text")
  in
  let server = Claude.Mcp_server.create ~name:"call-test" ~tools:[ tool ] () in
  let request =
    J.object'
      [
        J.mem (J.name "jsonrpc") (J.string "2.0");
        J.mem (J.name "id") (J.number 3.0);
        J.mem (J.name "method") (J.string "tools/call");
        J.mem (J.name "params")
          (J.object'
             [
               J.mem (J.name "name") (J.string "uppercase");
               J.mem (J.name "arguments")
                 (J.object' [ J.mem (J.name "text") (J.string "hello") ]);
             ]);
      ]
  in
  let response = Claude.Mcp_server.handle_json_message server request in
  (* Verify it contains the expected uppercase result *)
  let response_str =
    match Jsont_bytesrw.encode_string' Jsont.json response with
    | Ok s -> s
    | Error _ -> ""
  in
  (* Simple substring check for HELLO in response *)
  let contains_hello =
    let rec check i =
      if i + 5 > String.length response_str then false
      else if String.sub response_str i 5 = "HELLO" then true
      else check (i + 1)
    in
    check 0
  in
  Alcotest.(check bool) "contains HELLO" true contains_hello

let test_mcp_server_tool_not_found () =
  let server = Claude.Mcp_server.create ~name:"notfound-test" ~tools:[] () in
  let request =
    J.object'
      [
        J.mem (J.name "jsonrpc") (J.string "2.0");
        J.mem (J.name "id") (J.number 4.0);
        J.mem (J.name "method") (J.string "tools/call");
        J.mem (J.name "params")
          (J.object' [ J.mem (J.name "name") (J.string "nonexistent") ]);
      ]
  in
  let response = Claude.Mcp_server.handle_json_message server request in
  (* Should return an error response *)
  match response with
  | Jsont.Object (mems, _) ->
      let has_error = List.exists (fun ((k, _), _) -> k = "error") mems in
      Alcotest.(check bool) "has error" true has_error
  | _ -> Alcotest.fail "Expected object response"

let test_mcp_server_method_not_found () =
  let server =
    Claude.Mcp_server.create ~name:"method-notfound-test" ~tools:[] ()
  in
  let request =
    J.object'
      [
        J.mem (J.name "jsonrpc") (J.string "2.0");
        J.mem (J.name "id") (J.number 5.0);
        J.mem (J.name "method") (J.string "unknown/method");
        J.mem (J.name "params") (J.object' []);
      ]
  in
  let response = Claude.Mcp_server.handle_json_message server request in
  match response with
  | Jsont.Object (mems, _) ->
      let has_error = List.exists (fun ((k, _), _) -> k = "error") mems in
      Alcotest.(check bool) "has error" true has_error
  | _ -> Alcotest.fail "Expected object response"

let mcp_server_tests =
  [
    Alcotest.test_case "create server" `Quick test_mcp_server_create;
    Alcotest.test_case "initialize" `Quick test_mcp_server_initialize;
    Alcotest.test_case "tools/list" `Quick test_mcp_server_tools_list;
    Alcotest.test_case "tools/call" `Quick test_mcp_server_tools_call;
    Alcotest.test_case "tool not found" `Quick test_mcp_server_tool_not_found;
    Alcotest.test_case "method not found" `Quick
      test_mcp_server_method_not_found;
  ]

(* ============================================
   Structured Error Tests
   ============================================ *)

let test_error_detail_creation () =
  let error =
    Claude.Proto.Control.Response.error_detail ~code:`Method_not_found
      ~message:"Method not found" ()
  in
  Alcotest.(check int) "error code" (-32601) error.code;
  Alcotest.(check string) "error message" "Method not found" error.message

let test_error_code_conventions () =
  let codes =
    [
      (`Parse_error, -32700);
      (`Invalid_request, -32600);
      (`Method_not_found, -32601);
      (`Invalid_params, -32602);
      (`Internal_error, -32603);
      (`Custom 1, 1);
    ]
  in
  List.iter
    (fun (code, expected_int) ->
      let err =
        Claude.Proto.Control.Response.error_detail ~code ~message:"test" ()
      in
      Alcotest.(check int) "error code value" expected_int err.code)
    codes

let test_error_response_encoding () =
  let error_detail =
    Claude.Proto.Control.Response.error_detail ~code:`Invalid_params
      ~message:"Invalid parameters" ()
  in
  let error_resp =
    Claude.Proto.Control.Response.error ~request_id:"test-123"
      ~error:error_detail ()
  in
  match Jsont.Json.encode Claude.Proto.Control.Response.jsont error_resp with
  | Ok json -> (
      match Jsont.Json.decode Claude.Proto.Control.Response.jsont json with
      | Ok (Claude.Proto.Control.Response.Error decoded) ->
          Alcotest.(check string) "request_id" "test-123" decoded.request_id;
          Alcotest.(check int) "error code" (-32602) decoded.error.code;
          Alcotest.(check string)
            "error message" "Invalid parameters" decoded.error.message
      | Ok _ -> Alcotest.fail "Wrong response type decoded"
      | Error e -> Alcotest.fail e)
  | Error e -> Alcotest.fail e

let structured_error_tests =
  [
    Alcotest.test_case "error detail creation" `Quick test_error_detail_creation;
    Alcotest.test_case "error code conventions" `Quick
      test_error_code_conventions;
    Alcotest.test_case "error response encoding" `Quick
      test_error_response_encoding;
  ]

(* ============================================
   Tool_input Tests
   ============================================ *)

let test_tool_input_get_string () =
  let json = J.object' [ J.mem (J.name "foo") (J.string "bar") ] in
  let input = Claude.Tool_input.of_json json in
  Alcotest.(check (option string))
    "get_string foo" (Some "bar")
    (Claude.Tool_input.get_string input "foo");
  Alcotest.(check (option string))
    "get_string missing" None
    (Claude.Tool_input.get_string input "missing")

let test_tool_input_get_int () =
  let json = J.object' [ J.mem (J.name "count") (J.number 42.0) ] in
  let input = Claude.Tool_input.of_json json in
  Alcotest.(check (option int))
    "get_int count" (Some 42)
    (Claude.Tool_input.get_int input "count")

let test_tool_input_get_float () =
  let json = J.object' [ J.mem (J.name "pi") (J.number 3.14159) ] in
  let input = Claude.Tool_input.of_json json in
  match Claude.Tool_input.get_float input "pi" with
  | Some f ->
      Alcotest.(check bool)
        "get_float pi approx" true
        (abs_float (f -. 3.14159) < 0.0001)
  | None -> Alcotest.fail "Expected float"

let test_tool_input_get_bool () =
  let json =
    J.object'
      [ J.mem (J.name "yes") (J.bool true); J.mem (J.name "no") (J.bool false) ]
  in
  let input = Claude.Tool_input.of_json json in
  Alcotest.(check (option bool))
    "get_bool yes" (Some true)
    (Claude.Tool_input.get_bool input "yes");
  Alcotest.(check (option bool))
    "get_bool no" (Some false)
    (Claude.Tool_input.get_bool input "no")

let test_tool_input_get_string_list () =
  let json =
    J.object'
      [
        J.mem (J.name "items")
          (J.list [ J.string "a"; J.string "b"; J.string "c" ]);
      ]
  in
  let input = Claude.Tool_input.of_json json in
  Alcotest.(check (option (list string)))
    "get_string_list"
    (Some [ "a"; "b"; "c" ])
    (Claude.Tool_input.get_string_list input "items")

let tool_input_tests =
  [
    Alcotest.test_case "get_string" `Quick test_tool_input_get_string;
    Alcotest.test_case "get_int" `Quick test_tool_input_get_int;
    Alcotest.test_case "get_float" `Quick test_tool_input_get_float;
    Alcotest.test_case "get_bool" `Quick test_tool_input_get_bool;
    Alcotest.test_case "get_string_list" `Quick test_tool_input_get_string_list;
  ]

(* ============================================
   Main test runner
   ============================================ *)

let () =
  Alcotest.run "Claude SDK"
    [
      ("Protocol", protocol_tests);
      ("Tool", tool_tests);
      ("Mcp_server", mcp_server_tests);
      ("Structured errors", structured_error_tests);
      ("Tool_input", tool_input_tests);
    ]
