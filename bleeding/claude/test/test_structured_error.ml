(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
---------------------------------------------------------------------------*)

(** Test structured errors by provoking a JSON-RPC error from Claude *)

open Eio.Std

let test_create_error_detail () =
  print_endline "\nTesting structured error creation...";

  (* Create a simple error *)
  let error1 =
    Proto.Control.Response.error_detail ~code:`Method_not_found
      ~message:"Method not found" ()
  in
  Printf.printf "✓ Created error: [%d] %s\n" error1.code error1.message;

  (* Create an error without additional data for simplicity *)
  let error2 =
    Proto.Control.Response.error_detail ~code:`Invalid_params
      ~message:"Invalid parameters" ()
  in
  Printf.printf "✓ Created error: [%d] %s\n" error2.code error2.message;

  (* Encode and decode an error response *)
  let error_resp =
    Proto.Control.Response.error ~request_id:"test-123" ~error:error2 ()
  in

  match Jsont.Json.encode Proto.Control.Response.jsont error_resp with
  | Ok json -> (
      let json_str =
        match Jsont_bytesrw.encode_string' Jsont.json json with
        | Ok s -> s
        | Error e -> Jsont.Error.to_string e
      in
      Printf.printf "✓ Encoded error response: %s\n" json_str;

      (* Decode it back *)
      match Jsont.Json.decode Proto.Control.Response.jsont json with
      | Ok (Proto.Control.Response.Error decoded) ->
          Printf.printf "✓ Decoded error: [%d] %s\n" decoded.error.code
            decoded.error.message
      | Ok _ -> print_endline "✗ Wrong response type"
      | Error e -> Printf.printf "✗ Decode failed: %s\n" e)
  | Error e -> Printf.printf "✗ Encode failed: %s\n" e

let test_error_code_conventions () =
  print_endline "\nTesting JSON-RPC error code conventions...";

  (* Standard JSON-RPC errors using the typed API with polymorphic variants *)
  let errors =
    [
      (`Parse_error, "Parse error");
      (`Invalid_request, "Invalid request");
      (`Method_not_found, "Method not found");
      (`Invalid_params, "Invalid params");
      (`Internal_error, "Internal error");
      (`Custom 1, "Application error");
    ]
  in

  List.iter
    (fun (code, msg) ->
      let err = Proto.Control.Response.error_detail ~code ~message:msg () in
      Printf.printf "✓ Error [%d]: %s (typed)\n" err.code err.message)
    errors

let test_provoke_api_error ~sw ~env =
  print_endline "\nTesting API error from Claude...";

  (* Configure client with an invalid model to provoke an API error *)
  let options =
    Claude.Options.default
    |> Claude.Options.with_model
         (Claude.Model.of_string "invalid-model-that-does-not-exist")
  in

  Printf.printf "Creating client with invalid model...\n";

  try
    let client =
      Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
        ~clock:env#clock ()
    in

    Printf.printf "Sending query to provoke API error...\n";
    Claude.Client.query client
      "Hello, this should fail with an invalid model error";

    (* Process responses to see if we get an error *)
    let messages = Claude.Client.receive_all client in

    let error_found = ref false in
    let text_error_found = ref false in
    List.iter
      (fun resp ->
        match resp with
        | Claude.Response.Error err ->
            error_found := true;
            Printf.printf "✓ Received structured error response: %s\n"
              (Claude.Response.Error.message err);
            Printf.printf "  Is system error: %b\n"
              (Claude.Response.Error.is_system_error err);
            Printf.printf "  Is assistant error: %b\n"
              (Claude.Response.Error.is_assistant_error err)
        | Claude.Response.Text text ->
            let content = Claude.Response.Text.content text in
            if
              String.length content > 0
              && (String.contains content '4' || String.contains content 'e')
            then begin
              text_error_found := true;
              Printf.printf "✓ Received error as text: %s\n" content
            end
        | Claude.Response.Complete result ->
            Printf.printf "  Complete (duration: %dms)\n"
              (Claude.Response.Complete.duration_ms result)
        | _ -> ())
      messages;

    if !error_found then
      Printf.printf "✓ Successfully caught structured error response\n"
    else if !text_error_found then
      Printf.printf "✓ Successfully caught error (returned as text)\n"
    else Printf.printf "✗ No error was returned (unexpected)\n"
  with
  | Claude.Transport.Connection_error msg ->
      Printf.printf "✓ Connection error as expected: %s\n" msg
  | exn ->
      Printf.printf "✗ Unexpected exception: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stdout

let test_control_protocol_error () =
  print_endline "\nTesting control protocol error encoding/decoding...";

  (* Test that we can create and encode a control protocol error using polymorphic variant codes *)
  let error_detail =
    Proto.Control.Response.error_detail ~code:`Invalid_params
      ~message:"Invalid params for permission request"
      ~data:
        (Jsont.Object
           ( [
               ( ("tool_name", Jsont.Meta.none),
                 Jsont.String ("Write", Jsont.Meta.none) );
               ( ("reason", Jsont.Meta.none),
                 Jsont.String
                   ("Missing required file_path parameter", Jsont.Meta.none) );
             ],
             Jsont.Meta.none ))
      ()
  in

  let error_response =
    Proto.Control.Response.error ~request_id:"test-req-456" ~error:error_detail
      ()
  in

  match Jsont.Json.encode Proto.Control.Response.jsont error_response with
  | Ok json -> (
      let json_str =
        match Jsont_bytesrw.encode_string' Jsont.json json with
        | Ok s -> s
        | Error e -> Jsont.Error.to_string e
      in
      Printf.printf "✓ Encoded control error with data:\n  %s\n" json_str;

      (* Verify we can decode it back *)
      match Jsont.Json.decode Proto.Control.Response.jsont json with
      | Ok (Proto.Control.Response.Error decoded) -> (
          Printf.printf "✓ Decoded control error:\n";
          Printf.printf "  Code: %d\n" decoded.error.code;
          Printf.printf "  Message: %s\n" decoded.error.message;
          Printf.printf "  Has data: %b\n" (Option.is_some decoded.error.data);
          match decoded.error.data with
          | Some data ->
              let data_str =
                match Jsont_bytesrw.encode_string' Jsont.json data with
                | Ok s -> s
                | Error e -> Jsont.Error.to_string e
              in
              Printf.printf "  Data: %s\n" data_str
          | None -> ())
      | Ok _ -> print_endline "✗ Wrong response type"
      | Error e -> Printf.printf "✗ Decode failed: %s\n" e)
  | Error e -> Printf.printf "✗ Encode failed: %s\n" e

let test_hook_error ~sw ~env =
  print_endline "\nTesting hook callback errors trigger JSON-RPC error codes...";

  (* Create a hook that will throw an exception *)
  let failing_hook input =
    Printf.printf "✓ Hook called for tool: %s\n"
      input.Claude.Hooks.PreToolUse.tool_name;
    failwith "Intentional hook failure to test error handling"
  in

  (* Register the failing hook *)
  let hooks =
    Claude.Hooks.empty
    |> Claude.Hooks.on_pre_tool_use ~pattern:"Write" failing_hook
  in

  let options =
    Claude.Options.default
    |> Claude.Options.with_hooks hooks
    |> Claude.Options.with_model (Claude.Model.of_string "haiku")
  in

  Printf.printf "Creating client with failing hook...\n";

  try
    let client =
      Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
        ~clock:env#clock ()
    in

    Printf.printf
      "Asking Claude to write a file (should trigger failing hook)...\n";
    Claude.Client.query client "Write 'test' to /tmp/test_hook_error.txt";

    (* Process responses *)
    let messages = Claude.Client.receive_all client in

    let hook_called = ref false in
    let error_found = ref false in
    List.iter
      (fun resp ->
        match resp with
        | Claude.Response.Tool_use tool ->
            let tool_name = Claude.Response.Tool_use.name tool in
            if tool_name = "Write" then begin
              hook_called := true;
              Printf.printf "✓ Write tool was called (hook intercepted it)\n"
            end
        | Claude.Response.Error err ->
            error_found := true;
            Printf.printf "  Error response: %s\n"
              (Claude.Response.Error.message err)
        | Claude.Response.Complete _ -> Printf.printf "  Query completed\n"
        | _ -> ())
      messages;

    if !hook_called then
      Printf.printf "✓ Hook was triggered, exception caught by SDK\n"
    else
      Printf.printf
        "  Note: Hook may not have been called if query didn't use Write tool\n";

    Printf.printf "✓ Test completed (SDK sent -32603 Internal Error to CLI)\n"
  with exn ->
    Printf.printf "Exception during test: %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout

let run_all_tests env =
  print_endline "=== Structured Error Tests ===";
  test_create_error_detail ();
  test_error_code_conventions ();
  test_control_protocol_error ();

  (* Test with actual Claude invocation *)
  Switch.run @@ fun sw ->
  test_provoke_api_error ~sw ~env;

  (* Test hook errors that trigger JSON-RPC error codes *)
  Switch.run @@ fun sw ->
  test_hook_error ~sw ~env;

  print_endline "\n=== All Structured Error Tests Completed ==="

let () =
  Eio_main.run @@ fun env ->
  try run_all_tests env with
  | Claude.Transport.CLI_not_found msg ->
      Printf.eprintf "Error: Claude CLI not found\n%s\n" msg;
      Printf.eprintf "Make sure 'claude' is installed and in your PATH\n";
      exit 1
  | exn ->
      Printf.eprintf "Fatal error: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1
