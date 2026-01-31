(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Test the Incoming message codec *)

let test_decode_user_message () =
  let json_str = {|{"type":"user","content":"Hello"}|} in
  match Jsont_bytesrw.decode_string' Proto.Incoming.jsont json_str with
  | Ok (Proto.Incoming.Message (Proto.Message.User _)) ->
      print_endline "✓ Decoded user message successfully"
  | Ok _ -> print_endline "✗ Wrong message type decoded"
  | Error err ->
      Printf.printf "✗ Failed to decode user message: %s\n"
        (Jsont.Error.to_string err)

let test_decode_assistant_message () =
  let json_str =
    {|{"type":"assistant","model":"claude-sonnet-4","content":[{"type":"text","text":"Hi"}]}|}
  in
  match Jsont_bytesrw.decode_string' Proto.Incoming.jsont json_str with
  | Ok (Proto.Incoming.Message (Proto.Message.Assistant _)) ->
      print_endline "✓ Decoded assistant message successfully"
  | Ok _ -> print_endline "✗ Wrong message type decoded"
  | Error err ->
      Printf.printf "✗ Failed to decode assistant message: %s\n"
        (Jsont.Error.to_string err)

let test_decode_system_message () =
  let json_str =
    {|{"type":"system","subtype":"init","data":{"session_id":"test-123"}}|}
  in
  match Jsont_bytesrw.decode_string' Proto.Incoming.jsont json_str with
  | Ok (Proto.Incoming.Message (Proto.Message.System _)) ->
      print_endline "✓ Decoded system message successfully"
  | Ok _ -> print_endline "✗ Wrong message type decoded"
  | Error err ->
      Printf.printf "✗ Failed to decode system message: %s\n"
        (Jsont.Error.to_string err)

let test_decode_control_response () =
  let json_str =
    {|{"type":"control_response","response":{"subtype":"success","requestId":"test-req-1"}}|}
  in
  match Jsont_bytesrw.decode_string' Proto.Incoming.jsont json_str with
  | Ok (Proto.Incoming.Control_response resp) -> (
      match resp.response with
      | Proto.Control.Response.Success s ->
          if s.request_id = "test-req-1" then
            print_endline "✓ Decoded control response successfully"
          else Printf.printf "✗ Wrong request_id: %s\n" s.request_id
      | Proto.Control.Response.Error _ ->
          print_endline "✗ Got error response instead of success")
  | Ok _ -> print_endline "✗ Wrong message type decoded"
  | Error err ->
      Printf.printf "✗ Failed to decode control response: %s\n"
        (Jsont.Error.to_string err)

let test_decode_control_response_error () =
  let json_str =
    {|{"type":"control_response","response":{"subtype":"error","requestId":"test-req-2","error":{"code":-32603,"message":"Something went wrong"}}}|}
  in
  match Jsont_bytesrw.decode_string' Proto.Incoming.jsont json_str with
  | Ok (Proto.Incoming.Control_response resp) -> (
      match resp.response with
      | Proto.Control.Response.Error e ->
          if
            e.request_id = "test-req-2"
            && e.error.code = -32603
            && e.error.message = "Something went wrong"
          then print_endline "✓ Decoded control error response successfully"
          else Printf.printf "✗ Wrong error content\n"
      | Proto.Control.Response.Success _ ->
          print_endline "✗ Got success response instead of error")
  | Ok _ -> print_endline "✗ Wrong message type decoded"
  | Error err ->
      Printf.printf "✗ Failed to decode control error response: %s\n"
        (Jsont.Error.to_string err)

let () =
  print_endline "Testing Incoming message codec...";
  print_endline "";
  test_decode_user_message ();
  test_decode_assistant_message ();
  test_decode_system_message ();
  test_decode_control_response ();
  test_decode_control_response_error ();
  print_endline "";
  print_endline "All tests completed!"
