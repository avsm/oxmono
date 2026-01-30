(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio

let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->

  (* Example 1: Basic GET request *)
  Printf.printf "\n=== Example 1: Basic GET Request ===\n%!";
  let req = Requests.create ~sw env in
  let resp1 = Requests.get req "https://httpbin.org/get" in
  Printf.printf "Status: %d\n%!" (Requests.Response.status_code resp1);
  let body1 = Requests.Response.body resp1 |> Buf_read.of_flow ~max_size:max_int |> Buf_read.take_all in
  Printf.printf "Body length: %d bytes\n%!" (String.length body1);

  (* Example 2: POST with JSON body *)
  Printf.printf "\n=== Example 2: POST with JSON ===\n%!";
  let json_data = Jsont.Object ([
    ("name", Jsont.String "Alice");
    ("email", Jsont.String "alice@example.com");
    ("age", Jsont.Number 30.0)
  ], Jsont.Meta.none) in
  let resp2 = Requests.post req
    ~body:(Requests.Body.json json_data)
    "https://httpbin.org/post" in
  Printf.printf "POST status: %d\n%!" (Requests.Response.status_code resp2);

  (* Example 3: Custom headers and authentication *)
  Printf.printf "\n=== Example 3: Custom Headers and Auth ===\n%!";
  let headers = Requests.Headers.empty
    |> Requests.Headers.set "X-Custom-Header" "MyValue"
    |> Requests.Headers.user_agent "OCaml-Requests-Example/1.0" in
  let resp3 = Requests.get req
    ~headers
    ~auth:(Requests.Auth.bearer ~token:"demo-token-123")
    "https://httpbin.org/bearer" in
  Printf.printf "Auth status: %d\n%!" (Requests.Response.status_code resp3);

  (* Example 4: Session with default headers *)
  Printf.printf "\n=== Example 4: Session with Default Headers ===\n%!";
  let req2 = Requests.create ~sw env in
  let req2 = Requests.set_default_header req2 "User-Agent" "OCaml-Requests/1.0" in
  let req2 = Requests.set_default_header req2 "Accept" "application/json" in

  (* All requests with req2 will include these headers *)
  let resp4 = Requests.get req2 "https://httpbin.org/headers" in
  Printf.printf "Headers response status: %d\n%!" (Requests.Response.status_code resp4);

  (* Example 5: Query parameters *)
  Printf.printf "\n=== Example 5: Query Parameters ===\n%!";
  let resp5 = Requests.get req
    ~params:[("key1", "value1"); ("key2", "value2")]
    "https://httpbin.org/get" in
  Printf.printf "Query params status: %d\n%!" (Requests.Response.status_code resp5);

  (* Example 6: Form data submission *)
  Printf.printf "\n=== Example 6: Form Data ===\n%!";
  let form_body = Requests.Body.form [
    ("username", "demo");
    ("password", "secret123")
  ] in
  let resp6 = Requests.post req
    ~body:form_body
    "https://httpbin.org/post" in
  Printf.printf "Form POST status: %d\n%!" (Requests.Response.status_code resp6);

  (* Example 7: Retry configuration *)
  Printf.printf "\n=== Example 7: Retry Configuration ===\n%!";
  let retry_config = Requests.Retry.create_config
    ~max_retries:3
    ~backoff_factor:0.5
    () in
  let req_with_retry = Requests.create ~sw ~retry:retry_config env in
  let req_with_retry = Requests.set_timeout req_with_retry
    (Requests.Timeout.create ~total:10.0 ()) in

  (* This will retry on 5xx errors *)
  (try
    let resp7 = Requests.get req_with_retry "https://httpbin.org/status/200" in
    Printf.printf "Retry test status: %d\n%!" (Requests.Response.status_code resp7)
  with _ ->
    Printf.printf "Request failed even after retries\n%!");

  (* Example 8: Concurrent requests using Fiber.both *)
  Printf.printf "\n=== Example 8: Concurrent Requests ===\n%!";
  let start_time = Unix.gettimeofday () in

  let (r1, r2) = Fiber.both
    (fun () -> Requests.get req "https://httpbin.org/delay/1")
    (fun () -> Requests.get req "https://httpbin.org/delay/1") in

  let elapsed = Unix.gettimeofday () -. start_time in
  Printf.printf "Two 1-second delays completed in %.2f seconds (concurrent)\n%!" elapsed;
  Printf.printf "Response 1 status: %d\n%!" (Requests.Response.status_code r1);
  Printf.printf "Response 2 status: %d\n%!" (Requests.Response.status_code r2);

  (* Example 9: One-shot stateless request *)
  Printf.printf "\n=== Example 9: One-Shot Stateless Request ===\n%!";
  let resp9 = Requests.One.get
    ~sw
    ~clock:env#clock
    ~net:env#net
    "https://httpbin.org/get" in
  Printf.printf "One-shot status: %d\n%!" (Requests.Response.status_code resp9);

  (* Example 10: Error handling *)
  Printf.printf "\n=== Example 10: Error Handling ===\n%!";
  (try
    let resp = Requests.get req "https://httpbin.org/status/404" in
    (* By default, 4xx/5xx status codes don't raise exceptions *)
    if Requests.Response.ok resp then
      Printf.printf "Success\n%!"
    else
      Printf.printf "Got %d response (no exception by default)\n%!"
        (Requests.Response.status_code resp);
    (* Use raise_for_status to get exception behavior *)
    let _resp = Requests.Response.raise_for_status resp in
    ()
  with
  | Eio.Io (Requests.Error.E (Requests.Error.Http_error_status _), _) ->
      Printf.printf "HTTP error status raised via raise_for_status\n%!"
  | exn ->
      Printf.printf "Other error: %s\n%!" (Printexc.to_string exn));

  (* Example 11: Timeouts *)
  Printf.printf "\n=== Example 11: Timeouts ===\n%!";
  let req_timeout = Requests.create ~sw env in
  let req_timeout = Requests.set_timeout req_timeout
    (Requests.Timeout.create ~total:5.0 ()) in

  (try
    let resp11 = Requests.get req_timeout "https://httpbin.org/delay/2" in
    Printf.printf "Timeout test completed: %d\n%!" (Requests.Response.status_code resp11)
  with
  | Eio.Time.Timeout ->
      Printf.printf "Request correctly timed out\n%!"
  | exn ->
      Printf.printf "Other timeout error: %s\n%!" (Printexc.to_string exn));

  (* Example 12: Multiple concurrent requests with Fiber.all *)
  Printf.printf "\n=== Example 12: Multiple Concurrent Requests ===\n%!";
  let urls = [
    "https://httpbin.org/delay/1";
    "https://httpbin.org/get";
    "https://httpbin.org/headers";
  ] in

  let start_time = Unix.gettimeofday () in
  let responses = ref [] in

  Fiber.all (List.map (fun url ->
    fun () ->
      let resp = Requests.get req url in
      responses := resp :: !responses
  ) urls);

  let elapsed = Unix.gettimeofday () -. start_time in
  Printf.printf "Three requests completed in %.2f seconds (concurrent)\n%!" elapsed;
  List.iter (fun r ->
    Printf.printf "  Status: %d\n%!" (Requests.Response.status_code r)
  ) !responses;

  Printf.printf "\n=== All examples completed successfully! ===\n%!"
