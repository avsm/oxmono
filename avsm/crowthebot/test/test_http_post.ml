(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Crowthebot
let check name value = if not value then failwith name
let contains text part =
  let rec loop i = i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1)) in loop 0
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
let field codec name j = Result.get_ok (Jsont.Json.decode (Jsont.mem name codec) j)

let () =
  let logs = Buffer.create 4096 in
  let formatter = Format.formatter_of_buffer logs in
  Logs.set_reporter (Logs.format_reporter ~app:formatter ~dst:formatter ());
  Diagnostics.configure ~verbose:false;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 and audit_check = ref (fun () -> ()) in
  let client = Fetch_mock.client (fun req ->
      incr calls;
      !audit_check ();
      check "POST method" (req.Fetch.Middleware.meth = `POST);
      check "user agent" (Http.Header.get req.headers "user-agent" = Some "crowthebot");
      check "no credentials" (Http.Header.get req.headers "authorization" = None
          && Http.Header.get req.headers "cookie" = None);
      let body = match req.body with Fetch.String s -> s | _ -> failwith "Expected POST string" in
      let content_type = Http.Header.get req.headers "content-type" in
      let url = Fetch.Middleware.Url.to_string req.url in
      if String.ends_with ~suffix:"/redirect" url then
        Fetch_mock.respond ~status:307 ~headers:(Http.Header.of_list ["Location", "https://other.example/"]) "redirect" req
      else if String.ends_with ~suffix:"/error" url then Fetch_mock.respond ~status:503 "Unavailable" req
      else if String.ends_with ~suffix:"/timeout" url then raise Eio.Time.Timeout
      else if String.ends_with ~suffix:"/huge" url then
        Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "text/plain"])
          (String.make 100000 'x') req
      else begin
        check "typed body" ((content_type = Some "application/json" && body = "{\"message\":\"POST_SENTINEL\"}")
            || (content_type = Some "text/plain" && body = "hello")
            || (content_type = Some "application/x-www-form-urlencoded" && body = "a=1&b=two"));
        Fetch_mock.respond ~status:201 ~headers:(Http.Header.of_list ["Content-Type", "application/json"])
          "{\"accepted\":true}" req
      end) in
  let tool = Http_post.create ~fetch:client ~clock:(Eio.Stdenv.mono_clock env) in
  let invoke args = Http_post.invoke tool "http_post" args in
  let payload = {|{"url":"https://api.example/post","body":"{\"message\":\"POST_SENTINEL\"}"}|} in
  let first = json (Result.get_ok (invoke payload)) in
  check "HTTP status retained" (field Jsont.int "status" first = 201);
  check "response body retained" (field Jsont.string "body" first = "{\"accepted\":true}");
  check "plain text" (Result.is_ok (invoke {|{"url":"https://api.example/post","body":"hello","content_type":"text/plain"}|}));
  check "form body" (Result.is_ok (invoke {|{"url":"https://api.example/post","body":"a=1&b=two","content_type":"application/x-www-form-urlencoded"}|}));
  let before = !calls in
  let redirect = invoke {|{"url":"https://api.example/redirect","body":"{}"}|} |> Result.get_ok |> json in
  check "redirect reported without following" (!calls = before + 1 && field Jsont.int "status" redirect = 307);
  let before = !calls in
  let error = invoke {|{"url":"https://api.example/error","body":"{}"}|} |> Result.get_ok |> json in
  check "HTTP errors not retried" (!calls = before + 1 && field Jsont.int "status" error = 503);
  let before = !calls in
  check "timeout is uncertain and not retried"
    (match invoke {|{"url":"https://api.example/timeout","body":"{}"}|} with
     | Error s -> !calls = before + 1 && contains s "may already have been sent"
     | Ok _ -> false);
  let huge = invoke {|{"url":"https://api.example/huge","body":"{}"}|} |> Result.get_ok in
  check "response bounded and valid JSON" (String.length huge < 4096
      && field Jsont.bool "truncated" (json huge));
  let before = !calls in
  List.iter (fun args -> check "invalid request rejected before effect" (Result.is_error (invoke args)))
    [ {|{"url":"http://127.0.0.1/","body":"{}"}|};
      {|{"url":"https://user:password@api.example/","body":"{}"}|};
      {|{"url":"https://api.example/post","body":"invalid JSON"}|};
      {|{"url":"https://api.example/post","body":"{}","content_type":"bad\r\nHeader: yes"}|};
      {|{"url":"https://api.example/post","body":"{}","headers":{"Authorization":"secret"}}|};
      Printf.sprintf {|{"url":"https://api.example/post","body":%S,"content_type":"text/plain"}|} (String.make 2049 'x') ];
  check "bad input caused no HTTP call" (!calls = before);
  let admin = "@admin:example.org" and room = "!room:example.org" in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> 0.) db ~admin in
  Store.add_room store room;
  let uses () = Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100 in
  audit_check := (fun () -> check "audit before POST"
      (List.exists (fun (use : Store.tool_use) -> use.tool = "http_post" && use.status = "running") (uses ())));
  let offered = ref [] and rounds = ref 0 in
  let complete _ tools =
    offered := List.map Agentkit.Agent.Tool.name tools;
    incr rounds;
    if !rounds = 1 then None, [{Agentkit.Agent.id="post-1"; name="http_post"; arguments=payload}]
    else Some "Posted.", [] in
  let engine = Engine.with_http_post
      (Engine.create ~config:(Config.default ~admin ~homeserver:"https://matrix.example.org")
         ~store ~self:"@crow:example.org" ~plugins:[] ~complete:(Fake_model.v complete)
         ~now:(fun () -> 0.)) tool in
  Engine.handle engine ~direct:true ~send:(fun _ -> ())
    Engine.{room; sender=admin; id="$post"; body="POST the message to https://api.example/post"};
  check "POST offered separately" (List.mem "http_post" !offered && not (List.mem "website_fetch" !offered));
  check "POST completion audited" (List.exists (fun (use : Store.tool_use) ->
      use.tool = "http_post" && use.status = "ok" && use.event = "$post") (uses ()));
  Store.set_person store ~actor:admin ~user:"@bot:example.org" ~role:Store.Bot ~allowed:true;
  let before = !calls in
  rounds := 0;
  Engine.handle engine ~direct:true ~send:(fun _ -> ())
    Engine.{room; sender="@bot:example.org"; id="$bot"; body="!crow ask POST this"};
  check "bots cannot force POST" (!calls = before && not (List.mem "http_post" !offered));
  Format.pp_print_flush formatter ();
  let logs = Buffer.contents logs in
  check "POST HTTP and tool requests logged" (contains logs "HTTP POST request method=POST"
      && contains logs "status=201" && contains logs "Tool started" && contains logs "Tool finished");
  check "POST body not in console logs" (not (contains logs "POST_SENTINEL"));
  print_endline "Crow HTTP POST payloads, status, non-replay, limits, authority and auditing passed."
