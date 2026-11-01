(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Crowthebot
let check name value = if not value then failwith name
let contains text part =
  let rec loop i = i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1)) in loop 0
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
let field name j = Result.get_ok (Jsont.Json.decode (Jsont.mem name Jsont.json) j)
let string name j = Result.get_ok (Jsont.Json.decode Jsont.string (field name j))
let encoded j = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json j)

let () =
  let logs = Buffer.create 4096 in
  let formatter = Format.formatter_of_buffer logs in
  Logs.set_reporter (Logs.format_reporter ~app:formatter ~dst:formatter ());
  Diagnostics.configure ~verbose:false;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref [] in
  let audit_check = ref (fun () -> ()) in
  let html =
    "<html><head><title>Test &amp; title</title><style>SECRET_STYLE</style></head>"
    ^ "<body><h1>Hello &amp; world</h1><script>SECRET_SCRIPT</script>"
    ^ "<p hidden>SECRET_HIDDEN</p><template>SECRET_TEMPLATE</template>"
    ^ "<pre>  indented\n    code</pre><a href='/next'>Next</a><p>" ^ String.concat "" (List.init 3000 (fun _ -> "café "))
    ^ "</p></body></html>" in
  let client = Fetch_mock.client (fun req ->
      !audit_check ();
      calls := req.Fetch.Middleware.url :: !calls;
      check "website user agent" (Http.Header.get req.headers "user-agent" = Some "crowthebot");
      check "no credentials" (Http.Header.get req.headers "authorization" = None
          && Http.Header.get req.headers "cookie" = None);
      let url = Fetch.Middleware.Url.to_string req.url in
      if String.ends_with ~suffix:"/redirect" url then
        Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list ["Location", "/page"]) "" req
      else if String.ends_with ~suffix:"/downgrade" url then
        Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list ["Location", "http://site.example/page"]) "" req
      else if String.ends_with ~suffix:"/private" url then
        Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list ["Location", "http://127.0.0.1/"]) "" req
      else if String.ends_with ~suffix:"/missing" url then Fetch_mock.respond ~status:404 "" req
      else if String.ends_with ~suffix:"/image" url then
        Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "image/png"]) "png" req
      else if String.ends_with ~suffix:"/huge" url then
        Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "text/plain"])
          (String.make (Website.max_bytes + 1) 'x') req
      else if String.ends_with ~suffix:"/json" url then
        Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "application/json"])
          "{\"answer\":42}" req
      else Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "text/html; charset=utf-8"])
          html req) in
  let website = Website.create ~fetch:client ~clock:(Eio.Stdenv.mono_clock env) in
  let invoke name args = Website.invoke website name args in
  let fetch path = invoke "website_fetch"
      (Printf.sprintf {|{"url":"https://site.example/%s"}|} path) in
  let first_text = Result.get_ok (fetch "redirect#section") in
  check "JSON survives audit clipping" (String.length first_text < 4096);
  let first = json first_text in
  check "title and entities extracted" (string "title" first = "Test & title"
      && contains (string "text" first) "Hello & world");
  check "code whitespace preserved" (contains (string "text" first) "  indented\n    code");
  check "relative link resolved" (contains (string "text" first) "https://site.example/next");
  List.iter (fun secret -> check "hidden material excluded"
      (not (contains (string "text" first) secret)))
    ["SECRET_SCRIPT"; "SECRET_STYLE"; "SECRET_HIDDEN"; "SECRET_TEMPLATE"];
  check "redirect has final URL" (string "url" first = "https://site.example/page");
  check "fragment not sent" (List.length !calls = 2);
  let count = List.length !calls in
  let rec read page acc =
    let acc = acc ^ string "text" page in
    match field "next_offset" page with
    | Jsont.Null _ -> acc
    | offset ->
        let next = invoke "website_read" (Printf.sprintf {|{"id":%S,"offset":%s}|}
            (string "id" page) (encoded offset)) |> Result.get_ok in
        check "each page is bounded JSON" (String.length next < 4096);
        read (json next) acc in
  let text = read first "" in
  check "all Unicode page text retained" (contains text (String.trim (String.concat "" (List.init 3000 (fun _ -> "café ")))));
  check "paging makes no HTTP request" (List.length !calls = count);
  List.iter (fun path -> check "bad website response rejected" (Result.is_error (fetch path)))
    ["missing"; "image"; "huge"; "private"; "downgrade"];
  check "JSON website readable" (string "text" (json (Result.get_ok (fetch "json"))) = "{\"answer\":42}");
  let before = List.length !calls in
  List.iter (fun url -> check "bad URL rejected before network"
      (Result.is_error (invoke "website_fetch" (Printf.sprintf {|{"url":%S}|} url))))
    ["file:///etc/passwd"; "https://user:password@site.example/"; "http://127.1/"; "http://10.1.2.3/"; "http://[::1]/"];
  check "bad URLs never reached fetch" (List.length !calls = before);
  check "unknown arguments rejected" (Result.is_error (invoke "website_fetch" {|{"url":"https://site.example/","headers":{}}|}));
  check "negative offset rejected" (Result.is_error (invoke "website_read" {|{"id":"none","offset":-1}|}));
  check "missing snapshot rejected" (Result.is_error (invoke "website_read" {|{"id":"none","offset":0}|}));
  (* Engine tool dispatch must persist the call before it reaches HTTP. *)
  let admin = "@admin:example.org" and room = "!room:example.org" in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> 0.) db ~admin in
  Store.add_room store room;
  let rounds = ref 0 and offered = ref [] in
  let complete _ tools =
    offered := List.map Agentkit.Agent.Tool.name tools;
    incr rounds;
    if !rounds = 1 then None, [{Agentkit.Agent.id="web-1"; name="website_fetch";
        arguments={|{"url":"https://site.example/page"}|}}]
    else Some "The page says hello.", [] in
  let engine = Engine.with_website
      (Engine.create ~config:(Config.default ~admin ~homeserver:"https://matrix.example.org")
         ~store ~self:"@crow:example.org" ~plugins:[] ~complete:(Fake_model.v complete)
         ~now:(fun () -> 0.)) website in
  audit_check := (fun () ->
      check "durable audit starts before HTTP"
        (List.exists (fun (use : Store.tool_use) -> use.tool = "website_fetch"
            && use.status = "running")
           (Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100)));
  Engine.handle engine ~direct:true ~send:(fun _ -> ())
    Engine.{room; sender=admin; id="$website"; body="Analyse https://site.example/page"};
  check "website tool offered" (List.mem "website_fetch" !offered);
  let uses = Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100 in
  check "website tool audited" (List.exists (fun (use : Store.tool_use) ->
      use.tool = "website_fetch" && use.status = "ok"
      && contains use.arguments "site.example/page" && use.event = "$website") uses);
  let before = List.length !calls in
  Store.set_person store ~actor:admin ~user:"@bot:example.org" ~role:Store.Bot ~allowed:true;
  rounds := 0;
  Engine.handle engine ~direct:true ~send:(fun _ -> ())
    Engine.{room; sender="@bot:example.org"; id="$bot"; body="!crow ask read this website"};
  check "bots are not offered website tools" (not (List.mem "website_fetch" !offered));
  check "bots cannot force a website fetch" (List.length !calls = before);
  Format.pp_print_flush formatter ();
  let log = Buffer.contents logs in
  check "HTTP request and status logged" (contains log "Website request method=GET"
      && contains log "Website response" && contains log "status=200");
  check "tool start and completion logged" (contains log "Tool started" && contains log "Tool finished");
  check "page bodies not written to console logs" (not (contains log "SECRET_SCRIPT"));
  print_endline "Crow website HTTP, extraction, paging and audited dispatch passed."
