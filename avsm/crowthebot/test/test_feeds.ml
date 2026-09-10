open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let room = "!room:example.org"

let failed f =
  try
    ignore (f ());
    false
  with
  | Eio.Io _ | Invalid_argument _ | Failure _
  | Eio.Buf_read.Buffer_limit_exceeded
  ->
    true

let contains text needle =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let rss n =
  let items =
    List.init n (fun i ->
        Printf.sprintf
          "<item><guid isPermaLink='false'>post-%d</guid><title>Post \
           %d</title><link>https://articles.example/%d</link><description>Article \
           %d summary.</description></item>"
          i i i i)
  in
  "<rss version='2.0'><channel><title>Test \
   RSS</title><link>https://articles.example/</link><description>Testing</description>"
  ^ String.concat "" items ^ "</channel></rss>"

let atom =
  {|<feed xmlns="http://www.w3.org/2005/Atom"><id>urn:feed:test</id><title>Atom test</title><updated>2026-09-09T00:00:00Z</updated><author><name>Alice</name></author><entry><id>urn:entry:1</id><title>Atom post</title><updated>2026-09-09T00:00:00Z</updated><link href="/post"/><summary>Atom summary</summary></entry></feed>|}

let opml urls =
  "<opml version='2.0'><head><title>Friends</title></head><body>"
  ^ String.concat ""
      (List.map (fun url -> "<outline text='A' xmlUrl='" ^ url ^ "'/>") urls)
  ^ "</body></opml>"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let parse body = Feed_parse.decode ~url:"https://feeds.example/atom" body in
  check "RSS parsed from bytes"
    (match parse (rss 2) with
    | Ok (Feed (_, _, entries)) -> List.length entries = 2
    | _ -> false);
  check "Atom IDs and relative links"
    (match parse atom with
    | Ok (Feed ("atom", _, [ entry ])) ->
        entry.url = Some "https://feeds.example/post" && entry.published <> None
    | _ -> false);
  check "RSS missing GUIDs stay distinct"
    (match
       parse
         "<rss \
          version='2.0'><channel><title>A</title><link>https://a.example/</link><description>A</description><item><title>One</title></item><item><title>Two</title></item></channel></rss>"
     with
    | Ok (Feed (_, _, [ a; b ])) -> a.key <> b.key
    | _ -> false);
  List.iter
    (fun body -> check "unsafe XML rejected" (Result.is_error (parse body)))
    [
      "<!DOCTYPE rss [<!ENTITY x 'bad'>]><rss>&x;</rss>";
      "<html><body>not a feed</body></html>";
      String.concat "" (List.init 70 (fun _ -> "<x>"))
      ^ String.concat "" (List.init 70 (fun _ -> "</x>"));
    ];
  List.iter
    (fun url ->
      check "unsafe URL rejected" (failed (fun () -> Feed_http.normalize url)))
    [
      "file:///etc/passwd";
      "https://secret@feeds.example/rss";
      "http://127.1/feed";
      "http://2130706433/feed";
      "http://[::1]/rss";
      "https://feeds.example/rss#fragment";
    ];
  List.iter
    (fun ip ->
      check "reserved and private addresses rejected"
        (not (Feed_http.public_address (Ipaddr.of_string_exn ip))))
    [
      "100.64.0.1";
      "0.1.2.3";
      "169.254.169.254";
      "fc00::1";
      "fd12::1";
      "fe80::1";
      "::ffff:127.0.0.1";
      "64:ff9b::7f00:1";
      "2002:7f00:1::1";
    ];
  check "URL canonicalization"
    (Feed_http.normalize "HTTPS://FEEDS.EXAMPLE:443/a/../rss"
    = "https://feeds.example/rss");
  let net = Eio_mock.Net.make "feed DNS" in
  Eio_mock.Net.on_getaddrinfo net
    [ `Return [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 80) ] ];
  let connected = ref false in
  Eio_mock.Net.on_connect net
    [
      `Run
        (fun () ->
          connected := true;
          failwith "must not connect");
    ];
  check "resolved loopback denied before socket creation"
    (failed (fun () ->
         Feed_http.public_connect net ~sw ~host:"feeds.example" ~port:80)
    && not !connected);
  let clock = Eio.Stdenv.mono_clock env in
  let hops = ref 0 in
  let redirect_client =
    Fetch_mock.client (fun req ->
        incr hops;
        Fetch_mock.respond ~status:302
          ~headers:
            (Http.Header.of_list [ ("Location", "https://127.0.0.1/private") ])
          "" req)
  in
  check "redirect cannot reach private address"
    (failed (fun () ->
         Feed_http.create ~fetch:redirect_client ~clock
           ~url:"https://feeds.example/rss" ~etag:None ~last_modified:None)
    && !hops = 1);
  let redirect_client =
    Fetch_mock.client (fun req ->
        if Fetch.Middleware.Url.path_and_query req.url = "/start" then
          Fetch_mock.respond ~status:302
            ~headers:(Http.Header.of_list [ ("Location", "/feed") ])
            "" req
        else Fetch_mock.respond atom req)
  in
  check "public relative redirect retains final base URL"
    (match
       Feed_http.create ~fetch:redirect_client ~clock
         ~url:"https://feeds.example/start" ~etag:None ~last_modified:None
     with
    | Document { url; _ } -> url = "https://feeds.example/feed"
    | _ -> false);
  let flood =
    Fetch_mock.client
      (Fetch_mock.respond (String.make (Feed_http.max_bytes + 2) 'x'))
  in
  check "HTTP body bounded"
    (failed (fun () ->
         Feed_http.create ~fetch:flood ~clock ~url:"https://feeds.example/rss"
           ~etag:None ~last_modified:None));
  let time = ref 0.
  and version = ref 2
  and reads = ref 0
  and force_error = ref false in
  let outlines =
    ref [ "https://feeds.example/rss"; "https://feeds.example/atom" ]
  in
  let opml_version = ref 1 in
  let fetch =
    Fetch_mock.client (fun req ->
        incr reads;
        check "feed client is GET-only" (req.meth = `GET);
        check "feed client has no bearer credentials"
          (Http.Header.get req.headers "authorization" = None);
        let path = Fetch.Middleware.Url.path_and_query req.url in
        let tag, body =
          match path with
          | "/rss" -> (string_of_int !version, rss !version)
          | "/atom" -> ("atom", atom)
          | "/list" -> (string_of_int !opml_version, opml !outlines)
          | _ -> failwith "unexpected feed URL"
        in
        if !force_error && path = "/rss" then
          Fetch_mock.respond ~status:503 "unavailable" req
        else if Http.Header.get req.headers "if-none-match" = Some tag then
          Fetch_mock.respond ~status:304 "" req
        else
          Fetch_mock.respond
            ~headers:
              (Http.Header.of_list
                 [
                   ("ETag", tag);
                   ("Last-Modified", "Wed, 09 Sep 2026 00:00:00 GMT");
                 ])
            body req)
  in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> !time) db ~admin in
  Store.add_room store room;
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:true;
  Store.set_person store ~actor:admin ~user:"@bot:example.org" ~role:Bot
    ~allowed:true;
  let state = Store.feeds store in
  let feeds = Feeds.create ~state ~download:(Feed_http.create ~fetch ~clock) in
  let access actor = Feeds.for_request feeds ~actor ~room ~event:"$subscribe" in
  check "bot denied before HTTP"
    (Result.is_error
       (Feeds.invoke
          (access "@bot:example.org")
          "feeds_add" {|{"url":"https://feeds.example/rss"}|})
    && !reads = 0);
  let config = Config.default ~admin ~homeserver:"https://matrix.example.org" in
  let models = ref 0 and sent = ref [] and prompts = ref [] in
  let client =
    Openrouter.of_fetch ~base_url:"https://model.example/v1"
      (Fetch_mock.client (fun req ->
           incr models;
           (match req.body with
           | Fetch.String body -> prompts := body :: !prompts
           | _ -> failwith "expected JSON model request");
           Fetch_mock.respond
             ~headers:
               (Http.Header.of_list [ ("Content-Type", "application/json") ])
             {|{"id":"1","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"New post: https://articles.example/new"}}]}|}
             req))
  in
  let complete = App.complete env config client in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[] ~complete
      ~now:(fun () -> !time)
    |> fun e -> Engine.with_feeds e feeds
  in
  let send text = sent := text :: !sent in
  let event id body = Engine.{ room; sender = alice; id; body } in
  Engine.handle engine ~send
    (event "$subscribe" "!crow feeds add https://feeds.example/rss");
  check "explicit add is model-free" (!models = 0 && !reads = 1);
  let sub, source = Feed_store.get state ~actor:admin 1 in
  check "subscription provenance and metadata"
    (sub.event = "$subscribe" && sub.creator = alice && source.kind = "rss"
   && source.etag = Some "2");
  check "feed state does not pollute memory"
    (Store.search_facts store ~actor:admin ~query:"" = []);
  check "baseline entries cached"
    (List.length
       (Feed_store.entries state ~actor:admin ~subscription_id:1 ~after:0)
    = 2);
  check "cron target belongs to the feed tool"
    (match Store.reminders store ~actor:admin with
    | [ { target = Store.Tool { namespace = "feeds"; _ }; _ } ] -> true
    | _ -> false);
  ignore
    (Feeds.invoke (access admin) "feeds_add"
       {|{"url":"https://FEEDS.EXAMPLE:443/rss"}|});
  check "duplicate subscription is idempotent"
    (List.length (Feed_store.list state ~actor:admin ~after:0) = 1 && !reads = 1);
  sent := [];
  let run send =
    Cron.run_due store ~fire:(fun job ~run_id ->
        Engine.fire engine ~send job ~run_id)
  in
  run send;
  check "not polled early" (!reads = 1);
  time := 3600.;
  version := 3;
  run send;
  check "new entry triggers one scheduled model turn"
    (!models = 1 && List.length !sent = 1);
  check "OpenRouter receives entries and original subscription context"
    (List.for_all
       (contains (List.hd !prompts))
       [ "Post 2"; "Article 2 summary"; "$subscribe"; alice; room ]);
  run send;
  check "claimed cron occurrence not replayed" (!models = 1);
  time := 7200.;
  run send;
  check "304 is quiet and deduplicated"
    (!models = 1
    && List.length
         (Feed_store.entries state ~actor:admin ~subscription_id:1 ~after:0)
       = 3);
  time := 10800.;
  version := 4;
  run (fun _ -> failwith "delivery failure");
  let member = List.hd (Feed_store.members state ~actor:admin 1) in
  check "failed delivery retains cursor"
    (List.length
       (Feed_store.pending state ~actor:admin ~member_id:member.member_id)
    = 1);
  ignore (Store.create ~now:(fun () -> !time) db ~admin);
  time := 14400.;
  run send;
  check "pending item delivered after restart despite 304"
    (List.length !sent = 2
    && Feed_store.pending state ~actor:admin ~member_id:member.member_id = []);
  time := 15000.;
  ignore
    (Feeds.invoke (access alice) "feeds_add"
       {|{"url":"https://feeds.example/list"}|});
  let opml_id =
    (fst
       (List.find
          (fun (_, (s : Feed_store.source)) -> s.kind = "opml")
          (Feed_store.list state ~actor:admin ~after:0)))
      .subscription_id
  in
  let opml_args = Printf.sprintf {|{"id":%d}|} opml_id in
  check "OPML creates independent poll jobs"
    (List.length (Feed_store.members state ~actor:admin opml_id) = 3);
  run send;
  check "imported feeds baseline silently" (List.length !sent = 2);
  time := 18000.;
  version := 5;
  run send;
  check "overlapping OPML and direct subscriptions notify room once"
    (List.length !sent = 3);
  let opml_entries =
    Feed_store.entries state ~actor:admin ~subscription_id:opml_id ~after:0
  in
  check "OPML cache contains imported entries" (opml_entries <> []);
  time := 19000.;
  outlines := [ "https://feeds.example/atom" ];
  incr opml_version;
  ignore (Feeds.invoke (access admin) "feeds_poll" opml_args);
  check "OPML removal cancels only its child job"
    (List.length (Feed_store.members state ~actor:admin opml_id) = 2
    && List.length (Feed_store.members state ~actor:admin 1) = 1);
  let _, before = Feed_store.get state ~actor:admin 1 in
  force_error := true;
  ignore (Feeds.invoke (access alice) "feeds_poll" {|{"id":1}|});
  let _, after = Feed_store.get state ~actor:admin 1 in
  check "HTTP failure preserves validators and last success"
    (before.etag = after.etag
    && before.success_at = after.success_at
    && after.error <> None);
  let reads_before = !reads in
  time := 19060.;
  ignore (Feeds.invoke (access alice) "feeds_poll" {|{"id":1}|});
  check "error backoff persists" (!reads = reads_before);
  force_error := false;
  time := 20000.;
  ignore (Feeds.invoke (access alice) "feeds_poll" {|{"id":1}|});
  check "successful retry clears error"
    ((snd (Feed_store.get state ~actor:admin 1)).error = None);
  check "friend can remove another friend's OPML"
    (Result.is_ok (Feeds.invoke (access admin) "feeds_remove" opml_args));
  check "shared direct feed remains"
    ((snd (Feed_store.get state ~actor:admin 1)).etag = Some "5");
  let isolated = Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin in
  check "profiles do not share feed state"
    (Feed_store.list (Store.feeds isolated) ~actor:admin ~after:0 = []);
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:false;
  let before = !reads in
  time := 21600.;
  run send;
  check "revocation cancels automatic fetches" (!reads = before);
  check "captured capability observes revocation"
    (Result.is_error (Feeds.invoke (access alice) "feeds_entries" {|{"id":1}|}));
  let logs =
    Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100
  in
  check "scheduled feed polls audited"
    (List.exists
       (fun (u : Store.tool_use) ->
         u.tool = "feeds_poll" && u.source = "scheduler")
       logs);
  check "feed deletion collects unshared cache"
    (Feed_store.remove state ~actor:admin 1
    && Feed_store.list state ~actor:admin ~after:0 = []);
  let issued = ref false in
  let assistant =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~now:(fun () -> !time)
      ~complete:(fun _ tools ->
        if !issued then (Some "Subscribed to your feed.", [])
        else begin
          issued := true;
          check "memory, cron and feed tools available" (List.length tools = 15);
          ( None,
            [
              Openrouter.Tool.
                {
                  id = "subscribe";
                  name = "feeds_add";
                  arguments =
                    {|{"url":"https://feeds.example/rss","cron":"* * * * *"}|};
                };
            ] )
        end)
    |> fun e -> Engine.with_feeds e feeds
  in
  Engine.handle assistant ~direct:true ~send
    Engine.
      {
        room = "!dm:example.org";
        sender = admin;
        id = "$model-add";
        body = "Follow this RSS feed.";
      };
  let model_sub, _ = List.hd (Feed_store.list state ~actor:admin ~after:0) in
  check "model feed tool binds authenticated source"
    (model_sub.creator = admin
    && model_sub.room = "!dm:example.org"
    && model_sub.event = "$model-add");
  let cancelled =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~now:(fun () -> !time)
      ~complete:(fun _ _ ->
        ignore (Feed_store.remove state ~actor:admin model_sub.subscription_id);
        (Some "must not send", []))
    |> fun e -> Engine.with_feeds e feeds
  in
  let before = List.length !sent in
  time := 21720.;
  version := 6;
  Cron.run_due store ~fire:(fun job ~run_id ->
      Engine.fire cancelled ~send job ~run_id);
  check "subscription removal during inference suppresses delivery"
    (List.length !sent = before);
  print_endline
    "crowthebot: feed parsing, cron polls, conditional caching, cursors and \
     isolation passed"
