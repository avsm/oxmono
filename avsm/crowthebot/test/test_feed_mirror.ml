open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let room = "!room:example.org"
let root = "https://feeds.example/feed.xml"

let field name ty text =
  let codec =
    Jsont.Object.map Fun.id
    |> Jsont.Object.mem name ty ~enc:Fun.id
    |> Jsont.Object.finish
  in
  Result.get_ok (Jsont_bytesrw.decode_string codec text)

let rss ?(next = "") entries =
  {|<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:content="http://purl.org/rss/1.0/modules/content/"><channel><title>Big blog</title><link>https://articles.example/</link><description>Archive</description>|}
  ^ next ^ String.concat "" entries ^ "</channel></rss>"

let entry i content =
  Printf.sprintf
    {|<item><guid isPermaLink="false">post-%d</guid><title>Post %d</title><description>Short excerpt</description><content:encoded><![CDATA[%s]]></content:encoded></item>|}
    i i content

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let filename = Filename.temp_file "crow-feed-mirror" ".sqlite3" in
  let path = Eio.Path.(Eio.Stdenv.fs env / filename) in
  let now = ref 0.
  and reads = ref []
  and fail = ref false
  and refreshed = ref false in
  let article =
    String.make 2047 'a' ^ "🤖\nLateword telescope observations.\n"
    ^ String.concat "" (List.init 1500 (fun _ -> "\"\\\t\n"))
  in
  let big =
    rss ~next:{|<atom:link rel="next" href="/archive/2.xml"/>|}
      (entry 0 article
      :: List.init 2500 (fun i -> entry (i + 1) (String.make 1200 'x')))
  in
  check "fixture exceeds original cap" (String.length big > 2 * 1024 * 1024);
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        reads := url :: !reads;
        if !fail then failwith "temporary fixture failure";
        let body =
          if url = root then
            if !refreshed then rss [ entry 0 "Revisedarticle text." ] else big
          else if url = "https://feeds.example/archive/2.xml" then
            rss [ entry 2501 "Last archive page"; entry 1 "updated overlap" ]
          else failwith "unexpected feed page"
        in
        Fetch_mock.respond
          ~headers:
            (Http.Header.of_list
               [
                 ("ETag", if url = root then "root-version" else "page-version");
               ])
          body req)
  in
  let open_store sw =
    let db = Sqlite3_eio.open_path ~sw path in
    let store = Store.create ~now:(fun () -> !now) db ~admin in
    Store.add_room store room;
    let state = Store.feeds store in
    let feeds =
      Feeds.create ~state
        ~download:(Feed_http.create ~fetch ~clock:(Eio.Stdenv.mono_clock env))
    in
    let invoke name args =
      Result.get_ok
        (Feeds.invoke
           (Feeds.for_request feeds ~actor:admin ~room ~event:"$mirror")
           name args)
    in
    (store, state, feeds, invoke)
  in
  let first_entry = ref 0 in
  Eio.Switch.run (fun sw ->
      let _, state, _, invoke = open_store sw in
      ignore (invoke "feeds_add" (Printf.sprintf {|{"url":%S}|} root));
      let _, source = Feed_store.get state ~actor:admin 1 in
      check "large page mirrored with durable continuation"
        (source.error = None
        && source.next_url = Some "https://feeds.example/archive/2.xml");
      let search = invoke "feeds_search" {|{"id":1,"query":"Lateword"}|} in
      let found = field "entries" (Jsont.list Jsont.json) search in
      check "full-text search reaches beyond old excerpt" (List.length found = 1);
      let json =
        Result.get_ok (Jsont_bytesrw.encode_string Jsont.json (List.hd found))
      in
      first_entry := field "entry" Jsont.int json;
      let rec read offset acc =
        let result =
          invoke "feeds_read"
            (Printf.sprintf {|{"id":1,"entry":%d,"offset":%d}|} !first_entry
               offset)
        in
        check "article JSON fits tool limit" (String.length result <= 4096);
        let acc = acc ^ field "content" Jsont.string result in
        match field "next_offset" (Jsont.option Jsont.int) result with
        | None -> acc
        | Some next ->
            check "article cursor advances" (next > offset);
            read next acc
      in
      check "paginated content reconstructs exact article including Unicode"
        (read 0 "" = article);
      let rec entries after count =
        let result =
          invoke "feeds_entries" (Printf.sprintf {|{"id":1,"after":%d}|} after)
        in
        check "entry JSON fits tool limit" (String.length result <= 4096);
        let page = field "entries" (Jsont.list Jsont.json) result in
        let count = count + List.length page in
        match field "next_after" (Jsont.option Jsont.int) result with
        | None -> count
        | Some next ->
            check "entry cursor advances" (next > after);
            entries next count
      in
      check "mirror retains more than 2000 entries and exposes every page"
        (entries 0 0 = 2501);
      check "model queries never download the feed" (!reads = [ root ]));
  Eio.Switch.run (fun sw ->
      let store, state, feeds, invoke = open_store sw in
      now := 61.;
      fail := true;
      Cron.run_due store ~fire:(fun job ~run_id:_ ->
          match job.Store.target with
          | Tool { namespace = "feeds"; key } ->
              ignore (Feeds.prepare feeds ~actor:job.creator ~member_id:key);
              "polled"
          | _ -> assert false);
      let _, source = Feed_store.get state ~actor:admin 1 in
      check "failed continuation retains progress and mirror"
        (source.next_url <> None && source.error <> None);
      fail := false;
      now := 400.;
      ignore (invoke "feeds_poll" {|{"id":1}|});
      let _, source = Feed_store.get state ~actor:admin 1 in
      check "restart resumes advertised page and keeps root validators"
        (source.next_url = None
        && source.etag = Some "root-version"
        && source.error = None
        && List.hd !reads = "https://feeds.example/archive/2.xml");
      let member = List.hd (Feed_store.members state ~actor:admin 1) in
      check "complete first import establishes baseline without old-post digest"
        (Feed_store.pending state ~actor:admin ~member_id:member.member_id = []);
      let overlap =
        Feed_store.search state ~actor:admin ~subscription_id:1 ~after:0
          ~query:"overlap"
      in
      check "overlap updates existing entries" (List.length overlap = 1);
      refreshed := true;
      now := 800.;
      ignore (invoke "feeds_poll" {|{"id":1}|});
      check "FTS follows article updates"
        (Feed_store.search state ~actor:admin ~subscription_id:1 ~after:0
           ~query:"Lateword"
        = []);
      check "cached entry ID stable across refresh"
        ((List.hd
            (Feed_store.search state ~actor:admin ~subscription_id:1 ~after:0
               ~query:"Revisedarticle"))
           .entry_id = !first_entry);
      ignore (invoke "feeds_remove" {|{"id":1}|});
      check "deleted mirror no longer queryable"
        (Result.is_error
           (Feeds.invoke
              (Feeds.for_request feeds ~actor:admin ~room ~event:"$read")
              "feeds_read"
              (Printf.sprintf {|{"id":1,"entry":%d}|} !first_entry))));
  Eio.Path.unlink path;
  let legacy = Sqlite3_eio.open_memory ~sw () in
  Sqlite3.Rc.check
    (Sqlite3_eio.exec legacy
       {|
CREATE TABLE tool_schemas(name TEXT PRIMARY KEY,version INTEGER NOT NULL);
INSERT INTO tool_schemas VALUES('feeds',1);
CREATE TABLE feeds_sources(id INTEGER PRIMARY KEY AUTOINCREMENT,url TEXT UNIQUE NOT NULL,
 kind TEXT NOT NULL DEFAULT 'unknown',title TEXT NOT NULL DEFAULT '',etag TEXT,last_modified TEXT,
 checked_at REAL,success_at TEXT,error TEXT,failures INTEGER NOT NULL DEFAULT 0,retry_at REAL NOT NULL DEFAULT 0);
CREATE TABLE feeds_entries(id INTEGER PRIMARY KEY AUTOINCREMENT,
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id) ON DELETE CASCADE,
 entry_key TEXT NOT NULL,title TEXT NOT NULL,url TEXT,published TEXT,summary TEXT NOT NULL,
 observed_at TEXT NOT NULL,UNIQUE(source_id,entry_key));
INSERT INTO feeds_sources(id,url,etag,checked_at,retry_at) VALUES(1,'https://feeds.example/old','old-etag',1,99999);
INSERT INTO feeds_entries VALUES(42,1,'old-key','Old article',NULL,NULL,'Legacykeyword summary','1970-01-01T00:00:00Z');
|});
  let store = Store.create legacy ~admin in
  let state = Store.feeds store in
  let sub, _ =
    Feed_store.add state ~actor:admin ~room ~event:"$legacy"
      ~url:"https://feeds.example/old" ~cron:"0 * * * *" ~until_at:None
      ~next_at:9999999999.
  in
  let _, source = Feed_store.get state ~actor:admin sub.subscription_id in
  check "migration resets validators and backoff for full-content refresh"
    (source.etag = None && source.checked_at = None && source.retry_at = 0.);
  check "migration indexes existing summaries and preserves IDs"
    ((List.hd
        (Feed_store.search state ~actor:admin
           ~subscription_id:sub.subscription_id ~after:0 ~query:"Legacykeyword"))
       .entry_id = 42);
  check "migration makes old excerpts readable"
    (Feed_store.read_content state ~actor:admin
       ~subscription_id:sub.subscription_id ~entry_id:42 ~offset:0
    = ("Legacykeyword summary", 21));
  ignore (Store.create legacy ~admin);
  let member =
    List.hd (Feed_store.members state ~actor:admin sub.subscription_id)
  in
  let cycle =
    try
      Feed_store.complete_poll ~next_url:source.url ~page_url:source.url state
        ~actor:admin ~member_id:member.member_id ~kind:"rss" ~title:"Cycle"
        ~etag:None ~last_modified:None ~entries:[];
      false
    with Invalid_argument _ -> true
  in
  check "cyclic pagination fails without discarding existing mirror"
    (cycle
    && (List.hd
          (Feed_store.entries state ~actor:admin
             ~subscription_id:sub.subscription_id ~after:0))
         .entry_id = 42);
  print_endline
    "crowthebot: large feed mirror, FTS, pagination and restart continuation \
     passed"
