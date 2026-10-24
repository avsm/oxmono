open Crowthebot

let check s b = if not b then failwith s

let contains text needle =
  let rec loop offset =
    offset + String.length needle <= String.length text
    && (String.sub text offset (String.length needle) = needle
       || loop (offset + 1))
  in
  loop 0

let noop () = ()
let admin = "@admin:example.org"
let room = "!private:example.org"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let index = Retrieval.Index.memory ~sw () in
  let corpus = Bushel_knowledge.create index in
  let refreshes = ref 0 and allow = ref true in
  let tools =
    Bushel_agent.create ~clock:(Eio.Stdenv.clock env) ~corpus ~max_bytes:2048
      ~refresh:(fun () -> incr refreshes)
      ()
  in
  let knowledge =
    Knowledge.create ~tools ~allowed:(fun ~actor ~room:r ->
        !allow && actor = admin && r = room)
  in
  let invoke actor room =
    Knowledge.invoke knowledge ~actor ~room ~check:noop "contacts_resolve"
      {|{"query":"Alex"}|}
  in
  check "unauthorized actor denied before loading"
    (Result.is_error (invoke "@stranger:example.org" room) && !refreshes = 0);
  check "admin in unapproved room denied before loading"
    (Result.is_error (invoke admin "!public:example.org") && !refreshes = 0);
  check "authorized retrieval"
    (Result.is_ok (invoke admin room) && !refreshes = 1);
  let store =
    Store.create ~now:(fun () -> 0.) (Sqlite3_eio.open_memory ~sw ()) ~admin
  in
  Store.add_room store room;
  let rounds = ref 0 in
  let complete =
    Fake_model.v (fun _ definitions ->
        incr rounds;
        check "knowledge tools offered in allowed room"
          (List.exists
             (fun t -> Agentkit.Agent.Tool.name t = "knowledge_search")
             definitions);
        if !rounds = 1 then
          ( None,
            [
              Agentkit.Agent.
                {
                  id = "knowledge-call";
                  name = "contacts_resolve";
                  arguments = {|{"query":"Alex"}|};
                };
            ] )
        else (Some "No matching contact.", []))
  in
  let engine =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://example.org")
      ~store ~self:"@crow:example.org" ~plugins:[] ~complete ~now:(fun () -> 0.)
    |> fun e -> Engine.with_knowledge e knowledge
  in
  let sent = ref [] in
  Engine.handle engine ~mentioned:true
    ~send:(fun text -> sent := text :: !sent)
    Engine.{ room; sender = admin; id = "$knowledge"; body = "Find Alex" };
  check "engine executes tool and delivers synthesis"
    (!rounds = 2 && List.length !sent = 1);
  check "knowledge call durably audited"
    (List.exists
       (fun (u : Store.tool_use) ->
         u.tool = "contacts_resolve" && u.status = "ok")
       (Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int
          ~limit:20));
  let feed_downloads = ref 0 in
  let feeds =
    Feeds.create ~state:(Store.feeds store)
      ~download:(fun ~url:_ ~etag:_ ~last_modified:_ ->
        incr feed_downloads;
        failwith "Latest personal posts must not download feeds")
  in
  let latest_rounds = ref 0 in
  let latest_model =
    Fake_model.v (fun messages definitions ->
        incr latest_rounds;
        let offered name =
          List.exists
            (fun tool -> Agentkit.Agent.Tool.name tool = name)
            definitions
        in
        check "explicit Bushel search and latest-post tools are offered"
          (offered "bushel_search" && offered "bushel_latest_posts");
        check "feed polling remains available for unrelated subscriptions"
          (offered "feeds_poll");
        let prompt =
          Option.value ~default:"" (Agentkit.Chat.system_text messages)
        in
        check "own-site requests explicitly route to Bushel search"
          (contains prompt "my blog, my posts or my site, use bushel_search");
        check
          "latest-post requests explicitly route to chronological Bushel tool"
          (contains prompt
             "For my latest post or recent writing, call bushel_latest_posts");
        check "own-site routing explicitly excludes feed synchronization"
          (contains prompt
             "Do not use Sortal feed sync, feeds_add or feeds_poll");
        if !latest_rounds = 1 then
          ( None,
            [
              Agentkit.Agent.
                {
                  id = "latest-post-call";
                  name = "bushel_latest_posts";
                  arguments = "{}";
                };
            ] )
        else (
          check "model receives the dispatched latest-post tool result"
            (List.exists
               (function
                 | Agentkit.Chat.Tool_result { id; content } ->
                     id = "latest-post-call" && contains content "\"items\""
                 | _ -> false)
               messages);
          (Some "No published posts are available in this fixture.", [])))
  in
  let latest_engine =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://example.org")
      ~store ~self:"@crow:example.org" ~plugins:[] ~complete:latest_model
      ~now:(fun () -> 0.)
    |> fun engine ->
    Engine.with_knowledge engine knowledge |> fun engine ->
    Engine.with_feeds engine feeds
  in
  let latest_sent = ref [] in
  Engine.handle latest_engine ~mentioned:true
    ~send:(fun text -> latest_sent := text :: !latest_sent)
    Engine.
      {
        room;
        sender = admin;
        id = "$latest-posts";
        body = "What are the latest posts on my site?";
      };
  check "latest-post request completes without feed downloads"
    (!latest_rounds = 2 && List.length !latest_sent = 1 && !feed_downloads = 0);
  let latest_uses =
    Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:20
    |> List.filter (fun (use : Store.tool_use) -> use.event = "$latest-posts")
  in
  check
    "latest-post dispatch is audited as Bushel rather than feed synchronization"
    (match latest_uses with
    | [ use ] -> use.tool = "bushel_latest_posts" && use.status = "ok"
    | _ -> false);
  check "latest-post request dispatches neither feeds_sync nor feed polling"
    (List.for_all
       (fun (use : Store.tool_use) ->
         not (String.starts_with ~prefix:"feeds_" use.tool))
       latest_uses);
  let calls = ref 0 in
  let provider =
    Retrieval.Embedding.
      {
        model = "test";
        dimensions = 2;
        max_batch = 1;
        embed =
          (fun ~task:_ ~check inputs ->
            incr calls;
            allow := false;
            check ();
            List.map (fun _ -> [ 1.; 0. ]) inputs);
      }
  in
  let tools =
    Bushel_agent.create ~clock:(Eio.Stdenv.clock env) ~corpus ~provider
      ~max_bytes:2048 ()
  in
  let knowledge =
    Knowledge.create ~tools ~allowed:(fun ~actor:_ ~room:_ -> !allow)
  in
  check "revocation during inference rejects result"
    (Result.is_error
       (Knowledge.invoke knowledge ~actor:admin ~room ~check:noop
          "knowledge_search" {|{"query":"Alex","mode":"semantic"}|})
    && !calls = 1);
  allow := true;
  check "knowledge cancellation propagates instead of becoming a tool error"
    (try
       ignore
         (Knowledge.invoke knowledge ~actor:admin ~room
            ~check:(fun () -> raise (Eio.Cancel.Cancelled Exit))
            "contacts_resolve" {|{"query":"Alex"}|});
       false
     with Eio.Cancel.Cancelled Exit -> true);
  print_endline "crowthebot knowledge access tests passed"
