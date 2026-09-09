open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"

let () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  Sqlite3.Rc.check
    (Sqlite3_eio.exec db
       {|
PRAGMA user_version=4;
CREATE TABLE settings(key TEXT PRIMARY KEY,value TEXT NOT NULL);
INSERT INTO settings VALUES('admin','@admin:example.org');
CREATE TABLE facts(id INTEGER PRIMARY KEY AUTOINCREMENT,created_at TEXT NOT NULL,
 author TEXT NOT NULL,room TEXT NOT NULL,event TEXT NOT NULL,source TEXT NOT NULL,body TEXT NOT NULL);
INSERT INTO facts VALUES(9,'1970-01-01T00:00:00Z','@admin:example.org','!room:example.org','$fact','command','Preserve this memory.');
CREATE VIRTUAL TABLE facts_fts USING fts5(body,content='facts',content_rowid='id',tokenize='unicode61');
INSERT INTO facts_fts(rowid,body) SELECT id,body FROM facts;
CREATE TABLE reminders(id INTEGER PRIMARY KEY AUTOINCREMENT,
 fact_id INTEGER NOT NULL REFERENCES facts(id) ON DELETE CASCADE,
 creator TEXT NOT NULL,room TEXT NOT NULL,event TEXT NOT NULL,created_at TEXT NOT NULL,
 instruction TEXT NOT NULL,cron TEXT,until_at REAL,next_at REAL NOT NULL,state TEXT NOT NULL);
INSERT INTO reminders VALUES(77,9,'@admin:example.org','!room:example.org','$reminder','1970-01-01T00:00:00Z','Existing action','0 * * * *',NULL,3600,'active');
INSERT INTO reminders SELECT 78,fact_id,creator,room,event,created_at,instruction,cron,until_at,next_at,state FROM reminders WHERE id=77;
DELETE FROM reminders WHERE id=78;
|});
  let rejected =
    try
      ignore (Store.create db ~admin:"@wrong:example.org");
      false
    with Invalid_argument _ -> true
  in
  check "migration rejects authority mismatch" rejected;
  let store = Store.create ~now:(fun () -> 0.) db ~admin in
  check "old memory and FTS survive"
    (List.length (Store.search_facts store ~actor:admin ~query:"Preserve") = 1);
  check "old reminder is still linked to memory"
    (match Store.get_reminder store 77 with
    | Some r ->
        r.target = Store.Memory 9 && r.next_at = 3600. && r.event = "$reminder"
    | None -> false);
  let next =
    Store.add_reminder store ~actor:admin ~room:"!room:example.org"
      ~event:"$new" ~fact_id:9 ~instruction:"New action" ~cron:None
      ~until_at:None ~next_at:4000.
  in
  check "migration preserves unused high-water IDs" (next = 79);
  ignore (Store.create ~now:(fun () -> 0.) db ~admin);
  check "feed schema reopens idempotently"
    (Feed_store.list (Store.feeds store) ~actor:admin ~after:0 = []);
  ignore (Store.erase_fact store ~actor:admin 9);
  check "memory deletion still cascades after migration"
    (Store.get_reminder store 77 = None);
  Sqlite3.Rc.check
    (Sqlite3_eio.exec db
       {|
CREATE TABLE feed_test_fk(id INTEGER REFERENCES facts(id) DEFERRABLE INITIALLY DEFERRED);
CREATE TRIGGER feeds_test_constraint AFTER INSERT ON reminders BEGIN
 INSERT INTO feed_test_fk VALUES(999);
END;
|});
  let add () =
    Feed_store.add (Store.feeds store) ~actor:admin ~room:"!room:example.org"
      ~event:"$feed" ~url:"https://feeds.example/rss" ~cron:"0 * * * *"
      ~until_at:None ~next_at:3600.
  in
  let rejected =
    try
      ignore (add ());
      false
    with Sqlite3.SqliteError _ -> true
  in
  check "failed commit rolls back subscription and cron job"
    (rejected && Feed_store.list (Store.feeds store) ~actor:admin ~after:0 = []);
  Sqlite3.Rc.check
    (Sqlite3_eio.exec db
       "DROP TRIGGER feeds_test_constraint; DROP TABLE feed_test_fk;");
  let _, fresh = add () in
  check "store remains usable after a commit failure" fresh;
  let friend = "@friend:example.org" in
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:true;
  let state = Store.feeds store in
  let sub, _ =
    Feed_store.add state ~actor:friend ~room:"!room:example.org" ~event:"$list"
      ~url:"https://feeds.example/list" ~cron:"0 * * * *" ~until_at:None
      ~next_at:3600.
  in
  let root =
    List.hd (Feed_store.members state ~actor:admin sub.subscription_id)
  in
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:false;
  Feed_store.sync_opml state ~actor:admin ~member_id:root.member_id
    ~urls:[ "https://feeds.example/new" ];
  check "manual refresh cannot reactivate a revoked creator's jobs"
    (List.for_all
       (fun (_, _, state) -> state = "cancelled")
       (Feed_store.status state ~actor:admin
          ~subscription_id:sub.subscription_id ~after:0));
  let config = Config.default ~admin ~homeserver:"https://matrix.example.org" in
  let upgraded =
    Config.upgrade
      {
        config with
        plugins = [ "blogroll"; "custom" ];
        system_prompt =
          config.system_prompt
          ^ " Use the blogroll tool to look up feeds when useful.";
      }
  in
  check "legacy configuration loses only fixed blogroll defaults"
    (upgraded.plugins = [ "custom" ]
    && upgraded.system_prompt = config.system_prompt);
  print_endline
    "crowthebot: version-four feed migration and legacy configuration passed"
