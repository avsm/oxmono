open Crowthebot

let check name value = if not value then failwith name

let contains text needle =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let failed f =
  try
    ignore (f ());
    false
  with Failure _ | Invalid_argument _ -> true

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = ref 0. and admin = "@admin:example.org" in
  let db = Sqlite3_eio.open_memory ~sw () in
  (* An actual version-two schema, without any of the new tables. *)
  Sqlite3.Rc.check
    (Sqlite3_eio.exec db
       "PRAGMA user_version=2; CREATE TABLE settings(key TEXT PRIMARY \
        KEY,value TEXT); INSERT INTO settings \
        VALUES('admin','@admin:example.org');");
  let store = Store.create ~now:(fun () -> !clock) db ~admin in
  let log n =
    let id =
      Store.start_tool store ~actor:admin ~room:"!room:example.org"
        ~event:(string_of_int n) ~source:"model" ~call_id:(string_of_int n)
        ~tool:"test" ~arguments:(String.make 3000 'a')
    in
    Store.finish_tool store id ~status:"ok" ~result:(String.make 5000 'b');
    id
  in
  for i = 1 to 131 do
    ignore (log i)
  done;
  check "current day not summarized" (Store.pending_note_days store = []);
  clock := 86400.;
  check "UTC rollover queues previous day"
    (Store.pending_note_days store = [ "1970-01-01" ]);
  let requests = ref [] in
  let config =
    {
      (Config.default ~admin ~homeserver:"https://matrix.example.org") with
      context_bytes = 1024;
    }
  in
  let client =
    Openrouter.of_fetch ~base_url:"https://model.example/v1"
      (Fetch_mock.client (fun req ->
           let body =
             match req.body with Fetch.String s -> s | _ -> assert false
           in
           requests := body :: !requests;
           check "daily summaries cannot invoke tools"
             (not (contains body "\"tools\""));
           Fetch_mock.respond
             ~headers:
               (Http.Header.of_list [ ("Content-Type", "application/json") ])
             {|{"id":"1","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Daily note."}}]}|}
             req))
  in
  let complete = App.complete env config client in
  let note = Daily.generate ~store ~config ~complete ~day:"1970-01-01" in
  check "note has complete source count"
    (note.tool_count = 131 && note.last_tool_id = 131);
  check "all pages summarized"
    (List.length !requests > 50
    && List.for_all
         (fun n ->
           List.exists
             (fun text -> contains text (Printf.sprintf "#%d " n))
             !requests)
         (List.init 131 (fun i -> i + 1)));
  check "note timestamp is generation time"
    (note.generated_at = "1970-01-02T00:00:00Z");
  let count = List.length !requests in
  ignore (Daily.generate ~store ~config ~complete ~day:"1970-01-01");
  check "successful notes not regenerated" (List.length !requests = count);
  check "completed day leaves pending queue" (Store.pending_note_days store = []);
  clock := 0.;
  ignore (log 132);
  clock := 86400.;
  check "new records invalidate the watermark"
    (Store.pending_note_days store = [ "1970-01-01" ]);
  check "provider failure propagates"
    (failed (fun () ->
         Daily.generate ~store ~config ~day:"1970-01-01" ~complete:(fun _ _ ->
             failwith "synthetic failure")));
  check "failure leaves previous note intact"
    (Store.get_note store "1970-01-01" = Some note);
  let note = Daily.generate ~store ~config ~complete ~day:"1970-01-01" in
  check "retry catches up" (note.tool_count = 132);
  clock := 86400. *. 2.;
  check "empty days still receive notes"
    (Store.pending_note_days store = [ "1970-01-02" ]);
  check "summary tool calls rejected"
    (failed (fun () ->
         Daily.generate ~store ~config ~day:"1970-01-02" ~complete:(fun _ _ ->
             ( Some "forged",
               [
                 Openrouter.Tool.
                   { id = "forge"; name = "memory_erase"; arguments = "{}" };
               ] ))));
  check "invalid summary never saved" (Store.get_note store "1970-01-02" = None);
  let empty = Daily.generate ~store ~config ~complete ~day:"1970-01-02" in
  check "empty note is model-generated"
    (empty.tool_count = 0 && empty.body = "Daily note.");
  check "invalid dates rejected"
    (failed (fun () -> Store.validate_day "2025-02-29"));
  print_endline
    "crowthebot: daily OpenRouter notes, pagination, rollover and retries \
     passed"
