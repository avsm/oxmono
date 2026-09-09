open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let room = "!room:example.org"

let bad f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let contains text needle =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let () =
  let next expression after =
    Cron.next (Cron.parse expression) ~after:(Cron.time after) ~until:None
    |> Option.map Store.timestamp
  in
  check "cron minute steps"
    (next "*/15 * * * *" "2026-09-09T09:01:00Z" = Some "2026-09-09T09:15:00Z");
  check "cron permits tabs between fields"
    (next "0\t9 * * *" "2026-09-09T08:00:00Z" = Some "2026-09-09T09:00:00Z");
  check "cron ranges and lists"
    (next "0 9-17/2 * * 1,3,5" "2026-09-09T09:00:00Z"
    = Some "2026-09-09T11:00:00Z");
  check "calendar leap day"
    (next "0 0 29 2 *" "2025-01-01T00:00:00Z" = Some "2028-02-29T00:00:00Z");
  check "cron day/weekday OR rule"
    (next "0 0 1 * 1" "2026-09-01T00:00:00Z" = Some "2026-09-07T00:00:00Z");
  check "Sunday alias"
    (next "0 0 * * 7" "2026-09-09T00:00:00Z" = Some "2026-09-13T00:00:00Z");
  check "timestamp timezone normalization"
    (Store.timestamp (Cron.time "2026-09-09T10:00:00+01:00")
    = "2026-09-09T09:00:00Z");
  check "impossible calendar bounded"
    (next "0 0 31 2 *" "2026-01-01T00:00:00Z" = None);
  List.iter
    (fun expr ->
      check "invalid cron rejected" (bad (fun () -> Cron.parse expr)))
    [ "* * *"; "*/0 * * * *"; "60 * * * *"; "0 9-2 * * *" ];
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = ref 0. in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> !clock) db ~admin in
  Store.add_room store room;
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:true;
  let fact_id =
    Store.add_fact store ~actor:alice ~room ~event:"$source" ~source:"command"
      ~body:"Take jasmine tea to the meeting."
  in
  let access = Cron.for_request store ~actor:alice ~room ~event:"$source" in
  let create schedule =
    Cron.invoke access "cron_create"
      (Printf.sprintf
         {|{"fact_id":%d,"instruction":"Remind me about the meeting.",%s}|}
         fact_id schedule)
  in
  check "one-off tool registration"
    (create {|"at":"1970-01-01T00:01:00Z"|} = Ok "Registered reminder #1.");
  check "limited recurrence registration"
    (create {|"cron":"* * * * *","until":"1970-01-01T00:03:00Z"|}
    = Ok "Registered reminder #2.");
  check "indefinite recurrence registration"
    (create {|"cron":"*/2 * * * *"|} = Ok "Registered reminder #3.");
  check "past time rejected"
    (Result.is_error (create {|"at":"1969-12-31T00:00:00Z"|}));
  let bot_access =
    Cron.for_request store ~actor:"@bot:example.org" ~room ~event:""
  in
  check "unknown accounts cannot register jobs"
    (Result.is_error
       (Cron.invoke bot_access "cron_create"
          {|{"fact_id":1,"instruction":"forged","cron":"* * * * *"}|}));
  let sources = ref [] and issued_tool = ref false in
  let client =
    Openrouter.of_fetch ~base_url:"https://model.example/v1"
      (Fetch_mock.client (fun req ->
           let body =
             match req.body with Fetch.String s -> s | _ -> assert false
           in
           sources := body :: !sources;
           let response =
             if !issued_tool then
               {|{"id":"1","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Meeting reminder: bring jasmine tea."}}]}|}
             else begin
               issued_tool := true;
               {|{"id":"1","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"remember-fired","type":"function","function":{"name":"memory_store","arguments":"{\"fact\":\"The meeting preparation reminder was processed.\"}"}},{"id":"follow-up","type":"function","function":{"name":"cron_create","arguments":"{\"fact_id\":1,\"instruction\":\"Follow up on the meeting.\",\"at\":\"1970-01-02T00:00:00Z\"}"}}]}}]}|}
             end
           in
           Fetch_mock.respond
             ~headers:
               (Http.Header.of_list [ ("Content-Type", "application/json") ])
             response req))
  in
  let config =
    {
      (Config.default ~admin ~homeserver:"https://matrix.example.org") with
      plugins = [];
    }
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~complete:(App.complete env config client) ~now:(fun () -> !clock)
  in
  let sent = ref [] in
  let fire job ~run_id =
    Engine.fire engine ~send:(fun s -> sent := s :: !sent) job ~run_id
  in
  Cron.run_due store ~fire;
  check "not early" (!sent = []);
  clock := 60.;
  Cron.run_due store ~fire;
  check "one-off and recurring fired" (List.length !sent = 2);
  check "scheduled model can act through memory tools"
    (match Store.search_facts store ~actor:admin ~query:"preparation" with
    | [ f ] ->
        f.source = "observation" && String.starts_with ~prefix:"$cron-" f.event
    | _ -> false);
  check "nested reminders preserve the Matrix source event"
    (match Store.get_reminder store 4 with
    | Some job ->
        job.event = "$source" && job.room = room && job.creator = alice
    | None -> false);
  check "model receives memory and reminder provenance"
    (List.for_all
       (fun needle -> contains (List.hd !sources) needle)
       [
         "Take jasmine tea to the meeting";
         "$source";
         alice;
         room;
         "Remind me about the meeting";
         "1970-01-01T00:01:00Z";
       ]);
  Cron.run_due store ~fire;
  check "no duplicate occurrence" (List.length !sent = 2);
  ignore (Store.create ~now:(fun () -> !clock) db ~admin);
  Cron.run_due store ~fire;
  check "restart does not replay completed occurrence" (List.length !sent = 2);
  clock := 120.;
  Cron.run_due store ~fire;
  clock := 180.;
  Cron.run_due store ~fire;
  check "limited recurrence includes end minute" (List.length !sent = 5);
  clock := 600.;
  Cron.run_due store ~fire;
  check "missed recurrence coalesces into one action" (List.length !sent = 6);
  check "forever schedule advances beyond now"
    (match Store.get_reminder store 3 with
    | Some job -> job.next_at = 720. && job.state = "active"
    | None -> false);
  check "another friend can cancel" (Store.cancel_reminder store ~actor:admin 3);
  clock := 720.;
  Cron.run_due store ~fire;
  check "cancel stops future actions" (List.length !sent = 6);
  ignore (create {|"cron":"* * * * *"|});
  ignore (Store.erase_fact store ~actor:admin fact_id);
  clock := 780.;
  Cron.run_due store ~fire;
  check "erasing linked memory removes reminders"
    (List.length !sent = 6 && Store.reminders store ~actor:admin = []);
  let fact_id =
    Store.add_fact store ~actor:alice ~room ~event:"$revoked" ~source:"command"
      ~body:"Another meeting"
  in
  let id =
    Store.add_reminder store ~actor:alice ~room ~event:"$revoked" ~fact_id
      ~instruction:"meeting" ~cron:None ~until_at:None ~next_at:800.
  in
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:false;
  clock := 800.;
  Cron.run_due store ~fire;
  check "revoking creator cancels jobs"
    (List.length !sent = 6
    &&
    match Store.get_reminder store id with
    | Some r -> r.state = "cancelled"
    | None -> false);
  let logs =
    Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100
  in
  check "each firing and its tool action have durable audit entries"
    (List.length logs = 8
    && List.length
         (List.filter (fun (u : Store.tool_use) -> u.tool = "cron_fire") logs)
       = 6
    && List.for_all (fun (u : Store.tool_use) -> u.status = "ok") logs);
  let crash =
    Store.add_reminder store ~actor:admin ~room ~event:"$crash" ~fact_id
      ~instruction:"do not replay" ~cron:None ~until_at:None ~next_at:900.
  in
  clock := 900.;
  let job = Option.get (Store.get_reminder store crash) in
  check "claim persisted before effects"
    (Store.claim_reminder store job ~next_at:None <> None);
  ignore (Store.create ~now:(fun () -> !clock) db ~admin);
  Cron.run_due store ~fire;
  check "crashed occurrence is not replayed" (List.length !sent = 6);
  let cancelled =
    Store.add_reminder store ~actor:admin ~room ~event:"$cancel" ~fact_id
      ~instruction:"cancel while thinking" ~cron:None ~until_at:None
      ~next_at:960.
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~complete:(fun _ _ ->
        ignore (Store.cancel_reminder store ~actor:admin cancelled);
        (Some "must not deliver", []))
      ~now:(fun () -> !clock)
  in
  clock := 960.;
  Cron.run_due store ~fire:(fun job ~run_id ->
      Engine.fire engine ~send:(fun s -> sent := s :: !sent) job ~run_id);
  check "cancellation during inference prevents delivery" (List.length !sent = 6);
  print_endline
    "crowthebot: cron calendar, reminder actions, persistence and cancellation \
     passed"
