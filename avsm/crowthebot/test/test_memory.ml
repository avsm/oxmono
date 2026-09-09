open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let bob = "@bob:example.org"
let bot = "@bot:example.org"
let self = "@crow:example.org"
let room = "!room:example.org"
let other_room = "!other:example.org"

let bad f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let config =
  {
    (Config.default ~admin ~homeserver:"https://matrix.example.org") with
    plugins = [];
  }

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = ref 0. in
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create ~now:(fun () -> !clock) db ~admin in
  List.iter (Store.add_room store) [ room; other_room ];
  List.iter
    (fun user ->
      Store.set_person store ~actor:admin ~user ~role:Friend ~allowed:true)
    [ alice; bob ];
  Store.set_person store ~actor:admin ~user:bot ~role:Bot ~allowed:true;
  let fact =
    Store.add_fact store ~actor:alice ~room ~event:"$fact" ~source:"command"
      ~body:"Zoë likes café visits and red bicycles."
  in
  check "full timestamp and provenance"
    (match Store.get_fact store ~actor:bob fact with
    | Some f ->
        f.created_at = "1970-01-01T00:00:00Z"
        && f.author = alice && f.room = room && f.event = "$fact"
        && f.source = "command"
    | None -> false);
  List.iter
    (fun query ->
      check ("FTS " ^ query)
        (List.map
           (fun (f : Store.fact) -> f.fact_id)
           (Store.search_facts store ~actor:bob ~query)
        = [ fact ]))
    [ "cafe"; "\"red bicycles\""; "bicy*"; "Zoë AND café" ];
  check "malformed search handled"
    (bad (fun () -> Store.search_facts store ~actor:alice ~query:"\""));
  check "empty facts rejected"
    (bad (fun () ->
         Store.add_fact store ~actor:alice ~room ~event:"" ~source:"command"
           ~body:" "));
  List.iter
    (fun actor ->
      check "unauthorized get"
        (bad (fun () -> Store.get_fact store ~actor fact));
      check "unauthorized search"
        (bad (fun () -> Store.search_facts store ~actor ~query:"cafe"));
      check "unauthorized erase"
        (bad (fun () -> Store.erase_fact store ~actor fact));
      check "unauthorized store"
        (bad (fun () ->
             Store.add_fact store ~actor ~room ~event:"" ~source:"observation"
               ~body:"forged")))
    [ bot; "@unknown:example.org" ];
  check "friends erase each other's facts"
    (Store.erase_fact store ~actor:bob fact);
  check "erased fact absent" (Store.get_fact store ~actor:alice fact = None);
  check "erased fact unsearchable"
    (Store.search_facts store ~actor:alice ~query:"cafe" = []);
  let next =
    Store.add_fact store ~actor:admin ~room ~event:"" ~source:"observation"
      ~body:"A second observation"
  in
  check "fact IDs never reused" (next > fact);
  let stage = ref 0 and replies = ref [] in
  let complete _ tools =
    check "memory and cron tools available without plugins"
      (List.length tools = 7);
    incr stage;
    if !stage = 1 then
      ( None,
        [
          Openrouter.Tool.
            {
              id = "remember";
              name = "memory_store";
              arguments = {|{"fact":"Alice prefers jasmine tea."}|};
            };
        ] )
    else (Some "Remembered.", [])
  in
  let engine =
    Engine.create ~config ~store ~self ~plugins:[] ~complete ~now:(fun () ->
        !clock)
  in
  let send reply = replies := reply :: !replies in
  let event ?(sender = alice) ?(room = room) id body =
    Engine.{ sender; room; id; body }
  in
  Engine.handle engine ~send (event "observe" "!crow I prefer jasmine tea");
  let observed = Store.search_facts store ~actor:bob ~query:"jasmine" in
  check "model can remember observations"
    (match observed with
    | [ f ] ->
        f.source = "observation" && f.event = "observe" && f.author = alice
    | _ -> false);
  Engine.handle engine ~send
    (event ~sender:bob ~room:other_room "search" "!crow memory search jasmine");
  check "shared across rooms and friends"
    (String.starts_with ~prefix:"Up to 20 facts" (List.hd !replies));
  let id = (List.hd observed).fact_id in
  Engine.handle engine ~send
    (event ~sender:bob "erase" (Printf.sprintf "!crow memory erase %d" id));
  let logs () =
    Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100
  in
  check "every model and command memory operation logged"
    (List.length (logs ()) = 3);
  check "memory content not duplicated into audit"
    (List.for_all
       (fun (u : Store.tool_use) ->
         u.arguments = "[memory content omitted]"
         && u.result = "[memory content omitted]"
         && u.status = "ok" && u.finished_at <> None)
       (logs ()));
  Engine.handle engine ~send
    (event ~sender:bob "erase" (Printf.sprintf "!crow memory erase %d" id));
  check "replay creates no duplicate tool use" (List.length (logs ()) = 3);
  let hostile _ tools =
    check "bots receive no memory tools" (tools = []);
    ( None,
      [
        Openrouter.Tool.
          {
            id = "forge";
            name = "memory_store";
            arguments = {|{"fact":"forged"}|};
          };
      ] )
  in
  let engine =
    Engine.create ~config ~store ~self ~plugins:[] ~complete:hostile
      ~now:(fun () -> !clock)
  in
  (try Engine.handle engine ~send (event ~sender:bot "bot-forge" "!crow hello")
   with Failure _ -> ());
  check "model cannot forge friend authority"
    (Store.search_facts store ~actor:admin ~query:"forged" = []);
  check "rejected calls logged"
    (List.exists (fun (u : Store.tool_use) -> u.status = "rejected") (logs ()));
  let old_access =
    Memory.for_request store ~actor:alice ~room ~event:"old"
      ~source:"observation"
  in
  Store.set_person store ~actor:admin ~user:alice ~role:Friend ~allowed:false;
  check "captured capability honors revocation"
    (Result.is_error
       (Memory.invoke old_access "memory_get"
          (Printf.sprintf {|{"id":%d}|} next)));
  check "revocation denies shared memory"
    (bad (fun () -> Store.search_facts store ~actor:alice ~query:""));
  let audit tool f =
    Audit.run store ~actor:admin ~room ~event:"audit" ~source:"command"
      ~call_id:"" ~tool ~arguments:"query" f
  in
  (try ignore (audit "failing" (fun () -> failwith "private exception"))
   with Failure _ -> ());
  let pending, _ = Eio.Promise.create () in
  (try
     Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.02 (fun () ->
         ignore (audit "cancelled" (fun () -> Eio.Promise.await pending)))
   with Eio.Time.Timeout -> ());
  check "exceptions and cancellation durably recorded"
    (List.exists
       (fun (u : Store.tool_use) -> u.tool = "failing" && u.status = "error")
       (logs ())
    && List.exists
         (fun (u : Store.tool_use) ->
           u.tool = "cancelled" && u.status = "cancelled")
         (logs ()));
  let orphan =
    Store.start_tool store ~actor:admin ~room ~event:"crash" ~source:"model"
      ~call_id:"orphan" ~tool:"pending" ~arguments:""
  in
  let store = Store.create ~now:(fun () -> !clock) db ~admin in
  check "unfinished call survives restart as interrupted"
    (List.exists
       (fun (u : Store.tool_use) ->
         u.log_id = orphan && u.status = "interrupted")
       (Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int
          ~limit:100));
  check "facts persist across reopen"
    (Store.get_fact store ~actor:admin next <> None);
  print_endline
    "crowthebot: shared memory, FTS, authority and durable tool audit passed"
