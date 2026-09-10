open Crowthebot

let check name b = if not b then failwith name
let admin = "@admin:example.test"
let room = "!room:example.test"
let encode = Jsont_bytesrw.encode_string Jsont.json

let field name = function
  | Jsont.Object (fields, _) ->
      List.assoc_opt name (List.map (fun ((k, _), v) -> (k, v)) fields)
  | _ -> None

let string value = Jsont.Json.string value

let wire_synthesis env ~mode =
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create db ~admin in
  Store.add_room store room;
  let config = Config.default ~admin ~homeserver:"https://example.test" in
  let rounds = ref 0 and terminal = ref 0 and original_system = ref None in
  let seen_requests = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        incr rounds;
        let request =
          match req.body with
          | Fetch.String s ->
              Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)
          | _ -> failwith "request body expected"
        in
        seen_requests := request :: !seen_requests;
        let messages =
          match field "messages" request with
          | Some (Jsont.Array (xs, _)) -> xs
          | _ -> failwith "messages expected"
        in
        let reply ?(status = 200) body =
          Fetch_mock.respond ~status
            ~headers:
              (Http.Header.of_list [ ("content-type", "application/json") ])
            body req
        in
        (* Reproduce the provider's validation, before accepting a completion. *)
        if
          List.exists
            (fun m -> field "role" m = Some (string "system"))
            (List.tl messages)
        then
          reply ~status:400
            {|{"error":{"code":400,"message":"System message must be at the beginning."}}|}
        else (
          check "system stays first"
            (field "role" (List.hd messages) = Some (string "system"));
          let system =
            match field "content" (List.hd messages) with
            | Some (Jsont.String (s, _)) -> s
            | _ -> failwith "system content expected"
          in
          (match !original_system with
          | None -> original_system := Some system
          | Some prefix ->
              check "persona and authorization retained"
                (String.starts_with ~prefix system));
          let completed content =
            reply
              (Printf.sprintf
                 {|{"id":"synthesis","model":"test/model","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":%s}}]}|}
                 (Result.get_ok (encode (string content))))
          in
          if !rounds <= 6 then (
            check "normal rounds retain tools" (field "tools" request <> None);
            let name, arguments =
              if !rounds = 1 then
                ("memory_store", {|{"fact":"A synthetic meeting is at nine."}|})
              else ("memory_search", {|{"query":"meeting"}|})
            in
            reply
              (Printf.sprintf
                 {|{"id":"tool","model":"test/model","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":"","tool_calls":[{"id":"call-%d","type":"function","function":{"name":%s,"arguments":%s}}]}}]}|}
                 !rounds
                 (Result.get_ok (encode (string name)))
                 (Result.get_ok (encode (string arguments)))))
          else (
            incr terminal;
            check "synthesis cannot request tools" (field "tools" request = None);
            check "all results retained"
              (List.length
                 (List.filter
                    (fun m -> field "role" m = Some (string "tool"))
                    messages)
              = 6);
            check "tool result remains last"
              (field "role" (List.hd (List.rev messages)) = Some (string "tool"));
            if mode = "empty" && !terminal = 1 then completed ""
            else if (mode = "http" && !terminal = 1) || mode = "http-fallback"
            then
              reply ~status:503
                {|{"error":{"code":503,"message":"Provider unavailable"}}|}
            else completed "The synthetic meeting is at nine.")))
  in
  let client =
    Openrouter.of_fetch ~base_url:"https://example.test/v1/"
      (Trace.wrap (Store.trace store) fetch)
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
      ~now:(fun () -> 0.)
      ~complete:(App.complete env config client)
  in
  let reply = ref "" in
  let event =
    Engine.
      {
        room;
        sender = admin;
        id = "$wire-" ^ mode;
        body = "Check my synthetic meeting.";
      }
  in
  Engine.handle engine ~direct:true ~send:(fun text -> reply := text) event;
  check "bounded terminal requests"
    (!terminal = if mode = "normal" then 1 else 2);
  check "visible answer"
    (if mode = "http-fallback" then
       String.starts_with ~prefix:"I couldn't turn the tool results" !reply
     else !reply = "The synthetic meeting is at nine.");
  let tools =
    Store.tool_uses store ~day:(Store.today store) ~after:0 ~through:max_int
      ~limit:100
  in
  check "tools never replayed"
    (List.length tools = 6
    && List.for_all (fun (t : Store.tool_use) -> t.status = "ok") tools);
  check "answer committed"
    (List.exists
       (fun (m : Store.message) -> m.role = "assistant" && m.body = !reply)
       (Store.history store ~room ~user:admin));
  check "every request has provenance"
    (match
       field "items" (Inspect.read db ~section:"traces" ~after:0 ~limit:100)
     with
    | Some (Jsont.Array (xs, _)) -> List.length xs = !rounds
    | _ -> false)

let () =
  Eio_main.run @@ fun env ->
  List.iter
    (fun mode -> wire_synthesis env ~mode)
    [ "normal"; "empty"; "http"; "http-fallback" ];
  Eio.Switch.run @@ fun sw ->
  let store = Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin in
  Store.add_room store room;
  let round = ref 0 and reply = ref "" in
  let config = Config.default ~admin ~homeserver:"https://example.test" in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
      ~now:(fun () -> 0.)
      ~complete:(fun messages tools ->
        incr round;
        match !round with
        | 1 ->
            ( None,
              [
                Openrouter.Tool.
                  {
                    id = "fact";
                    name = "memory_store";
                    arguments = {|{"fact":"Office hours start at nine."}|};
                  };
              ] )
        | 2 -> (Some " \n", [])
        | 3 ->
            check "retry is tool-free" (tools = []);
            check "retry retains tool result context" (List.length messages >= 4);
            (Some "Saved your office hours.", [])
        | _ -> failwith "unbounded retry")
  in
  let event =
    Engine.
      {
        room;
        sender = admin;
        id = "$recover";
        body = "Remember my office hours";
      }
  in
  Engine.handle engine ~direct:true ~send:(fun text -> reply := text) event;
  check "empty synthesis recovered"
    (!round = 3 && !reply = "Saved your office hours.");
  let logs =
    Store.tool_uses store ~day:(Store.today store) ~after:0 ~through:max_int
      ~limit:100
  in
  check "write executed once"
    (List.length logs = 1 && (List.hd logs).status = "ok");
  let history = Store.history store ~room ~user:admin in
  check "recovered answer committed"
    (List.exists
       (fun (m : Store.message) -> m.role = "assistant" && m.body = !reply)
       history);
  Engine.handle engine ~direct:true
    ~send:(fun _ -> failwith "duplicate delivery")
    event;
  check "same event does not replay actions" (!round = 3);
  let calls = ref 0 in
  let empty =
    Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
      ~now:(fun () -> 0.)
      ~complete:(fun _ tools ->
        incr calls;
        if !calls = 1 then (None, [])
        else (
          check "recovery tools unavailable" (tools = []);
          ( None,
            [
              Openrouter.Tool.
                {
                  id = "rogue";
                  name = "memory_store";
                  arguments = {|{"fact":"must not execute"}|};
                };
            ] )))
  in
  Engine.handle empty ~direct:true
    ~send:(fun text -> reply := text)
    { event with id = "$empty" };
  check "unwanted recovery calls ignored with fallback"
    (!calls = 2
    && !reply = "I couldn't produce an answer this time. Please try again.");
  check "no extra tool actions"
    (List.length
       (Store.tool_uses store ~day:(Store.today store) ~after:0 ~through:max_int
          ~limit:100)
    = 1);
  print_endline
    "Empty synthesis recovery, fallback, persistence and no action replay \
     passed."
