open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let self = "@crow:example.org"
let room = "!room:example.org"
let event ?(sender = admin) id = Engine.{ room; sender; id; body = "hello" }

let yield () =
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let requests = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        check "typing uses PUT" (req.meth = `PUT);
        check "typing endpoint names bot and source room"
          (Uri.pct_decode (Fetch.Middleware.Url.path_and_query req.url)
          = "/_matrix/client/v3/rooms/!room:example.org/typing/@crow:example.org"
          );
        let body =
          match req.body with Fetch.String s -> s | _ -> assert false
        in
        let codec =
          Jsont.Object.map (fun typing timeout -> (typing, timeout))
          |> Jsont.Object.mem "typing" Jsont.bool
          |> Jsont.Object.opt_mem "timeout" Jsont.int
          |> Jsont.Object.finish
        in
        let typing, timeout =
          Result.get_ok (Jsont_bytesrw.decode_string codec body)
        in
        check "typing expires after thirty seconds"
          (timeout = if typing then Some 30000 else None);
        requests := !requests @ [ typing ];
        Fetch_mock.respond "{}" req)
  in
  let client =
    Matrix_eio.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://matrix.example.org")
      ~fetch ()
    |> fun client ->
    Matrix_eio.Client.with_session client
      {
        user_id = Matrix_proto.Id.User_id.of_string_exn self;
        device_id = Matrix_proto.Id.Device_id.of_string_exn "CROW";
        access_token = "fixture-token";
        refresh_token = None;
      }
  in
  let set typing =
    Matrix_eio.Typing.set_typing client
      ~room_id:(Matrix_proto.Id.Room_id.of_string_exn room)
      ~typing
      ?timeout:(if typing then Some 30000 else None)
      ()
  in
  Typing.with_session ~clock ~room ~event:"$refresh" ~set (fun typing ->
      check "typing stays off before acceptance" (!requests = []);
      Typing.start typing;
      Typing.start typing;
      check "accepted request starts typing once" (!requests = [ true ]);
      yield ();
      Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 15000000000L);
      yield ();
      check "typing refreshes during processing" (!requests = [ true; true ]);
      Typing.stop typing;
      check "typing cleared before delivery" (!requests = [ true; true; false ]);
      Typing.stop typing);
  Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 60000000000L);
  yield ();
  check "no refresh survives stop" (!requests = [ true; true; false ]);
  let emitted = ref [] in
  let set value = emitted := !emitted @ [ value ] in
  let raises f =
    try
      f ();
      false
    with Failure _ -> true
  in
  check "processing failure propagates"
    (raises (fun () ->
         Typing.with_session ~clock ~room ~event:"$failure" ~set (fun typing ->
             Typing.start typing;
             failwith "model failed")));
  check "failure clears typing" (!emitted = [ true; false ]);
  emitted := [];
  let pending, _ = Eio.Promise.create () in
  check "cancellation during initial notification propagates"
    (try
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.02 (fun () ->
           Typing.with_session ~clock ~room ~event:"$cancel-start"
             ~set:(fun typing ->
               set typing;
               if typing then Eio.Promise.await pending)
             (fun typing -> Typing.start typing));
       false
     with Eio.Time.Timeout -> true);
  check "cancellation clears an in-flight initial notification"
    (!emitted = [ true; false ]);
  emitted := [];
  Typing.with_session ~clock ~room ~event:"$cancel-refresh"
    ~set:(fun typing ->
      set typing;
      if typing && List.length !emitted = 2 then Eio.Promise.await pending)
    (fun typing ->
      Typing.start typing;
      yield ();
      Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 75000000000L);
      yield ();
      check "refresh in flight" (!emitted = [ true; true ]);
      Typing.stop typing);
  check "in-flight refresh is joined before clearing"
    (!emitted = [ true; true; false ]);
  emitted := [];
  Typing.with_session ~clock ~room ~event:"$typing-unavailable"
    ~set:(fun value ->
      set value;
      failwith "private server text")
    (fun typing ->
      Typing.start typing;
      Typing.stop typing);
  check "typing failures do not prevent delivery" (!emitted = [ true; false ]);
  emitted := [];
  let clearing, clear_started = Eio.Promise.create () in
  let clear_finished = ref false in
  Eio.Fiber.both
    (fun () ->
      Typing.with_session ~clock ~room ~event:"$clear-timeout"
        ~set:(fun value ->
          set value;
          if not value then begin
            Eio.Promise.resolve clear_started ();
            Eio.Promise.await pending
          end)
        (fun typing ->
          Typing.start typing;
          Typing.stop typing;
          clear_finished := true))
    (fun () ->
      Eio.Promise.await clearing;
      Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 80000000000L));
  check "clearing timeout is bounded and allows delivery"
    (!clear_finished && !emitted = [ true; false ]);
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create db ~admin in
  let tools_used = ref 0 in
  let plugin =
    Plugin.
      {
        name = "step";
        description = "test";
        run =
          (fun ~query:_ ->
            incr tools_used;
            "ok");
      }
  in
  let config =
    {
      (Config.default ~admin ~homeserver:"https://matrix.example.org") with
      plugins = [ "step" ];
    }
  in
  List.iter
    (fun count ->
      let round = ref 0 in
      let complete _ tools =
        incr round;
        if !round <= count then begin
          check "tools available through sixth call" (tools <> []);
          ( None,
            [
              Openrouter.Tool.
                { id = string_of_int !round; name = "step"; arguments = "{}" };
            ] )
        end
        else begin
          if count = 6 then check "final synthesis has no tools" (tools = []);
          (Some "done", [])
        end
      in
      let engine =
        Engine.create ~config ~store ~self ~plugins:[ plugin ] ~complete
          ~now:(fun () -> 0.)
      in
      emitted := [];
      let delivered = ref false in
      let request = event (Printf.sprintf "$tools-%d" count) in
      let handle request =
        Typing.with_session ~clock ~room ~event:request.Engine.id ~set
          (fun typing ->
            Engine.handle engine ~direct:true
              ~on_accept:(fun () -> Typing.start typing)
              ~send:(fun text ->
                Typing.stop typing;
                check "delivery follows typing clear"
                  (!emitted = [ true; false ]);
                check "synthesized answer" (text = "done");
                delivered := true)
              request)
      in
      handle request;
      check "four and six step tool turns complete"
        (!delivered && !round = count + 1);
      let before = !emitted in
      handle request;
      handle (event ~sender:"@unknown:example.org" "$unknown");
      check "duplicates and unapproved accounts do not emit typing"
        (!emitted = before))
    [ 4; 6 ];
  check "six-call budget executes every requested tool" (!tools_used = 10);
  let entered, enter = Eio.Promise.create () in
  let release, released = Eio.Promise.create () in
  let models = ref 0 and accepted = ref 0 and delivered = ref 0 in
  let engine =
    Engine.create ~config ~store ~self ~plugins:[ plugin ]
      ~now:(fun () -> 0.)
      ~complete:(fun _ _ ->
        incr models;
        if !models = 1 then begin
          Eio.Promise.resolve enter ();
          Eio.Promise.await release
        end;
        (Some "answer", []))
  in
  let run id =
    Engine.handle engine ~direct:true
      ~on_accept:(fun () -> incr accepted)
      ~send:(fun _ -> incr delivered)
      (event id)
  in
  Eio.Fiber.both
    (fun () -> run "$first")
    (fun () ->
      Eio.Promise.await entered;
      Eio.Fiber.both
        (fun () -> run "$second")
        (fun () ->
          yield ();
          check "waiting request is not accepted early"
            (!models = 1 && !accepted = 1);
          Eio.Promise.resolve released ()));
  check "queued follow-up is delivered without delay or loss"
    (!models = 2 && !accepted = 2 && !delivered = 2);
  let friend = "@friend:example.org" in
  Store.set_person store ~actor:admin ~user:friend ~role:Friend ~allowed:true;
  let before = !tools_used in
  check "revocation during acceptance prevents command effects"
    (raises (fun () ->
         Engine.handle engine ~direct:true
           ~on_accept:(fun () ->
             Store.set_person store ~actor:admin ~user:friend ~role:Friend
               ~allowed:false)
           ~send:(fun _ -> failwith "unexpected delivery")
           {
             (event ~sender:friend "$revoke-during-accept") with
             body = "step hi";
           })
    && !tools_used = before);
  print_endline
    "crowthebot: six-call turns, immediate follow-ups and typing lifecycle \
     passed"
