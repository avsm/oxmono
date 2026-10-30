(* Explicit synthetic-mail probe, excluded from runtest. *)
let () =
  if Array.length Sys.argv <> 3 then
    invalid_arg "Usage: classify_live API_BASE_URL MODEL";
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          let db = Siftbot.Store.memory ~sw () in
          let base = List.hd Siftbot.Demo.messages in
          let messages =
            List.init 8 (fun i ->
                { base with source = { base.source with id = string_of_int i } })
          in
          Siftbot.Store.commit_batch db ~job:"synthetic" ~expected:None
            ~cursor:"1" messages [];
          let profile =
            {
              (Siftbot.Profile.defaults ~name:"synthetic"
                 ~jmap_profile:"synthetic")
              with
              chat_base_url = Some Sys.argv.(1);
              chat_model = Some Sys.argv.(2);
            }
          in
          let model, provider =
            Siftbot.Assistant.completion
              ~json_schema:(Siftbot.Classifier.schema db)
              ~fetch:(Fetch_curl.v ~sw ()) profile
          in
          let active = ref 0 and peak = ref 0 in
          let complete request =
            incr active;
            peak := max !peak !active;
            Fun.protect
              ~finally:(fun () -> decr active)
              (fun () -> provider request)
          in
          let start = Eio.Time.now (Eio.Stdenv.clock env) in
          let count = Siftbot.Classifier.run ~jobs:4 db ~model ~complete in
          assert (count = 8 && !peak = 4 && !active = 0);
          assert (
            Siftbot.Classifier.run ~jobs:4 db ~model ~complete:(fun _ ->
                failwith "Cache miss")
            = 0);
          Printf.printf
            "Classified %d synthetic emails in %.1fs; peak concurrency %d; \
             repeat pass cached.\n"
            count
            (Eio.Time.now (Eio.Stdenv.clock env) -. start)
            !peak))
