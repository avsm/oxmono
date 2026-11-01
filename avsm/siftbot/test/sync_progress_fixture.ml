let () =
  Eio_main.run (fun env ->
      let clock = Eio.Stdenv.mono_clock env in
      try
        let has arg = Array.exists (( = ) arg) Sys.argv in
        if has "classify" then (
          let expected = if has "empty" then 0 else 2 in
          let result =
            Cli_progress.classify ~clock ~enabled:true
              (fun ~on_progress ~on_classified ->
                on_progress
                  Siftbot.Classifier.
                    { completed = 0; total = expected; remaining = expected };
                Eio.Time.Mono.sleep clock 0.05;
                if has "interrupt" then Eio.Fiber.await_cancel ();
                for completed = 1 to expected do
                  let m = List.hd Siftbot.Demo.messages in
                  on_classified ~subject:"Synthetic subject \027[2J\nnext"
                    (List.map
                       (fun (dim, value) ->
                         Siftbot_model.
                           {
                             target = m.source;
                             conversation = false;
                             dimension = dim;
                             value;
                             origin = Model;
                             version = "test";
                             source_revision = m.revision;
                             explanation = "Evidence";
                             evidence = [ m.source ];
                             score = Some 0.8;
                           })
                       Siftbot_model.
                         [
                           (Triage, "relevant");
                           (Delivery, "direct");
                           (Attention, "needs-reply");
                         ]);
                  on_progress
                    Siftbot.Classifier.
                      {
                        completed;
                        total = expected;
                        remaining = expected - completed;
                      };
                  Eio.Time.Mono.sleep clock 0.05
                done;
                expected)
          in
          assert (result = (expected, 0));
          print_endline "Complete")
        else
          let result =
            Cli_progress.sync ~clock ~enabled:true (fun ~on_progress ->
                let update phase listed total pending_threads =
                  on_progress
                    Siftbot.Import.{ phase; listed; total; pending_threads };
                  Eio.Time.Mono.sleep clock 0.05
                in
                update Listing 0 (Some 200) 0;
                if has "interrupt" then Eio.Fiber.await_cancel ();
                update Listing 100 (Some 200) 2;
                update Listing 200 (Some 200) 3;
                update Hydrating 200 (Some 200) 3;
                update Hydrating 200 (Some 200) 0;
                update Catching_up 200 (Some 200) 0;
                update Idle 200 (Some 200) 0;
                Siftbot.Import.Complete)
          in
          assert (result = Siftbot.Import.Complete);
          print_endline "Complete"
      with Sys.Break -> print_endline "Interrupted")
