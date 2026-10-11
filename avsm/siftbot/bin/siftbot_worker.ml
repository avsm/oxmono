open Cmdliner

let run profile demo =
  let response =
    try
      let raw = Siftbot_protocol.read stdin in
      match Siftbot_protocol.parse_request raw with
      | Error e -> Error e
      | Ok request ->
          Eio_main.run (fun env ->
              Eio.Switch.run (fun sw ->
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60.
                    (fun () ->
                      let name = if demo then "synthetic-demo" else profile in
                      let db =
                        Siftbot.Store.open_db ~sw
                          Eio.Path.(
                            Eio.Stdenv.fs env / Siftbot.Profile.data_path name)
                      in
                      if demo then Siftbot.Demo.seed db;
                      match request with
                      | Siftbot_model.Inspect query ->
                          Ok (Siftbot.Store.snapshot db ~query)
                      | Discuss (question, selected) ->
                          if demo then
                            invalid_arg
                              "Demo discussion uses no model credentials";
                          ignore
                            (Siftbot.Assistant.configured ~sw
                               ~fs:(Eio.Stdenv.fs env) db
                               (Siftbot.Profile.load profile)
                               ~selected question);
                          Ok (Siftbot.Store.snapshot db ~query:"")
                      | Edit (revision, edit, explanation, query) ->
                          Siftbot.Store.edit db ~revision edit ~explanation;
                          Ok (Siftbot.Store.snapshot db ~query))))
    with
    | Siftbot.Store.Conflict -> Error "Revision conflict. Refresh and retry."
    | Invalid_argument e | Failure e -> Error e
    | _ -> Error "Worker operation failed. Check the profile and database."
  in
  let raw =
    try Siftbot_protocol.response response
    with _ ->
      Siftbot_protocol.response
        (Error "Response exceeds the worker limit. Narrow the search.")
  in
  print_string raw

let () =
  exit
    (Cmd.eval
       (Cmd.v
          (Cmd.info "siftbot-worker")
          Term.(
            const run
            $ Arg.(value & opt string "personal" & info [ "profile" ])
            $ Arg.(value & flag & info [ "demo" ]))))
