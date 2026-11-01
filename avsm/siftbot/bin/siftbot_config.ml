let () =
  let response =
    try
      match Siftbot_setup.parse_request (Siftbot_setup.read stdin) with
      | Error _ -> Siftbot_setup.Error "Invalid setup request"
      | Ok request -> Eio_main.run (fun env -> Siftbot.Setup.handle env request)
    with _ -> Siftbot_setup.Error "Profile setup failed"
  in
  let raw =
    try Siftbot_setup.response response
    with _ -> Siftbot_setup.response (Error "Too many profiles to display")
  in
  print_string raw
