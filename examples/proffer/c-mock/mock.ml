open Proffer
open Proffer.Route

type env = { greeting : string }

let site =
  Site.of_routes
    [ get root (fun env _request respond ->
        Resp.text respond (env.greeting ^ ", world!\n"));
      get (s "echo" / str) (fun word _env _request respond ->
        Resp.text respond (Req.globalize word ^ "\n"));
      post (s "greet") (fun _env request respond ->
        match Req.form_param request "name" with
        | Some name -> Resp.see_other respond ("/hello/" ^ name)
        | None -> Resp.bad_request respond ()) ]

let env = { greeting = "Good evening" }

let show name response =
  Printf.printf "%s: %d %s\n" name
    (Status.code (Proffer_mock.status response))
    (String.escaped (Proffer_mock.body response))

let () =
  show "GET /" (Proffer_mock.request site env Get "/");
  show "GET /echo/hi" (Proffer_mock.request site env Get "/echo/hi");
  show "GET /missing" (Proffer_mock.request site env Get "/missing");
  show "HEAD /" (Proffer_mock.request site env Head "/");
  let response =
    Proffer_mock.request site env Post "/greet"
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
      ~body:"name=alice"
  in
  show "POST /greet" response;
  Printf.printf "  Location: %s\n"
    (Option.get (Proffer_mock.header response Location))
