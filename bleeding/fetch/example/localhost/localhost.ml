(* A small Proffer site that the Fetch tutorial talks to, so that every
   example works without a network connection. See ../README.md. *)

open Proffer
open Proffer.Route

type env = { clock : float Eio.Time.clock_ty Eio.Resource.t; attempts : int ref }

let show_headers request =
  let rec format (fields @ local) =
    match fields with
    | [] -> ""
    | (name, value) :: rest ->
        Req.globalize name ^ ": " ^ Req.globalize value ^ "\n" ^ format rest
  in
  let text = format (Headers.to_list (Req.headers request)) in
  text

let show_form request =
  Req.form request
  |> List.map (fun (name, value) -> name ^ " = " ^ value ^ "\n")
  |> String.concat ""

let site =
  Site.of_routes
    [ get (s "hello") (fun _env _request respond ->
        Resp.text respond "Hello from the local server!\n");

      get (s "json") (fun _env _request respond ->
        Resp.media respond "application/json"
          "{\"greeting\": \"hello\", \"from\": \"the local server\"}\n");

      get (s "headers") (fun _env request respond ->
        Resp.text respond (show_headers request));

      post (s "echo") (fun _env request respond ->
        let content_type =
          match Req.header request Content_type with
          | Some value -> Req.globalize value
          | None -> "application/octet-stream"
        in
        Resp.media respond content_type (Req.globalize (Req.body request)));

      post (s "form") (fun _env request respond ->
        Resp.text respond (show_form request));

      post (s "upload") (fun _env request respond ->
        Resp.text respond
          (Printf.sprintf "Received %d bytes.\n" (String.length (Req.body request))));

      moved (s "old") "/hello";

      get (s "todo") (fun _env _request respond ->
        Resp.media respond "application/json"
          "{\"id\": 1, \"title\": \"write the tutorial\", \"done\": false}\n");

      get (s "todos.jsonl") (fun _env _request respond ->
        Resp.media respond "application/jsonl"
          "{\"id\": 1, \"title\": \"write the tutorial\"}\n\
           {\"id\": 2, \"title\": \"read it back\", \"done\": true}\n\
           {\"id\": 3, \"title\": \"ship it\"}\n");

      get (s "about") (fun _env _request respond ->
        Resp.media respond "text/markdown; charset=utf-8"
          "# About\n\nThis server is written with *Proffer*.\n");

      get (s "login") (fun _env _request respond ->
        Resp.v respond ~status:See_other ~content_type:Null
          ~headers:[ Resp.h Set_cookie "session=abc123; Path=/";
                     Resp.h Location "/account" ]
          Body.Empty);

      get (s "account") (fun _env request respond ->
        match Req.header request Cookie with
        | Some cookie -> Resp.text respond ("Logged in with cookie " ^ Req.globalize cookie ^ "\n")
        | None -> Resp.text respond ~status:Unauthorized "Not logged in.\n");

      get (s "secret") (fun _env request respond ->
        match Req.header request Authorization with
        | Some "Bearer letmein" -> Resp.text respond "The secret is 42.\n"
        | _ -> Resp.text respond ~status:Unauthorized "Who are you?\n");

      get (s "flaky") (fun env _request respond ->
        incr env.attempts;
        if !(env.attempts) mod 3 = 0 then Resp.text respond "Third time lucky!\n"
        else
          Resp.text respond ~status:Service_unavailable
            ~headers:[ Resp.h Retry_after "1" ] "Try again later.\n");

      get (s "slow") (fun env _request respond ->
        Eio.Time.sleep env.clock 5.0;
        Resp.text respond "Sorry for the wait.\n");

      get (s "big") (fun _env _request respond ->
        Resp.stream respond "application/octet-stream" @@ fun sink ->
        let chunk = String.make 65536 'x' in
        for _ = 1 to 64 do Body.Sink.write sink chunk done) ]

let log (event : Proffer_httpz.event @ local) =
  Printf.printf "server: %s %s -> %d\n%!"
    (Method.to_string event.meth) (Req.globalize event.target) (Status.code event.status)

let run stdenv f =
  let clock = Eio.Stdenv.clock stdenv in
  let port, set_port = Eio.Promise.create () in
  let on_listening = function
    | `Tcp (_, port) -> Eio.Promise.resolve set_port port
    | `Unix _ -> assert false
  in
  Eio.Fiber.first
    (fun () ->
      Proffer_httpz.run stdenv ~port:0 ~on_listening ~on_event:log
        ~on_error:(function
          | Eio.Cancel.Cancelled _ -> ()
          | exn -> prerr_endline (Printexc.to_string exn))
        ~env:{ clock; attempts = ref 0 } site)
    (fun () -> f (Printf.sprintf "http://127.0.0.1:%d" (Eio.Promise.await port)))
