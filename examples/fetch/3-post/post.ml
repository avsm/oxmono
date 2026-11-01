let print_body env response =
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env)

let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->

  print_endline "A plain body:";
  Fetch.post ~sw client (base ^ "/echo")
    ~headers:Fetch.Header.[ content_type, media "text/plain" ]
    ~body:(String "Hello, server!\n")
  |> print_body env;

  print_endline "A form:";
  let headers, body =
    Fetch.Form.urlencoded [ ("name", "alice"); ("colour", "blue") ]
  in
  Fetch.post ~sw ~headers ~body client (base ^ "/form") |> print_body env;

  print_endline "A file upload, as the server receives it:";
  let headers, body =
    Fetch.Form.multipart
      [ Fetch.Form.field "name" "alice";
        Fetch.Form.file ~name:"avatar" ~filename:"avatar.txt"
          ~content_type:"text/plain" "Not really a picture.\n" ]
  in
  Fetch.post ~sw ~headers ~body client (base ^ "/echo") |> print_body env
