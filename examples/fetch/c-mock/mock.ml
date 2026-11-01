let greeting client = String.trim (Fetch.read client "https://example.com/hello")

let canned (request : Fetch.Middleware.request) =
  Printf.printf "The mock saw %s %s\n"
    (Http.Method.to_string request.meth)
    (Fetch.Middleware.Url.to_string request.url);
  Fetch_mock.respond "Hello from the mock!" request

let () =
  Eio_mock.Backend.run @@ fun () ->
  let client = Fetch_mock.client canned in
  Printf.printf "greeting returned %S\n" (greeting client)
