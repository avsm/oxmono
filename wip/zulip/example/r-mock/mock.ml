open Zulip_eio

let backend (request : Fetch.Middleware.request) =
  Format.printf "%s %s@."
    (Http.Method.to_string request.meth)
    (Fetch.Middleware.Url.path_and_query request.url);
  Fetch_mock.respond
    ~headers:(Http.Header.of_list [ ("content-type", "application/json") ])
    {|{"result":"success","msg":"","id":42}|} request

let () =
  Eio_main.run @@ fun _env ->
  let transport = Transport.of_fetch (Fetch_mock.client backend) in
  let auth =
    Auth.create ~site:"https://zulip.example" ~email:"bot@zulip.example"
      ~api_key:"mock-key"
    |> Error.or_raise
  in
  let client = Client.create ~transport ~auth () |> Error.or_raise in
  match
    Messages.send_channel client ~channel:"sandbox" ~topic:"OCaml tutorial"
      ~content:"Hello from a mock!" ()
  with
  | Ok message_id ->
      Format.printf "Sent message %a@." Zulip.Id.Message.pp message_id
  | Error error -> Format.eprintf "Send failed: %a@." Error.pp error
