open Zulip_eio

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let success = {|{"result":"success","msg":""}|}

let read_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let form request = Httpz_media.Urlencoded.decode (read_body request)

let with_client handler run =
  Eio_mock.Backend.run_full @@ fun env ->
  let auth =
    Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test"
      ~api_key:"key"
    |> ok
  in
  let transport =
    Transport.of_fetch ~clock:env#clock (Fetch_mock.client handler)
  in
  let client = Client.create ~transport ~auth () |> ok in
  run client

let decode codec text =
  match Jsont_bytesrw.decode_string codec text with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let has_member name = function
  | Jsont.Object (members, _) ->
      Option.is_some (Jsont.Json.find_mem name members)
  | _ -> false
