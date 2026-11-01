type t = { http : Fetch.plain; base : Uri.t; max_response : int }
type error = Http_status of int | Invalid_response of string

let v ?(max_response = 16 * 1024 * 1024) ?auth http ~url =
  let base = Uri.of_string url in
  if
    (not (List.mem (Uri.scheme base) [ Some "http"; Some "https" ]))
    || Uri.host base = None
    || Uri.userinfo base <> None
    || Uri.query base <> []
    || Uri.fragment base <> None
  then
    invalid_arg
      "Recorder URL must be an HTTP(S) URL without credentials, query or \
       fragment";
  if max_response < 1 then invalid_arg "max_response must be positive";
  let path = Uri.path base in
  let path = if String.ends_with ~suffix:"/" path then path else path ^ "/" in
  let base = Uri.with_path base path in
  let http =
    match auth with
    | None -> Fetch.read_only http
    | Some (user, password) ->
        Fetch.with_credentials
          ~scope:[ Uri.to_string base ]
          ~allow_insecure:true
          [ Fetch.Credential.basic ~user ~password ]
          http
        |> Fetch.read_only
  in
  { http; base; max_response }

let request t path query codec =
  Eio.Switch.run @@ fun sw ->
  let url =
    Uri.with_path t.base (Uri.path t.base ^ "api/0/" ^ path) |> fun uri ->
    Uri.with_query uri query |> Uri.to_string
  in
  let response = Fetch.get ~sw t.http url in
  let status = Fetch.status response in
  if status < 200 || status >= 300 then Error (Http_status status)
  else
    let reader =
      Bytesrw_eio.bytes_reader_of_flow (Fetch.body response)
      |> Bytesrw.Bytes.Reader.limit t.max_response
    in
    try Result.map_error (fun e -> Invalid_response e) (codec reader)
    with Bytesrw.Bytes.Stream.Error error ->
      Error (Invalid_response (Bytesrw.Bytes.Stream.error_message error))

let list_users t = request t "list" [] Owntracks.Recorder.decode_list

let list_devices t ~user =
  request t "list" [ ("user", [ user ]) ] Owntracks.Recorder.decode_list

let locations t ~user ~device ~from_date ~to_date =
  request t "locations"
    [
      ("user", [ user ]);
      ("device", [ device ]);
      ("from", [ from_date ]);
      ("to", [ to_date ]);
    ]
    Owntracks.Recorder.decode_locations

let pp_error ppf = function
  | Http_status status -> Format.fprintf ppf "Recorder HTTP status %d" status
  | Invalid_response message -> Format.fprintf ppf "Recorder JSON: %s" message
