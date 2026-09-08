let key_query_path = "/_matrix/client/v3/keys/query"

let is_key_query ~homeserver url =
  match Client.Url.append_path homeserver ~path:key_query_path () with
  | Error _ -> false
  | Ok endpoint ->
      Client.Url.same_origin endpoint url
      && Client.Url.path_segments endpoint = Client.Url.path_segments url
      && (not (Client.Url.has_query url))
      && not (Client.Url.has_fragment url)

let retry_request ~homeserver (request : Fetch.Middleware.request) =
  match request.meth with
  | `POST -> is_key_query ~homeserver request.url
  | _ -> true

let v ?max_retries ~homeserver () =
  let homeserver =
    match Client.Url.homeserver homeserver with
    | Ok homeserver -> homeserver
    | Error message -> invalid_arg ("Matrix_client.Http_retry.v: " ^ message)
  in
  Fetch.Retry.v ?max_retries
    ~allowed_methods:(`POST :: Fetch.Retry.default.allowed_methods)
    ~retry_request:(retry_request ~homeserver)
    ()

let default ~homeserver = v ~homeserver ()
