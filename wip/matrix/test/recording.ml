type recorded = {
  meth : string;
  url : string;
  headers : Http.Header.t;
  body : string option;
}

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let record log (req : Fetch.Middleware.request) =
  log :=
    {
      meth = Http.Method.to_string req.meth;
      url = Fetch.Middleware.Url.to_string req.url;
      headers = req.headers;
      body = body_of_request req;
    }
    :: !log

let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        handler req)
  in
  (log, client)
