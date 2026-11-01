module Url = Fetch.Middleware.Url

type meth = [ `GET | `POST | `PUT | `PATCH | `DELETE ]

type t = {
  transport : Transport.t;
  auth : Auth.t;
  fetch : Fetch.plain;
  base : Url.t;
  timeout : float option;
  max_body : int;
  allow_insecure : bool;
  user_agent : string;
}

let ( let* ) = Result.bind
let finite_positive value = Float.is_finite value && value > 0.

let authenticate ~auth ~allow_insecure fetch =
  let url = Url.of_string (Auth.site auth) |> Result.get_ok in
  Fetch.with_credentials
    ~scope:[ Url.origin url ]
    ~allow_insecure ~extend:false
    [ Auth.credential auth ]
    fetch

let create ?timeout ?(max_body = 16 * 1024 * 1024) ?(allow_insecure = false)
    ?(user_agent = "ocaml-zulip/fetch") ~transport ~auth () =
  if
    user_agent = ""
    || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) user_agent
  then
    Error
      (Error.Invalid_request
         "user_agent must be nonempty and contain no controls")
  else if max_body <= 0 || max_body = max_int then
    Error
      (Error.Invalid_request "max_body must be positive and less than max_int")
  else if Option.exists (fun x -> not (finite_positive x)) timeout then
    Error (Error.Invalid_request "timeout must be finite and positive")
  else if Option.is_some timeout && Option.is_none (Transport.clock transport)
  then
    Error
      (Error.Invalid_request "an explicit timeout requires a transport clock")
  else
    let site = Auth.site auth in
    let base =
      if String.ends_with ~suffix:"/api/v1" site then site
      else if String.ends_with ~suffix:"/api" site then site ^ "/v1"
      else site ^ "/api/v1"
    in
    let* base =
      Url.of_string base |> Result.map_error (fun e -> Error.Invalid_request e)
    in
    if Url.scheme base = `Http && not allow_insecure then
      Error
        (Error.Invalid_request "HTTP authentication requires allow_insecure")
    else
      let timeout =
        match (timeout, Transport.clock transport) with
        | None, Some _ -> Some 30.
        | _ -> timeout
      in
      try
        Ok
          {
            transport;
            auth;
            base;
            timeout;
            max_body;
            allow_insecure;
            user_agent;
            fetch =
              authenticate ~auth ~allow_insecure (Transport.fetch transport);
          }
      with Invalid_argument reason -> Error (Error.Invalid_request reason)

let transport t = t.transport
let auth t = t.auth
let site t = Auth.site t.auth
let user_agent t = t.user_agent
let path_segment = Httpz_uri.percent_encode ~component:`Path_segment

let api_url t path =
  let path =
    if path = "/api/v1" then ""
    else if String.starts_with ~prefix:"/api/v1/" path then
      String.sub path 8 (String.length path - 8)
    else if String.starts_with ~prefix:"/" path then
      String.sub path 1 (String.length path - 1)
    else path
  in
  if String.contains path '?' || String.contains path '#' then
    Error
      (Error.Invalid_request
         "Pass query parameters separately from the endpoint path")
  else
    let* url =
      Url.of_string (Url.to_string t.base ^ "/" ^ path)
      |> Result.map_error (fun e -> Error.Invalid_request e)
    in
    if Url.under ~prefix:t.base url then Ok url
    else Error (Error.Invalid_request "Endpoint path escapes the API prefix")

let field name = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        members
  | _ -> None

let string_field name json =
  match field name json with Some (Jsont.String (s, _)) -> Some s | _ -> None

let delay = function
  | Some (Jsont.Number (n, _)) when Float.is_finite n && n >= 0. -> Some n
  | _ -> None

let retry_header t response =
  match Fetch.header Fetch.Header.retry_after response with
  | Some (`Seconds value) -> Some (float value)
  | Some (`Date date) -> (
      match Transport.clock t.transport with
      | None -> None
      | Some clock -> (
          let now = Eio.Time.now clock in
          let bytes = Bytes.of_string date in
          let length = Bytes.length bytes in
          if length > 32767 then None else
          let span = Httpz.Span.make ~off:#0S
              ~len:(Stdlib_stable.Int16_u.of_int length) in
          match Httpz.Date.parse ~now bytes span with
          | #(Httpz.Date.Valid, time) ->
              Some (max 0.
                (Stdlib_upstream_compatible.Float_u.to_float time -. now))
          | _ -> None))
  | None -> None

let invalid_json message =
  Error.Json
    (Jsont.Error.make_msg Jsont.Error.Context.empty Jsont.Meta.none message)

let response_json t response =
  let status = Fetch.status response in
  let retry_after = retry_header t response in
  let text =
    try
      Some
        Eio.Buf_read.(
          take_all (of_flow ~max_size:(t.max_body + 1) (Fetch.body response)))
    with Eio.Buf_read.Buffer_limit_exceeded -> None
  in
  match text with
  | None ->
      Error
        (Error.Http
           {
             status;
             message = "Response exceeds configured body limit";
             retry_after;
           })
  | Some text when String.length text > t.max_body ->
      Error
        (Error.Http
           {
             status;
             message = "Response exceeds configured body limit";
             retry_after;
           })
  | Some text -> (
      match Fetch.Json.decode_string' Jsont.json text with
      | Error error when status >= 200 && status < 300 ->
          Error (Error.Json error)
      | Error _ ->
          Error
            (Error.Http
               {
                 status;
                 message = "Server returned a non-JSON error response";
                 retry_after;
               })
      | Ok json -> (
          let retry_after =
            match retry_after with
            | Some _ -> retry_after
            | None -> delay (field "retry-after" json)
          in
          match string_field "result" json with
          | Some "error" ->
              Error
                (Error.Api
                   {
                     status;
                     code =
                       Option.value (string_field "code" json)
                         ~default:"UNKNOWN";
                     message =
                       Option.value (string_field "msg" json)
                         ~default:"Zulip request failed";
                     extra = json;
                     retry_after;
                   })
          | _ when status < 200 || status >= 300 ->
              Error
                (Error.Http
                   {
                     status;
                     retry_after;
                     message =
                       Option.value (string_field "msg" json)
                         ~default:"Unexpected HTTP status";
                   })
          | Some "success" -> Ok json
          | _ -> Error (invalid_json "Missing or invalid Zulip result field")))

let redirect method_ =
  Fetch.Redirect.v ~max_hops:5
    ~on_hop:(fun ~from ~to_ _ ->
      if method_ = `GET && Url.same_origin from to_ then Fetch.Redirect.Follow
      else Fetch.Redirect.Stop)
    ()

let protect t timeout f =
  if Option.exists (fun value -> not (finite_positive value)) timeout then
    Error (Error.Invalid_request "timeout must be finite and positive")
  else if Option.is_some timeout && Option.is_none (Transport.clock t.transport)
  then
    Error
      (Error.Invalid_request "an explicit timeout requires a transport clock")
  else
    let action () =
      match (timeout, Transport.clock t.transport) with
      | Some seconds, Some clock -> (
          try Eio.Time.with_timeout_exn clock seconds f
          with Eio.Time.Timeout -> Error (Error.Timeout seconds))
      | _ -> f ()
    in
    try
      match Error.catch action with
      | Ok result -> result
      | Error error -> Error error
    with
    | Eio.Buf_read.Buffer_limit_exceeded ->
        Error
          (Error.Transport
             (Fetch.Protocol_error "Response exceeds configured body limit"))
    | Invalid_argument message -> Error (Error.Invalid_request message)

let execute t ~method_ ~url ~headers ~body ?timeout ?(longpoll = false) () =
  let timeout = match timeout with Some _ -> timeout | None -> t.timeout in
  protect t timeout (fun () ->
      let fetch =
        if longpoll then
          authenticate ~auth:t.auth ~allow_insecure:t.allow_insecure
            (Transport.poll_fetch t.transport
               ~timeout:(Option.value timeout ~default:90.))
        else t.fetch
      in
      let headers = Fetch.Header.((user_agent, t.user_agent) :: headers) in
      Fetch.with_response ~headers ~body ~redirect:(redirect method_) fetch
        (method_ :> Http.Method.t)
        (Url.to_string url) (response_json t))

let request t ~method_ ~path ?(params = []) ?timeout ?longpoll () =
  let* url = api_url t path in
  let url, headers, body =
    if method_ = `GET then
      (Url.set_query_params url params, Fetch.Header.[], Fetch.Empty)
    else
      let headers, body = Fetch.Form.urlencoded params in
      (url, headers, body)
  in
  execute t ~method_ ~url ~headers ~body ?timeout ?longpoll ()

let request_typed t ~method_ ~path ?params ?timeout ?longpoll ~codec () =
  let* json = request t ~method_ ~path ?params ?timeout ?longpoll () in
  Codec.decode codec json

let request_json t ~method_ ~path ?(params = []) ?timeout ?longpoll () =
  let rec encode = function
    | [] -> Ok []
    | (_, None) :: rest -> encode rest
    | (name, Some value) :: rest ->
        let* value =
          match value with
          | Jsont.String (text, _) -> Ok text
          | json -> Codec.encode Jsont.json json
        in
        let* rest = encode rest in
        Ok ((name, value) :: rest)
  in
  let* params = encode params in
  request t ~method_ ~path ~params ?timeout ?longpoll ()

let multipart t ~path ?(method_ = `POST) ?(params = []) ?timeout parts =
  let* url = api_url t path in
  try
    let fields =
      List.map (fun (name, value) -> Fetch.Form.field name value) params
    in
    let headers, body = Fetch.Form.multipart (fields @ parts) in
    execute t ~method_ ~url ~headers ~body ?timeout ()
  with Invalid_argument message -> Error (Error.Invalid_request message)

let download t ~url sink =
  let base = Url.of_string (Auth.site t.auth ^ "/") |> Result.get_ok in
  let* target =
    Url.resolve ~base url |> Result.map_error (fun e -> Error.Invalid_request e)
  in
  if not (Url.same_origin base target) then
    Error
      (Error.Invalid_request
         "Download URL is not on the configured Zulip origin")
  else
    let redirect =
      Fetch.Redirect.v ~max_hops:5
        ~on_hop:(fun ~from ~to_ _ ->
          if
            Url.scheme to_ = `Https
            || (t.allow_insecure && Url.scheme from = `Http)
          then Fetch.Redirect.Follow
          else Fetch.Redirect.Stop)
        ()
    in
    protect t t.timeout (fun () ->
        Fetch.with_response ~redirect
          ~headers:[ (Fetch.Header.user_agent, t.user_agent) ]
          t.fetch `GET (Url.to_string target)
          (fun response ->
            if Fetch.status response < 200 || Fetch.status response >= 300 then
              Result.map (Fun.const ()) (response_json t response)
            else (
              Eio.Flow.copy (Fetch.body response) sink;
              Ok ())))

let pp ppf t = Format.fprintf ppf "Zulip client(%s)" (Auth.site t.auth)
