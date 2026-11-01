type method_ = [ `GET | `POST | `PUT | `DELETE ]

type response = {
  status : int;
  etag : Fetch.Header.etag option;
  content_type : Fetch.Header.media_type option;
  body : string;
  expires_at : Ptime.t option;
}

type transport = {
  request :
    method_:method_ ->
    uri:Uriz.t ->
    headers:Fetch.Header.headers ->
    body:string option ->
    (response, Error.t) result;
  now : unit -> Ptime.t;
  sleep : float -> unit;
}

type error =
  | Transport_error of Error.t
  | Http_error of { status : int; body : string }
  | Invalid_response of string
  | Invalid_url of string
  | Missing_etag
  | Invalid_content_type of string
  | Empty_message
  | Expired
  | Closed

type lifecycle = Active | Closed_state | Expired_state

type t = {
  transport : transport;
  rendezvous_url : Uriz.t;
  mutable etag : Fetch.Header.etag;
  mutable expires_at : Ptime.t option;
  mutable lifecycle : lifecycle;
}

let validate_url uri =
  match Client.Url.of_uri uri with
  | Ok url when not (Client.Url.has_fragment url) -> Ok url
  | Ok _ | Error _ -> Error (Invalid_url (Uriz.to_string uri))

let validate_url_string value =
  match Client.Url.of_string value with
  | Ok url when not (Client.Url.has_fragment url) -> Ok url
  | Ok _ | Error _ -> Error (Invalid_url value)

let response_error (response : response) =
  if response.status >= 200 && response.status < 300 then Ok ()
  else Error (Http_error { status = response.status; body = response.body })

let terminal_status status = status = 404 || status = 410

let require_etag (response : response) =
  match response.etag with
  | Some ({ tag; _ } as etag) when tag <> "" -> Ok etag
  | _ -> Error Missing_etag

let plain_text_headers = Fetch.Header.[ (content_type, media "text/plain") ]

let if_match_headers validator =
  Fetch.Header.[ (if_match, `Etags [ validator ]) ]

let if_none_match_headers validator =
  Fetch.Header.[ (if_none_match, `Etags [ validator ]) ]

let http_date_to_ptime value =
  let buffer = Bytes.of_string value in
  let length = Bytes.length buffer in
  if length > 32767 then None else
  let span = Httpz.Span.make ~off:#0S
      ~len:(Stdlib_stable.Int16_u.of_int length) in
  match Httpz.Date.parse buffer span with
  | #(Httpz.Date.Valid, seconds) ->
      Ptime.of_float_s (Stdlib_upstream_compatible.Float_u.to_float seconds)
  | #(Httpz.Date.Invalid, _) -> None

(* The released Fetch codec tolerated the common unquoted ETag mistake.  A
   rendezvous validator is a protocol concurrency boundary, so require the
   RFC 9110 wire grammar before using the typed codec.  Keep this check even
   with newer strict Fetch releases so the Matrix invariant is local. *)
let valid_etag_wire value =
  let value = String.trim value in
  let length = String.length value in
  let first, last =
    if length >= 2 && value.[0] = '"' then (1, length - 1)
    else if length >= 4 && value.[0] = 'W' && value.[1] = '/' && value.[2] = '"'
    then (3, length - 1)
    else (-1, -1)
  in
  first >= 0
  && value.[last] = '"'
  && String.for_all
       (fun byte ->
         let code = Char.code byte in
         code = 0x21 || (code >= 0x23 && code <= 0x7e) || code >= 0x80)
       (String.sub value first (last - first))

let response_etag headers =
  match Http.Header.get headers "etag" with
  | Some value when valid_etag_wire value ->
      Fetch.Header.decode Fetch.Header.etag value
  | Some _ | None -> None

let create_response_jsont =
  Jsont.Object.(
    map Fun.id |> mem "url" Matrix_proto.Json.Codec.string ~enc:Fun.id |> finish)

let min_expiry left right =
  match (left, right) with
  | None, value | value, None -> value
  | Some left, Some right ->
      Some (if Ptime.compare left right <= 0 then left else right)

let expiry_due transport expires_at =
  match expires_at with
  | Some expires_at -> Ptime.compare (transport.now ()) expires_at >= 0
  | None -> false

let apply_expiry (t : t) (response : response) =
  t.expires_at <- min_expiry t.expires_at response.expires_at;
  if expiry_due t.transport t.expires_at then begin
    t.lifecycle <- Expired_state;
    Error Expired
  end
  else Ok ()

let active (t : t) =
  match t.lifecycle with
  | Closed_state -> Error Closed
  | Expired_state -> Error Expired
  | Active ->
      if expiry_due t.transport t.expires_at then begin
        t.lifecycle <- Expired_state;
        Error Expired
      end
      else Ok ()

let request transport ~method_ ~uri ~headers ~body =
  match transport.request ~method_ ~uri ~headers ~body with
  | Ok response -> Ok response
  | Error error -> Error (Transport_error error)

let create transport ~rendezvous_server ?expires_at () =
  match validate_url rendezvous_server with
  | Error _ as error -> error
  | Ok rendezvous_server_url -> (
      let rendezvous_server = Client.Url.to_uri rendezvous_server_url in
      match
        request transport ~method_:`POST ~uri:rendezvous_server
          ~headers:plain_text_headers ~body:(Some "")
      with
      | Error _ as error -> error
      | Ok response -> (
          match response_error response with
          | Error error -> Error error
          | Ok () -> (
              match require_etag response with
              | Error error -> Error error
              | Ok etag -> (
                  match
                    Client.Http.decode_response create_response_jsont
                      response.body
                  with
                  | Error error ->
                      Error (Invalid_response (Error.to_string error))
                  | Ok rendezvous_url -> (
                      match validate_url_string rendezvous_url with
                      | Error _ as error -> error
                      | Ok rendezvous_url ->
                          let rendezvous_url =
                            Client.Url.to_uri rendezvous_url
                          in
                          let channel =
                            {
                              transport;
                              rendezvous_url;
                              etag;
                              expires_at =
                                min_expiry expires_at response.expires_at;
                              lifecycle = Active;
                            }
                          in
                          if expiry_due transport channel.expires_at then
                            Error Expired
                          else Ok channel)))))

let accept transport ~rendezvous_url ?expires_at () =
  match validate_url rendezvous_url with
  | Error _ as error -> error
  | Ok rendezvous_url -> (
      let rendezvous_url = Client.Url.to_uri rendezvous_url in
      match
        request transport ~method_:`GET ~uri:rendezvous_url ~headers:[]
          ~body:None
      with
      | Error _ as error -> error
      | Ok response -> (
          match response_error response with
          | Error error -> Error error
          | Ok () -> (
              match require_etag response with
              | Error error -> Error error
              | Ok etag ->
                  let channel =
                    {
                      transport;
                      rendezvous_url;
                      etag;
                      expires_at = min_expiry expires_at response.expires_at;
                      lifecycle = Active;
                    }
                  in
                  if expiry_due transport channel.expires_at then Error Expired
                  else Ok (channel, response.body))))

let rendezvous_url t = t.rendezvous_url

let status t =
  ignore (active t : (unit, error) result);
  match t.lifecycle with
  | Active -> `Active
  | Closed_state -> `Closed
  | Expired_state -> `Expired

let send t body =
  match active t with
  | Error _ as error -> error
  | Ok () -> (
      match
        request t.transport ~method_:`PUT ~uri:t.rendezvous_url
          ~headers:
            (Fetch.Header.append (if_match_headers t.etag) plain_text_headers)
          ~body:(Some body)
      with
      | Error _ as error -> error
      | Ok response -> (
          match response_error response with
          | Error error ->
              if terminal_status response.status then
                t.lifecycle <- Closed_state;
              Error error
          | Ok () -> (
              match require_etag response with
              | Error error -> Error error
              | Ok etag ->
                  t.etag <- etag;
                  apply_expiry t response)))

let receive t =
  let rec loop () =
    match active t with
    | Error _ as error -> error
    | Ok () -> (
        match
          request t.transport ~method_:`GET ~uri:t.rendezvous_url
            ~headers:(if_none_match_headers t.etag)
            ~body:None
        with
        | Error _ as error -> error
        | Ok response -> (
            if terminal_status response.status then begin
              t.lifecycle <- Closed_state;
              Error
                (Http_error { status = response.status; body = response.body })
            end
            else
              match require_etag response with
              | Error error -> Error error
              | Ok etag -> (
                  t.etag <- etag;
                  match apply_expiry t response with
                  | Error _ as error -> error
                  | Ok () -> (
                      match response.status with
                      | 304 ->
                          t.transport.sleep 1.;
                          loop ()
                      | 200 -> (
                          match response.content_type with
                          | Some { media = "text/plain"; _ }
                            when response.body <> "" ->
                              Ok response.body
                          | Some content_type ->
                              Error
                                (Invalid_content_type
                                   (Fetch.Header.encode
                                      Fetch.Header.content_type content_type))
                          | None when response.body <> "" -> Ok response.body
                          | None -> Error Empty_message)
                      | status ->
                          Error (Http_error { status; body = response.body }))))
        )
  in
  loop ()

let close t =
  match t.lifecycle with
  | Closed_state | Expired_state ->
      t.lifecycle <- Closed_state;
      Ok ()
  | Active -> (
      match
        request t.transport ~method_:`DELETE ~uri:t.rendezvous_url
          ~headers:(if_match_headers t.etag) ~body:None
      with
      | Error _ as error -> error
      | Ok response ->
          if response_error response = Ok () || terminal_status response.status
          then begin
            t.lifecycle <- Closed_state;
            Ok ()
          end
          else
            Error
              (Http_error { status = response.status; body = response.body }))

let transport_of_client ~sleep ?(now = fun () -> Ptime_clock.now ()) client =
  {
    request =
      (fun ~method_ ~uri ~headers ~body ->
        match Client.Url.of_uri uri with
        | Error reason ->
            Error
              (Error.Policy_denied ("invalid MSC4108 rendezvous URL: " ^ reason))
        | Ok url
          when not (Client.Url.same_origin url (Client.homeserver_url client))
          ->
            Error
              (Error.Policy_denied
                 "MSC4108 rendezvous URL is outside the homeserver origin")
        | Ok url ->
            let meth =
              match method_ with
              | `GET -> `GET
              | `POST -> `POST
              | `PUT -> `PUT
              | `DELETE -> `DELETE
            in
            Result.map
              (fun (response : Client.Http.raw_response) ->
                {
                  status = response.status;
                  etag = response_etag response.headers;
                  content_type =
                    Fetch.Header.get Fetch.Header.content_type response.headers;
                  body = response.body;
                  expires_at =
                    Option.bind
                      (Fetch.Header.get Fetch.Header.expires response.headers)
                      http_date_to_ptime;
                })
              (Client.Http.request_url_unauthenticated client ~meth ~url
                 ~headers ?body ()));
    now;
    sleep;
  }
