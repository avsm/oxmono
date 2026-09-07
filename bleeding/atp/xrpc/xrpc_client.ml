(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  service : string;
  http : Fetch.plain;
  max_response_bytes : int;
  mutable session : Xrpc_types.session option;
  on_request : (t -> unit) option;
}

let normalize_service service =
  (match Fetch.Middleware.Url.of_string service with
   | Error _ -> invalid_arg "XRPC service must be an absolute HTTP(S) URL"
   | Ok _ -> ());
  let uri = Uri.of_string service in
  if Uri.userinfo uri <> None || Uri.verbatim_query uri <> None || Uri.fragment uri <> None then
    invalid_arg "XRPC service cannot contain credentials, query, or fragment";
  let service = Uri.to_string (Uri.canonicalize uri) in
  let rec trim n = if n > 0 && service.[n - 1] = '/' then trim (n - 1) else n in
  String.sub service 0 (trim (String.length service))

let of_fetch ~service ?(max_response_bytes = 16 * 1024 * 1024) ?on_request http =
  if max_response_bytes < 0 then invalid_arg "XRPC response limit must be non-negative";
  let service = normalize_service service in
  let http = Fetch.restrict http in
  { service; http;
    max_response_bytes; session = None; on_request }

let create ~sw ~env ~service ?http ?max_response_bytes ?on_request () =
  let http = match http with Some client -> Fetch.restrict client | None -> Fetch_curl.std ~sw env in
  of_fetch ~service ?max_response_bytes ?on_request http

let set_session t session = t.session <- Some session
let clear_session t = t.session <- None
let get_session t = t.session
let get_service t = t.service

let build_url t nsid params =
  if not (Atp.Nsid.is_valid nsid) then invalid_arg "Invalid XRPC NSID";
  Uri.of_string (t.service ^ "/xrpc/" ^ nsid)
  |> fun uri -> Uri.with_query' uri params |> Uri.to_string

let json_accept = Fetch.Header.[ accept, [ pref "application/json" ] ]
let any_accept = Fetch.Header.[ accept, [ pref "*/*" ] ]
let is_success status = status >= 200 && status < 300

(* Snapshot the session after the refresh interceptor, then scope the token to
   this service. Fetch owns credential stripping and response lifetime. *)
let request_client t =
  let http = t.http in
  match t.session with
  | None -> http
  | Some session -> Fetch.with_credentials ~scope:[t.service ^ "/"]
      Fetch.Credential.[Bearer (fun () -> session.access_jwt)] http

let with_errors f =
  try f () with
  | Eio.Io (Xrpc_error.E _, _) as ex -> raise ex
  | Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ }), _) ->
      raise (Xrpc_error.err (Parse_error {
        reason = Fetch.Media.error_to_string error; body_preview = None }))
  | Eio.Io (Fetch.E (Fetch.Invalid_request reason | Fetch.Invalid_url reason | Fetch.Denied reason), _) ->
      raise (Xrpc_error.err (Parse_error { reason; body_preview = None }))
  | Eio.Io _ as ex ->
      raise (Xrpc_error.err (Network_error { reason = Printexc.to_string ex }))

let raise_on_error response =
  let status = Fetch.status response in
  if not (is_success status) then begin
    let body = try Fetch.decode ~limit:(64 * 1024) Fetch.Media.octets response
      with Eio.Io (Fetch.E (Fetch.Decode_failure { error = Too_large _; _ }), _) ->
        "[response exceeds diagnostic limit]" in
    let payload = Fetch.Media.decode (Fetch.Json.v Xrpc_types.error_payload_jsont) body in
    let error, message = match payload with
      | Ok payload -> payload.error, payload.message
      | Error _ -> "UnknownError", Some (String.sub body 0 (min 100 (String.length body))) in
    raise (Xrpc_error.err (Xrpc_error { status; error; message }))
  end

let handle_response t decoder response =
  raise_on_error response;
  Fetch.decode ~limit:t.max_response_bytes (Fetch.Json.v decoder) response

let handle_bytes_response t response =
  raise_on_error response;
  let body = Fetch.decode ~limit:t.max_response_bytes Fetch.Media.octets response in
  let content_type = Option.value ~default:"application/octet-stream"
    (Http.Header.get (Fetch.headers response) "content-type") in
  body, content_type

let request t ~nsid ~params ~headers ?(body = Fetch.Empty) meth handle =
  let url = build_url t nsid params in
  Option.iter (fun f -> f t) t.on_request;
  with_errors @@ fun () ->
  let redirects = if meth = `GET then None else Some 0 in
  Fetch.with_response ~headers ~body ?redirects (request_client t) meth url handle

let query t ~nsid ~params ~decoder =
  request t ~nsid ~params ~headers:json_accept `GET (handle_response t decoder)

let encode_input input input_data =
  match input, input_data with
  | Some codec, Some value -> Fetch.encode (Fetch.Json.v codec) value
  | None, None -> [], Fetch.Empty
  | _ -> invalid_arg "XRPC input codec and value must be supplied together"

let procedure t ~nsid ~params ~input ~input_data ~decoder =
  let headers, body = encode_input input input_data in
  let headers = Fetch.Header.append headers json_accept in
  request t ~nsid ~params ~headers ~body `POST (handle_response t decoder)

let procedure_unit t ~nsid ~params ~input ~input_data =
  let headers, body = encode_input input input_data in
  let headers = Fetch.Header.append headers any_accept in
  request t ~nsid ~params ~headers ~body `POST raise_on_error

let content_type_header mime = Fetch.Header.[raw "Content-Type" mime]

let procedure_blob t ~nsid ~params ~blob ~content_type ~decoder =
  let headers = Fetch.Header.append (content_type_header content_type) json_accept in
  request t ~nsid ~params ~headers ~body:(Fetch.String blob) `POST (handle_response t decoder)

let query_bytes t ~nsid ~params =
  request t ~nsid ~params ~headers:any_accept `GET (handle_bytes_response t)

let procedure_bytes t ~nsid ~params ~body ~content_type =
  let headers, body = match body with
    | Some body -> Fetch.Header.append (content_type_header content_type) any_accept, Fetch.String body
    | None -> any_accept, Fetch.Empty in
  request t ~nsid ~params ~headers ~body `POST @@ fun response ->
  raise_on_error response;
  if Fetch.status response = 204 then None else Some (handle_bytes_response t response)
