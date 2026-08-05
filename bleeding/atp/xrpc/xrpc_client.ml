(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  service : string;
  http : Fetch.plain;
  mutable session : Xrpc_types.session option;
  on_request : (t -> unit) option;
}

let create ~sw ~env ~service ?http ?on_request () =
  let http =
    match http with
    | Some client -> Fetch.restrict client
    | None -> Fetch_curl.std ~sw env
  in
  { service; http; session = None; on_request }

let set_session t session = t.session <- Some session
let clear_session t = t.session <- None
let get_session t = t.session
let get_service t = t.service

(* Build XRPC URL: /xrpc/{nsid}?params *)
let build_url t nsid params =
  let base = t.service ^ "/xrpc/" ^ nsid in
  match params with
  | [] -> base
  | _ ->
      let query =
        String.concat "&"
          (List.map
             (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
             params)
      in
      base ^ "?" ^ query

let json_accept = Fetch.Header.[ accept, [ pref "application/json" ] ]
let any_accept = Fetch.Header.[ accept, [ pref "*/*" ] ]

(* Headers with optional auth.

   The bearer token is a per-request header rather than a
   [Fetch.Credential] attached to the client: a session comes and goes
   over a client's life (and Tangled swaps in a short-lived service-auth
   token aimed at a second host), whereas a credential is fixed at the
   point the client is narrowed and would send [Authorization] even
   before login. *)
let build_headers t base =
  match t.session with
  | None -> base
  | Some session ->
      Fetch.Header.append base
        Fetch.Header.[ authorization, `Bearer session.access_jwt ]

(* Truncate body for error preview *)
let body_preview ?(max_len = 100) body =
  if String.length body > max_len then String.sub body 0 max_len else body

(* Check if status indicates success *)
let is_success status = status >= 200 && status < 300

(* [Fetch] has no [Response.text]; the body is a flow. *)
let response_text response = Eio.Flow.read_all (Fetch.body response)

(* Parse XRPC error response *)
let parse_error_response status body =
  match Jsont_bytesrw.decode_string Xrpc_types.error_payload_jsont body with
  | Ok payload ->
      Xrpc_error.Xrpc_error
        { status; error = payload.error; message = payload.message }
  | Error _ ->
      Xrpc_error.Xrpc_error
        { status; error = "UnknownError"; message = Some (body_preview body) }

(* Raise error for non-success response *)
let raise_on_error response =
  let status = Fetch.status response in
  if not (is_success status) then begin
    let body = response_text response in
    raise (Xrpc_error.err (parse_error_response status body))
  end

(* Handle response, raising on error *)
let handle_response ~decoder response =
  raise_on_error response;
  let body = response_text response in
  match Jsont_bytesrw.decode_string decoder body with
  | Ok v -> v
  | Error e ->
      raise
        (Xrpc_error.err
           (Parse_error { reason = e; body_preview = Some (body_preview body) }))

(* Handle binary response. The content type is kept verbatim rather than
   re-encoded through a codec, since callers hand it straight back to
   whatever consumes the bytes. *)
let handle_bytes_response response =
  raise_on_error response;
  let body = response_text response in
  let content_type =
    Option.value ~default:"application/octet-stream"
      (Http.Header.get (Fetch.headers response) "content-type")
  in
  (body, content_type)

(* Call interceptor before request *)
let before_request t = Option.iter (fun f -> f t) t.on_request

(* Wrap network operations, converting non-Eio exceptions to Network_error *)
let with_network_error f =
  try f () with
  | Eio.Io _ as e -> raise e
  | exn ->
      raise (Xrpc_error.err (Network_error { reason = Printexc.to_string exn }))

(* Encode input data to a JSON request body *)
let encode_json_body input input_data =
  match (input, input_data) with
  | Some jsont, Some data ->
      Result.to_option (Jsont_bytesrw.encode_string jsont data)
      |> Option.map (fun s -> Fetch.String s)
  | _ -> None

let json_content_type = Fetch.Header.[ content_type, media "application/json" ]

(* A caller-supplied content type is passed through verbatim rather than
   parsed into a media type. [mime] is bound outside the local open,
   which would otherwise shadow it with the [content_type] codec. *)
let content_type_header mime = Fetch.Header.[ raw "Content-Type" mime ]

let query t ~nsid ~params ~decoder =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t json_accept in
  with_network_error @@ fun () ->
  Fetch.with_response ~headers t.http `GET url (handle_response ~decoder)

let procedure t ~nsid ~params ~input ~input_data ~decoder =
  before_request t;
  let url = build_url t nsid params in
  let body = encode_json_body input input_data in
  let headers =
    match body with
    | Some _ -> Fetch.Header.append json_content_type (build_headers t json_accept)
    | None -> build_headers t json_accept
  in
  let body = Option.value body ~default:Fetch.Empty in
  with_network_error @@ fun () ->
  Fetch.with_response ~headers ~body t.http `POST url (handle_response ~decoder)

let procedure_blob t ~nsid ~params ~blob ~content_type ~decoder =
  before_request t;
  let url = build_url t nsid params in
  let headers =
    Fetch.Header.append (content_type_header content_type)
      (build_headers t json_accept)
  in
  with_network_error @@ fun () ->
  Fetch.with_response ~headers ~body:(Fetch.String blob) t.http `POST url
    (handle_response ~decoder)

let query_bytes t ~nsid ~params =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t any_accept in
  with_network_error @@ fun () ->
  Fetch.with_response ~headers t.http `GET url handle_bytes_response

let procedure_bytes t ~nsid ~params ~body ~content_type =
  before_request t;
  let url = build_url t nsid params in
  let headers =
    match body with
    | Some _ ->
        Fetch.Header.append (content_type_header content_type)
          (build_headers t any_accept)
    | None -> build_headers t any_accept
  in
  let body =
    match body with Some b -> Fetch.String b | None -> Fetch.Empty
  in
  with_network_error @@ fun () ->
  Fetch.with_response ~headers ~body t.http `POST url @@ fun response ->
  let status = Fetch.status response in
  match status with
  | 204 -> None
  | _ when is_success status -> Some (handle_bytes_response response)
  | _ ->
      let body = response_text response in
      raise (Xrpc_error.err (parse_error_response status body))
