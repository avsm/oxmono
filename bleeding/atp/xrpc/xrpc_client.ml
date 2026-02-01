(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  service : string;
  requests : Requests.t;
  mutable session : Xrpc_types.session option;
  on_request : (t -> unit) option;
}

let create ~sw ~env ~service ?on_request () =
  let requests = Requests.create ~sw env in
  { service; requests; session = None; on_request }

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

(* Headers with optional auth *)
let build_headers t =
  let headers =
    Requests.Headers.empty |> Requests.Headers.set `Accept "application/json"
  in
  match t.session with
  | Some session ->
      Requests.Headers.set `Authorization
        ("Bearer " ^ session.access_jwt)
        headers
  | None -> headers

(* Truncate body for error preview *)
let body_preview ?(max_len = 100) body =
  if String.length body > max_len then String.sub body 0 max_len else body

(* Check if status indicates success *)
let is_success status = status >= 200 && status < 300

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
  let status = Requests.Response.status_code response in
  if not (is_success status) then begin
    let body = Requests.Response.text response in
    raise (Xrpc_error.err (parse_error_response status body))
  end

(* Handle response, raising on error *)
let handle_response ~decoder response =
  raise_on_error response;
  let body = Requests.Response.text response in
  match Jsont_bytesrw.decode_string decoder body with
  | Ok v -> v
  | Error e ->
      raise
        (Xrpc_error.err
           (Parse_error { reason = e; body_preview = Some (body_preview body) }))

(* Handle binary response *)
let handle_bytes_response response =
  raise_on_error response;
  let body = Requests.Response.text response in
  let content_type =
    Option.fold ~none:"application/octet-stream" ~some:Requests.Mime.to_string
      (Requests.Response.content_type response)
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

(* Encode input data to JSON body *)
let encode_json_body input input_data =
  match (input, input_data) with
  | Some jsont, Some data ->
      Result.to_option (Jsont_bytesrw.encode_string jsont data)
      |> Option.map (Requests.Body.of_string Requests.Mime.json)
  | _ -> None

let query t ~nsid ~params ~decoder =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t in
  with_network_error @@ fun () ->
  Requests.get t.requests ~headers url |> handle_response ~decoder

let procedure t ~nsid ~params ~input ~input_data ~decoder =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t in
  let body = encode_json_body input input_data in
  with_network_error @@ fun () ->
  let response =
    match body with
    | Some b -> Requests.post t.requests ~headers ~body:b url
    | None -> Requests.post t.requests ~headers url
  in
  handle_response ~decoder response

let procedure_blob t ~nsid ~params ~blob ~content_type ~decoder =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t in
  let body =
    Requests.Body.of_string (Requests.Mime.of_string content_type) blob
  in
  with_network_error @@ fun () ->
  Requests.post t.requests ~headers ~body url |> handle_response ~decoder

let query_bytes t ~nsid ~params =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t |> Requests.Headers.set `Accept "*/*" in
  with_network_error @@ fun () ->
  Requests.get t.requests ~headers url |> handle_bytes_response

let procedure_bytes t ~nsid ~params ~body ~content_type =
  before_request t;
  let url = build_url t nsid params in
  let headers = build_headers t |> Requests.Headers.set `Accept "*/*" in
  let req_body =
    Option.map
      (Requests.Body.of_string (Requests.Mime.of_string content_type))
      body
  in
  with_network_error @@ fun () ->
  let response =
    match req_body with
    | Some b -> Requests.post t.requests ~headers ~body:b url
    | None -> Requests.post t.requests ~headers url
  in
  let status = Requests.Response.status_code response in
  match status with
  | 204 -> None
  | _ when is_success status -> Some (handle_bytes_response response)
  | _ ->
      let body = Requests.Response.text response in
      raise (Xrpc_error.err (parse_error_response status body))
