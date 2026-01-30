(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.response" ~doc:"HTTP Response"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  status : int;
  headers : Headers.t;
  body : Eio.Flow.source_ty Eio.Resource.t;
  url : string;
  elapsed : float;
  mutable closed : bool;
}

let make ~sw ~status ~headers ~body ~url ~elapsed =
  Log.debug (fun m -> m "Creating response: status=%d url=%s elapsed=%.3fs" status url elapsed);
  let response = { status; headers; body; url; elapsed; closed = false } in

  (* Register cleanup with switch *)
  Eio.Switch.on_release sw (fun () ->
    if not response.closed then begin
      Log.debug (fun m -> m "Auto-closing response for %s via switch" url);
      response.closed <- true;
      (* Body cleanup happens automatically via Eio switch lifecycle.
         The body flow (created via Eio.Flow.string_source) is a memory-backed
         source that doesn't require explicit cleanup. File-based responses
         would have their file handles cleaned up by the switch. *)
    end
  );

  response

let status t = Status.of_int t.status

let status_code t = t.status

let ok t = Status.is_success (Status.of_int t.status)

let headers t = t.headers

let header name t = Headers.get name t.headers

let header_string name t = Headers.get_string name t.headers

(** Option monad operators for cleaner code *)
let ( let* ) = Option.bind
let ( let+ ) x f = Option.map f x

let content_type t =
  let+ ct = Headers.get `Content_type t.headers in
  Mime.of_string ct

let content_length t =
  let* len = Headers.get `Content_length t.headers in
  try Some (Int64.of_string len) with _ -> None

let location t = Headers.get `Location t.headers

(** {1 Conditional Request / Caching Headers}

    Per Recommendation #19: Conditional Request Helpers (ETag/Last-Modified)
    RFC 9110 Section 8.8.2-8.8.3 *)

let etag t = Headers.get `Etag t.headers

let last_modified t = Headers.get `Last_modified t.headers

let parse_http_date = Http_date.parse

let last_modified_ptime t =
  let* lm = last_modified t in
  Http_date.parse lm

let date t = Headers.get `Date t.headers

let date_ptime t =
  let* d = date t in
  Http_date.parse d

let expires t = Headers.get `Expires t.headers

let expires_ptime t =
  let* exp = expires t in
  Http_date.parse exp

let age t =
  let* s = Headers.get `Age t.headers in
  try Some (int_of_string s) with _ -> None

(** {1 Cache-Control Parsing}

    Per Recommendation #17: Response Caching with RFC 7234/9111 Compliance *)

let cache_control t =
  Option.map Cache_control.parse_response (Headers.get `Cache_control t.headers)

let cache_control_raw t = Headers.get `Cache_control t.headers

(** Check if response is cacheable based on status and Cache-Control *)
let is_cacheable t =
  match cache_control t with
  | Some cc -> Cache_control.is_cacheable ~response_cc:cc ~status:t.status
  | None ->
      (* No Cache-Control - use default cacheability based on status *)
      List.mem t.status [200; 203; 204; 206; 300; 301; 308; 404; 405; 410; 414; 501]

(** Calculate freshness lifetime in seconds *)
let freshness_lifetime t =
  match cache_control t with
  | Some cc ->
      Cache_control.freshness_lifetime
        ~response_cc:cc
        ?expires:(expires t)
        ?date:(date t)
        ()
  | None -> None

(** Check if response requires revalidation before use *)
let must_revalidate t =
  match cache_control t with
  | Some cc -> Cache_control.must_revalidate ~response_cc:cc
  | None -> false

(** Check if response is stale (current time exceeds freshness)
    Requires the current time as a parameter *)
let is_stale ~now t =
  match freshness_lifetime t, date_ptime t with
  | Some lifetime, Some response_date ->
      let response_age = match age t with
        | Some a -> a
        | None ->
            (* Calculate age from Date header *)
            let diff = Ptime.diff now response_date in
            Ptime.Span.to_int_s diff |> Option.value ~default:0
      in
      response_age > lifetime
  | _ -> false  (* Cannot determine staleness without freshness info *)

(** Check if this is a 304 Not Modified response *)
let is_not_modified t = t.status = 304

(** Get the Vary header which indicates which request headers affect caching *)
let vary t = Headers.get `Vary t.headers

(** Parse Vary header into list of header names *)
let vary_headers t =
  match vary t with
  | None -> []
  | Some v ->
      String.split_on_char ',' v
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")

let url t = t.url

let elapsed t = t.elapsed

let body t =
  if t.closed then
    invalid_arg "Response.body: response has been closed"
  else
    t.body

let text t =
  if t.closed then
    invalid_arg "Response.text: response has been closed"
  else
    Eio.Buf_read.of_flow t.body ~max_size:max_int |> Eio.Buf_read.take_all

let json t =
  let body_str = text t in
  match Jsont_bytesrw.decode_string' Jsont.json body_str with
  | Ok json -> json
  | Error e ->
      let preview = if String.length body_str > 200
        then String.sub body_str 0 200
        else body_str in
      raise (Error.err (Error.Json_parse_error {
        body_preview = preview;
        reason = Jsont.Error.to_string e
      }))

let jsonv (type a) (codec : a Jsont.t) t =
  let body_str = text t in
  match Jsont_bytesrw.decode_string' codec body_str with
  | Ok value -> value
  | Error e ->
      let preview = if String.length body_str > 200
        then String.sub body_str 0 200
        else body_str in
      raise (Error.err (Error.Json_parse_error {
        body_preview = preview;
        reason = Jsont.Error.to_string e
      }))

let raise_for_status t =
  if t.status >= 400 then
    raise (Error.err (Error.Http_error {
      url = t.url;
      status = t.status;
      reason = Status.reason_phrase (Status.of_int t.status);
      body_preview = None;
      headers = Headers.to_list t.headers;  (* Convert to list for error type *)
    }))
  else
    t

(** Result-based status check - per Recommendation #21.
    Returns Ok response for 2xx success, Error for 4xx/5xx errors.
    Enables functional error handling without exceptions. *)
let check_status t =
  if t.status >= 400 then
    Error (Error.Http_error {
      url = t.url;
      status = t.status;
      reason = Status.reason_phrase (Status.of_int t.status);
      body_preview = None;
      headers = Headers.to_list t.headers;
    })
  else
    Ok t

(* Pretty printers *)
let pp ppf t =
  Format.fprintf ppf "@[<v>Response:@,\
    status: %a@,\
    url: %s@,\
    elapsed: %.3fs@,\
    headers: @[%a@]@]"
    Status.pp (Status.of_int t.status) t.url t.elapsed
    Headers.pp_brief t.headers

let pp_detailed ppf t =
  Format.fprintf ppf "@[<v>Response:@,\
    status: %a@,\
    url: %s@,\
    elapsed: %.3fs@,\
    @[%a@]@]"
    Status.pp_hum (Status.of_int t.status) t.url t.elapsed
    Headers.pp t.headers

(* Private module *)
module Private = struct
  let make = make
end
