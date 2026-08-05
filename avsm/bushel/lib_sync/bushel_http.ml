(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP client using the requests library *)

let src = Logs.Src.create "bushel.http" ~doc:"HTTP client"
module Log = (val Logs.src_log src : Logs.LOG)

type t = Requests.t

let create ~sw env =
  Requests.create ~sw ~follow_redirects:true env

let get ~http url =
  Log.debug (fun m -> m "GET %s" url);
  try
    let response = Requests.get http url in
    if Requests.Response.ok response then begin
      let body = Requests.Response.body response |> Eio.Flow.read_all in
      Ok body
    end else begin
      let status = Requests.Response.status_code response in
      Error (Printf.sprintf "HTTP %d" status)
    end
  with exn ->
    Error (Printf.sprintf "Request failed: %s" (Printexc.to_string exn))

let get_with_header ~http ~header url =
  Log.debug (fun m -> m "GET %s (with header)" url);
  try
    (* Parse header "Name: Value" format *)
    let name, value = match String.index_opt header ':' with
      | Some i ->
        let name = String.sub header 0 i in
        let value = String.trim (String.sub header (i + 1) (String.length header - i - 1)) in
        (name, value)
      | None -> (header, "")
    in
    let headers = Requests.Headers.empty |> Requests.Headers.add_string name value in
    let response = Requests.get http ~headers url in
    if Requests.Response.ok response then begin
      let body = Requests.Response.body response |> Eio.Flow.read_all in
      Ok body
    end else begin
      let status = Requests.Response.status_code response in
      Error (Printf.sprintf "HTTP %d" status)
    end
  with exn ->
    Error (Printf.sprintf "Request failed: %s" (Printexc.to_string exn))

let post ~http ~content_type ~body url =
  Log.debug (fun m -> m "POST %s" url);
  try
    let mime = Requests.Mime.of_string content_type in
    let body = Requests.Body.of_string mime body in
    let response = Requests.post http ~body url in
    if Requests.Response.ok response then begin
      let body = Requests.Response.body response |> Eio.Flow.read_all in
      Ok body
    end else begin
      let status = Requests.Response.status_code response in
      Error (Printf.sprintf "HTTP %d" status)
    end
  with exn ->
    Error (Printf.sprintf "Request failed: %s" (Printexc.to_string exn))
