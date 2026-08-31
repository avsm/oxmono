(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP client backed by Fetch. *)

let src = Logs.Src.create "bushel.http" ~doc:"HTTP client"
module Log = (val Logs.src_log src : Logs.LOG)

type t = Fetch.plain

let create ~sw env =
  Fetch_curl.std ~sw env

let ok response =
  let status = Fetch.status response in
  status >= 200 && status < 300

let read_body response =
  if ok response then begin
    let body = Fetch.body response |> Eio.Flow.read_all in
    Ok body
  end else begin
    let status = Fetch.status response in
    Error (Printf.sprintf "HTTP %d" status)
  end

let get ~http url =
  Log.debug (fun m -> m "GET %s" url);
  try Fetch.with_response http `GET url read_body
  with exn ->
    Error (Printf.sprintf "Request failed: %s" (Printexc.to_string exn))

let get_with_header ~http ~header url =
  Log.debug (fun m -> m "GET %s (with header)" url);
  try
    let name, value = match String.index_opt header ':' with
      | Some i ->
        let name = String.sub header 0 i in
        let value = String.trim (String.sub header (i + 1) (String.length header - i - 1)) in
        (name, value)
      | None -> (header, "")
    in
    let hname = name and hvalue = value in
    let headers = Fetch.Header.[ raw hname hvalue ] in
    Fetch.with_response ~headers http `GET url read_body
  with exn ->
    Error (Printf.sprintf "Request failed: %s" (Printexc.to_string exn))

let post ~http ~content_type ~body url =
  Log.debug (fun m -> m "POST %s" url);
  try
    let ct = content_type in
    let headers = Fetch.Header.[ content_type, media ct ] in
    let body = Fetch.String body in
    Fetch.with_response ~headers ~body http `POST url read_body
  with exn ->
    Error (Printf.sprintf "Request failed: %s" (Printexc.to_string exn))
