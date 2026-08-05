(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type fetch_result = {
  body : string;
  etag : string option;
  last_modified : string option;
}

let fetch ~session ?etag ?last_modified url =
  try
    let headers =
      let h = Requests.Headers.empty in
      let h = match etag with
        | Some e -> Requests.Headers.set_string "If-None-Match" e h
        | None -> h
      in
      let h = match last_modified with
        | Some lm -> Requests.Headers.set_string "If-Modified-Since" lm h
        | None -> h
      in
      h
    in
    let response = Requests.get session ~headers url in
    let status = Requests.Response.status_code response in
    if status = 304 then
      Error `Not_modified
    else if status >= 200 && status < 300 then
      let body = Requests.Response.text response in
      let etag = Requests.Response.header_string "ETag" response in
      let last_modified = Requests.Response.header_string "Last-Modified" response in
      Ok { body; etag; last_modified }
    else
      Error (`Error (Printf.sprintf "HTTP %d" status))
  with exn ->
    Error (`Error (Printexc.to_string exn))
