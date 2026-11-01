(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type fetch_result = {
  body : string;
  etag : string option;
  last_modified : string option;
  content_type : string option;
}

let fetch ~session ?etag ?last_modified url =
  try
    let headers =
      (* [Fetch.Header] shadows [etag] and [last_modified], so the
         arguments are rebound before the local opens below. *)
      let prev_etag = etag and prev_modified = last_modified in
      let h = Fetch.Header.[] in
      let h = match prev_etag with
        (* The stored ETag is the value as received, quotes and all, so
           it goes back out verbatim rather than through the [etags]
           codec, which would normalise it. *)
        | Some e -> Fetch.Header.(append h [ raw "If-None-Match" e ])
        | None -> h
      in
      let h = match prev_modified with
        | Some lm -> Fetch.Header.(append h [ if_modified_since, lm ])
        | None -> h
      in
      h
    in
    Fetch.with_response ~headers session `GET url @@ fun response ->
    let status = Fetch.status response in
    if status = 304 then
      Error `Not_modified
    else if status >= 200 && status < 300 then
      let body = Fetch.body response |> Eio.Flow.read_all in
      let etag = Fetch.header (Fetch.Header.text "ETag") response in
      let last_modified = Fetch.header Fetch.Header.last_modified response in
      let content_type =
        Option.map (fun (mt : Fetch.Header.media_type) -> mt.media)
          (Fetch.header Fetch.Header.content_type response)
      in
      Ok { body; etag; last_modified; content_type }
    else
      Error (`Error (Printf.sprintf "HTTP %d" status))
  with exn ->
    Error (`Error (Printexc.to_string exn))
