(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module Url = Fetch.Middleware.Url

let normalize text =
  if String.length text > 2048 then invalid_arg "HTTP URL exceeds 2048 bytes.";
  match Url.of_string text with
  | Error _ -> invalid_arg "Use an absolute HTTP(S) URL without credentials."
  | Ok url ->
      (match Ipaddr.of_string (Url.host url) with
       | Ok ip when not (Feed_http.public_address ip) ->
           invalid_arg "HTTP URLs must use public network addresses."
       | _ -> ());
      Url.to_string url

let client ~label ~methods fetch =
  let fetch = Fetch.restrict ~methods ~filter:(fun req ->
      try ignore (normalize (Url.to_string req.url)); `Allow
      with Invalid_argument message -> `Reject message) fetch in
  Fetch.Middleware.middleware (fun next ~sw request ->
      let url = Url.to_string request.Fetch.Middleware.url in
      Diagnostics.Tools.info (fun m -> m "%s request method=%s url=%S"
          label (Http.Method.to_string request.meth) url);
      match next ~sw request with
      | response ->
          Diagnostics.Tools.info (fun m -> m "%s response url=%S status=%d"
              label url (Fetch.Middleware.status response));
          response
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Diagnostics.Tools.err (fun m -> m "%s request failed url=%S error=%s"
              label url (Diagnostics.error exn));
          Printexc.raise_with_backtrace exn bt) fetch
