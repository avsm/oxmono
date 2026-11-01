(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  fixed_components : bool;
  param_filter : bool;
  recurrence_id_on_first : bool;
  lenient_hrefs : bool;
}

let standard =
  {
    fixed_components = false;
    param_filter = true;
    recurrence_id_on_first = true;
    lenient_hrefs = false;
  }

let fastmail =
  {
    standard with
    fixed_components = true;
    param_filter = false;
    recurrence_id_on_first = false;
  }

let host url =
  match Fetch.Middleware.Url.of_string url with
  | Ok u -> Fetch.Middleware.Url.host u
  | Error _ -> ""

let of_url url =
  let h = host url in
  let under d = h = d || String.ends_with ~suffix:("." ^ d) h in
  if under "fastmail.com" || under "messagingengine.com" then fastmail
  else standard
