(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let parse s =
  Ptime.of_rfc3339 s |> Result.to_option |> Option.map (fun (t, _, _) -> t)

let format t = Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 t
let pp ppf t = Format.pp_print_string ppf (format t)

let jsont =
  let kind = "RFC 3339 timestamp" in
  let doc = "An RFC 3339 date-time string" in
  let dec s =
    match parse s with
    | Some t -> t
    | None ->
        Jsont.Error.msgf Jsont.Meta.none "%s: invalid RFC 3339 timestamp: %S"
          kind s
  in
  let enc = format in
  Jsont.map ~kind ~doc ~dec ~enc Jsont.string
