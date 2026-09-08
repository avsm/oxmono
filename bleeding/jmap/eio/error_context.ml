(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let step ?url operation =
  match url with
  | None -> operation
  | Some url ->
      let origin =
        match Fetch.Middleware.Url.of_string url with
        | Ok url -> Fetch.Middleware.Url.origin url
        | Error _ -> "<invalid URL>"
      in
      Fmt.str "%s at %s" operation origin

let describe ?url ~operation exn =
  let context = step ?url operation in
  let message =
    match exn with
    | Eio.Io _ -> Fmt.str "%a" Eio.Exn.pp (Eio.Exn.add_context exn "%s" context)
    | _ -> Fmt.str "%a, %s" Eio.Exn.pp exn context
  in
  Httpz_media.sanitize_diagnostic message

let with_io ?url ~operation f =
  try f ()
  with Eio.Io _ as exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context exn backtrace "%s" (step ?url operation)
