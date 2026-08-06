(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  message : string;
  status_code : int;
  code : string;
}

let error_jsont =
  Jsont.Object.map ~kind:"KarakeepError"
    (fun code message -> (code, message))
  |> Jsont.Object.mem "code" Jsont.string ~dec_absent:"unknown" ~enc:fst
  |> Jsont.Object.mem "message" Jsont.string ~dec_absent:"Unknown error" ~enc:snd
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let of_api_error (e : Openapi.Runtime.api_error) : t option =
  match Jsont_bytesrw.decode_string error_jsont e.body with
  | Ok (code, message) -> Some { message; status_code = e.status; code }
  | Error _ -> None

let error_style = Fmt.(styled (`Fg `Red) (styled `Bold string))

let status_style status =
  if status >= 500 then Fmt.(styled (`Fg `Red) int)
  else if status >= 400 then Fmt.(styled (`Fg `Yellow) int)
  else Fmt.(styled (`Fg `Green) int)

let pp ppf (e : t) =
  Fmt.pf ppf "%s (%s) [%a]" e.message e.code (status_style e.status_code) e.status_code

let to_string (e : t) : string =
  Printf.sprintf "%s (%s) [%d]" e.message e.code e.status_code

let is_auth_error (e : t) =
  e.status_code = 401 || e.status_code = 403

let is_not_found (e : t) =
  e.status_code = 404

let handle_exn exn =
  match exn with
  | Openapi.Runtime.Api_error e ->
      (match of_api_error e with
       | Some err ->
           Fmt.epr "%a %a@." error_style "Error:" pp err;
           if is_auth_error err then 77
           else if is_not_found err then 69
           else 1
       | None ->
           Fmt.epr "%a %s %s returned %a@.%s@."
             error_style "API Error:"
             e.method_ e.url
             (status_style e.status) e.status
             e.body;
           1)
  | Failure msg ->
      Fmt.epr "%a %s@." error_style "Error:" msg;
      1
  | exn ->
      raise exn

let run f =
  try f (); 0
  with exn -> handle_exn exn

exception Exit_code of int

let wrap f =
  try f ()
  with
  | Stdlib.Exit -> ()
  | Eio.Cancel.Cancelled Stdlib.Exit -> ()
  | exn ->
      let code = handle_exn exn in
      raise (Exit_code code)
