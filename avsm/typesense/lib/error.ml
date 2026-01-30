(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Typesense API error handling.

    Typesense returns errors in a simple format: {"message": "..."} *)

(** Typesense error response *)
type t = {
  message : string;
  status_code : int;
}

let error_jsont =
  Jsont.Object.map ~kind:"TypesenseError"
    (fun message -> message)
  |> Jsont.Object.mem "message" Jsont.string ~enc:Fun.id
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** Parse an API error into a structured Typesense error. *)
let of_api_error (e : Openapi.Runtime.api_error) : t option =
  match Jsont_bytesrw.decode_string error_jsont e.body with
  | Ok message -> Some { message; status_code = e.status }
  | Error _ -> None

(** {1 Styled Output Helpers} *)

(** Style for error labels (red, bold) *)
let error_style = Fmt.(styled (`Fg `Red) (styled `Bold string))

(** Style for status codes *)
let status_style status =
  if status >= 500 then Fmt.(styled (`Fg `Red) int)
  else if status >= 400 then Fmt.(styled (`Fg `Yellow) int)
  else Fmt.(styled (`Fg `Green) int)

(** Pretty-print a Typesense API error with colors. *)
let pp ppf (e : t) =
  Fmt.pf ppf "%s [%a]" e.message (status_style e.status_code) e.status_code

(** Convert to a human-readable string (without colors). *)
let to_string (e : t) : string =
  Printf.sprintf "%s [%d]" e.message e.status_code

(** Check if this is an authentication/authorization error. *)
let is_auth_error (e : t) =
  e.status_code = 401 || e.status_code = 403

(** Check if this is a "not found" error. *)
let is_not_found (e : t) =
  e.status_code = 404

(** Handle an exception, printing a nice error message if it's an API error.

    Returns an exit code:
    - 1 for most API errors
    - 77 for authentication errors (permission denied)
    - 69 for not found errors *)
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
           (* Not a Typesense error, show raw response *)
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
      (* Re-raise unknown exceptions *)
      raise exn

(** Wrap a function to handle API errors gracefully. *)
let run f =
  try f (); 0
  with exn -> handle_exn exn

(** Exception to signal desired exit code without calling [exit] directly.
    This avoids issues when running inside Eio's event loop. *)
exception Exit_code of int

(** Wrap a command action to handle API errors gracefully. *)
let wrap f =
  try f ()
  with
  | Stdlib.Exit ->
      (* exit() was called somewhere - treat as success *)
      ()
  | Eio.Cancel.Cancelled Stdlib.Exit ->
      (* Eio wraps Exit in Cancelled - treat as success *)
      ()
  | exn ->
      let code = handle_exn exn in
      raise (Exit_code code)
