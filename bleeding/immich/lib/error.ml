(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Immich API error handling using NestJS error format. *)

(** Re-export NestJS error type for convenience. *)
type t = Openapi.Nestjs.t = {
  status_code : int;
  error : string option;
  message : string;
  correlation_id : string option;
}

(** Parse an API error into a structured Immich/NestJS error. *)
let of_api_error = Openapi.Nestjs.of_api_error

(** {1 Styled Output Helpers} *)

(** Style for error labels (red, bold) *)
let error_style = Fmt.(styled (`Fg `Red) (styled `Bold string))

(** Style for status codes *)
let status_style status =
  if status >= 500 then Fmt.(styled (`Fg `Red) int)
  else if status >= 400 then Fmt.(styled (`Fg `Yellow) int)
  else Fmt.(styled (`Fg `Green) int)

(** Style for correlation IDs (dim) *)
let correlation_style = Fmt.(styled `Faint string)

(** Style for error type (bold) *)
let error_type_style = Fmt.(styled `Bold string)

(** Pretty-print an Immich API error with colors.

    Format: "Forbidden: Missing required permission [403] (correlationId: abc123)" *)
let pp ppf (e : t) =
  match e.error with
  | Some err ->
      Fmt.pf ppf "%a: %s [%a]"
        error_type_style err
        e.message
        (status_style e.status_code) e.status_code;
      (match e.correlation_id with
       | Some cid -> Fmt.pf ppf " (%a)" correlation_style (Printf.sprintf "correlationId: %s" cid)
       | None -> ())
  | None ->
      Fmt.pf ppf "%s [%a]"
        e.message
        (status_style e.status_code) e.status_code;
      (match e.correlation_id with
       | Some cid -> Fmt.pf ppf " (%a)" correlation_style (Printf.sprintf "correlationId: %s" cid)
       | None -> ())

(** Convert to a human-readable string (without colors). *)
let to_string (e : t) : string =
  let error_prefix = match e.error with
    | Some err -> err ^ ": "
    | None -> ""
  in
  match e.correlation_id with
  | Some cid ->
      Printf.sprintf "%s%s [%d] (correlationId: %s)"
        error_prefix e.message e.status_code cid
  | None ->
      Printf.sprintf "%s%s [%d]" error_prefix e.message e.status_code

(** Check if this is an authentication/authorization error. *)
let is_auth_error = Openapi.Nestjs.is_auth_error

(** Check if this is a "not found" error. *)
let is_not_found = Openapi.Nestjs.is_not_found

(** Handle an exception, printing a nice error message if it's an API error.

    Returns an exit code:
    - 0 if not an error (shouldn't happen, but for completeness)
    - 1 for most API errors
    - 77 for authentication errors (permission denied)
    - 69 for not found errors *)
let handle_exn exn =
  match exn with
  | Openapi.Runtime.Api_error e ->
      (match of_api_error e with
       | Some nestjs ->
           Fmt.epr "%a %a@." error_style "Error:" pp nestjs;
           if is_auth_error nestjs then 77
           else if is_not_found nestjs then 69
           else 1
       | None ->
           (* Not a NestJS error, show raw response *)
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

(** Wrap a function to handle API errors gracefully.

    Usage:
    {[
      let () = Immich_auth.Error.run (fun () ->
        let albums = Immich.Albums.get_all_albums client () in
        ...
      )
    ]} *)
let run f =
  try f (); 0
  with exn -> handle_exn exn

(** Exception to signal desired exit code without calling [exit] directly.
    This avoids issues when running inside Eio's event loop. *)
exception Exit_code of int

(** Wrap a command action to handle API errors gracefully.

    This is designed to be used in cmdliner command definitions:
    {[
      let list_action ~profile env =
        Immich_auth.Error.wrap (fun () ->
          let api = ... in
          let albums = Immich.Albums.get_all_albums api () in
          ...
        )

      let list_cmd env fs =
        Cmd.v info Term.(const list_action $ ...)
    ]}

    The wrapper catches API errors and prints a nice message,
    then raises [Exit_code] with an appropriate code. This exception
    should be caught by the main program outside the Eio event loop. *)
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
