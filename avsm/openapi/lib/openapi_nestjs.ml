(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** NestJS-style API error handling.

    NestJS/Express applications return errors in a standard format:
    {[
      {
        "message": "Missing required permission: person.read",
        "error": "Forbidden",
        "statusCode": 403,
        "correlationId": "koskgk9d"
      }
    ]}

    This module provides types and utilities for parsing and handling
    these errors in a structured way.

    {2 Usage}

    {[
      match Immich.People.get_all_people client () with
      | people -> ...
      | exception Openapi.Runtime.Api_error e ->
          match Openapi.Nestjs.of_api_error e with
          | Some nestjs_error ->
              Fmt.epr "Error: %s (correlation: %s)@."
                nestjs_error.message
                (Option.value ~default:"none" nestjs_error.correlation_id)
          | None ->
              (* Not a NestJS error, use raw body *)
              Fmt.epr "Error: %s@." e.body
    ]}
*)

(** {1 Error Types} *)

(** A structured NestJS HTTP exception. *)
type t = {
  status_code : int;
  (** HTTP status code (e.g., 403, 404, 500). *)

  error : string option;
  (** Error category (e.g., "Forbidden", "Not Found", "Internal Server Error"). *)

  message : string;
  (** Human-readable error message. Can be a single string or concatenated
      from an array of validation messages. *)

  correlation_id : string option;
  (** Request correlation ID for debugging/support. *)
}

(** {1 JSON Codec} *)

(** Jsont codec for NestJS errors.

    Handles both string and array message formats:
    - {[ "message": "error text" ]}
    - {[ "message": ["validation error 1", "validation error 2"] ]} *)
let jsont : t Jsont.t =
  (* Message can be string or array of strings *)
  let message_jsont =
    Jsont.map Jsont.json ~kind:"message"
      ~dec:(fun json ->
        match json with
        | Jsont.String (s, _) -> s
        | Jsont.Array (items, _) ->
            items
            |> List.filter_map (function
                | Jsont.String (s, _) -> Some s
                | _ -> None)
            |> String.concat "; "
        | _ -> "Unknown error")
      ~enc:(fun s -> Jsont.String (s, Jsont.Meta.none))
  in
  Jsont.Object.map ~kind:"NestjsError"
    (fun status_code error message correlation_id ->
      { status_code; error; message; correlation_id })
  |> Jsont.Object.mem "statusCode" Jsont.int ~enc:(fun e -> e.status_code)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun e -> e.error)
  |> Jsont.Object.mem "message" message_jsont ~enc:(fun e -> e.message)
  |> Jsont.Object.opt_mem "correlationId" Jsont.string ~enc:(fun e -> e.correlation_id)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Parsing} *)

(** Parse a JSON string into a NestJS error.
    Returns [None] if the string is not valid NestJS error JSON. *)
let of_string (s : string) : t option =
  match Jsont_bytesrw.decode_string jsont s with
  | Ok e -> Some e
  | Error _ -> None

(** Parse an {!Openapi.Runtime.Api_error} into a structured NestJS error.
    Returns [None] if the error body is not valid NestJS error JSON. *)
let of_api_error (e : Openapi_runtime.api_error) : t option =
  of_string e.body

(** {1 Convenience Functions} *)

(** Check if this is a permission/authorization error (401 or 403). *)
let is_auth_error (e : t) : bool =
  e.status_code = 401 || e.status_code = 403

(** Check if this is a "not found" error (404). *)
let is_not_found (e : t) : bool =
  e.status_code = 404

(** Check if this is a validation error (400 with message array). *)
let is_validation_error (e : t) : bool =
  e.status_code = 400

(** Check if this is a server error (5xx). *)
let is_server_error (e : t) : bool =
  e.status_code >= 500 && e.status_code < 600

(** {1 Pretty Printing} *)

(** Pretty-print a NestJS error. *)
let pp ppf (e : t) =
  match e.correlation_id with
  | Some cid ->
      Format.fprintf ppf "%s [%d] (correlationId: %s)"
        e.message e.status_code cid
  | None ->
      Format.fprintf ppf "%s [%d]" e.message e.status_code

(** Convert to a human-readable string. *)
let to_string (e : t) : string =
  Format.asprintf "%a" pp e

(** {1 Exception Handling} *)

(** Exception for NestJS-specific errors.
    Use this when you want to distinguish NestJS errors from generic API errors. *)
exception Error of t

(** Register a pretty printer for the exception. *)
let () =
  Printexc.register_printer (function
    | Error e -> Some (Format.asprintf "Nestjs.Error: %a" pp e)
    | _ -> None)

(** Handle an {!Openapi.Runtime.Api_error}, converting it to a NestJS error
    if possible.

    @raise Error if the error body parses as a NestJS error
    @raise Openapi.Runtime.Api_error if parsing fails (re-raises original) *)
let raise_if_nestjs (e : Openapi_runtime.api_error) =
  match of_api_error e with
  | Some nestjs -> raise (Error nestjs)
  | None -> raise (Openapi_runtime.Api_error e)
