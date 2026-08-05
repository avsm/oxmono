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

val jsont : t Jsont.t
(** Jsont codec for NestJS errors. *)

(** {1 Parsing} *)

val of_string : string -> t option
(** Parse a JSON string into a NestJS error.
    Returns [None] if the string is not valid NestJS error JSON. *)

val of_api_error : Openapi_runtime.api_error -> t option
(** Parse an {!Openapi_runtime.api_error} into a structured NestJS error.
    Returns [None] if the error body is not valid NestJS error JSON. *)

(** {1 Convenience Functions} *)

val is_auth_error : t -> bool
(** Check if this is a permission/authorization error (401 or 403). *)

val is_not_found : t -> bool
(** Check if this is a "not found" error (404). *)

val is_validation_error : t -> bool
(** Check if this is a validation error (400 with message array). *)

val is_server_error : t -> bool
(** Check if this is a server error (5xx). *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a NestJS error. *)

val to_string : t -> string
(** Convert to a human-readable string. *)

(** {1 Exception Handling} *)

exception Error of t
(** Exception for NestJS-specific errors.
    Use this when you want to distinguish NestJS errors from generic API errors. *)

val raise_if_nestjs : Openapi_runtime.api_error -> 'a
(** Handle an {!Openapi_runtime.api_error}, converting it to a NestJS error
    if possible.

    @raise Error if the error body parses as a NestJS error
    @raise Openapi_runtime.Api_error if parsing fails (re-raises original) *)
