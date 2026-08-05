(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Typesense API error handling.

    The Typesense API returns errors in a simple format:
    {[
      {
        "message": "Collection not found"
      }
    ]}

    This module provides utilities for parsing and displaying these errors. *)

(** {1 Error Type} *)

type t = {
  message : string;
  status_code : int;
}
(** A structured Typesense API error. *)

(** {1 Parsing} *)

val of_api_error : Openapi.Runtime.api_error -> t option
(** Parse an API error into a structured Typesense error.
    Returns [None] if the error body is not valid JSON. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a Typesense API error.

    Format: "Collection not found [404]" *)

val to_string : t -> string
(** Convert to a human-readable string. *)

(** {1 Error Classification} *)

val is_auth_error : t -> bool
(** Check if this is an authentication/authorization error (401 or 403). *)

val is_not_found : t -> bool
(** Check if this is a "not found" error (404). *)

(** {1 Exception Handling} *)

exception Exit_code of int
(** Exception raised to signal a desired exit code.
    This is used instead of calling [exit] directly to avoid issues
    when running inside Eio's event loop. Catch this exception in
    the main program outside the Eio context. *)

val handle_exn : exn -> int
(** Handle an exception, printing a nice error message if it's an API error.

    Returns an exit code:
    - 1 for most API errors
    - 77 for authentication errors (permission denied)
    - 69 for not found errors

    @raise exn if the exception is not an API error or Failure *)

val run : (unit -> unit) -> int
(** Wrap a function to handle API errors gracefully.

    Returns 0 on success, or an appropriate exit code on error. *)

val wrap : (unit -> unit) -> unit
(** Wrap a command action to handle API errors gracefully.

    Catches API errors, prints a nice message, and raises {!Exit_code}
    with an appropriate code. *)
