(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Error handling for PeerTube CLI.

    This module provides utilities for handling errors and managing
    exit codes in the CLI. *)

(** {1 Exit Codes} *)

(** Exception for CLI exit codes. *)
exception Exit_code of int

(** {1 Error Handling} *)

(** [handle_exn exn] handles an exception and returns an exit code.
    Prints formatted error messages for known error types. *)
val handle_exn : exn -> int

(** [wrap f] wraps a function to handle errors and return exit codes.
    Catches exceptions and converts them to appropriate exit codes. *)
val wrap : (unit -> int) -> int

(** {1 Error Reporting} *)

(** [exit_with code] raises [Exit_code code]. *)
val exit_with : int -> 'a

(** [fail msg] prints an error message and exits with code 1. *)
val fail : string -> 'a
