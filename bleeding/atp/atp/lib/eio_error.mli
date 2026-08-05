(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio error registration helpers.

    This module provides utilities for creating Eio-compatible error types with
    proper exception registration and pretty-printing.

    {2 Usage Pattern}

    Each module that defines errors follows this pattern:
    {[
      type error = [ `My_error of string | `Another_error ]

      let pp_error ppf = function
        | `My_error s -> Fmt.pf ppf "my error: %s" s
        | `Another_error -> Fmt.string ppf "another error"

      type Eio.Exn.err += E of error

      let () =
        Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)

      let raise_error e = Eio_error.raise_ (E e)
    ]} *)

val register_pp : 'e Fmt.t -> (Eio.Exn.err -> 'e option) -> unit
(** [register_pp pp extract] registers [pp] as the pretty-printer for errors
    extracted by [extract]. *)

val raise_ : Eio.Exn.err -> 'a
(** [raise_ err] raises [err] as an Eio exception. *)
