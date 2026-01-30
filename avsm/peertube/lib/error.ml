(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Exception for CLI exit codes *)
exception Exit_code of int

(** Style definitions *)
let error_style = Fmt.(styled (`Fg `Red) string)
let label_style = Fmt.(styled `Faint string)
let code_style = Fmt.(styled `Bold int)

(** Pretty-print an API error *)
let pp_api_error ppf (err : Openapi.Runtime.api_error) =
  Fmt.pf ppf "@[<v>%a API Error@,%a %a@,%a %s@]"
    error_style "PeerTube"
    label_style "Status:" code_style err.status
    label_style "Body:" err.body

(** Handle an exception and return an exit code *)
let handle_exn exn =
  match exn with
  | Openapi.Runtime.Api_error err ->
      Fmt.epr "%a@." pp_api_error err;
      1
  | Failure msg ->
      Fmt.epr "@[<v>%a %s@]@." error_style "Error:" msg;
      1
  | exn ->
      Fmt.epr "@[<v>%a %s@]@." error_style "Unexpected error:" (Printexc.to_string exn);
      125

(** Wrap a function to handle errors and set exit code *)
let wrap f =
  try f ()
  with
  | Exit_code code -> code
  | exn -> handle_exn exn

(** Exit with a specific code *)
let exit_with code = raise (Exit_code code)

(** Print an error message and exit *)
let fail msg =
  Fmt.epr "@[<v>%a %s@]@." error_style "Error:" msg;
  exit_with 1
