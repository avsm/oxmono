(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Error handling for claudeio. *)

type t =
  | Cli_not_found of string
  | Process_error of string
  | Connection_error of string
  | Protocol_error of string
  | Timeout of string
  | Permission_denied of { tool_name : string; message : string }
  | Hook_error of { callback_id : string; message : string }
  | Control_error of { request_id : string; message : string }

exception E of t

let pp ppf = function
  | Cli_not_found msg -> Fmt.pf ppf "CLI not found: %s" msg
  | Process_error msg -> Fmt.pf ppf "Process error: %s" msg
  | Connection_error msg -> Fmt.pf ppf "Connection error: %s" msg
  | Protocol_error msg -> Fmt.pf ppf "Protocol error: %s" msg
  | Timeout msg -> Fmt.pf ppf "Timeout: %s" msg
  | Permission_denied { tool_name; message } ->
      Fmt.pf ppf "Permission denied for tool '%s': %s" tool_name message
  | Hook_error { callback_id; message } ->
      Fmt.pf ppf "Hook error (callback_id=%s): %s" callback_id message
  | Control_error { request_id; message } ->
      Fmt.pf ppf "Control error (request_id=%s): %s" request_id message

let to_string err = Fmt.str "%a" pp err
let raise err = Stdlib.raise (E err)

(* Register exception printer for better error messages *)
let () =
  Printexc.register_printer (function
    | E err -> Some (to_string err)
    | _ -> None)

(** {1 Convenience Raisers} *)

let cli_not_found msg = raise (Cli_not_found msg)
let process_error msg = raise (Process_error msg)
let connection_error msg = raise (Connection_error msg)
let protocol_error msg = raise (Protocol_error msg)
let timeout msg = raise (Timeout msg)

let permission_denied ~tool_name ~message =
  raise (Permission_denied { tool_name; message })

let hook_error ~callback_id ~message =
  raise (Hook_error { callback_id; message })

let control_error ~request_id ~message =
  raise (Control_error { request_id; message })

(** {1 Result Helpers} *)

let get_ok ~msg = function
  | Ok x -> x
  | Error e -> raise (Protocol_error (msg ^ e))

let get_ok' ~msg = function
  | Ok x -> x
  | Error e -> raise (Protocol_error (msg ^ e))
