(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error =
  | Network_error of { reason : string }
  | Parse_error of { reason : string; body_preview : string option }
  | Xrpc_error of { status : int; error : string; message : string option }
  | Token_expired
  | Session_required

type Eio.Exn.err += E of error

let err e = Eio.Exn.create (E e)

let is_retryable = function
  | Network_error _ -> true
  | Xrpc_error { status; _ } -> status = 408 || status = 429 || status >= 500
  | Parse_error _ | Token_expired | Session_required -> false

let is_auth_error = function
  | Token_expired -> true
  | Xrpc_error { error; _ } ->
      error = "ExpiredToken" || error = "InvalidToken" || error = "AuthRequired"
  | Network_error _ | Parse_error _ | Session_required -> false

let of_eio_exn = function Eio.Io (E e, _) -> Some e | _ -> None

let pp ppf = function
  | Network_error { reason } -> Fmt.pf ppf "network error: %s" reason
  | Parse_error { reason; body_preview } -> (
      match body_preview with
      | Some preview ->
          Fmt.pf ppf "parse error: %s (body: %s...)" reason preview
      | None -> Fmt.pf ppf "parse error: %s" reason)
  | Xrpc_error { status; error; message } -> (
      match message with
      | Some msg -> Fmt.pf ppf "XRPC error %d %s: %s" status error msg
      | None -> Fmt.pf ppf "XRPC error %d %s" status error)
  | Token_expired -> Fmt.pf ppf "session token expired"
  | Session_required -> Fmt.pf ppf "authentication required"

let to_string e = Fmt.str "%a" pp e

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | E e ->
        Fmt.pf ppf "Xrpc_error.E(%a)" pp e;
        true
    | _ -> false)
