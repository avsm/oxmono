(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error =
  | Network_error of { reason : string }
  | Parse_error of { reason : string; body_preview : string option }
  | Xrpc_error of {
      status : int;
      error : string;
      message : string option;
      nsid : string;
      service : string;
    }
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
  | Xrpc_error { status; error; message; nsid; service } -> (
      let host =
        match String.index_opt service ':' with
        | Some i when String.length service > i + 3 ->
            String.sub service (i + 3) (String.length service - i - 3)
        | _ -> service
      in
      match (status, error) with
      | 404, ("UnknownError" | "MethodNotImplemented" | "XRPCNotSupported") ->
          (* A server without the method answers with its own 404 page, which
             says nothing more. *)
          Fmt.pf ppf "%s does not implement %s (HTTP 404)" host nsid
      | _ -> (
          let message = Option.map String.trim message in
          match message with
          | Some msg when msg <> "" ->
              Fmt.pf ppf "%s refused %s: %s (%s, HTTP %d)" host nsid msg error
                status
          | _ ->
              Fmt.pf ppf "%s refused %s: %s (HTTP %d)" host nsid error status))
  | Token_expired -> Fmt.pf ppf "session token expired"
  | Session_required -> Fmt.pf ppf "authentication required"

let to_string e = Fmt.str "%a" pp e

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | E e ->
        Fmt.pf ppf "Xrpc_error.E(%a)" pp e;
        true
    | _ -> false)
