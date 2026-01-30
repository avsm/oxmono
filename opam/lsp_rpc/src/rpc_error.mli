open Core
open Jsonrpc

(** [Fatal_parse_failure] happens when we receive a badly structured packet. Parsing stops
    and the [reader] and [writer] are closed.

    [Invalid_rpc_message] happens when we are able to parse the packet but not the message
    it contains because it does not conform to the JSON RPC 2.0 specification.

    [Response_for_unknown_request] happens when we receive a message that is meant to be a
    response to a synchronous request we sent but we don't have a record of that request
    (i.e., the response's message ID is unknown).

    [Batch_call] and [Batch_response] happen when a batch call or batch response is
    received, respectively. Batch calls and batch responses are supported in the JSON RPC
    2.0 specification but are disallowed in the LSP 3.18 specification:
    - https://github.com/microsoft/language-server-protocol/issues/988
    - https://github.com/microsoft/language-server-protocol/pull/1651 *)
type t =
  | Fatal_parse_failure of Error.t
  | Invalid_rpc_message of
      { error : string
      ; data : Json.t
      }
  | Response_for_unknown_request of Response.t
  | Batch_call of [ `Request of Request.t | `Notification of Notification.t ] list
  | Batch_response of Response.t list
[@@deriving sexp_of, yojson_of]

val raise : t -> 'a
