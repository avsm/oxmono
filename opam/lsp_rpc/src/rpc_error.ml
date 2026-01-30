open Core
open Jsonrpc
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type json = Json.t
type error = Error.t

let yojson_of_json = Fn.id
let yojson_of_error = Fn.compose yojson_of_string Error.to_string_hum

type t =
  | Fatal_parse_failure of error
  | Invalid_rpc_message of
      { error : string
      ; data : json
      }
  | Response_for_unknown_request of Response.t
  | Batch_call of [ `Request of Request.t | `Notification of Notification.t ] list
  | Batch_response of Response.t list
[@@deriving yojson_of]

let to_string t = t |> yojson_of_t |> Yojson.Safe.pretty_to_string
let sexp_of_t t = t |> to_string |> Sexp.of_string
let raise t = t |> to_string |> Error.of_string |> Error.raise
