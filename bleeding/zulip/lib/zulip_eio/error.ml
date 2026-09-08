type t =
  | Api of {
      status : int;
      code : string;
      message : string;
      extra : Jsont.json;
      retry_after : float option;
    }
  | Http of { status : int; message : string; retry_after : float option }
  | Json of Jsont.Error.t
  | Transport of Fetch.error
  | Timeout of float
  | Invalid_request of string
  | Storage of string

let pp ppf = function
  | Api { status; code; message; _ } ->
      Format.fprintf ppf "Zulip HTTP %d (%s): %S" status code message
  | Http { status; message; _ } ->
      Format.fprintf ppf "HTTP %d: %S" status message
  | Json error -> Format.fprintf ppf "JSON: %s" (Jsont.Error.to_string error)
  | Transport error -> Eio.Exn.pp ppf (Fetch.err error)
  | Timeout seconds ->
      Format.fprintf ppf "Request timed out after %g seconds" seconds
  | Invalid_request message -> Format.fprintf ppf "Invalid request: %S" message
  | Storage message -> Format.fprintf ppf "Storage: %S" message

let error_to_string error = Format.asprintf "%a" pp error

let retry_after = function
  | Api e -> e.retry_after
  | Http e -> e.retry_after
  | _ -> None

let is_rate_limit = function
  | Api { status = 429; _ }
  | Api { code = "RATE_LIMIT_HIT"; _ }
  | Http { status = 429; _ } ->
      true
  | _ -> false

let is_bad_queue = function
  | Api { code = "BAD_EVENT_QUEUE_ID"; _ } -> true
  | _ -> false

let is_terminal = function
  | Api { status = 401 | 403; _ }
  | Http { status = 401 | 403; _ }
  | Api
      {
        code =
          ( "INVALID_API_KEY" | "USER_DEACTIVATED" | "REALM_DEACTIVATED"
          | "PERMISSION_DENIED" );
        _;
      }
  | Invalid_request _
  | Transport
      ( Fetch.Denied _ | Fetch.Invalid_url _ | Fetch.Invalid_request _
      | Fetch.Tls_failure _ ) ->
      true
  | _ -> false

exception E of t

let or_raise = function Ok value -> value | Error error -> raise (E error)

let catch f =
  try Ok (f ()) with
  | E error -> Error error
  | Jsont.Error error -> Error (Json error)
  | Eio.Io (Fetch.E error, _) -> Error (Transport error)
  | Eio.Io _ as exn ->
      Error (Transport (Fetch.Protocol_error (Printexc.to_string exn)))
