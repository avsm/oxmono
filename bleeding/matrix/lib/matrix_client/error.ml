type errcode =
  | M_FORBIDDEN
  | M_UNKNOWN_TOKEN
  | M_MISSING_TOKEN
  | M_BAD_JSON
  | M_NOT_JSON
  | M_NOT_FOUND
  | M_LIMIT_EXCEEDED
  | M_UNRECOGNIZED
  | M_UNKNOWN
  | M_UNAUTHORIZED
  | M_USER_DEACTIVATED
  | M_USER_IN_USE
  | M_INVALID_USERNAME
  | M_ROOM_IN_USE
  | M_INVALID_ROOM_STATE
  | M_THREEPID_IN_USE
  | M_THREEPID_NOT_FOUND
  | M_THREEPID_AUTH_FAILED
  | M_THREEPID_DENIED
  | M_SERVER_NOT_TRUSTED
  | M_UNSUPPORTED_ROOM_VERSION
  | M_INCOMPATIBLE_ROOM_VERSION
  | M_BAD_STATE
  | M_GUEST_ACCESS_FORBIDDEN
  | M_CAPTCHA_NEEDED
  | M_CAPTCHA_INVALID
  | M_MISSING_PARAM
  | M_INVALID_PARAM
  | M_TOO_LARGE
  | M_CANNOT_OVERWRITE_MEDIA
  | M_WRONG_ROOM_KEYS_VERSION
  | M_EXCLUSIVE
  | M_RESOURCE_LIMIT_EXCEEDED
  | M_CANNOT_LEAVE_SERVER_NOTICE_ROOM
  | M_WEAK_PASSWORD
  | M_UNKNOWN_CODE of string

let errcode_to_string = function
  | M_FORBIDDEN -> "M_FORBIDDEN"
  | M_UNKNOWN_TOKEN -> "M_UNKNOWN_TOKEN"
  | M_MISSING_TOKEN -> "M_MISSING_TOKEN"
  | M_BAD_JSON -> "M_BAD_JSON"
  | M_NOT_JSON -> "M_NOT_JSON"
  | M_NOT_FOUND -> "M_NOT_FOUND"
  | M_LIMIT_EXCEEDED -> "M_LIMIT_EXCEEDED"
  | M_UNRECOGNIZED -> "M_UNRECOGNIZED"
  | M_UNKNOWN -> "M_UNKNOWN"
  | M_UNAUTHORIZED -> "M_UNAUTHORIZED"
  | M_USER_DEACTIVATED -> "M_USER_DEACTIVATED"
  | M_USER_IN_USE -> "M_USER_IN_USE"
  | M_INVALID_USERNAME -> "M_INVALID_USERNAME"
  | M_ROOM_IN_USE -> "M_ROOM_IN_USE"
  | M_INVALID_ROOM_STATE -> "M_INVALID_ROOM_STATE"
  | M_THREEPID_IN_USE -> "M_THREEPID_IN_USE"
  | M_THREEPID_NOT_FOUND -> "M_THREEPID_NOT_FOUND"
  | M_THREEPID_AUTH_FAILED -> "M_THREEPID_AUTH_FAILED"
  | M_THREEPID_DENIED -> "M_THREEPID_DENIED"
  | M_SERVER_NOT_TRUSTED -> "M_SERVER_NOT_TRUSTED"
  | M_UNSUPPORTED_ROOM_VERSION -> "M_UNSUPPORTED_ROOM_VERSION"
  | M_INCOMPATIBLE_ROOM_VERSION -> "M_INCOMPATIBLE_ROOM_VERSION"
  | M_BAD_STATE -> "M_BAD_STATE"
  | M_GUEST_ACCESS_FORBIDDEN -> "M_GUEST_ACCESS_FORBIDDEN"
  | M_CAPTCHA_NEEDED -> "M_CAPTCHA_NEEDED"
  | M_CAPTCHA_INVALID -> "M_CAPTCHA_INVALID"
  | M_MISSING_PARAM -> "M_MISSING_PARAM"
  | M_INVALID_PARAM -> "M_INVALID_PARAM"
  | M_TOO_LARGE -> "M_TOO_LARGE"
  | M_CANNOT_OVERWRITE_MEDIA -> "M_CANNOT_OVERWRITE_MEDIA"
  | M_WRONG_ROOM_KEYS_VERSION -> "M_WRONG_ROOM_KEYS_VERSION"
  | M_EXCLUSIVE -> "M_EXCLUSIVE"
  | M_RESOURCE_LIMIT_EXCEEDED -> "M_RESOURCE_LIMIT_EXCEEDED"
  | M_CANNOT_LEAVE_SERVER_NOTICE_ROOM -> "M_CANNOT_LEAVE_SERVER_NOTICE_ROOM"
  | M_WEAK_PASSWORD -> "M_WEAK_PASSWORD"
  | M_UNKNOWN_CODE s -> s

let errcode_of_string = function
  | "M_FORBIDDEN" -> M_FORBIDDEN
  | "M_UNKNOWN_TOKEN" -> M_UNKNOWN_TOKEN
  | "M_MISSING_TOKEN" -> M_MISSING_TOKEN
  | "M_BAD_JSON" -> M_BAD_JSON
  | "M_NOT_JSON" -> M_NOT_JSON
  | "M_NOT_FOUND" -> M_NOT_FOUND
  | "M_LIMIT_EXCEEDED" -> M_LIMIT_EXCEEDED
  | "M_UNRECOGNIZED" -> M_UNRECOGNIZED
  | "M_UNKNOWN" -> M_UNKNOWN
  | "M_UNAUTHORIZED" -> M_UNAUTHORIZED
  | "M_USER_DEACTIVATED" -> M_USER_DEACTIVATED
  | "M_USER_IN_USE" -> M_USER_IN_USE
  | "M_INVALID_USERNAME" -> M_INVALID_USERNAME
  | "M_ROOM_IN_USE" -> M_ROOM_IN_USE
  | "M_INVALID_ROOM_STATE" -> M_INVALID_ROOM_STATE
  | "M_THREEPID_IN_USE" -> M_THREEPID_IN_USE
  | "M_THREEPID_NOT_FOUND" -> M_THREEPID_NOT_FOUND
  | "M_THREEPID_AUTH_FAILED" -> M_THREEPID_AUTH_FAILED
  | "M_THREEPID_DENIED" -> M_THREEPID_DENIED
  | "M_SERVER_NOT_TRUSTED" -> M_SERVER_NOT_TRUSTED
  | "M_UNSUPPORTED_ROOM_VERSION" -> M_UNSUPPORTED_ROOM_VERSION
  | "M_INCOMPATIBLE_ROOM_VERSION" -> M_INCOMPATIBLE_ROOM_VERSION
  | "M_BAD_STATE" -> M_BAD_STATE
  | "M_GUEST_ACCESS_FORBIDDEN" -> M_GUEST_ACCESS_FORBIDDEN
  | "M_CAPTCHA_NEEDED" -> M_CAPTCHA_NEEDED
  | "M_CAPTCHA_INVALID" -> M_CAPTCHA_INVALID
  | "M_MISSING_PARAM" -> M_MISSING_PARAM
  | "M_INVALID_PARAM" -> M_INVALID_PARAM
  | "M_TOO_LARGE" -> M_TOO_LARGE
  | "M_CANNOT_OVERWRITE_MEDIA" -> M_CANNOT_OVERWRITE_MEDIA
  | "M_WRONG_ROOM_KEYS_VERSION" -> M_WRONG_ROOM_KEYS_VERSION
  | "M_EXCLUSIVE" -> M_EXCLUSIVE
  | "M_RESOURCE_LIMIT_EXCEEDED" -> M_RESOURCE_LIMIT_EXCEEDED
  | "M_CANNOT_LEAVE_SERVER_NOTICE_ROOM" -> M_CANNOT_LEAVE_SERVER_NOTICE_ROOM
  | "M_WEAK_PASSWORD" -> M_WEAK_PASSWORD
  | s -> M_UNKNOWN_CODE s

let errcode_jsont =
  Jsont.of_of_string ~kind:"errcode" ~enc:errcode_to_string (fun s ->
      Ok (errcode_of_string s))

type matrix_error = {
  errcode : errcode;
  error : string;
  retry_after_ms : int option;
  soft_logout : bool option;
}

let matrix_error_jsont =
  Jsont.Object.(
    map (fun errcode error retry_after_ms soft_logout ->
        { errcode; error; retry_after_ms; soft_logout })
    |> mem "errcode" errcode_jsont ~enc:(fun e -> e.errcode)
    |> mem "error" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun e -> e.error)
    |> opt_mem "retry_after_ms" Matrix_proto.Json.Codec.int ~enc:(fun e ->
        e.retry_after_ms)
    |> opt_mem "soft_logout" Jsont.bool ~enc:(fun e -> e.soft_logout)
    |> finish)

type t =
  | Matrix_error of matrix_error
  | Network_error of string
  | Policy_denied of string
  | Tls_error of string
  | Json_error of string
  | Http_error of { status : int; body : string }
  | No_session
  | No_content

let errcode = function Matrix_error e -> Some e.errcode | _ -> None

let equal a b =
  match (a, b) with
  | Matrix_error a, Matrix_error b ->
      a.errcode = b.errcode
      && String.equal a.error b.error
      && a.retry_after_ms = b.retry_after_ms
      && a.soft_logout = b.soft_logout
  | Network_error a, Network_error b -> String.equal a b
  | Policy_denied a, Policy_denied b -> String.equal a b
  | Tls_error a, Tls_error b -> String.equal a b
  | Json_error a, Json_error b -> String.equal a b
  | Http_error a, Http_error b ->
      a.status = b.status && String.equal a.body b.body
  | No_session, No_session | No_content, No_content -> true
  | _ -> false

let pp fmt = function
  | Matrix_error e ->
      Format.fprintf fmt "Matrix error %s: %s"
        (errcode_to_string e.errcode)
        e.error
  | Network_error s -> Format.fprintf fmt "Network error: %s" s
  | Policy_denied s -> Format.fprintf fmt "Request denied by policy: %s" s
  | Tls_error s -> Format.fprintf fmt "TLS error: %s" s
  | Json_error s -> Format.fprintf fmt "JSON error: %s" s
  | Http_error { status; body } ->
      Format.fprintf fmt "HTTP error %d: %s" status body
  | No_session -> Format.pp_print_string fmt "No session"
  | No_content -> Format.pp_print_string fmt "No content"

let to_string e = Format.asprintf "%a" pp e
