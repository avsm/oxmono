(* header_name.ml - HTTP header names *)

type t =
  | Cache_control
  | Connection
  | Date
  | Transfer_encoding
  | Upgrade
  | Via
  | Accept
  | Accept_charset
  | Accept_encoding
  | Accept_language
  | Accept_ranges
  | Authorization
  | Cookie
  | Expect
  | Host
  | If_match
  | If_modified_since
  | If_none_match
  | If_range
  | If_unmodified_since
  | Range
  | Referer
  | User_agent
  | Age
  | Etag
  | Location
  | Retry_after
  | Server
  | Set_cookie
  | Www_authenticate
  | Allow
  | Content_disposition
  | Content_encoding
  | Content_language
  | Content_length
  | Content_location
  | Content_range
  | Content_type
  | Expires
  | Last_modified
  | X_forwarded_for
  | X_forwarded_proto
  | X_forwarded_host
  | X_request_id
  | X_correlation_id
  | Other

(* Canonical header name string for known headers, "(unknown)" for Other *)
let canonical = function
  | Cache_control -> "Cache-Control"
  | Connection -> "Connection"
  | Date -> "Date"
  | Transfer_encoding -> "Transfer-Encoding"
  | Upgrade -> "Upgrade"
  | Via -> "Via"
  | Accept -> "Accept"
  | Accept_charset -> "Accept-Charset"
  | Accept_encoding -> "Accept-Encoding"
  | Accept_language -> "Accept-Language"
  | Accept_ranges -> "Accept-Ranges"
  | Authorization -> "Authorization"
  | Cookie -> "Cookie"
  | Expect -> "Expect"
  | Host -> "Host"
  | If_match -> "If-Match"
  | If_modified_since -> "If-Modified-Since"
  | If_none_match -> "If-None-Match"
  | If_range -> "If-Range"
  | If_unmodified_since -> "If-Unmodified-Since"
  | Range -> "Range"
  | Referer -> "Referer"
  | User_agent -> "User-Agent"
  | Age -> "Age"
  | Etag -> "ETag"
  | Location -> "Location"
  | Retry_after -> "Retry-After"
  | Server -> "Server"
  | Set_cookie -> "Set-Cookie"
  | Www_authenticate -> "WWW-Authenticate"
  | Allow -> "Allow"
  | Content_disposition -> "Content-Disposition"
  | Content_encoding -> "Content-Encoding"
  | Content_language -> "Content-Language"
  | Content_length -> "Content-Length"
  | Content_location -> "Content-Location"
  | Content_range -> "Content-Range"
  | Content_type -> "Content-Type"
  | Expires -> "Expires"
  | Last_modified -> "Last-Modified"
  | X_forwarded_for -> "X-Forwarded-For"
  | X_forwarded_proto -> "X-Forwarded-Proto"
  | X_forwarded_host -> "X-Forwarded-Host"
  | X_request_id -> "X-Req-Id"
  | X_correlation_id -> "X-Correlation-Id"
  | Other -> "(unknown)"
;;

let lowercase = function
  | Cache_control -> "cache-control"
  | Connection -> "connection"
  | Date -> "date"
  | Transfer_encoding -> "transfer-encoding"
  | Upgrade -> "upgrade"
  | Via -> "via"
  | Accept -> "accept"
  | Accept_charset -> "accept-charset"
  | Accept_encoding -> "accept-encoding"
  | Accept_language -> "accept-language"
  | Accept_ranges -> "accept-ranges"
  | Authorization -> "authorization"
  | Cookie -> "cookie"
  | Expect -> "expect"
  | Host -> "host"
  | If_match -> "if-match"
  | If_modified_since -> "if-modified-since"
  | If_none_match -> "if-none-match"
  | If_range -> "if-range"
  | If_unmodified_since -> "if-unmodified-since"
  | Range -> "range"
  | Referer -> "referer"
  | User_agent -> "user-agent"
  | Age -> "age"
  | Etag -> "etag"
  | Location -> "location"
  | Retry_after -> "retry-after"
  | Server -> "server"
  | Set_cookie -> "set-cookie"
  | Www_authenticate -> "www-authenticate"
  | Allow -> "allow"
  | Content_disposition -> "content-disposition"
  | Content_encoding -> "content-encoding"
  | Content_language -> "content-language"
  | Content_length -> "content-length"
  | Content_location -> "content-location"
  | Content_range -> "content-range"
  | Content_type -> "content-type"
  | Expires -> "expires"
  | Last_modified -> "last-modified"
  | X_forwarded_for -> "x-forwarded-for"
  | X_forwarded_proto -> "x-forwarded-proto"
  | X_forwarded_host -> "x-forwarded-host"
  | X_request_id -> "x-request-id"
  | X_correlation_id -> "x-correlation-id"
  | Other -> ""
;;

(* Parse header name from span. TODO: replace with a DFA *)
let of_span (local_ buf : bytes) (sp : Span.t) : t =
  match Span.len sp with
  | 3 ->
    if Span.equal_caseless buf sp "age"
    then Age
    else if Span.equal_caseless buf sp "via"
    then Via
    else Other
  | 4 ->
    if Span.equal_caseless buf sp "date"
    then Date
    else if Span.equal_caseless buf sp "etag"
    then Etag
    else if Span.equal_caseless buf sp "host"
    then Host
    else Other
  | 5 ->
    if Span.equal_caseless buf sp "allow"
    then Allow
    else if Span.equal_caseless buf sp "range"
    then Range
    else Other
  | 6 ->
    if Span.equal_caseless buf sp "accept"
    then Accept
    else if Span.equal_caseless buf sp "cookie"
    then Cookie
    else if Span.equal_caseless buf sp "expect"
    then Expect
    else if Span.equal_caseless buf sp "server"
    then Server
    else Other
  | 7 ->
    if Span.equal_caseless buf sp "expires"
    then Expires
    else if Span.equal_caseless buf sp "referer"
    then Referer
    else if Span.equal_caseless buf sp "upgrade"
    then Upgrade
    else Other
  | 8 ->
    if Span.equal_caseless buf sp "if-match"
    then If_match
    else if Span.equal_caseless buf sp "if-range"
    then If_range
    else if Span.equal_caseless buf sp "location"
    then Location
    else Other
  | 10 ->
    if Span.equal_caseless buf sp "connection"
    then Connection
    else if Span.equal_caseless buf sp "set-cookie"
    then Set_cookie
    else if Span.equal_caseless buf sp "user-agent"
    then User_agent
    else Other
  | 11 -> if Span.equal_caseless buf sp "retry-after" then Retry_after else Other
  | 12 ->
    if Span.equal_caseless buf sp "content-type"
    then Content_type
    else if Span.equal_caseless buf sp "x-request-id"
    then X_request_id
    else Other
  | 13 ->
    if Span.equal_caseless buf sp "accept-ranges"
    then Accept_ranges
    else if Span.equal_caseless buf sp "authorization"
    then Authorization
    else if Span.equal_caseless buf sp "cache-control"
    then Cache_control
    else if Span.equal_caseless buf sp "content-range"
    then Content_range
    else if Span.equal_caseless buf sp "if-none-match"
    then If_none_match
    else if Span.equal_caseless buf sp "last-modified"
    then Last_modified
    else Other
  | 14 ->
    if Span.equal_caseless buf sp "accept-charset"
    then Accept_charset
    else if Span.equal_caseless buf sp "content-length"
    then Content_length
    else Other
  | 15 ->
    if Span.equal_caseless buf sp "accept-encoding"
    then Accept_encoding
    else if Span.equal_caseless buf sp "accept-language"
    then Accept_language
    else if Span.equal_caseless buf sp "x-forwarded-for"
    then X_forwarded_for
    else if Span.equal_caseless buf sp "x-correlation-id"
    then X_correlation_id
    else Other
  | 16 ->
    if Span.equal_caseless buf sp "content-encoding"
    then Content_encoding
    else if Span.equal_caseless buf sp "content-language"
    then Content_language
    else if Span.equal_caseless buf sp "content-location"
    then Content_location
    else if Span.equal_caseless buf sp "www-authenticate"
    then Www_authenticate
    else if Span.equal_caseless buf sp "x-forwarded-host"
    then X_forwarded_host
    else Other
  | 17 ->
    if Span.equal_caseless buf sp "if-modified-since"
    then If_modified_since
    else if Span.equal_caseless buf sp "transfer-encoding"
    then Transfer_encoding
    else if Span.equal_caseless buf sp "x-forwarded-proto"
    then X_forwarded_proto
    else Other
  | 19 ->
    if Span.equal_caseless buf sp "content-disposition"
    then Content_disposition
    else if Span.equal_caseless buf sp "if-unmodified-since"
    then If_unmodified_since
    else Other
  | _ -> Other
;;

let pp fmt t =
  Stdlib.Format.fprintf fmt "%s" (canonical t)
;;
