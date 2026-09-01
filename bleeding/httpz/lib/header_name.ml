type t =
  | Cache_control
  | Connection
  | Date
  | Transfer_encoding
  | Trailer
  | Te
  | Upgrade
  | Via
  | Accept
  | Accept_charset
  | Accept_encoding
  | Accept_language
  | Accept_ranges
  | Authorization
  | Proxy_authorization
  | Proxy_authenticate
  | Cookie
  | Expect
  | Host
  | Max_forwards
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
  | Vary
  | X_correlation_id
  | X_cache
  | Depth
  | Destination
  | Overwrite
  | Lock_token
  | Dav
  | If
  | Access_control_allow_origin
  | Access_control_allow_methods
  | Access_control_allow_headers
  | Other

let canonical = function
  | Cache_control -> "Cache-Control"
  | Connection -> "Connection"
  | Date -> "Date"
  | Transfer_encoding -> "Transfer-Encoding"
  | Trailer -> "Trailer"
  | Te -> "TE"
  | Upgrade -> "Upgrade"
  | Via -> "Via"
  | Accept -> "Accept"
  | Accept_charset -> "Accept-Charset"
  | Accept_encoding -> "Accept-Encoding"
  | Accept_language -> "Accept-Language"
  | Accept_ranges -> "Accept-Ranges"
  | Authorization -> "Authorization"
  | Proxy_authorization -> "Proxy-Authorization"
  | Proxy_authenticate -> "Proxy-Authenticate"
  | Cookie -> "Cookie"
  | Expect -> "Expect"
  | Host -> "Host"
  | Max_forwards -> "Max-Forwards"
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
  | X_request_id -> "X-Request-Id"
  | Vary -> "Vary"
  | X_correlation_id -> "X-Correlation-Id"
  | X_cache -> "X-Cache"
  | Depth -> "Depth"
  | Destination -> "Destination"
  | Overwrite -> "Overwrite"
  | Lock_token -> "Lock-Token"
  | Dav -> "DAV"
  | If -> "If"
  | Access_control_allow_origin -> "Access-Control-Allow-Origin"
  | Access_control_allow_methods -> "Access-Control-Allow-Methods"
  | Access_control_allow_headers -> "Access-Control-Allow-Headers"
  | Other -> "(unknown)"
;;

let of_span (local_ (buf : bytes)) (sp : Span.t) : t =
  let n = Span.len sp in
  if n < 2 || n > 28
  then Other
  else (
    let c0 =
      match Bytes.unsafe_get buf (Span.off sp) with
      | 'A' .. 'Z' as c -> Char.unsafe_chr (Char.code c + 32)
      | c -> c
    in
    let[@inline] eq lit = Span.equal_caseless buf sp lit in
    match n, c0 with
    | 2, 'i' -> if eq "if" then If else Other
    | 2, 't' -> if eq "te" then Te else Other
    | 3, 'a' -> if eq "age" then Age else Other
    | 3, 'v' -> if eq "via" then Via else Other
    | 3, 'd' -> if eq "dav" then Dav else Other
    | 4, 'd' -> if eq "date" then Date else Other
    | 4, 'e' -> if eq "etag" then Etag else Other
    | 4, 'h' -> if eq "host" then Host else Other
    | 4, 'v' -> if eq "vary" then Vary else Other
    | 5, 'a' -> if eq "allow" then Allow else Other
    | 5, 'r' -> if eq "range" then Range else Other
    | 5, 'd' -> if eq "depth" then Depth else Other
    | 6, 'a' -> if eq "accept" then Accept else Other
    | 6, 'c' -> if eq "cookie" then Cookie else Other
    | 6, 'e' -> if eq "expect" then Expect else Other
    | 6, 's' -> if eq "server" then Server else Other
    | 7, 'e' -> if eq "expires" then Expires else Other
    | 7, 'r' -> if eq "referer" then Referer else Other
    | 7, 't' -> if eq "trailer" then Trailer else Other
    | 7, 'u' -> if eq "upgrade" then Upgrade else Other
    | 7, 'x' -> if eq "x-cache" then X_cache else Other
    | 8, 'i' ->
      if eq "if-match" then If_match else if eq "if-range" then If_range else Other
    | 8, 'l' -> if eq "location" then Location else Other
    | 9, 'o' -> if eq "overwrite" then Overwrite else Other
    | 10, 'c' -> if eq "connection" then Connection else Other
    | 10, 's' -> if eq "set-cookie" then Set_cookie else Other
    | 10, 'u' -> if eq "user-agent" then User_agent else Other
    | 10, 'l' -> if eq "lock-token" then Lock_token else Other
    | 11, 'r' -> if eq "retry-after" then Retry_after else Other
    | 11, 'd' -> if eq "destination" then Destination else Other
    | 12, 'c' -> if eq "content-type" then Content_type else Other
    | 12, 'm' -> if eq "max-forwards" then Max_forwards else Other
    | 12, 'x' -> if eq "x-request-id" then X_request_id else Other
    | 13, 'a' ->
      if eq "accept-ranges"
      then Accept_ranges
      else if eq "authorization"
      then Authorization
      else Other
    | 13, 'c' ->
      if eq "cache-control"
      then Cache_control
      else if eq "content-range"
      then Content_range
      else Other
    | 13, 'i' -> if eq "if-none-match" then If_none_match else Other
    | 13, 'l' -> if eq "last-modified" then Last_modified else Other
    | 14, 'a' -> if eq "accept-charset" then Accept_charset else Other
    | 14, 'c' -> if eq "content-length" then Content_length else Other
    | 15, 'a' ->
      if eq "accept-encoding"
      then Accept_encoding
      else if eq "accept-language"
      then Accept_language
      else Other
    | 15, 'x' -> if eq "x-forwarded-for" then X_forwarded_for else Other
    | 16, 'c' ->
      if eq "content-encoding"
      then Content_encoding
      else if eq "content-language"
      then Content_language
      else if eq "content-location"
      then Content_location
      else Other
    | 16, 'w' -> if eq "www-authenticate" then Www_authenticate else Other
    | 16, 'x' ->
      if eq "x-forwarded-host"
      then X_forwarded_host
      else if eq "x-correlation-id"
      then X_correlation_id
      else Other
    | 17, 'i' -> if eq "if-modified-since" then If_modified_since else Other
    | 17, 't' -> if eq "transfer-encoding" then Transfer_encoding else Other
    | 17, 'x' -> if eq "x-forwarded-proto" then X_forwarded_proto else Other
    | 18, 'p' -> if eq "proxy-authenticate" then Proxy_authenticate else Other
    | 19, 'c' -> if eq "content-disposition" then Content_disposition else Other
    | 19, 'i' -> if eq "if-unmodified-since" then If_unmodified_since else Other
    | 19, 'p' -> if eq "proxy-authorization" then Proxy_authorization else Other
    | 27, 'a' ->
      if eq "access-control-allow-origin" then Access_control_allow_origin else Other
    | 28, 'a' ->
      if eq "access-control-allow-methods"
      then Access_control_allow_methods
      else if eq "access-control-allow-headers"
      then Access_control_allow_headers
      else Other
    | _, _ -> Other)
;;

let pp fmt t = Stdlib.Format.fprintf fmt "%s" (canonical t)
