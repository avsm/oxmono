type t =
  [ `OK
  | `Created
  | `Accepted
  | `No_content
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Temporary_redirect
  | `Permanent_redirect
  | `Bad_request
  | `Unauthorized
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Payload_too_large
  | `Unsupported_media_type
  | `Unprocessable_entity
  | `Too_many_requests
  | `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout ]

let code = function
  | `OK -> 200
  | `Created -> 201
  | `Accepted -> 202
  | `No_content -> 204
  | `Moved_permanently -> 301
  | `Found -> 302
  | `See_other -> 303
  | `Not_modified -> 304
  | `Temporary_redirect -> 307
  | `Permanent_redirect -> 308
  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Not_acceptable -> 406
  | `Request_timeout -> 408
  | `Conflict -> 409
  | `Gone -> 410
  | `Length_required -> 411
  | `Precondition_failed -> 412
  | `Payload_too_large -> 413
  | `Unsupported_media_type -> 415
  | `Unprocessable_entity -> 422
  | `Too_many_requests -> 429
  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Bad_gateway -> 502
  | `Service_unavailable -> 503
  | `Gateway_timeout -> 504

(* The phrases are those of RFC 9110, which renamed 422 to "Unprocessable
   Content". The constructor keeps the older spelling that every other HTTP
   library uses. *)
let reason = function
  | `OK -> "OK"
  | `Created -> "Created"
  | `Accepted -> "Accepted"
  | `No_content -> "No Content"
  | `Moved_permanently -> "Moved Permanently"
  | `Found -> "Found"
  | `See_other -> "See Other"
  | `Not_modified -> "Not Modified"
  | `Temporary_redirect -> "Temporary Redirect"
  | `Permanent_redirect -> "Permanent Redirect"
  | `Bad_request -> "Bad Request"
  | `Unauthorized -> "Unauthorized"
  | `Forbidden -> "Forbidden"
  | `Not_found -> "Not Found"
  | `Method_not_allowed -> "Method Not Allowed"
  | `Not_acceptable -> "Not Acceptable"
  | `Request_timeout -> "Request Timeout"
  | `Conflict -> "Conflict"
  | `Gone -> "Gone"
  | `Length_required -> "Length Required"
  | `Precondition_failed -> "Precondition Failed"
  | `Payload_too_large -> "Payload Too Large"
  | `Unsupported_media_type -> "Unsupported Media Type"
  | `Unprocessable_entity -> "Unprocessable Content"
  | `Too_many_requests -> "Too Many Requests"
  | `Internal_server_error -> "Internal Server Error"
  | `Not_implemented -> "Not Implemented"
  | `Bad_gateway -> "Bad Gateway"
  | `Service_unavailable -> "Service Unavailable"
  | `Gateway_timeout -> "Gateway Timeout"
