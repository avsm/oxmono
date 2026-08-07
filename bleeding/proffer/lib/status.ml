type t =
  [ `OK
  | `Created
  | `No_content
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Bad_request
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Conflict
  | `Length_required
  | `Payload_too_large
  | `Unsupported_media_type
  | `Internal_server_error
  | `Not_implemented ]

let code = function
  | `OK -> 200
  | `Created -> 201
  | `No_content -> 204
  | `Moved_permanently -> 301
  | `Found -> 302
  | `See_other -> 303
  | `Not_modified -> 304
  | `Bad_request -> 400
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Conflict -> 409
  | `Length_required -> 411
  | `Payload_too_large -> 413
  | `Unsupported_media_type -> 415
  | `Internal_server_error -> 500
  | `Not_implemented -> 501

let reason = function
  | `OK -> "OK"
  | `Created -> "Created"
  | `No_content -> "No Content"
  | `Moved_permanently -> "Moved Permanently"
  | `Found -> "Found"
  | `See_other -> "See Other"
  | `Not_modified -> "Not Modified"
  | `Bad_request -> "Bad Request"
  | `Forbidden -> "Forbidden"
  | `Not_found -> "Not Found"
  | `Method_not_allowed -> "Method Not Allowed"
  | `Conflict -> "Conflict"
  | `Length_required -> "Length Required"
  | `Payload_too_large -> "Payload Too Large"
  | `Unsupported_media_type -> "Unsupported Media Type"
  | `Internal_server_error -> "Internal Server Error"
  | `Not_implemented -> "Not Implemented"
