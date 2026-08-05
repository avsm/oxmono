type t =
  | Bearer of (unit -> string)
  | Header of string * (Middleware.request -> string)
  | Query of (string * string) list
