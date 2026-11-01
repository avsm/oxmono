type t =
  | Bearer of (unit -> string)
  | Basic of (unit -> string * string)
  | Header of string * (Middleware.request -> string)
  | Query of (string * string) list

let bearer token =
  (try ignore (Header.pair Header.authorization (`Bearer token))
   with Invalid_argument _ ->
     invalid_arg
       "Fetch.Credential.bearer: token is not an RFC 6750 b64token");
  Bearer (Fun.const token)

let basic ~user ~password =
  (try ignore (Header.pair Header.authorization (`Basic (user, password)))
   with Invalid_argument _ ->
     invalid_arg
       "Fetch.Credential.basic: credentials must be printable ASCII with no colon in the user-id");
  Basic (fun () -> (user, password))
