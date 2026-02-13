type t =
  | Connection_failed of string
  | Query_failed of string
  | Invalid_result
  | Stream_exhausted
  | Use_after_close

exception E of t

let to_string = function
  | Connection_failed msg -> "Connection failed: " ^ msg
  | Query_failed msg -> "Query failed: " ^ msg
  | Invalid_result -> "Invalid result"
  | Stream_exhausted -> "Stream exhausted"
  | Use_after_close -> "Use after close"

let pp fmt t = Format.pp_print_string fmt (to_string t)

let () =
  Printexc.Safe.register_printer (function
    | E e -> Some ("Chdb_error: " ^ to_string e)
    | _ -> None)

let raise_connection_failed msg = raise (E (Connection_failed msg))
let raise_query_failed msg = raise (E (Query_failed msg))
