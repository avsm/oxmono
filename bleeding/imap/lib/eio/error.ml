type t =
  | Closed
  | Protocol of string
  | Transport of string
  | Rejected of { tag : string; status : [ `No | `Bad ];
      code : Imap.Response.code option; text : string }
  | State of string
  | Missing_uid of int64
  | Limit of string
  | Uncertain of string

let pp ppf = function
  | Closed -> Format.pp_print_string ppf "IMAP connection closed"
  | Protocol s -> Format.fprintf ppf "IMAP protocol error: %s" s
  | Transport s -> Format.fprintf ppf "IMAP transport error: %s" s
  | Rejected {tag; status; code; text} ->
      let label = Option.bind code Imap.Response.response_code_name in
      Format.fprintf ppf "IMAP %s %s%s: %s" tag
        (match status with `No -> "NO" | `Bad -> "BAD")
        (match label with None -> "" | Some name -> " [" ^ name ^ "]") text
  | State s -> Format.fprintf ppf "IMAP state error: %s" s
  | Missing_uid uid -> Format.fprintf ppf "IMAP UID %Ld vanished" uid
  | Limit s -> Format.fprintf ppf "IMAP limit error: %s" s
  | Uncertain s -> Format.fprintf ppf "IMAP uncertain outcome: %s" s

let to_string e = Format.asprintf "%a" pp e
