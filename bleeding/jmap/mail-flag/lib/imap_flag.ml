type system = Seen | Answered | Flagged | Deleted | Draft

type t =
  | System of system
  | Recent
  | Keyword of string
  | Extension of string

let valid_atom s =
  let len = String.length s in
  len > 0
  && String.for_all
       (fun c ->
         let n = Char.code c in
         n > 32 && n < 127
         && not (String.contains "(){ %*\"\\]" c))
       s

let of_wire s =
  if not (String.length s > 0) then Error "empty IMAP flag"
  else if s.[0] = '\\' then
    let tail = String.sub s 1 (String.length s - 1) in
    if not (valid_atom tail) then Error "invalid IMAP system flag"
    else
      match String.lowercase_ascii tail with
      | "seen" -> Ok (System Seen)
      | "answered" -> Ok (System Answered)
      | "flagged" -> Ok (System Flagged)
      | "deleted" -> Ok (System Deleted)
      | "draft" -> Ok (System Draft)
      | "recent" -> Ok Recent
      | _ -> Ok (Extension s)
  else if valid_atom s then Ok (Keyword s)
  else Error "invalid IMAP keyword"

let system s = System s

let keyword s =
  match of_wire s with
  | Ok (Keyword _ as flag) -> Ok flag
  | Ok _ | Error _ -> Error "invalid IMAP keyword"

let to_wire = function
  | System Seen -> "\\Seen"
  | System Answered -> "\\Answered"
  | System Flagged -> "\\Flagged"
  | System Deleted -> "\\Deleted"
  | System Draft -> "\\Draft"
  | Recent -> "\\Recent"
  | Keyword s | Extension s -> s

let semantic = function
  | System Seen -> Some `Seen
  | System Answered -> Some `Answered
  | System Flagged -> Some `Flagged
  | System Deleted -> Some `Deleted
  | System Draft -> Some `Draft
  | Recent | Extension _ -> None
  | Keyword s ->
      (* A keyword has no system-flag semantics even when its name looks like
         one. Reporting no projection is preferable to conflating the two. *)
      (match Keyword.of_string s with
      | (`Seen | `Answered | `Flagged | `Deleted | `Draft) -> None
      | other -> Some other)

let compare a b =
  let rank = function
    | System _ -> 0
    | Recent -> 1
    | Keyword _ -> 2
    | Extension _ -> 3
  in
  let ra = rank a and rb = rank b in
  if ra <> rb then Int.compare ra rb
  else
    match (a, b) with
    | System a, System b -> Stdlib.compare a b
    | Recent, Recent -> 0
    | Keyword a, Keyword b | Extension a, Extension b ->
        String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)
    | _ -> assert false

let equal a b = compare a b = 0
let pp ppf t = Format.pp_print_string ppf (to_wire t)

let durable flags =
  List.filter (function Recent -> false | _ -> true) flags
  |> List.sort_uniq compare

let equal_durable left right =
  List.equal equal (durable left) (durable right)
