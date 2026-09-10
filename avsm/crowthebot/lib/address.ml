let member name = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((key, _), value) -> if key = name then Some value else None)
        fields
  | _ -> None

let mentions ~self content =
  match Option.bind (member "m.mentions" content) (member "user_ids") with
  | Some (Jsont.Array (users, _)) ->
      List.exists
        (function Jsont.String (user, _) -> user = self | _ -> false)
        users
  | _ -> false

let body ~reply text =
  (* Legacy reply fallbacks quote another sender's text and identity. *)
  if reply && String.starts_with ~prefix:"> " text then
    let rec skip = function
      | line :: rest when String.starts_with ~prefix:">" line -> skip rest
      | "" :: rest -> String.concat "\n" rest
      | _ -> text
    in
    skip (String.split_on_char '\n' text)
  else text

let whitespace = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let prefix name text =
  let n = String.length name in
  if text = name then Some ""
  else if String.starts_with ~prefix:name text && whitespace text.[n] then
    Some (String.trim (String.sub text n (String.length text - n)))
  else None

let mention_at ~self text i =
  let n = String.length self in
  let before =
    i = 0 || whitespace text.[i - 1] || List.mem text.[i - 1] [ '('; '[' ]
  in
  let after = i + n in
  before
  && after <= String.length text
  && String.sub text i n = self
  && (after = String.length text
     || whitespace text.[after]
     || List.mem text.[after] [ ':'; ','; '!'; '?'; ')'; ']' ])

let command ~self ~mentioned ~direct text =
  let text = String.trim text in
  let help text = if text = "" then "help" else text in
  match prefix "!crow" text with
  | Some text -> Some (help text)
  | None ->
      if mention_at ~self text 0 then
        let n = String.length self in
        let n =
          if n < String.length text && List.mem text.[n] [ ':'; ',' ] then n + 1
          else n
        in
        let rest = String.trim (String.sub text n (String.length text - n)) in
        Some (help (Option.value ~default:rest (prefix "!crow" rest)))
      else
        let rec contains i =
          i < String.length text && (mention_at ~self text i || contains (i + 1))
        in
        if text <> "" && (direct || mentioned || contains 0) then Some text
        else if mentioned then Some "help"
        else None

let direct_peer ?admin ~self ~marked ~complete members =
  if not complete then None
  else
    match List.sort_uniq String.compare members with
    | [ a; b ] when a = self && (marked || admin = Some b) -> Some b
    | [ a; b ] when b = self && (marked || admin = Some a) -> Some a
    | _ -> None
