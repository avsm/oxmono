let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
let spaced text = String.map (fun c -> if is_space c then ' ' else c) text

let argv text =
  String.split_on_char ' ' (spaced text)
  |> List.filter (fun word -> not (String.equal word ""))

let parse ~prefix body =
  let body = String.trim body in
  let width = String.length prefix in
  if String.length body <= width || not (String.starts_with ~prefix body) then
    None
  else
    let rest = String.sub body width (String.length body - width) in
    let stop =
      match String.index_opt (spaced rest) ' ' with
      | Some index -> index
      | None -> String.length rest
    in
    let name = String.sub rest 0 stop in
    let args = String.trim (String.sub rest stop (String.length rest - stop)) in
    if String.equal name "" then None else Some (name, args)

let find_word (c : Event.command) n = List.nth_opt c.argv n
let find_int c n = Option.bind (find_word c n) int_of_string_opt

let find_user c n =
  Option.bind (find_word c n) (fun word ->
      Result.to_option (Matrix_proto.Id.User_id.of_string word))
