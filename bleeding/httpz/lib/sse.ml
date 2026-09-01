type sink = string -> unit

let media_type = "text/event-stream"

let invalid_line name value =
  if String.exists
       (function
         | '\r' | '\n' -> true
         | _ -> false)
       value
  then invalid_arg ("Httpz.Sse: " ^ name ^ " contains a newline")
;;

let line sink field value =
  sink field;
  sink ": ";
  sink value;
  sink "\n"
;;

let iter_lines f value =
  let length = String.length value in
  let line first last =
    if first = 0 && last = length
    then f value
    else f (String.sub value first (last - first))
  in
  let rec loop first index =
    if index = length
    then line first index
    else (
      match value.[index] with
      | '\n' ->
        line first index;
        loop (index + 1) (index + 1)
      | '\r' ->
        line first index;
        let next =
          if index + 1 < length && Char.equal value.[index + 1] '\n'
          then index + 2
          else index + 1
        in
        loop next next
      | _ -> loop first (index + 1))
  in
  loop 0 0
;;

let send sink ?name ?id data =
  Option.iter (invalid_line "event name") name;
  Option.iter
    (fun id ->
      invalid_line "event id" id;
      if String.contains id '\000' then invalid_arg "Httpz.Sse: event id contains NUL")
    id;
  Option.iter (line sink "event") name;
  iter_lines (line sink "data") data;
  Option.iter (line sink "id") id;
  sink "\n"
;;

let comment sink text =
  if
    String.exists
      (fun c ->
         let n = Char.code c in
         (n < 0x20 && c <> '\t' && c <> '\r' && c <> '\n') || n = 0x7f)
      text
  then invalid_arg "Httpz.Sse: comment contains a forbidden control byte";
  iter_lines
    (fun text ->
      sink ": ";
      sink text;
      sink "\n")
    text;
  sink "\n"
;;

let retry sink milliseconds =
  if milliseconds < 0 then invalid_arg "Httpz.Sse: retry is negative";
  line sink "retry" (string_of_int milliseconds);
  sink "\n"
;;
