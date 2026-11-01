type sink = string -> unit
type slice_sink = #(string * int * int) -> unit

let media_type = "text/event-stream"

let invalid_line name value =
  if String.exists
       (function
         | '\r' | '\n' -> true
         | _ -> false)
       value
  then invalid_arg ("Httpz_media.Sse: " ^ name ^ " contains a newline")
;;

(* A comment has no field name and a field may have an empty value. Writing either as
   a zero-length chunk would terminate chunked framing in a sink that frames what it is
   given. *)
let write (sink : slice_sink @ local) #(value, off, len) =
  if len > 0 then sink #(value, off, len)
;;

let line (sink : slice_sink @ local) field #(value, off, len) =
  write sink #(field, 0, String.length field);
  sink #(": ", 0, 2);
  write sink #(value, off, len);
  sink #("\n", 0, 1)
;;

let iter_lines (f : slice_sink @ local) value =
  let length = String.length value in
  let line first last =
    let () = f #(value, first, last - first) in ()
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
  let () = loop 0 0 in ()
;;

let send_sub (sink : slice_sink @ local) ?name ?id data =
  Option.iter (invalid_line "event name") name;
  Option.iter
    (fun id ->
      invalid_line "event id" id;
      if String.contains id '\000' then invalid_arg "Httpz_media.Sse: event id contains NUL")
    id;
  (match name with None -> () | Some s -> line sink "event" #(s, 0, String.length s));
  iter_lines (line sink "data") data;
  (match id with None -> () | Some s -> line sink "id" #(s, 0, String.length s));
  sink #("\n", 0, 1)
;;

let comment_sub (sink : slice_sink @ local) text =
  if
    String.exists
      (fun c ->
         let n = Char.code c in
         (n < 0x20 && c <> '\t' && c <> '\r' && c <> '\n') || n = 0x7f)
      text
  then invalid_arg "Httpz_media.Sse: comment contains a forbidden control byte";
  iter_lines
    (fun #(text, off, len) ->
      sink #(": ", 0, 2);
      if len > 0 then sink #(text, off, len);
      sink #("\n", 0, 1))
    text;
  sink #("\n", 0, 1)
;;

let emit_owned sink #(value, off, len) =
  sink (if off = 0 && len = String.length value then value else String.sub value off len)

let send sink ?name ?id data = send_sub (emit_owned sink) ?name ?id data
let comment sink text = comment_sub (emit_owned sink) text

let retry sink milliseconds =
  if milliseconds < 0 then invalid_arg "Httpz_media.Sse: retry is negative";
  let value = string_of_int milliseconds in
  line (emit_owned sink) "retry" #(value, 0, String.length value);
  sink "\n"
;;
