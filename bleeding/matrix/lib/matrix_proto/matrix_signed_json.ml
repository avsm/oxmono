type error = [ `Msg of string ]

let add_json_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let invalid kind detail = Error (`Msg (kind ^ ": " ^ detail))

let add_json_number buf f =
  if not (Float.is_finite f) then invalid "canonical JSON number" "not finite"
  else if not (Float.is_integer f) then
    invalid "canonical JSON number" "must be an integer"
  else if Float.abs f > 9_007_199_254_740_991. then
    invalid "canonical JSON number" "outside the Matrix-safe integer range"
  else (
    (* [Printf] renders negative zero as [-0]. Matrix canonical JSON uses [0]
       for both signs of zero. *)
    if f = 0. then Buffer.add_string buf "0"
    else Buffer.add_string buf (Printf.sprintf "%.0f" f);
    Ok ())

let rec add_json buf (j : Jsont.json) =
  match j with
  | Jsont.Null _ ->
      Buffer.add_string buf "null";
      Ok ()
  | Jsont.Bool (b, _) ->
      Buffer.add_string buf (if b then "true" else "false");
      Ok ()
  | Jsont.Number (f, _) -> add_json_number buf f
  | Jsont.String (s, _) ->
      add_json_string buf s;
      Ok ()
  | Jsont.Array (l, _) ->
      Buffer.add_char buf '[';
      let rec add_values first = function
        | [] -> Ok ()
        | value :: rest -> (
            if not first then Buffer.add_char buf ',';
            match add_json buf value with
            | Error _ as error -> error
            | Ok () -> add_values false rest)
      in
      let result = add_values true l in
      Buffer.add_char buf ']';
      result
  | Jsont.Object (mems, _) ->
      let mems =
        List.stable_sort
          (fun ((a, _), _) ((b, _), _) -> String.compare a b)
          mems
      in
      Buffer.add_char buf '{';
      let rec add_members first = function
        | [] -> Ok ()
        | ((name, _), value) :: rest -> (
            if not first then Buffer.add_char buf ',';
            add_json_string buf name;
            Buffer.add_char buf ':';
            match add_json buf value with
            | Error _ as error -> error
            | Ok () -> add_members false rest)
      in
      let result = add_members true mems in
      Buffer.add_char buf '}';
      result

let canonical_json_result j =
  match Matrix_json.Codec.validate j with
  | Error message -> Error (`Msg message)
  | Ok () -> (
      let buf = Buffer.create 256 in
      match add_json buf j with
      | Error _ as error -> error
      | Ok () -> Ok (Buffer.contents buf))

let canonical_json j =
  match canonical_json_result j with
  | Ok value -> value
  | Error (`Msg message) ->
      invalid_arg ("Matrix_proto.Signed_json.canonical_json: " ^ message)

let json_for_signing (j : Jsont.json) =
  (* Validate before dropping [signatures] and [unsigned]. Otherwise duplicate
     names in one of the dropped members could make an ambiguous input appear
     valid merely because it is not covered by the resulting bytes. *)
  (match canonical_json_result j with
  | Ok _ -> ()
  | Error (`Msg message) ->
      invalid_arg ("Matrix_proto.Signed_json.json_for_signing: " ^ message));
  match j with
  | Jsont.Object (mems, meta) ->
      let keep ((name, _), _) = name <> "signatures" && name <> "unsigned" in
      Jsont.Object (List.filter keep mems, meta)
  | j -> j
