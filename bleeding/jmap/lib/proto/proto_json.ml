(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module String_set = Set.MakePortable (String)

let errorf fmt =
  Printf.ksprintf
    (Jsont.Error.make_msg Jsont.Error.Context.empty Jsont.Meta.none)
    fmt

let is_noncharacter u =
  (u >= 0xfdd0 && u <= 0xfdef)
  || u land 0xffff = 0xfffe
  || u land 0xffff = 0xffff

let check_string ~(kind : string) s =
  let rec loop i =
    if i = String.length s then Ok ()
    else
      let decoded = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid decoded) then
        Error (errorf "JMAP I-JSON %s is not valid UTF-8 at byte %d" kind i)
      else
        let u = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
        if is_noncharacter u then
          Error (errorf "JMAP I-JSON %s contains noncharacter U+%04X" kind u)
        else loop (i + Uchar.utf_decode_length decoded)
  in
  loop 0

let check_number number =
  if Float.is_finite number then Ok ()
  else Error (errorf "JMAP I-JSON number exceeds binary64 magnitude")

let rec check_value = function
  | Jsont.Null _ | Bool _ -> Ok ()
  | Number (number, _) -> check_number number
  | String (value, _) -> check_string ~kind:"string" value
  | Array (values, _) -> check_values values
  | Object (members, _) -> check_value_members (String_set.of_list []) members

and check_values = function
  | [] -> Ok ()
  | value :: values ->
      Result.bind (check_value value) (fun () -> check_values values)

and check_value_members names = function
  | [] -> Ok ()
  | ((name, _), value) :: members ->
      if String_set.mem name names then
        Error (errorf "JMAP I-JSON object has duplicate member %S" name)
      else
        Result.bind (check_string ~kind:"object member name" name) (fun () ->
            Result.bind (check_value value) (fun () ->
                check_value_members (String_set.add name names) members))

let check s =
  let decoder = Jsonm.decoder ~encoding:`UTF_8 (`String s) in
  (* Jsonm emits [`Name] only inside an object and a close only after the
     matching open, so [objects] is never empty at either point. *)
  let rec loop objects =
    match Jsonm.decode decoder with
    | `Lexeme `Os -> loop (Some (String_set.of_list []) :: objects)
    | `Lexeme `As -> loop (None :: objects)
    | `Lexeme (`Name name) -> (
        match objects with
        | Some names :: rest ->
            if String_set.mem name names then
              Error (errorf "JMAP I-JSON object has duplicate member %S" name)
            else
              Result.bind (check_string ~kind:"object member name" name)
                (fun () -> loop (Some (String_set.add name names) :: rest))
        | None :: _ | [] -> assert false)
    | `Lexeme (`String value) ->
        Result.bind (check_string ~kind:"string" value) (fun () -> loop objects)
    | `Lexeme (`Float number) ->
        Result.bind (check_number number) (fun () -> loop objects)
    | `Lexeme (`Oe | `Ae) -> (
        match objects with _ :: rest -> loop rest | [] -> assert false)
    | `Lexeme _ -> loop objects
    | `End -> Ok ()
    | `Error error ->
        Error
          (errorf "invalid JMAP JSON: %s"
             (Format.asprintf "%a" Jsonm.pp_error error))
    | `Await -> assert false
  in
  loop []

let decode ?locs ?(max_depth = Httpz_media.Json.default_max_depth) jsont s =
  if max_depth < 0 then
    invalid_arg "Jmap.Proto.Json.decode: max_depth must be non-negative";
  Result.bind (check s) (fun () ->
      Httpz_media.Json.decode_string' ?locs ~max_depth jsont s)

let encode ?format jsont value =
  Result.bind (Jsont.Json.encode' jsont value) (fun json ->
      Result.bind (check_value json) (fun () ->
          Jsont_bytesrw.encode_string' ?format Jsont.json json))

let pp jsont ppf value =
  match Jsont.Json.encode' jsont value with
  | Ok json -> Jsont.Json.pp ppf json
  | Error e -> Jsont.pp_string ppf (Jsont.Error.to_string e)

let loc_of_error (_, meta, _) =
  let loc = Jsont.Meta.textloc meta in
  if Jsont.Textloc.is_none loc then None
  else
    Some
      (Httpz_media.Loc.v
         ~first_byte:(Jsont.Textloc.first_byte loc)
         ~last_byte:(Jsont.Textloc.last_byte loc)
         ~first_line:(Jsont.Textloc.first_line loc)
         ~last_line:(Jsont.Textloc.last_line loc))

let malformed error =
  Httpz_media.malformed ?loc:(loc_of_error error)
    ~detail:(Httpz_media.Json.Error error)
    (Httpz_media.sanitize_diagnostic (Jsont.Error.to_string error))

let media ?(media = "application/json") ?(accept = [ "application/*+json" ])
    ?format ?(locs = true) ?max_depth jsont =
  Httpz_media.v_reader ~accept media
    ~encode:(fun value writer ->
      match encode ?format jsont value with
      | Ok encoded -> Bytesrw.Bytes.Writer.write_string writer encoded
      | Error error ->
          invalid_arg
            ("Jmap.Proto.Json: "
            ^ Httpz_media.sanitize_diagnostic (Jsont.Error.to_string error)))
    ~decode:(fun reader ->
      let source = Bytesrw.Bytes.Reader.to_string reader in
      Result.map_error malformed (decode ~locs ?max_depth jsont source))
