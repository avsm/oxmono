module String_map = Map.MakePortable (String)
module String_set = Set.MakePortable (String)

module Codec = struct
  type 'a t = 'a Jsont.t

  let max_safe_integer = 9_007_199_254_740_991L
  let min_safe_integer = Int64.neg max_safe_integer
  let error meta kind message = Jsont.Error.msgf meta "%s: %s" kind message

  let checked_string ~kind =
    let check meta value =
      if String.is_valid_utf_8 value then value
      else error meta kind "value is not valid UTF-8"
    in
    Jsont.Base.string
      (Jsont.Base.map ~kind ~dec:check ~enc:(check Jsont.Meta.none) ())

  let bool = Jsont.bool
  let string = checked_string ~kind:"Matrix JSON string"

  let number =
    let check meta value =
      if Float.is_finite value then value
      else error meta "Matrix JSON number" "value must be finite"
    in
    Jsont.Base.number
      (Jsont.Base.map ~kind:"Matrix JSON number" ~dec:check
         ~enc:(check Jsont.Meta.none) ())

  let check_range_args caller min max =
    if min > max then invalid_arg (caller ^ ": minimum exceeds maximum");
    if min < min_safe_integer || max > max_safe_integer then
      invalid_arg (caller ^ ": bounds exceed the Matrix-safe integer range")

  let int64_range ~min ~max =
    check_range_args "Matrix_proto.Json.Codec.int64_range" min max;
    let kind = "Matrix JSON integer" in
    let decode meta value =
      if not (Float.is_finite value) then error meta kind "value must be finite"
      else if not (Float.is_integer value) then
        error meta kind "value must not have a fractional part"
      else if
        Float.compare value (Int64.to_float min) < 0
        || Float.compare value (Int64.to_float max) > 0
      then
        error meta kind
          (Printf.sprintf "value must be between %Ld and %Ld" min max)
      else Int64.of_float value
    in
    let encode value =
      if value < min || value > max then
        error Jsont.Meta.none kind
          (Printf.sprintf "value must be between %Ld and %Ld" min max)
      else Int64.to_float value
    in
    Jsont.Base.number (Jsont.Base.map ~kind ~dec:decode ~enc:encode ())

  let int64 = int64_range ~min:min_safe_integer ~max:max_safe_integer
  let uint64 = int64_range ~min:0L ~max:max_safe_integer

  let int_range ~min ~max =
    if min > max then
      invalid_arg "Matrix_proto.Json.Codec.int_range: minimum exceeds maximum";
    let min64 = Int64.of_int min and max64 = Int64.of_int max in
    let codec = int64_range ~min:min64 ~max:max64 in
    Jsont.map ~dec:Int64.to_int ~enc:Int64.of_int codec

  let platform_min =
    if Int64.of_int min_int < min_safe_integer then min_safe_integer
    else Int64.of_int min_int

  let platform_max =
    if Int64.of_int max_int > max_safe_integer then max_safe_integer
    else Int64.of_int max_int

  let int =
    Jsont.map ~dec:Int64.to_int ~enc:Int64.of_int
      (int64_range ~min:platform_min ~max:platform_max)

  let uint = int_range ~min:0 ~max:(Int64.to_int platform_max)
  let int8 = int_range ~min:(-128) ~max:127
  let int16 = int_range ~min:(-32_768) ~max:32_767
  let uint8 = int_range ~min:0 ~max:255
  let uint16 = int_range ~min:0 ~max:65_535

  let int32 =
    Jsont.map ~dec:Int64.to_int32 ~enc:Int64.of_int32
      (int64_range
         ~min:(Int64.of_int32 Int32.min_int)
         ~max:(Int64.of_int32 Int32.max_int))

  let nullable codec = Jsont.option codec

  let rec validate_at path = function
    | Jsont.Null _ | Jsont.Bool _ -> Ok ()
    | Jsont.Number (value, _) ->
        if Float.is_finite value then Ok ()
        else Error (path ^ ": number must be finite")
    | Jsont.String (value, _) ->
        if String.is_valid_utf_8 value then Ok ()
        else Error (path ^ ": string is not valid UTF-8")
    | Jsont.Array (values, _) -> validate_array path 0 values
    | Jsont.Object (members, _) ->
        validate_object path (String_set.of_list []) members

  and validate_array path index = function
    | [] -> Ok ()
    | value :: rest -> (
        match validate_at (Printf.sprintf "%s[%d]" path index) value with
        | Error _ as error -> error
        | Ok () -> validate_array path (index + 1) rest)

  and validate_object path names = function
    | [] -> Ok ()
    | ((name, _), value) :: rest -> (
        if not (String.is_valid_utf_8 name) then
          Error (path ^ ": object member name is not valid UTF-8")
        else if String_set.mem name names then
          Error (Printf.sprintf "%s: duplicate object member %S" path name)
        else
          match validate_at (path ^ "." ^ name) value with
          | Error _ as error -> error
          | Ok () -> validate_object path (String_set.add name names) rest)

  let validate value = validate_at "$" value

  let json =
    let check value =
      match validate value with
      | Ok () -> ()
      | Error message -> Jsont.Error.msg Jsont.Meta.none message
    in
    Jsont.iter ~dec:check ~enc:check Jsont.json

  type container = Array | Object of String_set.t

  let validate_text source =
    let decoder = Jsonm.decoder ~encoding:`UTF_8 (`String source) in
    let range () =
      let (first_line, first_column), _ = Jsonm.decoded_range decoder in
      Printf.sprintf "line %d, column %d" first_line first_column
    in
    let rec loop stack =
      match Jsonm.decode decoder with
      | `Await ->
          Error
            (Printf.sprintf "%s: string decoder unexpectedly requested input"
               (range ()))
      | `End -> Ok ()
      | `Error jsonm_error ->
          Error (Format.asprintf "%s: %a" (range ()) Jsonm.pp_error jsonm_error)
      | `Lexeme `Os -> loop (Object (String_set.of_list []) :: stack)
      | `Lexeme `As -> loop (Array :: stack)
      | `Lexeme `Oe | `Lexeme `Ae ->
          loop (match stack with [] -> [] | _ :: rest -> rest)
      | `Lexeme (`Name name) -> (
          match stack with
          | Object names :: rest ->
              if String_set.mem name names then
                Error
                  (Printf.sprintf "%s: duplicate object member %S" (range ())
                     name)
              else loop (Object (String_set.add name names) :: rest)
          | _ -> loop stack)
      | `Lexeme (`Null | `Bool _ | `Float _ | `String _) -> loop stack
    in
    loop []

  let string_map codec =
    let decode_value (name, value) =
      match Jsont.Json.decode codec value with
      | Ok value -> (name, value)
      | Error message ->
          Jsont.Error.msgf Jsont.Meta.none "object member %S: %s" name message
    in
    let encode_value (name, value) =
      match Jsont.Json.encode codec value with
      | Ok value -> Jsont.Json.mem (Jsont.Json.name name) value
      | Error message ->
          Jsont.Error.msgf Jsont.Meta.none "object member %S: %s" name message
    in
    Jsont.map
      ~dec:(function
        | Jsont.Object (members, _) ->
            members
            |> List.map (fun ((name, _), value) -> decode_value (name, value))
            |> List.sort (fun (left, _) (right, _) -> String.compare left right)
        | value -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object value)
      ~enc:(fun bindings ->
        let bindings =
          List.sort
            (fun (left, _) (right, _) -> String.compare left right)
            bindings
        in
        Jsont.Json.object' (List.map encode_value bindings))
      json

  let check_map_key meta name =
    if not (String.is_valid_utf_8 name) then
      error meta "Matrix JSON object" "member name is not valid UTF-8"

  let string_map_mems codec =
    let dec_add meta name value members =
      check_map_key meta name;
      if String_map.mem name members then
        Jsont.Error.msgf meta "duplicate object member %S" name;
      String_map.add name value members
    in
    let enc =
      {
        Jsont.Object.Mems.enc =
          (fun add members acc ->
            String_map.fold
              (fun name value acc ->
                check_map_key Jsont.Meta.none name;
                add Jsont.Meta.none name value acc)
              members acc);
      }
    in
    Jsont.Object.Mems.map
      ~dec_empty:(fun () -> String_map.of_seq Seq.empty)
      ~dec_add
      ~dec_finish:(fun _ members -> members)
      ~enc codec

  let as_string_map codec =
    Jsont.Object.(
      map Fun.id |> keep_unknown (string_map_mems codec) ~enc:Fun.id |> finish)

  let keyed_map ?(skip_invalid = false) ~(what : string)
      ~(of_string @ portable) ~(to_string @ portable) codec =
    let decode_key (name, value) =
      match of_string name with
      | Ok key -> Some (key, value)
      | Error (`Msg message) ->
          if skip_invalid then None
          else
            Jsont.Error.msgf Jsont.Meta.none "invalid %s %S: %s" what name
              message
    in
    Jsont.map
      ~dec:(fun value -> (List.filter_map decode_key) value)
      ~enc:(fun values ->
        List.map (fun (key, value) -> (to_string key, value)) values)
      (string_map codec)

  let ptime =
    Jsont.map ~kind:"RFC 3339 timestamp"
      ~dec:(fun value ->
        match Ptime.of_rfc3339 value with
        | Ok (time, _, _) -> time
        | Error (`RFC3339 (_, error)) ->
            Jsont.Error.msgf Jsont.Meta.none "%a" Ptime.pp_rfc3339_error error)
      ~enc:(fun value -> (Ptime.to_rfc3339 ~tz_offset_s:0) value)
      string

  module Legacy = struct
    let int = Jsont.legacy_int
    let int64 = Jsont.legacy_int64
  end
end

let as_string = function
  | Jsont.String (s, _) when String.is_valid_utf_8 s -> Some s
  | _ -> None

let as_bool = function Jsont.Bool (b, _) -> Some b | _ -> None

let decode_projection codec json =
  match Jsont.Json.decode codec json with
  | Ok value -> Some value
  | Error _ -> None

let as_int json = decode_projection Codec.int json
let as_int64 json = decode_projection Codec.int64 json
let as_float json = decode_projection Codec.number json
let as_array = function Jsont.Array (l, _) -> Some l | _ -> None
let as_object = function Jsont.Object (o, _) -> Some o | _ -> None

let find_mem name j =
  match as_object j with
  | None -> None
  | Some o -> Option.map snd (Jsont.Json.find_mem name o)

let find_string name j = Option.bind (find_mem name j) as_string
let find_bool name j = Option.bind (find_mem name j) as_bool
let find_int name j = Option.bind (find_mem name j) as_int
