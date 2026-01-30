(** Runtime utilities for generated OpenAPI clients.

    This module provides utilities used by generated client code:
    - Path template rendering
    - Query parameter building
    - JSON encoding/decoding helpers
*)

(** {1 Path Templates} *)

module Path = struct
  (** Render a path template like "/users/{id}/posts/{postId}" with parameters *)
  let render ~(params : (string * string) list) (template : string) : string =
    List.fold_left
      (fun path (name, value) ->
        match String.split_on_char '{' path with
        | [only] -> only
        | parts ->
            String.concat "" (List.mapi (fun i part ->
              if i = 0 then part
              else
                match String.split_on_char '}' part with
                | [var; rest] when var = name -> value ^ rest
                | _ -> "{" ^ part
            ) parts))
      template params

  (** Extract parameter names from a path template *)
  let parameters (template : string) : string list =
    let rec extract acc s =
      match String.index_opt s '{' with
      | None -> List.rev acc
      | Some i ->
          let rest = String.sub s (i + 1) (String.length s - i - 1) in
          match String.index_opt rest '}' with
          | None -> List.rev acc
          | Some j ->
              let name = String.sub rest 0 j in
              let remaining = String.sub rest (j + 1) (String.length rest - j - 1) in
              extract (name :: acc) remaining
    in
    extract [] template
end

(** {1 Query Parameters} *)

module Query = struct
  type param = string * string

  (** Helper for optional parameters with custom stringifier *)
  let optional_with ~key ~value ~to_string : param list =
    Option.fold ~none:[] ~some:(fun v -> [(key, to_string v)]) value

  let singleton ~key ~value : param list = [(key, value)]

  let optional ~key ~value : param list =
    optional_with ~key ~value ~to_string:Fun.id

  let list ~key ~values : param list =
    List.map (fun v -> (key, v)) values

  let int ~key ~value : param list = [(key, string_of_int value)]

  let int_opt ~key ~value : param list =
    optional_with ~key ~value ~to_string:string_of_int

  let bool ~key ~value : param list =
    [(key, if value then "true" else "false")]

  let bool_opt ~key ~value : param list =
    optional_with ~key ~value ~to_string:(fun b -> if b then "true" else "false")

  let float ~key ~value : param list = [(key, string_of_float value)]

  let float_opt ~key ~value : param list =
    optional_with ~key ~value ~to_string:string_of_float

  let encode (params : param list) : string =
    if params = [] then ""
    else
      "?" ^
      String.concat "&" (List.map (fun (k, v) ->
        (* URL encode the value *)
        let encode_char c =
          match c with
          | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
              String.make 1 c
          | c ->
              Printf.sprintf "%%%02X" (Char.code c)
        in
        let encoded_v = String.to_seq v
          |> Seq.map encode_char
          |> List.of_seq
          |> String.concat ""
        in
        k ^ "=" ^ encoded_v
      ) params)
end

(** {1 JSON Helpers} *)

module Json = struct
  let decode codec s =
    Jsont_bytesrw.decode_string codec s

  let decode' codec s =
    Jsont_bytesrw.decode_string' codec s

  let encode codec v =
    Jsont_bytesrw.encode_string codec v

  let encode' codec v =
    Jsont_bytesrw.encode_string' codec v

  let encode_compact codec v =
    Jsont_bytesrw.encode_string ~format:Jsont.Minify codec v

  (** Decode a Jsont.json value through a codec.
      Encodes to string then decodes - not optimal but works. *)
  let decode_json (codec : 'a Jsont.t) (json : Jsont.json) : ('a, string) result =
    match Jsont_bytesrw.encode_string Jsont.json json with
    | Ok s -> Jsont_bytesrw.decode_string codec s
    | Error e -> Error e

  (** Decode a Jsont.json value, raising on error *)
  let decode_json_exn (codec : 'a Jsont.t) (json : Jsont.json) : 'a =
    match decode_json codec json with
    | Ok v -> v
    | Error e -> failwith e

  (** Encode a value to Jsont.json *)
  let encode_json (codec : 'a Jsont.t) (v : 'a) : Jsont.json =
    match Jsont_bytesrw.encode_string codec v with
    | Ok s ->
        (match Jsont_bytesrw.decode_string Jsont.json s with
         | Ok json -> json
         | Error _ -> Jsont.Null ((), Jsont.Meta.none))
    | Error _ -> Jsont.Null ((), Jsont.Meta.none)
end

(** {1 HTTP Method} *)

type http_method = Get | Post | Put | Patch | Delete | Head | Options

let string_of_method = function
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Patch -> "PATCH"
  | Delete -> "DELETE"
  | Head -> "HEAD"
  | Options -> "OPTIONS"

(** {1 Common Types} *)

(** ISO 8601 date-time codec *)
let ptime_jsont : Ptime.t Jsont.t =
  Jsont.map Jsont.string ~kind:"datetime"
    ~dec:(fun s ->
      match Ptime.of_rfc3339 s with
      | Ok (t, _, _) -> t
      | Error _ -> Jsont.Error.msgf Jsont.Meta.none "Invalid datetime: %s" s)
    ~enc:(fun t -> Ptime.to_rfc3339 t)

(** UUID as string *)
let uuid_jsont : string Jsont.t = Jsont.string

(** Base64 encoded bytes *)
let base64_jsont : string Jsont.t = Jsont.string

(** {1 Nullable wrapper} *)

let nullable (codec : 'a Jsont.t) : 'a option Jsont.t =
  Jsont.option codec

(** Nullable combinator that handles explicit JSON null values.
    Use this for fields marked as "nullable: true" in OpenAPI specs.
    Unlike Jsont.option, this properly decodes explicit null as None. *)
let nullable_any (base_codec : 'a Jsont.t) : 'a option Jsont.t =
  let null_codec = Jsont.null None in
  let some_codec = Jsont.map base_codec
    ~kind:"nullable_some"
    ~dec:(fun v -> Some v)
    ~enc:(function Some v -> v | None -> failwith "unreachable")
  in
  (* Use Jsont.any to dispatch based on the JSON value type *)
  Jsont.any
    ~dec_null:null_codec
    ~dec_string:some_codec
    ~dec_number:some_codec
    ~dec_bool:some_codec
    ~dec_array:some_codec
    ~dec_object:some_codec
    ~enc:(function
      | None -> null_codec
      | Some _ -> some_codec)
    ()

(** Nullable string that handles both absent and explicit null *)
let nullable_string : string option Jsont.t =
  nullable_any Jsont.string

(** Nullable ptime that handles both absent and explicit null *)
let nullable_ptime : Ptime.t option Jsont.t =
  nullable_any ptime_jsont

(** Nullable int that handles both absent and explicit null *)
let nullable_int : int option Jsont.t =
  nullable_any Jsont.int

(** Nullable float that handles both absent and explicit null *)
let nullable_float : float option Jsont.t =
  nullable_any Jsont.number

(** Nullable bool that handles both absent and explicit null *)
let nullable_bool : bool option Jsont.t =
  nullable_any Jsont.bool

(** {1 Any JSON value wrapper} *)

type json = Jsont.json

let json_jsont : json Jsont.t = Jsont.json

(** {1 Validation} *)

(** Validation error type *)
type validation_error =
  | Min_length of { actual: int; min: int }
  | Max_length of { actual: int; max: int }
  | Pattern_mismatch of { value: string; pattern: string }
  | Min_value of { actual: float; min: float; exclusive: bool }
  | Max_value of { actual: float; max: float; exclusive: bool }
  | Min_items of { actual: int; min: int }
  | Max_items of { actual: int; max: int }
  | Duplicate_items of { count: int }

let validation_error_to_string = function
  | Min_length { actual; min } ->
      Printf.sprintf "string too short: %d < %d" actual min
  | Max_length { actual; max } ->
      Printf.sprintf "string too long: %d > %d" actual max
  | Pattern_mismatch { value; pattern } ->
      Printf.sprintf "value %S does not match pattern %S" value pattern
  | Min_value { actual; min; exclusive } ->
      if exclusive then Printf.sprintf "value %g must be > %g" actual min
      else Printf.sprintf "value %g must be >= %g" actual min
  | Max_value { actual; max; exclusive } ->
      if exclusive then Printf.sprintf "value %g must be < %g" actual max
      else Printf.sprintf "value %g must be <= %g" actual max
  | Min_items { actual; min } ->
      Printf.sprintf "array too short: %d < %d items" actual min
  | Max_items { actual; max } ->
      Printf.sprintf "array too long: %d > %d items" actual max
  | Duplicate_items { count } ->
      Printf.sprintf "array contains %d duplicate items" count

(** Validated string codec with optional length and pattern constraints.
    Pattern validation uses the Re library with PCRE syntax. *)
let validated_string
    ?min_length ?max_length ?pattern
    (base : string Jsont.t) : string Jsont.t =
  (* Compile regex if pattern is provided *)
  let pattern_re = Option.map (fun p ->
    try Some (Re.compile (Re.Pcre.re p))
    with _ -> None  (* Invalid regex - skip validation *)
  ) pattern in
  Jsont.map base ~kind:"validated_string"
    ~dec:(fun s ->
      let len = String.length s in
      (match min_length with
       | Some min when len < min ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Min_length { actual = len; min }))
       | _ -> ());
      (match max_length with
       | Some max when len > max ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Max_length { actual = len; max }))
       | _ -> ());
      (match pattern_re, pattern with
       | Some (Some re), Some pat ->
           if not (Re.execp re s) then
             Jsont.Error.msgf Jsont.Meta.none "%s"
               (validation_error_to_string (Pattern_mismatch { value = s; pattern = pat }))
       | _ -> ());
      s)
    ~enc:Fun.id

(** Validated int codec with optional min/max constraints *)
let validated_int
    ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum
    (base : int Jsont.t) : int Jsont.t =
  Jsont.map base ~kind:"validated_int"
    ~dec:(fun n ->
      let f = float_of_int n in
      (match exclusive_minimum with
       | Some min when f <= min ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Min_value { actual = f; min; exclusive = true }))
       | _ -> ());
      (match minimum with
       | Some min when f < min ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Min_value { actual = f; min; exclusive = false }))
       | _ -> ());
      (match exclusive_maximum with
       | Some max when f >= max ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Max_value { actual = f; max; exclusive = true }))
       | _ -> ());
      (match maximum with
       | Some max when f > max ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Max_value { actual = f; max; exclusive = false }))
       | _ -> ());
      n)
    ~enc:Fun.id

(** Validated float codec with optional min/max constraints *)
let validated_float
    ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum
    (base : float Jsont.t) : float Jsont.t =
  Jsont.map base ~kind:"validated_float"
    ~dec:(fun f ->
      (match exclusive_minimum with
       | Some min when f <= min ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Min_value { actual = f; min; exclusive = true }))
       | _ -> ());
      (match minimum with
       | Some min when f < min ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Min_value { actual = f; min; exclusive = false }))
       | _ -> ());
      (match exclusive_maximum with
       | Some max when f >= max ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Max_value { actual = f; max; exclusive = true }))
       | _ -> ());
      (match maximum with
       | Some max when f > max ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Max_value { actual = f; max; exclusive = false }))
       | _ -> ());
      f)
    ~enc:Fun.id

(** Validated list codec with optional min/max items and uniqueness constraints *)
let validated_list
    ?min_items ?max_items ?(unique_items = false)
    (elem_codec : 'a Jsont.t) : 'a list Jsont.t =
  let base = Jsont.list elem_codec in
  Jsont.map base ~kind:"validated_list"
    ~dec:(fun lst ->
      let len = List.length lst in
      (match min_items with
       | Some min when len < min ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Min_items { actual = len; min }))
       | _ -> ());
      (match max_items with
       | Some max when len > max ->
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Max_items { actual = len; max }))
       | _ -> ());
      (* Check for unique items using structural equality *)
      (if unique_items then
         let rec count_duplicates seen count = function
           | [] -> count
           | x :: xs ->
               if List.exists (( = ) x) seen then
                 count_duplicates seen (count + 1) xs
               else
                 count_duplicates (x :: seen) count xs
         in
         let dup_count = count_duplicates [] 0 lst in
         if dup_count > 0 then
           Jsont.Error.msgf Jsont.Meta.none "%s"
             (validation_error_to_string (Duplicate_items { count = dup_count })));
      lst)
    ~enc:Fun.id

(** {1 Polymorphic Variant Codecs for Union Types} *)

(** Create a try-each decoder for polymorphic variant union types.
    Attempts each decoder in order until one succeeds. *)
let poly_union_decoder (decoders : (Jsont.json -> 'a option) list) (json : Jsont.json) : 'a =
  match List.find_map (fun dec -> dec json) decoders with
  | Some v -> v
  | None -> Jsont.Error.msgf Jsont.Meta.none "No variant matched for union type"

(** {1 API Errors} *)

(** Parsed error body, in increasing levels of typed-ness *)
type error_body =
  | Raw of string                    (** Unparsed string *)
  | Json of Jsont.json              (** Parsed but untyped JSON *)
  | Typed of string * Jsont.json    (** schema_name, typed value as JSON *)

(** Error raised when an API call fails with a non-2xx status code *)
type api_error = {
  operation : string;
  method_ : string;
  url : string;
  status : int;
  body : string;                     (** Always keep raw body for debugging *)
  parsed_body : error_body option;   (** Parsed/typed body if available *)
}

exception Api_error of api_error

let () =
  Printexc.register_printer (function
    | Api_error e ->
        let parsed_info = match e.parsed_body with
          | None -> ""
          | Some (Raw _) -> " (raw)"
          | Some (Json _) -> " (json)"
          | Some (Typed (schema, _)) -> Printf.sprintf " (typed: %s)" schema
        in
        Some (Printf.sprintf "Api_error: %s %s returned %d%s: %s"
          e.method_ e.url e.status parsed_info e.body)
    | _ -> None)

(** Helper to try parsing error body with a list of status-keyed parsers *)
let try_parse_error_body ~status ~body
    (parsers : (string * (string -> (Jsont.json, string) result)) list) : error_body option =
  let status_str = string_of_int status in
  (* Try exact status match first, then wildcard, then default *)
  let try_parser key =
    List.find_map (fun (k, parser) ->
      if k = key then
        match parser body with
        | Ok json -> Some json
        | Error _ -> None
      else None
    ) parsers
  in
  match try_parser status_str with
  | Some json -> Some (Json json)
  | None ->
      let wildcard = String.make 1 status_str.[0] ^ "XX" in
      match try_parser wildcard with
      | Some json -> Some (Json json)
      | None ->
          match try_parser "default" with
          | Some json -> Some (Json json)
          | None ->
              (* Try parsing as generic JSON *)
              match Jsont_bytesrw.decode_string Jsont.json body with
              | Ok json -> Some (Json json)
              | Error _ -> Some (Raw body)
