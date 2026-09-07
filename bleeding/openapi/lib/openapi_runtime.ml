(** Runtime utilities for generated OpenAPI clients.

    This module provides utilities used by generated client code:
    - Path template rendering
    - Query parameter building
    - JSON encoding/decoding helpers
*)

(** {1 Path Templates} *)

(** Encode one URI component using RFC 3986 unreserved bytes. *)
let percent_encode value =
  let b = Buffer.create (String.length value) in
  String.iter (function
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~') as c ->
        Buffer.add_char b c
    | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))) value;
  Buffer.contents b

module Path = struct
  (** Substitute each template parameter once, escaping it as a path segment.
      Dot segments are rejected because URL parsers normalize even escaped dots. *)
  let render ~params template =
    let b = Buffer.create (String.length template) in
    let rec loop i =
      if i < String.length template then
        if template.[i] = '{' then begin
          let stop = match String.index_from_opt template (i + 1) '}' with
            | Some j -> j
            | None -> invalid_arg "OpenAPI: unterminated path parameter" in
          let name = String.sub template (i + 1) (stop - i - 1) in
          let value = match List.assoc_opt name params with
            | Some value -> value
            | None -> invalid_arg ("OpenAPI: missing path parameter " ^ name) in
          if value = "." || value = ".." then
            invalid_arg "OpenAPI: dot segment in path parameter";
          Buffer.add_string b (percent_encode value);
          loop (stop + 1)
        end else begin
          Buffer.add_char b template.[i];
          loop (i + 1)
        end
    in
    loop 0;
    Buffer.contents b

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
    match params with
    | [] -> ""
    | _ -> "?" ^ String.concat "&" (List.map (fun (k, v) ->
        percent_encode k ^ "=" ^ percent_encode v) params)

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

  (** Decode a JSON string through a codec, raising on error.
      Used by generated clients on the success path. *)
  let decode_exn (codec : 'a Jsont.t) (s : string) : 'a =
    match Jsont_bytesrw.decode_string codec s with
    | Ok v -> v
    | Error e -> failwith e

  (** Encode a value to a compact JSON string, raising on error.
      Used by generated clients to build request bodies. *)
  let encode_exn (codec : 'a Jsont.t) (v : 'a) : string =
    match Jsont_bytesrw.encode_string ~format:Jsont.Minify codec v with
    | Ok s -> s
    | Error e -> failwith e

  (** Decode directly from the JSON tree, preserving decoding errors. *)
  let decode_json codec json = Jsont.Json.decode codec json

  (** Decode a Jsont.json value, raising on error *)
  let decode_json_exn (codec : 'a Jsont.t) (json : Jsont.json) : 'a =
    match decode_json codec json with
    | Ok v -> v
    | Error e -> failwith e

  (** Encode a value to Jsont.json *)
  let encode_json codec v =
    match Jsont.Json.encode codec v with
    | Ok json -> json
    | Error error -> invalid_arg error

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

(** Compatibility name for {!Jsont.option}, which maps JSON null to [None]. *)
let nullable_any codec = Jsont.option codec

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
  let pattern_re = Option.map (fun p -> Re.compile (Re.Pcre.re p)) pattern in
  let validate s =
      let rec length i count =
        if i = String.length s then count else
        let u = String.get_utf_8_uchar s i in
        if not (Uchar.utf_decode_is_valid u) then
          Jsont.Error.msg Jsont.Meta.none "Invalid UTF-8 string";
        length (i + Uchar.utf_decode_length u) (count + 1)
      in
      let len = length 0 0 in
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
       | Some re, Some pat ->
           if not (Re.execp re s) then
             Jsont.Error.msgf Jsont.Meta.none "%s"
               (validation_error_to_string (Pattern_mismatch { value = s; pattern = pat }))
       | _ -> ());
      s
  in
  Jsont.map base ~kind:"validated_string" ~dec:validate ~enc:validate

(** Compare an integer with a finite or infinite floating bound without rounding
    the integer through [float_of_int]. Bounds themselves follow the spec model's
    floating-point representation. *)
let compare_integer n bound =
  if Float.is_nan bound then invalid_arg "OpenAPI: NaN integer bound";
  if bound >= 0x1p63 then -1
  else if bound < -.0x1p63 then 1
  else
    let integral = Int64.of_float bound in
    let cmp = Int64.compare n integral in
    if cmp <> 0 then cmp else Float.compare (Float.trunc bound) bound

let validated_integer ~to_int64 ~to_float
    ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base =
  let validate n =
    let i = to_int64 n and actual = to_float n in
    List.iter (fun (bound, exclusive) -> match bound with
      | Some min when compare_integer i min < (if exclusive then 1 else 0) ->
          Jsont.Error.msg Jsont.Meta.none
            (validation_error_to_string (Min_value { actual; min; exclusive }))
      | _ -> ()) [minimum, false; exclusive_minimum, true];
    List.iter (fun (bound, exclusive) -> match bound with
      | Some max when compare_integer i max > (if exclusive then -1 else 0) ->
          Jsont.Error.msg Jsont.Meta.none
            (validation_error_to_string (Max_value { actual; max; exclusive }))
      | _ -> ()) [maximum, false; exclusive_maximum, true];
    n
  in
  Jsont.map base ~kind:"validated_integer" ~dec:validate ~enc:validate

let validated_int ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base =
  validated_integer ~to_int64:Int64.of_int ~to_float:float_of_int
    ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base

let validated_int32 ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base =
  validated_integer ~to_int64:Int64.of_int32 ~to_float:Int32.to_float
    ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base

let validated_int64 ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base =
  validated_integer ~to_int64:Fun.id ~to_float:Int64.to_float
    ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum base

(** Validated float codec with optional min/max constraints *)
let validated_float
    ?(minimum : float option) ?(maximum : float option)
    ?(exclusive_minimum : float option) ?(exclusive_maximum : float option)
    (base : float Jsont.t) : float Jsont.t =
  let validate f =
      if not (Float.is_finite f) then
        Jsont.Error.msg Jsont.Meta.none "Expected a finite JSON number";
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
      f
  in
  Jsont.map base ~kind:"validated_float" ~dec:validate ~enc:validate

(** OpenAPI integers are JSON numbers, not truncated fractions or strings.
    Jsont's large-integer string extension is rejected rather than sent on the
    wire as a different JSON type. *)
let integer_jsont base =
  Jsont.map Jsont.json ~kind:"integer"
    ~dec:(fun json ->
      (match json with
       | Jsont.Number (n, _) when Float.is_finite n && Float.is_integer n -> ()
       | _ -> Jsont.Error.msg Jsont.Meta.none "Expected an integral JSON number");
      match Jsont.Json.decode base json with
      | Ok value -> value
      | Error error -> Jsont.Error.msg Jsont.Meta.none error)
    ~enc:(fun value ->
      match Jsont.Json.encode base value with
      | Ok (Jsont.Number _ as json) -> json
      | Ok _ -> Jsont.Error.msg Jsont.Meta.none "Integer cannot be represented exactly as a JSON number"
      | Error error -> Jsont.Error.msg Jsont.Meta.none error)

let int_jsont = integer_jsont Jsont.int
let int32_jsont = integer_jsont Jsont.int32
let int64_jsont = integer_jsont Jsont.int64
let number_jsont = validated_float Jsont.number

(** Validated list codec with optional min/max items and uniqueness constraints *)
let validated_list
    ?min_items ?max_items ?(unique_items = false)
    (elem_codec : 'a Jsont.t) : 'a list Jsont.t =
  let base = Jsont.list elem_codec in
  let validate lst =
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
      (if unique_items then begin
         let sorted = List.map (Json.encode_json elem_codec) lst
           |> List.sort Jsont.Json.compare in
         let _, duplicates = List.fold_left (fun (previous, count) json ->
           let duplicate = match previous with
             | Some previous -> Jsont.Json.equal previous json
             | None -> false in
           Some json, count + if duplicate then 1 else 0) (None, 0) sorted in
         if duplicates > 0 then
           Jsont.Error.msg Jsont.Meta.none
             (validation_error_to_string (Duplicate_items { count = duplicates }))
       end);
      lst
  in
  Jsont.map base ~kind:"validated_list" ~dec:validate ~enc:validate

(** {1 Polymorphic Variant Codecs for Union Types} *)

(** Try union decoders in order. With [exclusive], exactly one must succeed;
    otherwise the first successful result is returned. *)
let poly_union_decoder ?(exclusive = false) decoders json =
  let rec loop matched = function
    | [] -> (match matched with
        | Some v -> v
        | None -> Jsont.Error.msg Jsont.Meta.none "No variant matched for union type")
    | decode :: rest -> match decode json with
      | None -> loop matched rest
      | Some v when not exclusive -> v
      | Some v -> match matched with
        | None -> loop (Some v) rest
        | Some _ -> Jsont.Error.msg Jsont.Meta.none "Multiple variants matched oneOf"
  in
  loop None decoders

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
  body : string;                     (** Raw diagnostic body, or a marker if larger than 64 KiB. *)
  parsed_body : error_body option;   (** Parsed/typed body if available *)
}

exception Api_error of api_error

let () =
  Printexc.Safe.register_printer (function
    | Api_error e ->
        let parsed_info = match e.parsed_body with
          | None -> ""
          | Some (Raw _) -> " (raw)"
          | Some (Json _) -> " (json)"
          | Some (Typed (schema, _)) -> Printf.sprintf " (typed: %s)" schema
        in
        Some (Printf.sprintf "Api_error: %s %s returned %d%s: %s"
          e.method_ e.url e.status parsed_info (Fetch.Media.sanitize_diagnostic e.body))
    | _ -> None)

(** Select a response definition before parsing; an exact status takes
    precedence even when its body does not validate against that schema. *)
let response_parser ~status parsers =
  match List.assoc_opt (string_of_int status) parsers with
  | Some p -> Some p
  | None -> match List.assoc_opt (string_of_int (status / 100) ^ "XX") parsers with
    | Some p -> Some p
    | None -> List.assoc_opt "default" parsers

let fallback_error_body body =
  match Fetch.Media.decode Fetch.Json.json body with
  | Ok json -> Some (Json json)
  | Error _ -> Some (Raw body)

(** Helper to try parsing an error with a status-keyed parser. *)
let try_parse_error_body ~status ~body parsers =
  match Option.bind (response_parser ~status parsers) (fun parser ->
    Result.to_option (parser body)) with
  | Some json -> Some (Json json)
  | None -> fallback_error_body body

(** {1 Fetch transport shared by generated clients} *)

module Client = struct
  type t = {
    session : Fetch.plain;
    base_url : string;
    max_response_bytes : int;
  }

  let of_fetch ?(max_response_bytes = 16 * 1024 * 1024) ~base_url session =
    if max_response_bytes < 0 then invalid_arg "OpenAPI: negative response limit";
    (match Fetch.Middleware.Url.of_string base_url with
     | Ok _ -> ()
     | Error _ -> invalid_arg "OpenAPI: base URL must be an absolute HTTP(S) URL");
    let uri = Uri.of_string base_url in
    if Uri.userinfo uri <> None || Uri.verbatim_query uri <> None || Uri.fragment uri <> None then
      invalid_arg "OpenAPI: base URL cannot contain credentials, a query, or a fragment";
    let base_url = Uri.to_string (Uri.canonicalize uri) in
    let rec trim n = if n > 0 && base_url.[n - 1] = '/' then trim (n - 1) else n in
    let base_url = String.sub base_url 0 (trim (String.length base_url)) in
    { session = Fetch.restrict session; base_url; max_response_bytes }

  let base_url t = t.base_url
  let session t = t.session

  (* Error URLs omit query values, including caller-supplied secrets. *)
  let diagnostic_url url = Uri.to_string (Uri.with_query (Uri.of_string url) [])

  let error ~operation ~method_ ~url parsers response =
    let status = Fetch.status response in
    let body =
      try Fetch.decode ~limit:(64 * 1024) Fetch.Media.octets response with
      | Eio.Io (Fetch.E (Fetch.Decode_failure { error = Too_large _; _ }), _) ->
          "[response exceeds diagnostic limit]"
    in
    let parsed_body = match Option.bind (response_parser ~status parsers) (fun p -> p body) with
      | Some body -> Some body
      | None -> fallback_error_body body
    in
    raise (Api_error { operation; method_; url = diagnostic_url url; status; body; parsed_body })

  let typed_error name codec body =
    match Fetch.Media.decode (Fetch.Json.v codec) body with
    | Ok value -> (match Jsont.Json.encode codec value with
        | Ok json -> Some (Typed (name, json))
        | Error _ -> None)
    | Error _ -> None

  let call ?(headers = Fetch.Header.[]) ?body ?(errors = [])
      ~operation ~path ~query ~decode t method_ =
    let url = t.base_url ^ path ^ query in
    (* Requests with bodies and mutating methods must not forward writes to a
       redirect target. GET/HEAD retain Fetch's normal redirect policy. *)
    let redirects = match method_, body with
      | (`GET | `HEAD), None -> None
      | _ -> Some 0 in
    try
      Fetch.with_response ~headers ?body ?redirects t.session method_ url @@ fun response ->
      let status = Fetch.status response in
      if status >= 200 && status < 300 then decode ~limit:t.max_response_bytes response
      else error ~operation ~method_:(Http.Method.to_string method_) ~url errors response
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "calling OpenAPI operation %s" operation
end
