let encode_json kind codec value =
  match Jsont_bytesrw.encode_string codec value with
  | Ok text -> text
  | Error message -> failwith ("cannot encode " ^ kind ^ ": " ^ message)

module Schema = struct
  type t =
    | Null
    | String
    | String_guided of {
        constant : string option;
        choices : string list option;
        pattern : string option;
      }
    | Integer
    | Integer_guided of { minimum : int option; maximum : int option }
    | Number
    | Number_guided of { minimum : float option; maximum : float option }
    | Boolean
    | Array of { item : t; minimum : int option; maximum : int option }
    | Object of {
        name : string;
        description : string option;
        explicit_null : bool;
        properties : property list;
      }
    | One_of of {
        name : string;
        description : string option;
        choices : string list;
      }
    | Any_of of { name : string; description : string option; choices : t list }
    | Reference of string
    | Document of { root : t; dependencies : t list }

  and property = {
    name : string;
    description : string option;
    schema : t;
    optional : bool;
  }

  let null = Null
  let string = String

  let string_guided ?constant ?choices ?pattern () =
    Option.iter
      (function
        | [] -> invalid_arg "Schema.string_guided: empty choices"
        | values
          when List.length (List.sort_uniq String.compare values)
               <> List.length values ->
            invalid_arg "Schema.string_guided: duplicate choices"
        | _ -> ())
      choices;
    (match (constant, choices) with
    | Some value, Some values when not (List.mem value values) ->
        invalid_arg "Schema.string_guided: constant is not one of choices"
    | _ -> ());
    String_guided { constant; choices; pattern }

  let integer = Integer

  let integer_range ?minimum ?maximum () =
    (match (minimum, maximum) with
    | Some a, Some b when a > b ->
        invalid_arg "Schema.integer_range: minimum is greater than maximum"
    | _ -> ());
    Integer_guided { minimum; maximum }

  let number = Number

  let number_range ?minimum ?maximum () =
    Option.iter
      (fun x ->
        if not (Float.is_finite x) then
          invalid_arg "Schema.number_range: bounds must be finite")
      minimum;
    Option.iter
      (fun x ->
        if not (Float.is_finite x) then
          invalid_arg "Schema.number_range: bounds must be finite")
      maximum;
    (match (minimum, maximum) with
    | Some a, Some b when a > b ->
        invalid_arg "Schema.number_range: minimum is greater than maximum"
    | _ -> ());
    Number_guided { minimum; maximum }

  let boolean = Boolean

  let array ?minimum ?maximum item =
    Option.iter
      (fun value ->
        if value < 0 then invalid_arg "Schema.array: bounds must be nonnegative")
      minimum;
    Option.iter
      (fun value ->
        if value < 0 then invalid_arg "Schema.array: bounds must be nonnegative")
      maximum;
    (match (minimum, maximum) with
    | Some minimum, Some maximum when minimum > maximum ->
        invalid_arg "Schema.array: minimum is greater than maximum"
    | _ -> ());
    Array { item; minimum; maximum }

  let valid_name name =
    let valid_first = function
      | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
      | _ -> false
    in
    let valid_rest = function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    String.length name > 0
    && valid_first name.[0]
    && String.for_all valid_rest name

  let distinct_strings values =
    let seen = Hashtbl.create (List.length values) in
    List.for_all
      (fun value ->
        if Hashtbl.mem seen value then false
        else (
          Hashtbl.add seen value ();
          true))
      values

  let distinct_names properties =
    let names = Hashtbl.create (List.length properties) in
    List.for_all
      (fun property ->
        if Hashtbl.mem names property.name then false
        else (
          Hashtbl.add names property.name ();
          true))
      properties

  let object_ ~name ?description ?(explicit_null = false) properties =
    if not (valid_name name) then
      Error
        "schema names must contain only ASCII letters, digits, and \
         underscores, and must not start with a digit"
    else if
      not (List.for_all (fun property -> valid_name property.name) properties)
    then
      Error
        "property names must contain only ASCII letters, digits, and \
         underscores, and must not start with a digit"
    else if not (distinct_names properties) then
      Error "property names must be unique within an object schema"
    else Ok (Object { name; description; explicit_null; properties })

  let one_of ~name ?description choices =
    if not (valid_name name) then
      Error
        "schema names must contain only ASCII letters, digits, and \
         underscores, and must not start with a digit"
    else if choices = [] then Error "one_of needs at least one choice"
    else if not (distinct_strings choices) then
      Error "one_of choices must be unique"
    else Ok (One_of { name; description; choices })

  let any_of ~name ?description choices =
    if not (valid_name name) then
      Error
        "schema names must contain only ASCII letters, digits, and \
         underscores, and must not start with a digit"
    else if choices = [] then Error "any_of needs at least one choice"
    else Ok (Any_of { name; description; choices })

  let reference name =
    if valid_name name then Reference name
    else invalid_arg "Schema.reference: invalid schema name"

  let with_dependencies root dependencies = Document { root; dependencies }

  let property ?description ?(optional = false) name schema =
    { name; description; schema; optional }

  let document = function
    | Document { root; dependencies } -> (root, dependencies)
    | root -> (root, [])

  let rec defines name = function
    | Object { name = candidate; _ }
    | One_of { name = candidate; _ }
    | Any_of { name = candidate; _ } ->
        String.equal name candidate
    | Document { root; dependencies } ->
        defines name root || List.exists (defines name) dependencies
    | _ -> false

  type bound_wire = Integer_bound of int | Number_bound of float
  type choices_wire = String_choices of string list | Schema_choices of t list

  type node_wire = {
    kind : string;
    constant : string option;
    choices : choices_wire option;
    pattern : string option;
    minimum : bound_wire option;
    maximum : bound_wire option;
    items : t option;
    minimum_items : int option;
    maximum_items : int option;
    name : string option;
    explicit_null : bool option;
    properties : property list option;
    description : string option;
  }

  let empty_wire kind =
    {
      kind;
      constant = None;
      choices = None;
      pattern = None;
      minimum = None;
      maximum = None;
      items = None;
      minimum_items = None;
      maximum_items = None;
      name = None;
      explicit_null = None;
      properties = None;
      description = None;
    }

  let rec node_wire = function
    | Null -> empty_wire "null"
    | String -> empty_wire "string"
    | String_guided { constant; choices; pattern } ->
        {
          (empty_wire "string") with
          constant;
          choices = Option.map (fun values -> String_choices values) choices;
          pattern;
        }
    | Integer -> empty_wire "integer"
    | Integer_guided { minimum; maximum } ->
        {
          (empty_wire "integer") with
          minimum = Option.map (fun value -> Integer_bound value) minimum;
          maximum = Option.map (fun value -> Integer_bound value) maximum;
        }
    | Number -> empty_wire "number"
    | Number_guided { minimum; maximum } ->
        {
          (empty_wire "number") with
          minimum = Option.map (fun value -> Number_bound value) minimum;
          maximum = Option.map (fun value -> Number_bound value) maximum;
        }
    | Boolean -> empty_wire "boolean"
    | Array { item; minimum; maximum } ->
        {
          (empty_wire "array") with
          items = Some item;
          minimum_items = minimum;
          maximum_items = maximum;
        }
    | Object { name; description; explicit_null; properties } ->
        {
          (empty_wire "object") with
          name = Some name;
          description;
          explicit_null = Some explicit_null;
          properties = Some properties;
        }
    | One_of { name; description; choices } ->
        {
          (empty_wire "one_of") with
          name = Some name;
          description;
          choices = Some (String_choices choices);
        }
    | Any_of { name; description; choices } ->
        {
          (empty_wire "any_of") with
          name = Some name;
          description;
          choices = Some (Schema_choices choices);
        }
    | Reference name -> { (empty_wire "reference") with name = Some name }
    | Document { root; _ } -> node_wire root

  let node_jsont =
    let self =
      Jsont.Portable_lazy.from_fun_fixed (fun self ->
          let node = Jsont.rec' self in
          let integer_bound =
            Jsont.map Jsont.int ~enc:(function
              | Integer_bound value -> value
              | Number_bound _ -> assert false)
          in
          let number_bound =
            Jsont.map Jsont.number ~enc:(function
              | Number_bound value -> value
              | Integer_bound _ -> assert false)
          in
          let bound =
            Jsont.any ~kind:"schema bound"
              ~enc:(function
                | Integer_bound _ -> integer_bound
                | Number_bound _ -> number_bound)
              ()
          in
          let string_choices =
            Jsont.map (Jsont.list Jsont.string) ~enc:(function
              | String_choices values -> values
              | Schema_choices _ -> assert false)
          in
          let schema_choices =
            Jsont.map (Jsont.list node) ~enc:(function
              | Schema_choices values -> values
              | String_choices _ -> assert false)
          in
          let choices =
            Jsont.any ~kind:"schema choices"
              ~enc:(function
                | String_choices _ -> string_choices
                | Schema_choices _ -> schema_choices)
              ()
          in
          let property =
            Jsont.Object.enc_only ~kind:"schema property" ()
            |> Jsont.Object.mem "name" Jsont.string
                 ~enc:(fun (property : property) -> property.name)
            |> Jsont.Object.mem "optional" Jsont.bool
                 ~enc:(fun (property : property) -> property.optional)
            |> Jsont.Object.mem "schema" node ~enc:(fun (property : property) ->
                   property.schema)
            |> Jsont.Object.opt_mem "description" Jsont.string
                 ~enc:(fun (property : property) -> property.description)
            |> Jsont.Object.finish
          in
          Jsont.Object.enc_only ~kind:"schema" ()
          |> Jsont.Object.mem "type" Jsont.string ~enc:(fun wire -> wire.kind)
          |> Jsont.Object.opt_mem "constant" Jsont.string ~enc:(fun wire ->
                 wire.constant)
          |> Jsont.Object.opt_mem "choices" choices ~enc:(fun wire ->
                 wire.choices)
          |> Jsont.Object.opt_mem "pattern" Jsont.string ~enc:(fun wire ->
                 wire.pattern)
          |> Jsont.Object.opt_mem "minimum" bound ~enc:(fun wire -> wire.minimum)
          |> Jsont.Object.opt_mem "maximum" bound ~enc:(fun wire -> wire.maximum)
          |> Jsont.Object.opt_mem "items" node ~enc:(fun wire -> wire.items)
          |> Jsont.Object.opt_mem "minimum_items" Jsont.int ~enc:(fun wire ->
                 wire.minimum_items)
          |> Jsont.Object.opt_mem "maximum_items" Jsont.int ~enc:(fun wire ->
                 wire.maximum_items)
          |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun wire ->
                 wire.name)
          |> Jsont.Object.opt_mem "explicit_null" Jsont.bool ~enc:(fun wire ->
                 wire.explicit_null)
          |> Jsont.Object.opt_mem "properties" (Jsont.list property)
               ~enc:(fun wire -> wire.properties)
          |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun wire ->
                 wire.description)
          |> Jsont.Object.finish |> Jsont.map ~enc:node_wire)
    in
    Jsont.rec' self

  let document_jsont =
    Jsont.Object.enc_only ~kind:"schema document" ()
    |> Jsont.Object.mem "root" node_jsont ~enc:(fun schema ->
           fst (document schema))
    |> Jsont.Object.mem "dependencies" (Jsont.list node_jsont)
         ~enc:(fun schema -> snd (document schema))
    |> Jsont.Object.finish

  let to_document_string schema =
    encode_json "schema document" document_jsont schema

  let pp formatter schema =
    let json =
      match schema with
      | Document _ -> to_document_string schema
      | schema -> encode_json "schema" node_jsont schema
    in
    Format.pp_print_string formatter json
end

type availability =
  [ `Available
  | `Device_not_eligible
  | `Apple_intelligence_not_enabled
  | `Model_not_ready
  | `Unavailable of string ]

external raw_availability : unit -> int * string = "caml_apple_fm_availability"

let availability () =
  match raw_availability () with
  | 0, _ -> `Available
  | 1, _ -> `Device_not_eligible
  | 2, _ -> `Apple_intelligence_not_enabled
  | 3, _ -> `Model_not_ready
  | _, message -> `Unavailable message

let pp_availability formatter = function
  | `Available -> Format.pp_print_string formatter "available"
  | `Device_not_eligible ->
      Format.pp_print_string formatter "device not eligible"
  | `Apple_intelligence_not_enabled ->
      Format.pp_print_string formatter "Apple Intelligence not enabled"
  | `Model_not_ready -> Format.pp_print_string formatter "model not ready"
  | `Unavailable message -> Format.fprintf formatter "unavailable: %s" message

module Codec = struct
  type 'a value = {
    jsont : 'a Jsont.t;
    schema : Schema.t;
    dependencies : Schema.t list;
  }

  let make_value ?(dependencies = []) ~schema jsont =
    { jsont; schema; dependencies }

  let value_schema value =
    match value.dependencies with
    | [] -> value.schema
    | dependencies -> Schema.with_dependencies value.schema dependencies

  let validated ~kind check jsont =
    let (check @ portable) value =
      match check value with
      | Ok () -> ()
      | Error message -> Jsont.Error.msg Jsont.Meta.none message
    in
    Jsont.iter ~kind ~dec:check ~enc:check jsont

  let (check_range @ portable) ~kind ~compare ~show ?minimum ?maximum value =
    match minimum with
    | Some bound when compare value bound < 0 ->
        Error
          (Printf.sprintf "%s %s is less than the minimum %s" kind (show value)
             (show bound))
    | _ -> (
        match maximum with
        | Some bound when compare value bound > 0 ->
            Error
              (Printf.sprintf "%s %s is greater than the maximum %s" kind
                 (show value) (show bound))
        | _ -> Ok ())

  let null = make_value ~schema:Schema.null (Jsont.null ())
  let string = make_value ~schema:Schema.string Jsont.string

  let string_guided ?constant ?choices ?pattern () =
    let schema = Schema.string_guided ?constant ?choices ?pattern () in
    let (check @ portable) value =
      match constant with
      | Some expected when not (String.equal value expected) ->
          Error
            (Printf.sprintf "string %S differs from the required value %S" value
               expected)
      | _ -> (
          match choices with
          | Some allowed when not (List.mem value allowed) ->
              Error (Printf.sprintf "string %S is not an allowed choice" value)
          | _ -> Ok ())
    in
    make_value ~schema (validated ~kind:"guided string" check Jsont.string)

  let bool = make_value ~schema:Schema.boolean Jsont.bool
  let int = make_value ~schema:Schema.integer Jsont.int

  let int_range ?minimum ?maximum () =
    let schema = Schema.integer_range ?minimum ?maximum () in
    let (check @ portable) value =
      check_range ~kind:"integer" ~compare:Int.compare ~show:string_of_int
        ?minimum ?maximum value
    in
    make_value ~schema (validated ~kind:"bounded integer" check Jsont.int)

  let (finite @ portable) value =
    if Float.is_finite value then Ok ()
    else Error (Printf.sprintf "number %g is not finite" value)

  let float =
    make_value ~schema:Schema.number
      (validated ~kind:"finite number" finite Jsont.number)

  let float_range ?minimum ?maximum () =
    let schema = Schema.number_range ?minimum ?maximum () in
    let (check @ portable) value =
      match finite value with
      | Error _ as error -> error
      | Ok () ->
          check_range ~kind:"number" ~compare:Float.compare
            ~show:(Printf.sprintf "%g") ?minimum ?maximum value
    in
    make_value ~schema (validated ~kind:"bounded number" check Jsont.number)

  let array ?minimum ?maximum item =
    let schema = Schema.array ?minimum ?maximum item.schema in
    let (check @ portable) values =
      check_range ~kind:"array length" ~compare:Int.compare ~show:string_of_int
        ?minimum ?maximum (List.length values)
    in
    make_value ~schema ~dependencies:item.dependencies
      (validated ~kind:"bounded array" check (Jsont.list item.jsont))

  let enum ~name ?description choices =
    let schema =
      match Schema.one_of ~name ?description (List.map fst choices) with
      | Ok schema -> schema
      | Error message -> invalid_arg ("Codec.enum: " ^ message)
    in
    make_value ~schema (Jsont.enum choices)

  type 'b case =
    | Case : {
        value : 'a value;
        inject : ('a -> 'b) @@ portable;
        project : ('b -> 'a option) @@ portable;
      }
        -> 'b case

  type encoded_case = Encoded_case : 'a Jsont.t * 'a -> encoded_case

  let case ~inject ~project value = Case { value; inject; project }

  let any_of ~name ?description cases =
    let schemas = List.map (fun (Case case) -> case.value.schema) cases in
    let schema =
      match Schema.any_of ~name ?description schemas with
      | Ok schema -> schema
      | Error message -> invalid_arg ("Codec.any_of: " ^ message)
    in
    let json_text json = encode_json "union value" Jsont.json json in
    let decode json =
      let text = json_text json in
      let matches =
        List.filter_map
          (fun (Case case) ->
            match Jsont_bytesrw.decode_string case.value.jsont text with
            | Ok value -> Some (case.inject value)
            | Error _ -> None)
          cases
      in
      match matches with
      | [ value ] -> value
      | [] ->
          Jsont.Error.msg Jsont.Meta.none
            "value does not match any union alternative"
      | _ ->
          Jsont.Error.msg Jsont.Meta.none
            "value matches more than one union alternative"
    in
    let encode value =
      let matches =
        List.filter_map
          (fun (Case case) ->
            Option.map
              (fun value -> Encoded_case (case.value.jsont, value))
              (case.project value))
          cases
      in
      match matches with
      | [ Encoded_case (codec, value) ] -> (
          match Jsont_bytesrw.encode_string codec value with
          | Error message -> Jsont.Error.msg Jsont.Meta.none message
          | Ok text -> (
              match Jsont_bytesrw.decode_string Jsont.json text with
              | Ok json -> json
              | Error message -> Jsont.Error.msg Jsont.Meta.none message))
      | [] ->
          Jsont.Error.msg Jsont.Meta.none
            "value does not select a union alternative"
      | _ ->
          Jsont.Error.msg Jsont.Meta.none
            "value selects more than one union alternative"
    in
    let dependencies =
      List.concat_map (fun (Case case) -> case.value.dependencies) cases
    in
    make_value ~schema ~dependencies
      (Jsont.map Jsont.json ~dec:decode ~enc:encode)

  let recursive ~name define =
    if not (Schema.valid_name name) then
      invalid_arg "Codec.recursive: invalid schema name";
    let definition =
      Jsont.Portable_lazy.from_fun_fixed (fun definition ->
          let reference =
            {
              jsont =
                Jsont.rec'
                  (Jsont.Portable_lazy.map definition ~f:(fun value ->
                       value.jsont));
              schema = Schema.reference name;
              dependencies = [];
            }
          in
          define reference)
      |> Jsont.Portable_lazy.force
    in
    if not (Schema.defines name definition.schema) then
      invalid_arg
        "Codec.recursive: the definition must be a named schema with the same \
         name";
    {
      jsont = definition.jsont;
      schema = Schema.reference name;
      dependencies = definition.schema :: definition.dependencies;
    }

  let map_value ~dec ~enc value =
    { value with jsont = Jsont.map ~dec ~enc value.jsont }

  module Object = struct
    type ('o, 'dec) map = {
      name : string;
      object_codec : ('o, 'dec) Jsont.Object.map;
      properties : Schema.property list;
      dependencies : Schema.t list;
    }

    let map name dec =
      {
        name;
        object_codec = Jsont.Object.map ~kind:name dec;
        properties = [];
        dependencies = [];
      }

    let param ~enc ?description ?default name value map =
      Option.iter
        (fun default ->
          match Jsont_bytesrw.encode_string value.jsont default with
          | Ok _ -> ()
          | Error message ->
              invalid_arg
                (Printf.sprintf "Codec.Invoke.param: invalid default for %S: %s"
                   name message))
        default;
      let dec_absent =
        match default with
        | None -> None
        | Some value -> Some (Obj.magic_portable (fun () -> value))
      in
      {
        map with
        object_codec =
          Jsont.Object.mem ?doc:description ?dec_absent ~enc name
            value.jsont map.object_codec;
        properties =
          Schema.property ?description ~optional:(Option.is_some default) name
            value.schema
          :: map.properties;
        dependencies = value.dependencies @ map.dependencies;
      }

    let optional ~enc ?description name value map =
      {
        map with
        object_codec =
          Jsont.Object.opt_mem ?doc:description ~enc name value.jsont
            map.object_codec;
        properties =
          Schema.property ?description ~optional:true name value.schema
          :: map.properties;
        dependencies = value.dependencies @ map.dependencies;
      }

    let seal map =
      let schema =
        match Schema.object_ ~name:map.name (List.rev map.properties) with
        | Ok schema -> schema
        | Error message -> invalid_arg ("Codec.Object.seal: " ^ message)
      in
      make_value ~schema ~dependencies:map.dependencies
        (Jsont.Object.finish map.object_codec)
  end

  type 'a t = { name : string; arguments : 'a value }

  module Invoke = struct
    type ('o, 'dec) map = ('o, 'dec) Object.map

    let map = Object.map
    let param = Object.param
    let optional = Object.optional
    let seal map = { name = map.Object.name; arguments = Object.seal map }
  end

  let map ~dec ~enc codec =
    { codec with arguments = map_value ~dec ~enc codec.arguments }

  let name codec = codec.name
  let schema codec = value_schema codec.arguments

  let decode_arguments codec arguments =
    Jsont_bytesrw.decode_string codec.arguments.jsont arguments

  let encode_arguments codec value =
    Jsont_bytesrw.encode_string codec.arguments.jsont value
end

module Tool = struct
  type t = {
    name : string;
    description : string;
    parameters : Schema.t;
    includes_schema_in_instructions : bool;
    invoke : string -> string;
    invoke_json : string -> string;
  }

  let protect_handler handler value =
    try Ok (handler value) with
    | (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow) as exn ->
        raise exn
    | exn -> Error ("Error: " ^ Printexc.to_string exn)

  let make ~description ~includes_schema_in_instructions codec handler output =
    let invoke arguments =
      match Codec.decode_arguments codec arguments with
      | Error message -> Error ("Error: " ^ message)
      | Ok value -> protect_handler handler value
    in
    let encode value = Jsont_bytesrw.encode_string output value in
    let encode_error message = encode_json "tool error" Jsont.string message in
    {
      name = Codec.name codec;
      description;
      parameters = Codec.schema codec;
      includes_schema_in_instructions;
      invoke =
        (fun arguments ->
          match invoke arguments with
          | Ok value -> (
              match encode value with
              | Ok json -> json
              | Error message -> "Error: " ^ message)
          | Error message -> message);
      invoke_json =
        (fun arguments ->
          match invoke arguments with
          | Ok value -> (
              match encode value with
              | Ok json -> json
              | Error message -> encode_error ("Error: " ^ message))
          | Error message -> encode_error message);
    }

  let v ?(includes_schema_in_instructions = true) ~description codec handler =
    let tool =
      make ~description ~includes_schema_in_instructions codec handler
        Jsont.string
    in
    {
      tool with
      invoke =
        (fun arguments ->
          match Codec.decode_arguments codec arguments with
          | Error message -> "Error: " ^ message
          | Ok value -> (
              match protect_handler handler value with
              | Ok value -> value
              | Error message -> message));
    }

  let v_json ?(includes_schema_in_instructions = true) ~description ~output
      codec handler =
    make ~description ~includes_schema_in_instructions codec handler output

  let name tool = tool.name
  let description tool = tool.description
  let schema tool = tool.parameters
  let invoke tool arguments = tool.invoke arguments
  let invoke_json tool arguments = tool.invoke_json arguments

  let includes_schema_in_instructions tool =
    tool.includes_schema_in_instructions

  let pp formatter tool =
    Format.fprintf formatter
      "{@[<hov>name=%S;@ description=%S;@ schema=%a;@ \
       includes_schema_in_instructions=%b@]}"
      tool.name tool.description Schema.pp tool.parameters
      tool.includes_schema_in_instructions
end

let tool_jsont =
  Jsont.Object.enc_only ~kind:"tool definition" ()
  |> Jsont.Object.mem "name" Jsont.string ~enc:Tool.name
  |> Jsont.Object.mem "description" Jsont.string ~enc:Tool.description
  |> Jsont.Object.mem "schema" Schema.document_jsont ~enc:Tool.schema
  |> Jsont.Object.mem "includesSchemaInInstructions" Jsont.bool
       ~enc:Tool.includes_schema_in_instructions
  |> Jsont.Object.finish

let tools_to_json tools =
  encode_json "tool definitions" (Jsont.list tool_jsont) tools

module Transcript = struct
  type t = string

  let of_json json =
    match Jsont_bytesrw.decode_string Jsont.json json with
    | Ok _ -> Ok json
    | Error message -> Error message

  let to_json transcript = transcript
  let pp formatter transcript = Format.pp_print_string formatter transcript
end

module Prompt = struct
  type image = { path : string; label : string option }
  type t = { text : string; images : image list }

  let image ?label path =
    if not (Filename.is_relative path) then { path; label }
    else invalid_arg "Prompt.image: path must be absolute"

  let v ?(images = []) text = { text; images }
  let text text = v text

  let image_jsont =
    Jsont.Object.enc_only ~kind:"prompt image" ()
    |> Jsont.Object.mem "path" Jsont.string ~enc:(fun image -> image.path)
    |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun image -> image.label)
    |> Jsont.Object.finish

  let jsont =
    Jsont.Object.enc_only ~kind:"prompt" ()
    |> Jsont.Object.mem "text" Jsont.string ~enc:(fun prompt -> prompt.text)
    |> Jsont.Object.mem "images" (Jsont.list image_jsont) ~enc:(fun prompt ->
           prompt.images)
    |> Jsont.Object.finish

  let to_json prompt = encode_json "prompt" jsont prompt

  let pp formatter prompt =
    Format.fprintf formatter "{@[<hov>text=%S;@ images=%d@]}" prompt.text
      (List.length prompt.images)
end

type use_case = [ `General | `Content_tagging ]
type guardrails = [ `Default | `Permissive_content_transformations ]

let pp_use_case formatter = function
  | `General -> Format.pp_print_string formatter "general"
  | `Content_tagging -> Format.pp_print_string formatter "content tagging"

let pp_guardrails formatter = function
  | `Default -> Format.pp_print_string formatter "default"
  | `Permissive_content_transformations ->
      Format.pp_print_string formatter "permissive content transformations"

type model = { use_case : use_case; guardrails : guardrails }

let model ?(use_case = `General) ?(guardrails = `Default) () =
  { use_case; guardrails }

let default_model = model ()

let pp_model formatter model =
  Format.fprintf formatter "{@[<hov>use_case=%a;@ guardrails=%a@]}" pp_use_case
    model.use_case pp_guardrails model.guardrails

let model_jsont =
  Jsont.Object.enc_only ~kind:"model configuration" ()
  |> Jsont.Object.mem "useCase" Jsont.string ~enc:(fun model ->
         match model.use_case with
         | `General -> "general"
         | `Content_tagging -> "content_tagging")
  |> Jsont.Object.mem "guardrails" Jsont.string ~enc:(fun model ->
         match model.guardrails with
         | `Default -> "default"
         | `Permissive_content_transformations ->
             "permissive_content_transformations")
  |> Jsont.Object.finish

let model_to_json model = encode_json "model configuration" model_jsont model

type capabilities = {
  vision : bool;
  guided_generation : bool;
  reasoning : bool;
  tool_calling : bool;
}

let pp_capabilities formatter capabilities =
  Format.fprintf formatter
    "{@[<hov>vision=%b;@ guided_generation=%b;@ reasoning=%b;@ \
     tool_calling=%b@]}"
    capabilities.vision capabilities.guided_generation capabilities.reasoning
    capabilities.tool_calling

type model_info = {
  context_size : int;
  supported_languages : string list;
  variant : string option;
  capabilities : capabilities;
}

let pp_model_info formatter info =
  Format.fprintf formatter
    "{@[<hov>context_size=%d;@ variant=%a;@ languages=%d;@ capabilities=%a@]}"
    info.context_size
    (Format.pp_print_option Format.pp_print_string)
    info.variant
    (List.length info.supported_languages)
    pp_capabilities info.capabilities

type reasoning_level = [ `Light | `Moderate | `Deep | `Custom of string ]

type context = {
  include_schema_in_prompt : bool option;
  reasoning_level : reasoning_level option;
}

let context ?include_schema_in_prompt ?reasoning_level () =
  { include_schema_in_prompt; reasoning_level }

let pp_reasoning_level formatter = function
  | `Light -> Format.pp_print_string formatter "light"
  | `Moderate -> Format.pp_print_string formatter "moderate"
  | `Deep -> Format.pp_print_string formatter "deep"
  | `Custom value -> Format.fprintf formatter "custom %S" value

let pp_context formatter context =
  Format.fprintf formatter
    "{@[<hov>include_schema_in_prompt=%a;@ reasoning=%a@]}"
    (Format.pp_print_option Format.pp_print_bool)
    context.include_schema_in_prompt
    (Format.pp_print_option pp_reasoning_level)
    context.reasoning_level

type usage = {
  input_tokens : int;
  cached_input_tokens : int;
  output_tokens : int;
  reasoning_tokens : int;
}

let pp_usage formatter usage =
  Format.fprintf formatter
    "{@[<hov>input=%d;@ cached_input=%d;@ output=%d;@ reasoning=%d@]}"
    usage.input_tokens usage.cached_input_tokens usage.output_tokens
    usage.reasoning_tokens

type top_k = { k : int; seed : int64 option }
type probability = { threshold : float; seed : int64 option }

type sampling =
  [ `Default | `Greedy | `Top_k of top_k | `Probability of probability ]

type tool_calling = [ `Allowed | `Required | `Disallowed ]

let pp_sampling formatter = function
  | `Default -> Format.pp_print_string formatter "default"
  | `Greedy -> Format.pp_print_string formatter "greedy"
  | `Top_k { k; seed = None } -> Format.fprintf formatter "top-k %d" k
  | `Top_k { k; seed = Some seed } ->
      Format.fprintf formatter "top-k %d, seed %Ld" k seed
  | `Probability { threshold; seed = None } ->
      Format.fprintf formatter "probability %g" threshold
  | `Probability { threshold; seed = Some seed } ->
      Format.fprintf formatter "probability %g, seed %Ld" threshold seed

let pp_tool_calling formatter = function
  | `Allowed -> Format.pp_print_string formatter "allowed"
  | `Required -> Format.pp_print_string formatter "required"
  | `Disallowed -> Format.pp_print_string formatter "disallowed"

type options = {
  sampling : sampling;
  temperature : float option;
  maximum_response_tokens : int option;
  tool_calling : tool_calling;
}

let options ?(sampling = `Default) ?temperature ?maximum_response_tokens
    ?(tool_calling = `Allowed) () =
  let maximum_int32 = Int32.to_int Int32.max_int in
  Option.iter
    (fun temperature ->
      if
        (not (Float.is_finite temperature))
        || temperature < 0. || temperature > 1.
      then invalid_arg "temperature must be between 0 and 1")
    temperature;
  Option.iter
    (fun maximum ->
      if maximum <= 0 then
        invalid_arg "maximum_response_tokens must be positive"
      else if maximum > maximum_int32 then
        invalid_arg "maximum_response_tokens exceeds the supported range")
    maximum_response_tokens;
  (match sampling with
  | `Top_k { k; _ } when k <= 0 -> invalid_arg "top-k must be positive"
  | `Top_k { k; _ } when k > maximum_int32 ->
      invalid_arg "top-k exceeds the supported range"
  | `Probability { threshold; _ }
    when (not (Float.is_finite threshold)) || threshold <= 0. || threshold > 1.
    ->
      invalid_arg "probability threshold must be in (0, 1]"
  | _ -> ());
  { sampling; temperature; maximum_response_tokens; tool_calling }

let pp_option pp formatter = function
  | None -> Format.pp_print_string formatter "default"
  | Some value -> pp formatter value

let pp_options formatter options =
  Format.fprintf formatter
    "{@[<hov>sampling=%a;@ temperature=%a;@ maximum_response_tokens=%a;@ \
     tool_calling=%a@]}"
    pp_sampling options.sampling
    (pp_option Format.pp_print_float)
    options.temperature
    (pp_option Format.pp_print_int)
    options.maximum_response_tokens pp_tool_calling options.tool_calling

type context_size_exceeded = {
  context_size : int option;
  token_count : int option;
  message : string;
}

type rate_limited = { reset_time : float option; message : string }
type unsupported_capability = { capability : string option; message : string }
type unsupported_guide = { schema : string option; message : string }
type unsupported_language = { language : string option; message : string }

type error =
  [ `Context_size_exceeded of context_size_exceeded
  | `Rate_limited of rate_limited
  | `Guardrail_violation of string
  | `Refusal of string
  | `Unsupported_capability of unsupported_capability
  | `Unsupported_transcript of string
  | `Unsupported_guide of unsupported_guide
  | `Unsupported_language of unsupported_language
  | `Timeout of string
  | `Concurrent_requests of string
  | `Transcript_mutation of string
  | `Assets_unavailable of string
  | `Decoding_failure of string
  | `Tool_failure of string
  | `Cancelled of string
  | `Unsupported_version of string
  | `Framework_error of string
  | `Closed ]

let pp_error formatter = function
  | `Context_size_exceeded { context_size; token_count; message } ->
      Format.fprintf formatter "context size exceeded%a%a: %s"
        (fun ppf -> function
          | None -> ()
          | Some n -> Format.fprintf ppf " (limit %d" n)
        context_size
        (fun ppf -> function
          | None ->
              if Option.is_some context_size then Format.pp_print_char ppf ')'
          | Some n ->
              Format.fprintf ppf "%sused %d)"
                (if Option.is_some context_size then ", " else " (")
                n)
        token_count message
  | `Rate_limited { reset_time; message } ->
      Format.fprintf formatter "rate limited%a: %s"
        (fun ppf -> function
          | None -> ()
          | Some t -> Format.fprintf ppf " until %.0f" t)
        reset_time message
  | `Guardrail_violation message ->
      Format.fprintf formatter "guardrail violation: %s" message
  | `Refusal message -> Format.fprintf formatter "refusal: %s" message
  | `Unsupported_capability { capability; message } ->
      Format.fprintf formatter "unsupported capability%a: %s"
        (fun ppf -> function
          | None -> ()
          | Some s -> Format.fprintf ppf " %s" s)
        capability message
  | `Unsupported_transcript message ->
      Format.fprintf formatter "unsupported transcript: %s" message
  | `Unsupported_guide { schema; message } ->
      Format.fprintf formatter "unsupported generation guide%a: %s"
        (fun ppf -> function
          | None -> ()
          | Some s -> Format.fprintf ppf " %s" s)
        schema message
  | `Unsupported_language { language; message } ->
      Format.fprintf formatter "unsupported language%a: %s"
        (fun ppf -> function
          | None -> ()
          | Some s -> Format.fprintf ppf " %s" s)
        language message
  | `Timeout message -> Format.fprintf formatter "timeout: %s" message
  | `Concurrent_requests message ->
      Format.fprintf formatter "concurrent requests: %s" message
  | `Transcript_mutation message ->
      Format.fprintf formatter "transcript mutation: %s" message
  | `Assets_unavailable message ->
      Format.fprintf formatter "model assets unavailable: %s" message
  | `Decoding_failure message ->
      Format.fprintf formatter "decoding failure: %s" message
  | `Tool_failure message -> Format.fprintf formatter "tool failure: %s" message
  | `Cancelled message -> Format.fprintf formatter "cancelled: %s" message
  | `Unsupported_version message -> Format.pp_print_string formatter message
  | `Framework_error message -> Format.pp_print_string formatter message
  | `Closed -> Format.pp_print_string formatter "the session is closed"

type error_wire = {
  kind : string;
  message : string;
  context_size : int option;
  token_count : int option;
  reset_time : float option;
  detail : string option;
}

let error_wire_jsont =
  Jsont.Object.map ~kind:"foundation-models error"
    (fun kind message context_size token_count reset_time detail ->
      { kind; message; context_size; token_count; reset_time; detail })
  |> Jsont.Object.mem "kind" Jsont.string
  |> Jsont.Object.mem "message" Jsont.string
  |> Jsont.Object.opt_mem "contextSize" Jsont.int
  |> Jsont.Object.opt_mem "tokenCount" Jsont.int
  |> Jsont.Object.opt_mem "resetTime" Jsont.number
  |> Jsont.Object.opt_mem "detail" Jsont.string
  |> Jsont.Object.finish

let decode_error message =
  match Jsont_bytesrw.decode_string error_wire_jsont message with
  | Error _ -> `Framework_error message
  | Ok wire -> (
      match wire.kind with
      | "closed" -> `Closed
      | "context_size_exceeded" ->
          `Context_size_exceeded
            {
              context_size = wire.context_size;
              token_count = wire.token_count;
              message = wire.message;
            }
      | "rate_limited" ->
          `Rate_limited { reset_time = wire.reset_time; message = wire.message }
      | "guardrail_violation" -> `Guardrail_violation wire.message
      | "refusal" -> `Refusal wire.message
      | "unsupported_capability" ->
          `Unsupported_capability
            { capability = wire.detail; message = wire.message }
      | "unsupported_transcript" -> `Unsupported_transcript wire.message
      | "unsupported_guide" ->
          `Unsupported_guide { schema = wire.detail; message = wire.message }
      | "unsupported_language" ->
          `Unsupported_language
            { language = wire.detail; message = wire.message }
      | "timeout" -> `Timeout wire.message
      | "concurrent_requests" -> `Concurrent_requests wire.message
      | "transcript_mutation" -> `Transcript_mutation wire.message
      | "assets_unavailable" -> `Assets_unavailable wire.message
      | "decoding_failure" -> `Decoding_failure wire.message
      | "tool_failure" -> `Tool_failure wire.message
      | "cancelled" -> `Cancelled wire.message
      | "unsupported_version" -> `Unsupported_version wire.message
      | _ -> `Framework_error wire.message)

type Eio.Exn.err += E of error

let () =
  Eio.Exn.register_pp (fun formatter -> function
    | E error ->
        Format.fprintf formatter "Foundation Models: %a" pp_error error;
        true
    | _ -> false)

let raise_error context error =
  raise (Eio.Exn.add_context (Eio.Exn.create (E error)) "%s" context)

module Raw = struct
  type options = int * float * bool * int64 * bool * float * int * int
  type request = string * string option * int * string option * options
  type event = int * int64 * string * string

  external model_info : string -> string option -> int * string
    = "caml_apple_fm_model_info"

  external token_count : string -> int -> string -> int64 * string
    = "caml_apple_fm_model_token_count"

  external compact_transcript : string -> string -> int -> int * string
    = "caml_apple_fm_transcript_compact"

  external create :
    string option -> string -> string -> string option -> nativeint * string
    = "caml_apple_fm_session_create"

  external destroy : nativeint -> unit = "caml_apple_fm_session_destroy"

  external prewarm : nativeint -> string option -> int * string
    = "caml_apple_fm_session_prewarm"

  external start : nativeint -> request -> int * string
    = "caml_apple_fm_session_start"

  external next_event : nativeint -> int -> event
    = "caml_apple_fm_session_next_event"

  external resolve_tool : nativeint -> int64 -> string -> int * string
    = "caml_apple_fm_session_resolve_tool"

  external transcript : nativeint -> int * string
    = "caml_apple_fm_session_transcript"

  external replace_transcript : nativeint -> string -> int * string
    = "caml_apple_fm_session_replace_transcript"

  external usage : nativeint -> int * string = "caml_apple_fm_session_usage"
  external cancel : nativeint -> unit = "caml_apple_fm_session_cancel"
end

let compact_transcript ?(keep_last_turns = 2) ~summary transcript =
  if keep_last_turns < 0 then
    invalid_arg "compact_transcript: keep_last_turns must be nonnegative";
  if keep_last_turns > Int32.to_int Int32.max_int then
    invalid_arg
      "compact_transcript: keep_last_turns exceeds the supported range";
  if String.trim summary = "" then
    invalid_arg "compact_transcript: summary must not be empty";
  match
    Raw.compact_transcript
      (Transcript.to_json transcript)
      summary keep_last_turns
  with
  | 0, json -> json
  | _, message ->
      raise_error "compacting a Foundation Models transcript"
        (decode_error message)

type model_info_wire = {
  context_size_w : int;
  supported_languages_w : string list;
  variant_w : string option;
  vision_w : bool;
  guided_w : bool;
  reasoning_w : bool;
  tools_w : bool;
  locale_supported_w : bool option;
}

let model_info_jsont =
  Jsont.Object.map ~kind:"model info"
    (fun
      context_size_w
      supported_languages_w
      variant_w
      vision_w
      guided_w
      reasoning_w
      tools_w
      locale_supported_w
    ->
      {
        context_size_w;
        supported_languages_w;
        variant_w;
        vision_w;
        guided_w;
        reasoning_w;
        tools_w;
        locale_supported_w;
      })
  |> Jsont.Object.mem "contextSize" Jsont.int
  |> Jsont.Object.mem "supportedLanguages" (Jsont.list Jsont.string)
  |> Jsont.Object.opt_mem "variant" Jsont.string
  |> Jsont.Object.mem "vision" Jsont.bool
  |> Jsont.Object.mem "guidedGeneration" Jsont.bool
  |> Jsont.Object.mem "reasoning" Jsont.bool
  |> Jsont.Object.mem "toolCalling" Jsont.bool
  |> Jsont.Object.opt_mem "localeSupported" Jsont.bool
  |> Jsont.Object.finish

let read_model_info model locale =
  match Raw.model_info (model_to_json model) locale with
  | code, message when code <> 0 ->
      raise_error "reading Foundation Models model information"
        (decode_error message)
  | _, json -> (
      match Jsont_bytesrw.decode_string model_info_jsont json with
      | Error message ->
          raise_error "decoding Foundation Models model information"
            (`Decoding_failure message)
      | Ok wire -> wire)

let model_info ?(model = default_model) () =
  let wire = read_model_info model None in
  {
    context_size = wire.context_size_w;
    supported_languages = wire.supported_languages_w;
    variant = wire.variant_w;
    capabilities =
      {
        vision = wire.vision_w;
        guided_generation = wire.guided_w;
        reasoning = wire.reasoning_w;
        tool_calling = wire.tools_w;
      };
  }

let supports_locale ?(model = default_model) locale =
  match (read_model_info model (Some locale)).locale_supported_w with
  | Some supported -> supported
  | None -> assert false

let token_count ?(model = default_model) kind payload =
  let count, message =
    Eio_unix.run_in_systhread ~label:"apple-fm.token-count" (fun () ->
        Raw.token_count (model_to_json model) kind payload)
  in
  if Int64.compare count 0L < 0 then
    raise_error "counting Foundation Models tokens" (decode_error message)
  else Int64.to_int count

let count_prompt_tokens ?model prompt =
  token_count ?model 0 (Prompt.to_json prompt)

let count_text_tokens ?model text =
  count_prompt_tokens ?model (Prompt.text text)

let count_instructions_tokens ?model instructions =
  token_count ?model 1 instructions

let count_schema_tokens ?model schema =
  token_count ?model 2 (Schema.to_document_string schema)

let count_transcript_tokens ?model transcript =
  token_count ?model 3 (Transcript.to_json transcript)

let count_tool_tokens ?model tools = token_count ?model 4 (tools_to_json tools)

module Session = struct
  type t = {
    mutable raw : nativeint;
    tools : (string, Tool.t) Hashtbl.t;
    lock : Eio.Mutex.t;
    cancel_requested : bool Atomic.t;
    responding : bool Atomic.t;
  }

  let is_closed session = Nativeint.equal session.raw Nativeint.zero

  let destroy_unlocked session =
    if not (is_closed session) then (
      let raw = session.raw in
      session.raw <- Nativeint.zero;
      Raw.destroy raw)

  let create ~sw ?(model = default_model) ?instructions ?transcript tools =
    if Option.is_some instructions && Option.is_some transcript then
      invalid_arg
        "Session.create: instructions and transcript are mutually exclusive";
    let table = Hashtbl.create (List.length tools) in
    let duplicate =
      List.find_opt
        (fun tool ->
          let name = Tool.name tool in
          if Hashtbl.mem table name then true
          else (
            Hashtbl.add table name tool;
            false))
        tools
    in
    match duplicate with
    | Some tool ->
        invalid_arg ("Session.create: duplicate tool name " ^ Tool.name tool)
    | None ->
        let definitions = tools_to_json tools in
        let transcript = Option.map Transcript.to_json transcript in
        let raw, message =
          Raw.create instructions definitions (model_to_json model) transcript
        in
        if Nativeint.equal raw Nativeint.zero then
          raise_error "creating a Foundation Models session"
            (decode_error message)
        else
          let session =
            {
              raw;
              tools = table;
              lock = Eio.Mutex.create ();
              cancel_requested = Atomic.make false;
              responding = Atomic.make false;
            }
          in
          Eio.Switch.on_release sw (fun () -> destroy_unlocked session);
          session

  let close session =
    Eio.Mutex.use_ro session.lock (fun () -> destroy_unlocked session)

  let prewarm ?prompt_prefix session =
    Eio.Mutex.use_ro session.lock (fun () ->
        if is_closed session then
          raise_error "prewarming a Foundation Models session" `Closed
        else
          match Raw.prewarm session.raw prompt_prefix with
          | 0, _ -> ()
          | _, message ->
              raise_error "prewarming a Foundation Models session"
                (decode_error message))

  let cancel session =
    if (not (is_closed session)) && Atomic.get session.responding then (
      Atomic.set session.cancel_requested true;
      Raw.cancel session.raw)

  let is_responding session = Atomic.get session.responding

  let raw_options options =
    let sampling_kind, sampling_value, seed =
      match options.sampling with
      | `Default -> (0, 0., None)
      | `Greedy -> (1, 0., None)
      | `Top_k { k; seed } -> (2, Float.of_int k, seed)
      | `Probability { threshold; seed } -> (3, threshold, seed)
    in
    let has_seed, seed =
      match seed with None -> (false, 0L) | Some seed -> (true, seed)
    in
    let has_temperature, temperature =
      match options.temperature with
      | None -> (false, 0.)
      | Some temperature -> (true, temperature)
    in
    let maximum = Option.value ~default:0 options.maximum_response_tokens in
    let tool_calling =
      match options.tool_calling with
      | `Allowed -> 0
      | `Required -> 1
      | `Disallowed -> 2
    in
    ( sampling_kind,
      sampling_value,
      has_seed,
      seed,
      has_temperature,
      temperature,
      maximum,
      tool_calling )

  let resolve session id result =
    match Raw.resolve_tool session.raw id result with
    | 0, _ -> Ok ()
    | _, message -> Error (decode_error message)

  let run_tool session id name arguments =
    match Hashtbl.find_opt session.tools name with
    | None ->
        let message = "Error: unknown tool " ^ name in
        resolve session id (encode_json "tool error" Jsont.string message)
    | Some tool -> resolve session id (tool.Tool.invoke_json arguments)

  let next_event session =
    Eio.Cancel.protect (fun () ->
        match
          Eio_unix.run_in_systhread ~label:"apple-fm.next-event" (fun () ->
              Raw.next_event session.raw 100)
        with
        | event -> event
        | exception Failure message ->
            raise_error "waiting for a Foundation Models event"
              (`Framework_error message))

  let cancel_and_drain session =
    Eio.Cancel.protect (fun () ->
        Raw.cancel session.raw;
        let rec drain () =
          match next_event session with (3 | 4), _, _, _ -> () | _ -> drain ()
        in
        drain ())

  let context_raw = function
    | None -> (-1, None)
    | Some context ->
        let include_schema =
          match context.include_schema_in_prompt with
          | None -> -1
          | Some false -> 0
          | Some true -> 1
        in
        let reasoning =
          Option.map
            (function
              | `Light -> "light"
              | `Moderate -> "moderate"
              | `Deep -> "deep"
              | `Custom value -> "custom:" ^ value)
            context.reasoning_level
        in
        (include_schema, reasoning)

  let run ?(options = options ()) ?context ?schema ~output session prompt =
    Eio.Mutex.use_ro session.lock (fun () ->
        if is_closed session then
          raise_error "starting a Foundation Models response" `Closed
        else (
          Atomic.set session.cancel_requested false;
          Atomic.set session.responding true;
          let include_schema, reasoning = context_raw context in
          let request =
            ( Prompt.to_json prompt,
              Option.map Schema.to_document_string schema,
              include_schema,
              reasoning,
              raw_options options )
          in
          match Raw.start session.raw request with
          | code, message when code <> 0 ->
              Atomic.set session.responding false;
              raise_error "starting a Foundation Models response"
                (decode_error message)
          | _ ->
              if Atomic.get session.cancel_requested then Raw.cancel session.raw;
              let terminal = ref false in
              let rec loop () =
                match next_event session with
                | 0, _, _, _ ->
                    Eio.Fiber.yield ();
                    loop ()
                | 1, _, text, _ ->
                    Eio.Flow.copy_string text output;
                    loop ()
                | 2, id, name, arguments -> (
                    match run_tool session id name arguments with
                    | Ok () -> loop ()
                    | Error error ->
                        raise_error
                          (Printf.sprintf "resolving Foundation Models tool %S"
                             name)
                          error)
                | 3, _, text, _ ->
                    terminal := true;
                    if Atomic.get session.cancel_requested then (
                      destroy_unlocked session;
                      raise_error "generating a Foundation Models response"
                        (`Cancelled "generation cancelled"))
                    else text
                | 4, _, message, _ ->
                    terminal := true;
                    if Atomic.get session.cancel_requested then
                      destroy_unlocked session;
                    raise_error "generating a Foundation Models response"
                      (decode_error message)
                | kind, _, _, _ ->
                    raise_error "reading a Foundation Models response"
                      (`Framework_error
                        (Printf.sprintf "unknown bridge event %d" kind))
              in
              Fun.protect
                ~finally:(fun () ->
                  Fun.protect
                    ~finally:(fun () -> Atomic.set session.responding false)
                    (fun () ->
                      if not !terminal then (
                        cancel_and_drain session;
                        destroy_unlocked session)))
                loop))

  let respond_prompt_stream ?options ?context ~output session prompt =
    run ?options ?context ~output session prompt

  let respond_stream ?options ?context ~output session prompt =
    run ?options ?context ~output session (Prompt.text prompt)

  let respond_prompt ?options ?context session prompt =
    run ?options ?context ~output:Eio.Flow.null session prompt

  let respond ?options ?context session prompt =
    respond_prompt ?options ?context session (Prompt.text prompt)

  let respond_prompt_json ?options ?context ?(include_schema_in_prompt = true)
      session prompt (codec : _ Codec.value) =
    let context =
      match context with
      | None ->
          Some
            {
              include_schema_in_prompt = Some include_schema_in_prompt;
              reasoning_level = None;
            }
      | Some value ->
          Some
            {
              value with
              include_schema_in_prompt =
                (match value.include_schema_in_prompt with
                | None -> Some include_schema_in_prompt
                | some -> some);
            }
    in
    let json =
      run ?options ?context ~schema:(Codec.value_schema codec)
        ~output:Eio.Flow.null session prompt
    in
    match Jsont_bytesrw.decode_string codec.Codec.jsont json with
    | Ok value -> value
    | Error message ->
        raise_error "decoding a structured Foundation Models response"
          (`Decoding_failure message)

  let respond_json ?options ?context ?include_schema_in_prompt session prompt
      codec =
    respond_prompt_json ?options ?context ?include_schema_in_prompt session
      (Prompt.text prompt) codec

  let transcript session =
    Eio.Mutex.use_ro session.lock (fun () ->
        if is_closed session then
          raise_error "reading a Foundation Models transcript" `Closed
        else
          match Raw.transcript session.raw with
          | 0, json -> json
          | _, message ->
              raise_error "reading a Foundation Models transcript"
                (decode_error message))

  let replace_transcript session transcript =
    Eio.Mutex.use_ro session.lock (fun () ->
        if is_closed session then
          raise_error "replacing a Foundation Models transcript" `Closed
        else
          match
            Raw.replace_transcript session.raw (Transcript.to_json transcript)
          with
          | 0, _ -> ()
          | _, message ->
              raise_error "replacing a Foundation Models transcript"
                (decode_error message))

  type usage_wire = { input : int; cached : int; output : int; reasoning : int }

  let usage_jsont =
    Jsont.Object.map ~kind:"usage" (fun input cached output reasoning ->
        { input; cached; output; reasoning })
    |> Jsont.Object.mem "inputTokens" Jsont.int
    |> Jsont.Object.mem "cachedInputTokens" Jsont.int
    |> Jsont.Object.mem "outputTokens" Jsont.int
    |> Jsont.Object.mem "reasoningTokens" Jsont.int
    |> Jsont.Object.finish

  let usage session =
    Eio.Mutex.use_ro session.lock (fun () ->
        if is_closed session then
          raise_error "reading Foundation Models usage" `Closed;
        match Raw.usage session.raw with
        | code, message when code <> 0 ->
            raise_error "reading Foundation Models usage" (decode_error message)
        | _, "null" -> None
        | _, json -> (
            match Jsont_bytesrw.decode_string usage_jsont json with
            | Ok u ->
                Some
                  {
                    input_tokens = u.input;
                    cached_input_tokens = u.cached;
                    output_tokens = u.output;
                    reasoning_tokens = u.reasoning;
                  }
            | Error message ->
                raise_error "decoding Foundation Models usage"
                  (`Decoding_failure message)))
end
