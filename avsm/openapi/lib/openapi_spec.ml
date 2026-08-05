(** OpenAPI 3.x specification types with jsont codecs.

    This module defines types that mirror the OpenAPI 3.0/3.1 specification,
    with bidirectional JSON codecs using jsont. *)

(** {1 Reference handling} *)

type 'a or_ref =
  | Ref of string  (** A $ref pointer like "#/components/schemas/Pet" *)
  | Value of 'a    (** An inline value *)

(** Find a member by name in an object's member list *)
let find_member name (mems : Jsont.mem list) : Jsont.json option =
  List.find_map (fun ((n, _meta), v) ->
    if n = name then Some v else None
  ) mems

(** Create an or_ref codec that handles $ref pointers.
    Uses JSON as intermediate to detect $ref field. *)
let or_ref_jsont (value_jsont : 'a Jsont.t) : 'a or_ref Jsont.t =
  Jsont.map Jsont.json ~kind:"or_ref"
    ~dec:(fun json ->
      match json with
      | Jsont.Object (mems, _meta) ->
          (match find_member "$ref" mems with
           | Some (Jsont.String (ref_str, _)) -> Ref ref_str
           | _ ->
               (* Not a $ref, decode as value using bytesrw *)
               match Jsont_bytesrw.decode_string value_jsont
                       (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json)) with
               | Ok v -> Value v
               | Error e -> Jsont.Error.msg Jsont.Meta.none e)
      | _ ->
          (* Non-object, decode as value *)
          match Jsont_bytesrw.decode_string value_jsont
                  (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json)) with
          | Ok v -> Value v
          | Error e -> Jsont.Error.msg Jsont.Meta.none e)
    ~enc:(function
      | Ref r -> Jsont.Object ([(("$ref", Jsont.Meta.none), Jsont.String (r, Jsont.Meta.none))], Jsont.Meta.none)
      | Value v ->
          match Jsont_bytesrw.encode_string value_jsont v with
          | Ok s ->
              (match Jsont_bytesrw.decode_string Jsont.json s with
               | Ok json -> json
               | Error _ -> Jsont.Null ((), Jsont.Meta.none))
          | Error _ -> Jsont.Null ((), Jsont.Meta.none))

(** {1 String Map} *)

module StringMap = Map.Make(String)

let string_map_jsont (value_jsont : 'a Jsont.t) : (string * 'a) list Jsont.t =
  let map_jsont = Jsont.Object.as_string_map value_jsont in
  Jsont.map ~kind:"string_map"
    ~dec:(fun m -> StringMap.bindings m)
    ~enc:(fun pairs -> List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty pairs)
    map_jsont

(** {1 Contact} *)

type contact = {
  name : string option;
  url : string option;
  email : string option;
}

let contact_jsont : contact Jsont.t =
  Jsont.Object.map ~kind:"Contact"
    (fun name url email -> { name; url; email })
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun c -> c.name)
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun c -> c.url)
  |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun c -> c.email)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 License} *)

type license = {
  name : string;
  url : string option;
}

let license_jsont : license Jsont.t =
  Jsont.Object.map ~kind:"License"
    (fun name url -> { name; url })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun l -> l.name)
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun l -> l.url)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Info} *)

type info = {
  title : string;
  description : string option;
  terms_of_service : string option;
  contact : contact option;
  license : license option;
  version : string;
}

let info_jsont : info Jsont.t =
  Jsont.Object.map ~kind:"Info"
    (fun title description terms_of_service contact license version ->
      { title; description; terms_of_service; contact; license; version })
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun i -> i.title)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun i -> i.description)
  |> Jsont.Object.opt_mem "termsOfService" Jsont.string ~enc:(fun i -> i.terms_of_service)
  |> Jsont.Object.opt_mem "contact" contact_jsont ~enc:(fun i -> i.contact)
  |> Jsont.Object.opt_mem "license" license_jsont ~enc:(fun i -> i.license)
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun i -> i.version)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Server} *)

type server_variable = {
  enum : string list option;
  default : string;
  description : string option;
}

let server_variable_jsont : server_variable Jsont.t =
  Jsont.Object.map ~kind:"ServerVariable"
    (fun enum default description -> { enum; default; description })
  |> Jsont.Object.opt_mem "enum" Jsont.(list string) ~enc:(fun sv -> sv.enum)
  |> Jsont.Object.mem "default" Jsont.string ~enc:(fun sv -> sv.default)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun sv -> sv.description)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

type server = {
  url : string;
  description : string option;
  variables : (string * server_variable) list;
}

let server_jsont : server Jsont.t =
  Jsont.Object.map ~kind:"Server"
    (fun url description variables -> { url; description; variables })
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun s -> s.url)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun s -> s.description)
  |> Jsont.Object.mem "variables" (string_map_jsont server_variable_jsont)
       ~dec_absent:[] ~enc:(fun s -> s.variables)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 External Documentation} *)

type external_docs = {
  description : string option;
  url : string;
}

let external_docs_jsont : external_docs Jsont.t =
  Jsont.Object.map ~kind:"ExternalDocs"
    (fun description url -> { description; url })
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun ed -> ed.description)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun ed -> ed.url)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Tag} *)

type tag = {
  name : string;
  description : string option;
  external_docs : external_docs option;
}

let tag_jsont : tag Jsont.t =
  Jsont.Object.map ~kind:"Tag"
    (fun name description external_docs -> { name; description; external_docs })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun t -> t.description)
  |> Jsont.Object.opt_mem "externalDocs" external_docs_jsont ~enc:(fun t -> t.external_docs)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Discriminator} *)

type discriminator = {
  property_name : string;
  mapping : (string * string) list;
}

let discriminator_jsont : discriminator Jsont.t =
  Jsont.Object.map ~kind:"Discriminator"
    (fun property_name mapping -> { property_name; mapping })
  |> Jsont.Object.mem "propertyName" Jsont.string ~enc:(fun d -> d.property_name)
  |> Jsont.Object.mem "mapping" (string_map_jsont Jsont.string)
       ~dec_absent:[] ~enc:(fun d -> d.mapping)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Schema}

    JSON Schema with OpenAPI extensions. We use a simplified approach
    where references are stored as schema or_ref. *)

type schema = {
  title : string option;
  description : string option;
  type_ : string option;
  format : string option;
  default : Jsont.json option;
  nullable : bool;
  read_only : bool;
  write_only : bool;
  deprecated : bool;
  (* Validation *)
  enum : Jsont.json list option;
  const : Jsont.json option;
  minimum : float option;
  maximum : float option;
  exclusive_minimum : float option;
  exclusive_maximum : float option;
  multiple_of : float option;
  min_length : int option;
  max_length : int option;
  pattern : string option;
  min_items : int option;
  max_items : int option;
  unique_items : bool;
  min_properties : int option;
  max_properties : int option;
  (* Composition - stored as JSON for simplicity *)
  all_of : Jsont.json list option;
  one_of : Jsont.json list option;
  any_of : Jsont.json list option;
  not_ : Jsont.json option;
  (* Object - stored as JSON for simplicity *)
  properties : (string * Jsont.json) list;
  required : string list;
  additional_properties : Jsont.json option;
  (* Array *)
  items : Jsont.json option;
  (* Discriminator *)
  discriminator : discriminator option;
  (* Examples *)
  example : Jsont.json option;
}

let empty_schema = {
  title = None; description = None; type_ = None; format = None; default = None;
  nullable = false; read_only = false; write_only = false; deprecated = false;
  enum = None; const = None; minimum = None; maximum = None;
  exclusive_minimum = None; exclusive_maximum = None; multiple_of = None;
  min_length = None; max_length = None; pattern = None;
  min_items = None; max_items = None; unique_items = false;
  min_properties = None; max_properties = None;
  all_of = None; one_of = None; any_of = None; not_ = None;
  properties = []; required = []; additional_properties = None;
  items = None; discriminator = None; example = None;
}

let schema_jsont : schema Jsont.t =
  Jsont.Object.map ~kind:"Schema"
    (fun title description type_ format default nullable read_only write_only
         deprecated enum const minimum maximum exclusive_minimum exclusive_maximum
         multiple_of min_length max_length pattern min_items max_items unique_items
         min_properties max_properties all_of one_of any_of not_ properties required
         additional_properties items discriminator example ->
      { title; description; type_; format; default; nullable; read_only; write_only;
        deprecated; enum; const; minimum; maximum; exclusive_minimum; exclusive_maximum;
        multiple_of; min_length; max_length; pattern; min_items; max_items; unique_items;
        min_properties; max_properties; all_of; one_of; any_of; not_; properties; required;
        additional_properties; items; discriminator; example })
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:(fun s -> s.title)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun s -> s.description)
  |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun s -> s.type_)
  |> Jsont.Object.opt_mem "format" Jsont.string ~enc:(fun s -> s.format)
  |> Jsont.Object.opt_mem "default" Jsont.json ~enc:(fun s -> s.default)
  |> Jsont.Object.mem "nullable" Jsont.bool ~dec_absent:false ~enc:(fun s -> s.nullable)
  |> Jsont.Object.mem "readOnly" Jsont.bool ~dec_absent:false ~enc:(fun s -> s.read_only)
  |> Jsont.Object.mem "writeOnly" Jsont.bool ~dec_absent:false ~enc:(fun s -> s.write_only)
  |> Jsont.Object.mem "deprecated" Jsont.bool ~dec_absent:false ~enc:(fun s -> s.deprecated)
  |> Jsont.Object.opt_mem "enum" Jsont.(list json) ~enc:(fun s -> s.enum)
  |> Jsont.Object.opt_mem "const" Jsont.json ~enc:(fun s -> s.const)
  |> Jsont.Object.opt_mem "minimum" Jsont.number ~enc:(fun s -> s.minimum)
  |> Jsont.Object.opt_mem "maximum" Jsont.number ~enc:(fun s -> s.maximum)
  |> Jsont.Object.opt_mem "exclusiveMinimum" Jsont.number ~enc:(fun s -> s.exclusive_minimum)
  |> Jsont.Object.opt_mem "exclusiveMaximum" Jsont.number ~enc:(fun s -> s.exclusive_maximum)
  |> Jsont.Object.opt_mem "multipleOf" Jsont.number ~enc:(fun s -> s.multiple_of)
  |> Jsont.Object.opt_mem "minLength" Jsont.int ~enc:(fun s -> s.min_length)
  |> Jsont.Object.opt_mem "maxLength" Jsont.int ~enc:(fun s -> s.max_length)
  |> Jsont.Object.opt_mem "pattern" Jsont.string ~enc:(fun s -> s.pattern)
  |> Jsont.Object.opt_mem "minItems" Jsont.int ~enc:(fun s -> s.min_items)
  |> Jsont.Object.opt_mem "maxItems" Jsont.int ~enc:(fun s -> s.max_items)
  |> Jsont.Object.mem "uniqueItems" Jsont.bool ~dec_absent:false ~enc:(fun s -> s.unique_items)
  |> Jsont.Object.opt_mem "minProperties" Jsont.int ~enc:(fun s -> s.min_properties)
  |> Jsont.Object.opt_mem "maxProperties" Jsont.int ~enc:(fun s -> s.max_properties)
  |> Jsont.Object.opt_mem "allOf" Jsont.(list json) ~enc:(fun s -> s.all_of)
  |> Jsont.Object.opt_mem "oneOf" Jsont.(list json) ~enc:(fun s -> s.one_of)
  |> Jsont.Object.opt_mem "anyOf" Jsont.(list json) ~enc:(fun s -> s.any_of)
  |> Jsont.Object.opt_mem "not" Jsont.json ~enc:(fun s -> s.not_)
  |> Jsont.Object.mem "properties" (string_map_jsont Jsont.json)
       ~dec_absent:[] ~enc:(fun s -> s.properties)
  |> Jsont.Object.mem "required" Jsont.(list string)
       ~dec_absent:[] ~enc:(fun s -> s.required)
  |> Jsont.Object.opt_mem "additionalProperties" Jsont.json
       ~enc:(fun s -> s.additional_properties)
  |> Jsont.Object.opt_mem "items" Jsont.json ~enc:(fun s -> s.items)
  |> Jsont.Object.opt_mem "discriminator" discriminator_jsont ~enc:(fun s -> s.discriminator)
  |> Jsont.Object.opt_mem "example" Jsont.json ~enc:(fun s -> s.example)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let schema_or_ref_jsont = or_ref_jsont schema_jsont

(** {1 Parameter} *)

type parameter_location = Query | Header | Path | Cookie

let parameter_location_jsont : parameter_location Jsont.t =
  Jsont.map Jsont.string ~kind:"parameter_location"
    ~dec:(function
      | "query" -> Query
      | "header" -> Header
      | "path" -> Path
      | "cookie" -> Cookie
      | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown parameter location: %s" s)
    ~enc:(function
      | Query -> "query"
      | Header -> "header"
      | Path -> "path"
      | Cookie -> "cookie")

type parameter_style =
  | Matrix | Label | Form | Simple | SpaceDelimited
  | PipeDelimited | DeepObject

let parameter_style_jsont : parameter_style Jsont.t =
  Jsont.map Jsont.string ~kind:"parameter_style"
    ~dec:(function
      | "matrix" -> Matrix
      | "label" -> Label
      | "form" -> Form
      | "simple" -> Simple
      | "spaceDelimited" -> SpaceDelimited
      | "pipeDelimited" -> PipeDelimited
      | "deepObject" -> DeepObject
      | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown parameter style: %s" s)
    ~enc:(function
      | Matrix -> "matrix"
      | Label -> "label"
      | Form -> "form"
      | Simple -> "simple"
      | SpaceDelimited -> "spaceDelimited"
      | PipeDelimited -> "pipeDelimited"
      | DeepObject -> "deepObject")

(** {1 Example} *)

type example = {
  summary : string option;
  description : string option;
  value : Jsont.json option;
  external_value : string option;
}

let example_jsont : example Jsont.t =
  Jsont.Object.map ~kind:"Example"
    (fun summary description value external_value ->
      { summary; description; value; external_value })
  |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:(fun e -> e.summary)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun e -> e.description)
  |> Jsont.Object.opt_mem "value" Jsont.json ~enc:(fun e -> e.value)
  |> Jsont.Object.opt_mem "externalValue" Jsont.string ~enc:(fun e -> e.external_value)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let example_or_ref_jsont = or_ref_jsont example_jsont

(** {1 Header} *)

type header = {
  description : string option;
  required : bool;
  deprecated : bool;
  schema : schema or_ref option;
}

let header_jsont : header Jsont.t =
  Jsont.Object.map ~kind:"Header"
    (fun description required deprecated schema ->
      { description; required; deprecated; schema })
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun h -> h.description)
  |> Jsont.Object.mem "required" Jsont.bool ~dec_absent:false ~enc:(fun h -> h.required)
  |> Jsont.Object.mem "deprecated" Jsont.bool ~dec_absent:false ~enc:(fun h -> h.deprecated)
  |> Jsont.Object.opt_mem "schema" schema_or_ref_jsont ~enc:(fun h -> h.schema)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let header_or_ref_jsont = or_ref_jsont header_jsont

(** {1 Encoding} *)

type encoding = {
  content_type : string option;
  headers : (string * header or_ref) list;
  style : parameter_style option;
  explode : bool option;
  allow_reserved : bool;
}

let encoding_jsont : encoding Jsont.t =
  Jsont.Object.map ~kind:"Encoding"
    (fun content_type headers style explode allow_reserved ->
      { content_type; headers; style; explode; allow_reserved })
  |> Jsont.Object.opt_mem "contentType" Jsont.string ~enc:(fun e -> e.content_type)
  |> Jsont.Object.mem "headers" (string_map_jsont header_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun e -> e.headers)
  |> Jsont.Object.opt_mem "style" parameter_style_jsont ~enc:(fun e -> e.style)
  |> Jsont.Object.opt_mem "explode" Jsont.bool ~enc:(fun e -> e.explode)
  |> Jsont.Object.mem "allowReserved" Jsont.bool ~dec_absent:false ~enc:(fun e -> e.allow_reserved)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Media Type} *)

type media_type = {
  schema : schema or_ref option;
  example : Jsont.json option;
  examples : (string * example or_ref) list;
  encoding : (string * encoding) list;
}

let media_type_jsont : media_type Jsont.t =
  Jsont.Object.map ~kind:"MediaType"
    (fun schema example examples encoding ->
      { schema; example; examples; encoding })
  |> Jsont.Object.opt_mem "schema" schema_or_ref_jsont ~enc:(fun mt -> mt.schema)
  |> Jsont.Object.opt_mem "example" Jsont.json ~enc:(fun mt -> mt.example)
  |> Jsont.Object.mem "examples" (string_map_jsont example_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun mt -> mt.examples)
  |> Jsont.Object.mem "encoding" (string_map_jsont encoding_jsont)
       ~dec_absent:[] ~enc:(fun mt -> mt.encoding)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Parameter} *)

type parameter = {
  name : string;
  in_ : parameter_location;
  description : string option;
  required : bool;
  deprecated : bool;
  allow_empty_value : bool;
  style : parameter_style option;
  explode : bool option;
  allow_reserved : bool;
  schema : schema or_ref option;
  example : Jsont.json option;
  content : (string * media_type) list;
}

let parameter_jsont : parameter Jsont.t =
  Jsont.Object.map ~kind:"Parameter"
    (fun name in_ description required deprecated allow_empty_value style
         explode allow_reserved schema example content ->
      { name; in_; description; required; deprecated; allow_empty_value;
        style; explode; allow_reserved; schema; example; content })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun p -> p.name)
  |> Jsont.Object.mem "in" parameter_location_jsont ~enc:(fun p -> p.in_)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun p -> p.description)
  |> Jsont.Object.mem "required" Jsont.bool ~dec_absent:false ~enc:(fun p -> p.required)
  |> Jsont.Object.mem "deprecated" Jsont.bool ~dec_absent:false ~enc:(fun p -> p.deprecated)
  |> Jsont.Object.mem "allowEmptyValue" Jsont.bool ~dec_absent:false ~enc:(fun p -> p.allow_empty_value)
  |> Jsont.Object.opt_mem "style" parameter_style_jsont ~enc:(fun p -> p.style)
  |> Jsont.Object.opt_mem "explode" Jsont.bool ~enc:(fun p -> p.explode)
  |> Jsont.Object.mem "allowReserved" Jsont.bool ~dec_absent:false ~enc:(fun p -> p.allow_reserved)
  |> Jsont.Object.opt_mem "schema" schema_or_ref_jsont ~enc:(fun p -> p.schema)
  |> Jsont.Object.opt_mem "example" Jsont.json ~enc:(fun p -> p.example)
  |> Jsont.Object.mem "content" (string_map_jsont media_type_jsont)
       ~dec_absent:[] ~enc:(fun p -> p.content)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let parameter_or_ref_jsont = or_ref_jsont parameter_jsont

(** {1 Request Body} *)

type request_body = {
  description : string option;
  content : (string * media_type) list;
  required : bool;
}

let request_body_jsont : request_body Jsont.t =
  Jsont.Object.map ~kind:"RequestBody"
    (fun description content required -> { description; content; required })
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun rb -> rb.description)
  |> Jsont.Object.mem "content" (string_map_jsont media_type_jsont)
       ~dec_absent:[] ~enc:(fun rb -> rb.content)
  |> Jsont.Object.mem "required" Jsont.bool ~dec_absent:false ~enc:(fun rb -> rb.required)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let request_body_or_ref_jsont = or_ref_jsont request_body_jsont

(** {1 Link} *)

type link = {
  operation_ref : string option;
  operation_id : string option;
  parameters : (string * Jsont.json) list;
  request_body : Jsont.json option;
  description : string option;
  server : server option;
}

let link_jsont : link Jsont.t =
  Jsont.Object.map ~kind:"Link"
    (fun operation_ref operation_id parameters request_body description server ->
      { operation_ref; operation_id; parameters; request_body; description; server })
  |> Jsont.Object.opt_mem "operationRef" Jsont.string ~enc:(fun l -> l.operation_ref)
  |> Jsont.Object.opt_mem "operationId" Jsont.string ~enc:(fun l -> l.operation_id)
  |> Jsont.Object.mem "parameters" (string_map_jsont Jsont.json)
       ~dec_absent:[] ~enc:(fun l -> l.parameters)
  |> Jsont.Object.opt_mem "requestBody" Jsont.json ~enc:(fun l -> l.request_body)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun l -> l.description)
  |> Jsont.Object.opt_mem "server" server_jsont ~enc:(fun l -> l.server)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let link_or_ref_jsont = or_ref_jsont link_jsont

(** {1 Response} *)

type response = {
  description : string;
  headers : (string * header or_ref) list;
  content : (string * media_type) list;
  links : (string * link or_ref) list;
}

let response_jsont : response Jsont.t =
  Jsont.Object.map ~kind:"Response"
    (fun description headers content links ->
      { description; headers; content; links })
  |> Jsont.Object.mem "description" Jsont.string ~dec_absent:"" ~enc:(fun r -> r.description)
  |> Jsont.Object.mem "headers" (string_map_jsont header_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun r -> r.headers)
  |> Jsont.Object.mem "content" (string_map_jsont media_type_jsont)
       ~dec_absent:[] ~enc:(fun r -> r.content)
  |> Jsont.Object.mem "links" (string_map_jsont link_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun r -> r.links)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let response_or_ref_jsont = or_ref_jsont response_jsont

(** {1 Responses} *)

type responses = {
  default : response or_ref option;
  responses : (string * response or_ref) list;  (* status code -> response *)
}

let responses_jsont : responses Jsont.t =
  (* Responses is an object where keys are status codes or "default" *)
  Jsont.map (Jsont.Object.as_string_map response_or_ref_jsont) ~kind:"Responses"
    ~dec:(fun m ->
      let default = StringMap.find_opt "default" m in
      let responses =
        StringMap.bindings m
        |> List.filter (fun (k, _) -> k <> "default")
      in
      { default; responses })
    ~enc:(fun r ->
      let m = List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty r.responses in
      match r.default with
      | Some d -> StringMap.add "default" d m
      | None -> m)

(** {1 Security Requirement} *)

type security_requirement = (string * string list) list

let security_requirement_jsont : security_requirement Jsont.t =
  string_map_jsont Jsont.(list string)

(** {1 Callback - simplified to JSON} *)

type callback = Jsont.json

let callback_jsont : callback Jsont.t = Jsont.json
let callback_or_ref_jsont = or_ref_jsont callback_jsont

(** {1 Operation} *)

type operation = {
  tags : string list;
  summary : string option;
  description : string option;
  external_docs : external_docs option;
  operation_id : string option;
  parameters : parameter or_ref list;
  request_body : request_body or_ref option;
  responses : responses;
  callbacks : (string * callback or_ref) list;
  deprecated : bool;
  security : security_requirement list option;
  servers : server list;
}

let operation_jsont : operation Jsont.t =
  Jsont.Object.map ~kind:"Operation"
    (fun tags summary description external_docs operation_id parameters
         request_body responses callbacks deprecated security servers ->
      { tags; summary; description; external_docs; operation_id; parameters;
        request_body; responses; callbacks; deprecated; security; servers })
  |> Jsont.Object.mem "tags" Jsont.(list string) ~dec_absent:[] ~enc:(fun o -> o.tags)
  |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:(fun o -> o.summary)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun o -> o.description)
  |> Jsont.Object.opt_mem "externalDocs" external_docs_jsont ~enc:(fun o -> o.external_docs)
  |> Jsont.Object.opt_mem "operationId" Jsont.string ~enc:(fun o -> o.operation_id)
  |> Jsont.Object.mem "parameters" Jsont.(list parameter_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun o -> o.parameters)
  |> Jsont.Object.opt_mem "requestBody" request_body_or_ref_jsont ~enc:(fun o -> o.request_body)
  |> Jsont.Object.mem "responses" responses_jsont
       ~dec_absent:{ default = None; responses = [] } ~enc:(fun o -> o.responses)
  |> Jsont.Object.mem "callbacks" (string_map_jsont callback_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun o -> o.callbacks)
  |> Jsont.Object.mem "deprecated" Jsont.bool ~dec_absent:false ~enc:(fun o -> o.deprecated)
  |> Jsont.Object.opt_mem "security" Jsont.(list security_requirement_jsont)
       ~enc:(fun o -> o.security)
  |> Jsont.Object.mem "servers" Jsont.(list server_jsont)
       ~dec_absent:[] ~enc:(fun o -> o.servers)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Path Item} *)

type path_item = {
  ref_ : string option;
  summary : string option;
  description : string option;
  get : operation option;
  put : operation option;
  post : operation option;
  delete : operation option;
  options : operation option;
  head : operation option;
  patch : operation option;
  trace : operation option;
  servers : server list;
  parameters : parameter or_ref list;
}

let path_item_jsont : path_item Jsont.t =
  Jsont.Object.map ~kind:"PathItem"
    (fun ref_ summary description get put post delete options head patch trace
         servers parameters ->
      { ref_; summary; description; get; put; post; delete; options; head;
        patch; trace; servers; parameters })
  |> Jsont.Object.opt_mem "$ref" Jsont.string ~enc:(fun pi -> pi.ref_)
  |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:(fun pi -> pi.summary)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun pi -> pi.description)
  |> Jsont.Object.opt_mem "get" operation_jsont ~enc:(fun pi -> pi.get)
  |> Jsont.Object.opt_mem "put" operation_jsont ~enc:(fun pi -> pi.put)
  |> Jsont.Object.opt_mem "post" operation_jsont ~enc:(fun pi -> pi.post)
  |> Jsont.Object.opt_mem "delete" operation_jsont ~enc:(fun pi -> pi.delete)
  |> Jsont.Object.opt_mem "options" operation_jsont ~enc:(fun pi -> pi.options)
  |> Jsont.Object.opt_mem "head" operation_jsont ~enc:(fun pi -> pi.head)
  |> Jsont.Object.opt_mem "patch" operation_jsont ~enc:(fun pi -> pi.patch)
  |> Jsont.Object.opt_mem "trace" operation_jsont ~enc:(fun pi -> pi.trace)
  |> Jsont.Object.mem "servers" Jsont.(list server_jsont)
       ~dec_absent:[] ~enc:(fun pi -> pi.servers)
  |> Jsont.Object.mem "parameters" Jsont.(list parameter_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun pi -> pi.parameters)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let path_item_or_ref_jsont = or_ref_jsont path_item_jsont

(** {1 Security Scheme} *)

type security_scheme_type =
  | ApiKey
  | Http
  | OAuth2
  | OpenIdConnect

let security_scheme_type_jsont : security_scheme_type Jsont.t =
  Jsont.map Jsont.string ~kind:"security_scheme_type"
    ~dec:(function
      | "apiKey" -> ApiKey
      | "http" -> Http
      | "oauth2" -> OAuth2
      | "openIdConnect" -> OpenIdConnect
      | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown security scheme type: %s" s)
    ~enc:(function
      | ApiKey -> "apiKey"
      | Http -> "http"
      | OAuth2 -> "oauth2"
      | OpenIdConnect -> "openIdConnect")

type oauth_flow = {
  authorization_url : string option;
  token_url : string option;
  refresh_url : string option;
  scopes : (string * string) list;
}

let oauth_flow_jsont : oauth_flow Jsont.t =
  Jsont.Object.map ~kind:"OAuthFlow"
    (fun authorization_url token_url refresh_url scopes ->
      { authorization_url; token_url; refresh_url; scopes })
  |> Jsont.Object.opt_mem "authorizationUrl" Jsont.string ~enc:(fun f -> f.authorization_url)
  |> Jsont.Object.opt_mem "tokenUrl" Jsont.string ~enc:(fun f -> f.token_url)
  |> Jsont.Object.opt_mem "refreshUrl" Jsont.string ~enc:(fun f -> f.refresh_url)
  |> Jsont.Object.mem "scopes" (string_map_jsont Jsont.string)
       ~dec_absent:[] ~enc:(fun f -> f.scopes)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

type oauth_flows = {
  implicit : oauth_flow option;
  password : oauth_flow option;
  client_credentials : oauth_flow option;
  authorization_code : oauth_flow option;
}

let oauth_flows_jsont : oauth_flows Jsont.t =
  Jsont.Object.map ~kind:"OAuthFlows"
    (fun implicit password client_credentials authorization_code ->
      { implicit; password; client_credentials; authorization_code })
  |> Jsont.Object.opt_mem "implicit" oauth_flow_jsont ~enc:(fun f -> f.implicit)
  |> Jsont.Object.opt_mem "password" oauth_flow_jsont ~enc:(fun f -> f.password)
  |> Jsont.Object.opt_mem "clientCredentials" oauth_flow_jsont ~enc:(fun f -> f.client_credentials)
  |> Jsont.Object.opt_mem "authorizationCode" oauth_flow_jsont ~enc:(fun f -> f.authorization_code)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

type security_scheme = {
  type_ : security_scheme_type;
  description : string option;
  name : string option;
  in_ : parameter_location option;
  scheme : string option;
  bearer_format : string option;
  flows : oauth_flows option;
  open_id_connect_url : string option;
}

let security_scheme_jsont : security_scheme Jsont.t =
  Jsont.Object.map ~kind:"SecurityScheme"
    (fun type_ description name in_ scheme bearer_format flows open_id_connect_url ->
      { type_; description; name; in_; scheme; bearer_format; flows; open_id_connect_url })
  |> Jsont.Object.mem "type" security_scheme_type_jsont ~enc:(fun ss -> ss.type_)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun ss -> ss.description)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun ss -> ss.name)
  |> Jsont.Object.opt_mem "in" parameter_location_jsont ~enc:(fun ss -> ss.in_)
  |> Jsont.Object.opt_mem "scheme" Jsont.string ~enc:(fun ss -> ss.scheme)
  |> Jsont.Object.opt_mem "bearerFormat" Jsont.string ~enc:(fun ss -> ss.bearer_format)
  |> Jsont.Object.opt_mem "flows" oauth_flows_jsont ~enc:(fun ss -> ss.flows)
  |> Jsont.Object.opt_mem "openIdConnectUrl" Jsont.string ~enc:(fun ss -> ss.open_id_connect_url)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let security_scheme_or_ref_jsont = or_ref_jsont security_scheme_jsont

(** {1 Components} *)

type components = {
  schemas : (string * schema or_ref) list;
  responses : (string * response or_ref) list;
  parameters : (string * parameter or_ref) list;
  examples : (string * example or_ref) list;
  request_bodies : (string * request_body or_ref) list;
  headers : (string * header or_ref) list;
  security_schemes : (string * security_scheme or_ref) list;
  links : (string * link or_ref) list;
  callbacks : (string * callback or_ref) list;
  path_items : (string * path_item or_ref) list;
}

let components_jsont : components Jsont.t =
  Jsont.Object.map ~kind:"Components"
    (fun schemas responses parameters examples request_bodies headers
         security_schemes links callbacks path_items ->
      { schemas; responses; parameters; examples; request_bodies;
        headers; security_schemes; links; callbacks; path_items })
  |> Jsont.Object.mem "schemas" (string_map_jsont schema_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.schemas)
  |> Jsont.Object.mem "responses" (string_map_jsont response_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.responses)
  |> Jsont.Object.mem "parameters" (string_map_jsont parameter_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.parameters)
  |> Jsont.Object.mem "examples" (string_map_jsont example_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.examples)
  |> Jsont.Object.mem "requestBodies" (string_map_jsont request_body_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.request_bodies)
  |> Jsont.Object.mem "headers" (string_map_jsont header_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.headers)
  |> Jsont.Object.mem "securitySchemes" (string_map_jsont security_scheme_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.security_schemes)
  |> Jsont.Object.mem "links" (string_map_jsont link_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.links)
  |> Jsont.Object.mem "callbacks" (string_map_jsont callback_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.callbacks)
  |> Jsont.Object.mem "pathItems" (string_map_jsont path_item_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun c -> c.path_items)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 OpenAPI Document} *)

type t = {
  openapi : string;
  info : info;
  servers : server list;
  paths : (string * path_item) list;
  webhooks : (string * path_item or_ref) list;
  components : components option;
  security : security_requirement list;
  tags : tag list;
  external_docs : external_docs option;
}

let jsont : t Jsont.t =
  Jsont.Object.map ~kind:"OpenAPI"
    (fun openapi info servers paths webhooks components security tags external_docs ->
      { openapi; info; servers; paths; webhooks; components; security; tags; external_docs })
  |> Jsont.Object.mem "openapi" Jsont.string ~enc:(fun t -> t.openapi)
  |> Jsont.Object.mem "info" info_jsont ~enc:(fun t -> t.info)
  |> Jsont.Object.mem "servers" Jsont.(list server_jsont)
       ~dec_absent:[] ~enc:(fun t -> t.servers)
  |> Jsont.Object.mem "paths" (string_map_jsont path_item_jsont)
       ~dec_absent:[] ~enc:(fun t -> t.paths)
  |> Jsont.Object.mem "webhooks" (string_map_jsont path_item_or_ref_jsont)
       ~dec_absent:[] ~enc:(fun t -> t.webhooks)
  |> Jsont.Object.opt_mem "components" components_jsont ~enc:(fun t -> t.components)
  |> Jsont.Object.mem "security" Jsont.(list security_requirement_jsont)
       ~dec_absent:[] ~enc:(fun t -> t.security)
  |> Jsont.Object.mem "tags" Jsont.(list tag_jsont)
       ~dec_absent:[] ~enc:(fun t -> t.tags)
  |> Jsont.Object.opt_mem "externalDocs" external_docs_jsont ~enc:(fun t -> t.external_docs)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(** {1 Parsing} *)

let of_string s =
  Jsont_bytesrw.decode_string jsont s

let of_string' s =
  Jsont_bytesrw.decode_string' jsont s

let to_string t =
  Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont t

let to_string' t =
  Jsont_bytesrw.encode_string' ~format:Jsont.Indent jsont t

(** {1 Reference Resolution} *)

let resolve_schema_ref (ref_str : string) (spec : t) : schema option =
  (* Parse $ref like "#/components/schemas/Pet" *)
  if not (String.length ref_str > 0 && ref_str.[0] = '#') then None
  else
    let parts = String.split_on_char '/' ref_str in
    match parts with
    | ["#"; "components"; "schemas"; name] ->
        (match spec.components with
         | None -> None
         | Some c ->
             match List.assoc_opt name c.schemas with
             | Some (Value s) -> Some s
             | Some (Ref _) -> None  (* nested refs not supported yet *)
             | None -> None)
    | _ -> None
