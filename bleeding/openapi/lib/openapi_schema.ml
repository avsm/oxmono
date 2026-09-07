(** Validation of the schema information which cannot be expressed by OCaml
    record types alone. The immutable graph also validates opaque recursive
    fields, without depending on generated module ordering. *)
module Spec = Openapi_spec

type dialect = Oas30 | Oas31

let dialect version =
  let valid prefix = String.starts_with ~prefix version &&
    let patch = String.sub version (String.length prefix) (String.length version - String.length prefix) in
    patch <> "" && String.for_all (function '0'..'9' -> true | _ -> false) patch in
  if valid "3.0." then Oas30 else if valid "3.1." then Oas31
  else invalid_arg ("OpenAPI: unsupported specification version " ^ version)

let json codec v = match Jsont.Json.encode codec v with
  | Ok json -> json | Error e -> invalid_arg e
let schema_json s = json Spec.schema_or_ref_jsont s
let parse json = match Jsont.Json.decode Spec.schema_jsont json with
  | Ok schema -> schema | Error e -> invalid_arg ("OpenAPI schema: " ^ e)

let pointer_unescape name =
  let b = Buffer.create (String.length name) in
  let rec loop i =
    if i < String.length name then
      if name.[i] = '~' then begin
        if i + 1 >= String.length name then invalid_arg "OpenAPI: invalid JSON pointer escape";
        Buffer.add_char b (match name.[i+1] with '0' -> '~' | '1' -> '/'
          | _ -> invalid_arg "OpenAPI: invalid JSON pointer escape");
        loop (i + 2)
      end else begin Buffer.add_char b name.[i]; loop (i + 1) end
  in loop 0; Buffer.contents b

let pointer_escape name =
  let b = Buffer.create (String.length name) in
  String.iter (function '~' -> Buffer.add_string b "~0" | '/' -> Buffer.add_string b "~1"
    | c -> Buffer.add_char b c) name;
  Buffer.contents b

let reference_segments reference =
  let reference = match Uriz.pct_decode reference with
    | This reference -> reference
    | Null -> invalid_arg "OpenAPI: invalid percent escape in schema reference" in
  let prefix = "#/components/schemas/" in
  if not (String.starts_with ~prefix reference) then
    invalid_arg ("OpenAPI: unsupported schema reference " ^ reference);
  String.sub reference (String.length prefix) (String.length reference - String.length prefix)
  |> String.split_on_char '/' |> List.map pointer_unescape

let reference_name reference =
  reference_segments reference |> List.map pointer_escape |> String.concat "/"

let resolve_pointer definitions key =
  let fail () = invalid_arg ("OpenAPI: unresolved schema reference " ^ key) in
  let parts = String.split_on_char '/' key |> List.map pointer_unescape in
  let rec descend json = function
    | [] -> json
    | part :: rest -> match json with
      | Jsont.Object (fields,_) ->
          (match Spec.find_member part fields with Some value -> descend value rest | None -> fail ())
      | Jsont.Array (items,_) ->
          (match int_of_string_opt part with
           | Some n when n >= 0 && string_of_int n = part -> (match List.nth_opt items n with Some value -> descend value rest | None -> fail ())
           | _ -> fail ())
      | _ -> fail ()
  in
  match parts with
  | name :: rest -> (match List.assoc_opt name definitions with Some root -> descend root rest | None -> fail ())
  | [] -> fail ()

type node = {
  schema : Spec.schema;
  types : string list;
  reference : string option;
  pattern : Re.re option;
  multiple : Q.t option;
  properties : (string * node) list;
  items : node option;
  additional : node option;
  all : node list;
  any : node list option;
  one : node list option;
  not_ : node option;
}

type t = { dialect : dialect; raw_definitions : (string * Jsont.json) list; definitions : (string * node) list }

(* Shortest decimal spelling that round-trips to the represented float. This
   avoids binary-division errors for decimal multiples such as 0.3 / 0.1.
   Original JSON numeric lexemes beyond float precision remain unavailable. *)
let decimal n =
  let rec spell precision =
    let s = Printf.sprintf "%.*g" precision n in
    if precision = 17 || float_of_string s = n then Q.of_string s
    else spell (precision + 1)
  in spell 1

let rec compile dialect raw =
  let s = parse raw in
  let s = if dialect = Oas30 && s.reference <> None then
    { Spec.empty_schema with reference = s.reference } else s in
  let fail message = invalid_arg ("OpenAPI schema: " ^ message) in
  (match dialect with
   | Oas30 ->
       if s.boolean_schema <> None then fail "boolean schemas require OpenAPI 3.1";
       if s.type_union <> None then fail "type arrays require OpenAPI 3.1";
       if (s.exclusive_minimum <> None && s.exclusive_minimum_flag = None) ||
          (s.exclusive_maximum <> None && s.exclusive_maximum_flag = None) then
         fail "OpenAPI 3.0 exclusive bounds must be booleans"
   | Oas31 ->
       if s.exclusive_minimum_flag <> None || s.exclusive_maximum_flag <> None then
         fail "OpenAPI 3.1 exclusive bounds must be numbers");
  let types = Spec.schema_types s in
  if s.type_union = Some [] then fail "type array must not be empty";
  if List.length types <> List.length (List.sort_uniq String.compare types) then fail "duplicate type";
  List.iter (fun t -> if not (List.mem t ["null";"boolean";"object";"array";"number";"integer";"string"]) then
    fail ("unknown type " ^ t)) types;
  if dialect = Oas30 && List.mem "null" types then fail "null type requires OpenAPI 3.1";
  let types = if dialect = Oas30 && s.nullable && s.type_ <> None then "null" :: types else types in
  List.iter (fun (name, n) -> match n with
    | Some n when n < 0 -> fail (name ^ " must be nonnegative") | _ -> ())
    ["minLength",s.min_length;"maxLength",s.max_length;"minItems",s.min_items;"maxItems",s.max_items;
     "minProperties",s.min_properties;"maxProperties",s.max_properties];
  List.iter (function Some n when not (Float.is_finite n) -> fail "non-finite bound" | _ -> ())
    [s.minimum;s.maximum;s.exclusive_minimum;s.exclusive_maximum;s.multiple_of];
  List.iter (fun (name, _) ->
    if List.mem name ["$id";"$dynamicRef";"$dynamicAnchor";"unevaluatedProperties";"unevaluatedItems";
      "patternProperties";"dependentSchemas";"dependentRequired";"prefixItems";"contains";
      "propertyNames";"if";"then";"else"] then fail ("unsupported validating keyword " ^ name)) s.extra_fields;
  let multiple = Option.map (fun n -> if n <= 0. then fail "multipleOf must be positive"; decimal n) s.multiple_of in
  let child = compile dialect in
  let additional = Option.map (function
    | Jsont.Bool (b, _) -> { schema = { Spec.empty_schema with boolean_schema = Some b };
        types=[];reference=None;pattern=None;multiple=None;properties=[];items=None;additional=None;
        all=[];any=None;one=None;not_=None }
    | json -> child json) s.additional_properties in
  let node = { schema=s; types;
    reference=Option.map reference_name s.reference;
    pattern=Option.map (fun pattern -> Re.compile (Re.Pcre.re pattern)) s.pattern;
    multiple; properties=List.map (fun (n,s) -> n, child s) s.properties;
    items=Option.map child s.items; additional;
    all=List.map child (Option.value ~default:[] s.all_of);
    any=Option.map (List.map child) s.any_of;
    one=Option.map (List.map child) s.one_of;
    not_=Option.map child s.not_ } in
  (* OpenAPI 3.0 Reference Objects ignore siblings. *)
  if dialect = Oas30 && s.reference <> None then
    { node with schema=Spec.empty_schema;types=[];pattern=None;multiple=None;properties=[];
      items=None;additional=None;all=[];any=None;one=None;not_=None }
  else node

let rec references node =
  Option.to_list node.reference @
  List.concat_map (fun (_,node) -> references node) node.properties @
  List.concat_map references (node.all @ Option.value ~default:[] node.any @ Option.value ~default:[] node.one) @
  List.concat_map (fun n -> Option.fold ~none:[] ~some:references n) [node.items;node.additional;node.not_]

let extend context keys =
  let rec collect definitions = function
    | [] -> { context with definitions }
    | key :: rest when List.mem_assoc key definitions -> collect definitions rest
    | key :: rest ->
        let node = compile context.dialect (resolve_pointer context.raw_definitions key) in
        collect ((key,node) :: definitions) (references node @ rest)
  in collect context.definitions keys

let create ~version raw_definitions =
  extend { dialect = dialect version; raw_definitions; definitions = [] }
    (List.map (fun (name, _) -> pointer_escape name) raw_definitions)

let of_json ~version raw = match raw with
  | Jsont.Object (fields, _) -> create ~version (List.map (fun ((n,_),v) -> n,v) fields)
  | _ -> invalid_arg "OpenAPI: expected schema definitions object"

let of_string ~version raw = match Jsont_bytesrw.decode_string Jsont.json raw with
  | Ok json -> of_json ~version json | Error e -> invalid_arg e

let rec validate_node context ~depth ~path node value =
  let fail message = Jsont.Error.msgf (Jsont.Json.meta value) "%s: %s" path message in
  if depth > 128 then fail "schema validation depth exceeded (possibly a reference cycle)";
  let validate ?(path=path) n value = validate_node context ~depth:(depth+1) ~path n value in
  let s = node.schema in
  (match s.boolean_schema with Some false -> fail "false schema rejects every value" | _ -> ());
  Option.iter (fun name -> match List.assoc_opt name context.definitions with
    | Some n -> validate n value | None -> fail ("unresolved reference " ^ name)) node.reference;
  let has_type = function
    | "null" -> (match value with Jsont.Null _ -> true | _ -> false)
    | "boolean" -> (match value with Jsont.Bool _ -> true | _ -> false)
    | "string" -> (match value with Jsont.String _ -> true | _ -> false)
    | "array" -> (match value with Jsont.Array _ -> true | _ -> false)
    | "object" -> (match value with Jsont.Object _ -> true | _ -> false)
    | "number" -> (match value with Jsont.Number (n,_) -> Float.is_finite n | _ -> false)
    | "integer" -> (match value with Jsont.Number (n,_) -> Float.is_finite n && Float.is_integer n | _ -> false)
    | _ -> false in
  if node.types <> [] && not (List.exists has_type node.types) then fail "unexpected JSON type";
  Option.iter (fun choices -> if not (List.exists (Jsont.Json.equal value) choices) then fail "value is not in enum") s.enum;
  Option.iter (fun expected -> if not (Jsont.Json.equal value expected) then fail "value differs from const") s.const;
  List.iter (fun n -> validate n value) node.all;
  let matches n = try validate n value; true with Jsont.Error _ -> false in
  Option.iter (fun choices -> if not (List.exists matches choices) then fail "no anyOf branch matched") node.any;
  Option.iter (fun choices ->
    let count = List.fold_left (fun count n -> if matches n then count+1 else count) 0 choices in
    if count <> 1 then fail "oneOf requires exactly one matching branch") node.one;
  Option.iter (fun n -> if matches n then fail "not schema matched") node.not_;
  let minimum name bound actual = Option.iter (fun bound -> if actual < bound then fail name) bound in
  let maximum name bound actual = Option.iter (fun bound -> if actual > bound then fail name) bound in
  match value with
  | Jsont.Number (n,_) ->
      if not (Float.is_finite n) then fail "number exceeds finite representation";
      minimum "below minimum" s.minimum n; maximum "above maximum" s.maximum n;
      Option.iter (fun b -> if n <= b then fail "below exclusiveMinimum") s.exclusive_minimum;
      Option.iter (fun b -> if n >= b then fail "above exclusiveMaximum") s.exclusive_maximum;
      Option.iter (fun divisor -> if not (Z.equal (Q.den (Q.div (decimal n) divisor)) Z.one) then fail "not a multipleOf") node.multiple
  | Jsont.String (text,_) ->
      let rec length i count = if i = String.length text then count else
        let u = String.get_utf_8_uchar text i in
        if not (Uchar.utf_decode_is_valid u) then fail "invalid UTF-8";
        length (i + Uchar.utf_decode_length u) (count+1) in
      let n = length 0 0 in
      minimum "below minLength" s.min_length n; maximum "above maxLength" s.max_length n;
      Option.iter (fun pattern -> if not (Re.execp pattern text) then fail "pattern mismatch") node.pattern
  | Jsont.Array (values,_) ->
      let n = List.length values in
      minimum "below minItems" s.min_items n; maximum "above maxItems" s.max_items n;
      if s.unique_items && List.length (List.sort_uniq Jsont.Json.compare values) <> n then fail "duplicate array items";
      Option.iter (fun item -> List.iteri (fun i value -> validate ~path:(path ^ "/" ^ string_of_int i) item value) values) node.items
  | Jsont.Object (fields,_) ->
      let names = List.map (fun ((name,_),_) -> name) fields in
      if List.length names <> List.length (List.sort_uniq String.compare names) then fail "duplicate object member";
      minimum "below minProperties" s.min_properties (List.length fields);
      maximum "above maxProperties" s.max_properties (List.length fields);
      List.iter (fun name -> if not (List.mem name names) then fail ("missing required property " ^ name)) s.required;
      List.iter (fun ((name,_),value) -> match List.assoc_opt name node.properties with
        | Some prop -> validate ~path:(path ^ "/" ^ name) prop value
        | None -> Option.iter (fun prop -> validate ~path:(path ^ "/" ^ name) prop value) node.additional) fields
  | _ -> ()

let validate context node value = validate_node context ~depth:0 ~path:"$" node value

let guard_node context node codec =
  Jsont.map Jsont.json ~kind:"validated OpenAPI schema"
    ~dec:(fun value ->
      validate context node value;
      match Jsont.Json.decode codec value with
      | Ok v -> v | Error e -> Jsont.Error.msg (Jsont.Json.meta value) e)
    ~enc:(fun value ->
      let value = match Jsont.Json.encode codec value with
        | Ok v -> v | Error e -> Jsont.Error.msg Jsont.Meta.none e in
      validate context node value;
      value)

let guard context schema codec =
  let node = compile context.dialect schema in
  let context = extend context (references node) in
  guard_node context node codec

let guard_ref context name codec = match List.assoc_opt (pointer_escape name) context.definitions with
  | Some node -> guard_node context node codec
  | None -> invalid_arg ("OpenAPI: unresolved schema " ^ name)


let guard_string context raw codec =
  match Jsont_bytesrw.decode_string Jsont.json raw with
  | Ok schema -> guard context schema codec
  | Error e -> invalid_arg ("OpenAPI schema: " ^ e)

let guard_response context schemas status codec =
  match Openapi_runtime.response_parser ~status schemas with
  | Some schema -> guard_string context schema codec
  | None -> codec
