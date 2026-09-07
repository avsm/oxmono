(** Code generation from OpenAPI specifications.

    This module generates OCaml code from parsed OpenAPI specs:
    - Nested module structure grouped by common schema prefixes
    - Abstract types with accessor and constructor functions
    - Client functions placed in relevant type modules
    - Proper Eio error handling with context
*)

module Spec = Openapi_spec
module Schema = Openapi_schema

(** {1 Name Conversion} *)

module Name = struct
  module StringSet = Set.Make(String)

  let ocaml_keywords = StringSet.of_list [
    "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
    "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
    "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
    "val"; "virtual"; "when"; "while"; "with"
  ]

  let escape_keyword s =
    if StringSet.mem s ocaml_keywords then s ^ "_" else s

  let to_snake_case s =
    let buf = Buffer.create (String.length s) in
    let prev_upper = ref false in
    String.iteri (fun i c ->
      match c with
      | 'A'..'Z' ->
          if i > 0 && not !prev_upper && Buffer.length buf > 0 &&
             Buffer.nth buf (Buffer.length buf - 1) <> '_' then Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c);
          prev_upper := true
      | 'a'..'z' | '0'..'9' | '_' ->
          Buffer.add_char buf c;
          prev_upper := false
      | '-' | ' ' | '.' | '/' ->
          Buffer.add_char buf '_';
          prev_upper := false
      | _ ->
          prev_upper := false
    ) s;
    let name = Buffer.contents buf in
    let name = if name = "" || name = "_" then "value"
      else if name.[0] >= '0' && name.[0] <= '9' then "n_" ^ name
      else name in
    escape_keyword name

  let to_module_name s =
    let snake = to_snake_case s in
    let parts = String.split_on_char '_' snake in
    String.concat "" (List.map String.capitalize_ascii parts)

  let to_type_name s = String.lowercase_ascii (to_snake_case s)

  let to_variant_name s =
    let name = String.capitalize_ascii (to_snake_case s) in
    if name.[0] = '_' then "V" ^ name else name

  (** Split a schema name into prefix and suffix for nested modules.
      E.g., "AlbumResponseDto" -> ("Album", "ResponseDto") *)
  let split_schema_name (name : string) : string * string =
    (* Common suffixes to look for *)
    let suffixes = [
      "ResponseDto"; "RequestDto"; "CreateDto"; "UpdateDto"; "Dto";
      "Response"; "Request"; "Create"; "Update"; "Config"; "Info";
      "Status"; "Type"; "Entity"; "Item"; "Entry"; "Data"; "Result"
    ] in
    let found = List.find_opt (fun suffix ->
      String.length name > String.length suffix &&
      String.ends_with ~suffix name
    ) suffixes in
    match found with
    | Some suffix ->
        let prefix_len = String.length name - String.length suffix in
        let prefix = String.sub name 0 prefix_len in
        if prefix = "" then (name, "T")
        else (prefix, suffix)
    | None ->
        (* No known suffix, use as-is with submodule T *)
        (name, "T")

  let operation_name ~(method_ : string) ~(path : string) ~(operation_id : string option) =
    match operation_id with
    | Some id -> to_snake_case id
    | None ->
        let method_name = String.lowercase_ascii method_ in
        let path_parts = String.split_on_char '/' path
          |> List.filter (fun s -> s <> "")
          |> List.map (fun s -> if s.[0] = '{' then "by_" ^ to_snake_case s else s)
        in
        let path_name = String.concat "_" (List.map to_snake_case path_parts) in
        method_name ^ "_" ^ path_name
end

(** {1 OCamldoc Helpers} *)

let escape_doc s =
  (* OCaml comments nest; neutralize both delimiters in spec-provided text. *)
  let s = Re.replace_string (Re.compile (Re.str "(*")) ~by:"( *" s in
  let s = Re.replace_string (Re.compile (Re.str "*)")) ~by:"* )" s in
  let s = String.concat "\\}" (String.split_on_char '}' s) in
  String.concat "\\{" (String.split_on_char '{' s)

let format_doc ?(indent=0) description =
  let prefix = String.make indent ' ' in
  match description with
  | None | Some "" -> ""
  | Some desc -> Printf.sprintf "%s(** %s *)\n" prefix (escape_doc desc)

let format_doc_block ?(indent=0) ~summary ?description () =
  let prefix = String.make indent ' ' in
  match summary, description with
  | None, None -> ""
  | Some s, None -> Printf.sprintf "%s(** %s *)\n" prefix (escape_doc s)
  | None, Some d -> Printf.sprintf "%s(** %s *)\n" prefix (escape_doc d)
  | Some s, Some d ->
      Printf.sprintf "%s(** %s\n\n%s    %s *)\n" prefix (escape_doc s) prefix (escape_doc d)

let format_param_doc name description =
  match description with
  | None | Some "" -> ""
  | Some d -> Printf.sprintf "    @param %s %s\n" name (escape_doc d)

(** {1 JSON Helpers} *)

let json_string = function
  | Jsont.String (s, _) -> Some s
  | _ -> None

let json_object = function
  | Jsont.Object (mems, _) -> Some mems
  | _ -> None

let get_ref json =
  Option.bind (json_object json) (fun mems ->
    List.find_map (fun ((n, _), v) ->
      if n = "$ref" then json_string v else None
    ) mems)

let get_member name json =
  Option.bind (json_object json) (fun mems ->
    List.find_map (fun ((n, _), v) ->
      if n = name then Some v else None
    ) mems)

let get_string_member name json =
  Option.bind (get_member name json) json_string

(** {1 Schema Analysis} *)

let schema_name_from_ref ref_ =
  if String.starts_with ~prefix:"#/components/schemas/" ref_ then (match Schema.reference_segments ref_ with [name] -> Some name | _ -> None)
  else None

(** Resolve a schema reference to its definition *)
let resolve_schema_ref ~components ref_str =
  let rec resolve seen ref_str =
    if List.mem ref_str seen then invalid_arg ("OpenAPI: cyclic schema alias " ^ ref_str);
    match schema_name_from_ref ref_str, components with
    | Some name, Some (comps : Spec.components) ->
        (match List.assoc_opt name comps.schemas with
         | Some (Spec.Value schema) -> Some schema
         | Some (Spec.Ref ref_) -> resolve (ref_str :: seen) ref_
         | None -> None)
    | _ -> None
  in resolve [] ref_str

(** Flatten allOf composition by merging properties from all schemas *)
let rec flatten_all_of ?(seen = []) ~(components : Spec.components option) (schemas : Jsont.json list) : (string * Jsont.json) list * string list =
  List.fold_left (fun (props, reqs) json ->
    match get_ref json with
    | Some ref_str ->
      if List.mem ref_str seen then invalid_arg ("OpenAPI: cyclic allOf reference " ^ ref_str);
      (* Resolve the reference and get its properties *)
      (match resolve_schema_ref ~components ref_str with
       | Some schema ->
         let (nested_props, nested_reqs) =
           match schema.all_of with
           | Some all_of ->
               let props, reqs = flatten_all_of ~seen:(ref_str :: seen) ~components all_of in
               schema.properties @ props, schema.required @ reqs
           | None -> (schema.properties, schema.required)
         in
         (props @ nested_props, reqs @ nested_reqs)
       | None -> (props, reqs))
    | None ->
      (* Inline schema - get properties directly *)
      let inline_props = match get_member "properties" json with
        | Some (Jsont.Object (mems, _)) ->
          List.map (fun ((n, _), v) -> (n, v)) mems
        | _ -> []
      in
      let inline_reqs = match get_member "required" json with
        | Some (Jsont.Array (items, _)) ->
          List.filter_map (function Jsont.String (s, _) -> Some s | _ -> None) items
        | _ -> []
      in
      (props @ inline_props, reqs @ inline_reqs)
  ) ([], []) schemas

(** Expand a schema by resolving allOf composition *)
let expand_schema ~(components : Spec.components option) (schema : Spec.schema) : Spec.schema =
  match schema.all_of with
  | None -> schema
  | Some all_of_jsons ->
    let (all_props, all_reqs) = flatten_all_of ~components all_of_jsons in
    (* Merge with any direct properties on the schema *)
    let merged_props = schema.properties @ all_props in
    let merged_reqs = schema.required @ all_reqs in
    (* Deduplicate by property name, keeping later definitions *)
    let seen = Hashtbl.create 32 in
    let deduped_props = List.filter (fun (name, _) ->
      if Hashtbl.mem seen name then false
      else (Hashtbl.add seen name (); true)
    ) (List.rev merged_props) |> List.rev in
    let deduped_reqs = List.sort_uniq String.compare merged_reqs in
    { schema with properties = deduped_props; required = deduped_reqs; all_of = None }

let rec find_refs_in_json (json : Jsont.json) : string list =
  match json with
  | Jsont.Object (mems, _) ->
      (match List.find_map (fun ((n, _), v) ->
        if n = "$ref" then json_string v else None) mems with
       | Some ref_ -> Option.to_list (schema_name_from_ref ref_)
       | None -> List.concat_map (fun (_, v) -> find_refs_in_json v) mems)
  | Jsont.Array (items, _) -> List.concat_map find_refs_in_json items
  | _ -> []

let find_schema_dependencies (schema : Spec.schema) : string list =
  let from_properties = List.concat_map (fun (_, json) -> find_refs_in_json json) schema.properties in
  let refs_from_list = Option.fold ~none:[] ~some:(List.concat_map find_refs_in_json) in
  let from_reference = Option.bind schema.reference schema_name_from_ref |> Option.to_list in
  let from_additional = Option.fold ~none:[] ~some:find_refs_in_json schema.additional_properties in
  let from_items = Option.fold ~none:[] ~some:find_refs_in_json schema.items in
  List.sort_uniq String.compare
    (from_reference @ from_additional @ from_properties @ from_items @ refs_from_list schema.all_of
     @ refs_from_list schema.one_of @ refs_from_list schema.any_of)

(** {1 Module Tree Structure} *)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(** {1 Forward Reference Tracking}

    Track which modules come after the current module in the sorted order.
    This is used to detect forward references and replace them with Jsont.json. *)

type generation_context = {
  known_schemas : StringSet.t;
  forward_refs : StringSet.t;
}

let is_forward_ref ~context module_name =
  StringSet.mem module_name context.forward_refs

(** {1 Topological Sort} *)

(** Kahn's algorithm for topological sorting.
    Returns nodes in dependency order (dependencies first).
    Self-dependencies are ignored (they don't affect ordering). *)
let topological_sort (nodes : string list) (deps : string -> string list) : string list =
  (* Build adjacency list and in-degree map *)
  let nodes_set = StringSet.of_list nodes in
  let in_degree = List.fold_left (fun m node ->
    StringMap.add node 0 m
  ) StringMap.empty nodes in
  let adj = List.fold_left (fun m node ->
    StringMap.add node [] m
  ) StringMap.empty nodes in
  (* Add edges: if A depends on B, add edge B -> A
     Ignore self-dependencies (node depending on itself) *)
  let (in_degree, adj) = List.fold_left (fun (in_degree, adj) node ->
    let node_deps = deps node
      |> List.filter (fun d -> StringSet.mem d nodes_set && d <> node) in
    let in_degree = StringMap.add node (List.length node_deps) in_degree in
    let adj = List.fold_left (fun adj dep ->
      let existing = Option.value ~default:[] (StringMap.find_opt dep adj) in
      StringMap.add dep (node :: existing) adj
    ) adj node_deps in
    (in_degree, adj)
  ) (in_degree, adj) nodes in
  (* Start with nodes that have no dependencies *)
  let queue = List.filter (fun n ->
    StringMap.find n in_degree = 0
  ) nodes in
  let rec process queue in_degree result processed =
    match queue with
    | [] ->
        (* Check for remaining nodes (cycles) - break cycles by picking one node *)
        let remaining = List.filter (fun n ->
          not (StringSet.mem n processed) && StringMap.find n in_degree > 0
        ) nodes in
        (match remaining with
         | [] -> List.rev result
         | node :: _ ->
             (* Pick a node from the cycle and add it, then continue *)
             let result = node :: result in
             let processed = StringSet.add node processed in
             let dependents = Option.value ~default:[] (StringMap.find_opt node adj) in
             let (queue', in_degree) = List.fold_left (fun (q, deg) dep ->
               if StringSet.mem dep processed then (q, deg)
               else
                 let new_deg = StringMap.find dep deg - 1 in
                 let deg = StringMap.add dep new_deg deg in
                 if new_deg = 0 then (dep :: q, deg) else (q, deg)
             ) ([], in_degree) dependents in
             process queue' in_degree result processed)
    | node :: rest ->
        let result = node :: result in
        let processed = StringSet.add node processed in
        let dependents = Option.value ~default:[] (StringMap.find_opt node adj) in
        let (queue', in_degree) = List.fold_left (fun (q, deg) dep ->
          if StringSet.mem dep processed then (q, deg)
          else
            let new_deg = StringMap.find dep deg - 1 in
            let deg = StringMap.add dep new_deg deg in
            if new_deg = 0 then (dep :: q, deg) else (q, deg)
        ) (rest, in_degree) dependents in
        process queue' in_degree result processed
  in
  process queue in_degree [] StringSet.empty

(** Validation constraints extracted from JSON Schema *)
type validation_constraints = {
  minimum : float option;
  maximum : float option;
  exclusive_minimum : float option;
  exclusive_maximum : float option;
  min_length : int option;
  max_length : int option;
  pattern : string option;
  min_items : int option;
  max_items : int option;
  unique_items : bool;
}

let empty_constraints = {
  minimum = None; maximum = None;
  exclusive_minimum = None; exclusive_maximum = None;
  min_length = None; max_length = None; pattern = None;
  min_items = None; max_items = None; unique_items = false;
}

let has_constraints c =
  c.minimum <> None || c.maximum <> None ||
  c.exclusive_minimum <> None || c.exclusive_maximum <> None ||
  c.min_length <> None || c.max_length <> None || c.pattern <> None ||
  c.min_items <> None || c.max_items <> None || c.unique_items

(** Inline union variant for field-level oneOf/anyOf *)
type inline_union_variant =
  | Ref_variant of string * string   (** variant_name, schema_ref *)
  | Prim_variant of string * string  (** variant_name, primitive_type (string, int, etc.) *)

(** Field-level union info *)
type field_union_info = {
  field_variants : inline_union_variant list;
  field_union_style : [ `OneOf | `AnyOf ];
}

type field_info = {
  ocaml_name : string;
  json_name : string;
  ocaml_type : string;
  base_type : string;
  is_optional : bool;
  is_required : bool;
  is_nullable : bool;  (** JSON schema nullable: true *)
  description : string option;
  constraints : validation_constraints;  (** Validation constraints *)
  field_union : field_union_info option;  (** Inline union type info *)
  default_value : string option;  (** OCaml literal for default value *)
}

(** Union variant info for oneOf/anyOf schemas *)
type union_variant = {
  variant_name : string;      (** OCaml constructor name: "Crop" *)
  schema_ref : string;        (** Schema name: "AssetEditActionCrop" *)
}

(** Union type info for oneOf/anyOf schemas *)
type union_info = {
  discriminator_field : string option;  (** e.g., "type" or "action" *)
  discriminator_mapping : (string * string) list;  (** tag -> schema_ref *)
  variants : union_variant list;
  style : [ `OneOf | `AnyOf ];
}

type schema_info = {
  original_name : string;
  prefix : string;
  suffix : string;
  schema : Spec.schema;
  fields : field_info list;
  is_enum : bool;
  enum_variants : (string * string) list;  (* ocaml_name, json_value *)
  enum_base_type : string;  (* "string" or "int" for enum schemas *)
  description : string option;
  is_recursive : bool;
  is_opaque : bool;  (** Cycles that need a whole JSON representation. *)
  is_union : bool;  (** true if this is a oneOf/anyOf schema *)
  union_info : union_info option;
}

(** Error response info for typed error handling *)
type error_response = {
  status_code : string;       (** "400", "404", "5XX", "default" *)
  schema_ref : string option; (** Reference to error schema if present *)
  error_description : string;
}

(** The wire shape is independent of module placement. In particular, an
    array of component references must keep its array wrapper. *)
type wire_type = Any_json | Reference of string | Primitive of string
  | Array of wire_type | Nullable of wire_type | Checked of string * wire_type

type request_encoding = Json_body of string * wire_type
  | Form_body | Multipart_body | Raw_body of string

type response_encoding = Json_response of string list * wire_type
  | Raw_response of string list | Empty_response

type operation_info = {
  func_name : string;
  operation_id : string option;
  summary : string option;
  description : string option;
  tags : string list;
  path : string;
  method_ : string;
  path_params : (string * string * string option * bool) list;  (* ocaml, json, desc, required *)
  query_params : (string * string * string option * bool) list;
  header_params : (string * string * string option * bool) list;
  request_encoding : request_encoding option;
  body_required : bool;
  response_encoding : response_encoding;
  response_validation : (string * string) list;
  empty_statuses : string list;
  nonempty_statuses : string list;
  body_schema_ref : string option;
  has_request_body : bool;
  response_schema_ref : string option;
  error_responses : error_response list;  (** Typed error responses *)
}

type module_node = {
  name : string;
  schemas : schema_info list;
  operations : operation_info list;
  dependencies : StringSet.t;  (* Other prefix modules this depends on *)
  children : module_node StringMap.t;
}

let empty_node name = { name; schemas = []; operations = []; dependencies = StringSet.empty; children = StringMap.empty }

(** {1 Type Resolution} *)

(** Extract validation constraints from a JSON schema *)
let extract_constraints (json : Jsont.json) : validation_constraints =
  let get_float name =
    match get_member name json with
    | Some (Jsont.Number (f, _)) -> Some f
    | _ -> None
  in
  let get_int name =
    match get_member name json with
    | Some (Jsont.Number (f, _)) -> Some (int_of_float f)
    | _ -> None
  in
  let get_bool name =
    match get_member name json with
    | Some (Jsont.Bool (b, _)) -> b
    | _ -> false
  in
  {
    minimum = get_float "minimum";
    maximum = get_float "maximum";
    exclusive_minimum = get_float "exclusiveMinimum";
    exclusive_maximum = get_float "exclusiveMaximum";
    min_length = get_int "minLength";
    max_length = get_int "maxLength";
    pattern = get_string_member "pattern" json;
    min_items = get_int "minItems";
    max_items = get_int "maxItems";
    unique_items = get_bool "uniqueItems";
  }

(** Extract and convert a default value to an OCaml literal.
    Returns None if no default or if the default can't be represented. *)
let extract_default_value (json : Jsont.json) (base_type : string) : string option =
  match get_member "default" json with
  | None -> None
  | Some default_json ->
      match default_json, base_type with
      | Jsont.Bool (b, _), "bool" ->
          Some (if b then "true" else "false")
      | Jsont.Number (f, _), "int" when Result.is_ok (Openapi_runtime.Json.decode_json Openapi_runtime.int_jsont default_json) ->
          Some (Printf.sprintf "%d" (int_of_float f))
      | Jsont.Number (f, _), "int32" when Result.is_ok (Openapi_runtime.Json.decode_json Openapi_runtime.int32_jsont default_json) ->
          Some (Printf.sprintf "%ldl" (Int32.of_float f))
      | Jsont.Number (f, _), "int64" when Result.is_ok (Openapi_runtime.Json.decode_json Openapi_runtime.int64_jsont default_json) ->
          Some (Printf.sprintf "%LdL" (Int64.of_float f))
      | Jsont.Number (f, _), "float" when Float.is_finite f ->
          let s = Printf.sprintf "%.17g" f in
          (* Ensure it's a valid float literal *)
          if String.contains s '.' || String.contains s 'e' then Some s
          else Some (s ^ ".")
      | Jsont.String (s, _), "string" ->
          Some (Printf.sprintf "%S" s)
      | Jsont.Array ([], _), t when String.ends_with ~suffix:" list" t ->
          Some "[]"
      | _ -> None  (* Complex defaults not yet supported *)

(** Analyze inline oneOf/anyOf for field-level unions *)
let analyze_field_union (json : Jsont.json) : field_union_info option =
  let extract_variants style items =
    let variants = List.filter_map (fun item ->
      match get_ref item with
      | Some ref_ ->
          schema_name_from_ref ref_ |> Option.map (fun schema_ref ->
            let variant_name = Name.to_module_name schema_ref in
            Ref_variant (variant_name, schema_ref))
      | None ->
          (* Check for primitive type *)
          match get_string_member "type" item with
          | Some "string" -> Some (Prim_variant ("String", "string"))
          | Some "integer" -> Some (Prim_variant ("Int", "int"))
          | Some "number" -> Some (Prim_variant ("Float", "float"))
          | Some "boolean" -> Some (Prim_variant ("Bool", "bool"))
          | Some "null" -> Some (Prim_variant ("Null", "unit"))
          | _ -> None
    ) items in
    if List.length variants >= 2 && List.length variants = List.length items &&
       List.length variants = List.length (List.sort_uniq compare variants) then
      Some { field_variants = variants; field_union_style = style }
    else
      None
  in
  match get_member "oneOf" json with
  | Some (Jsont.Array (items, _)) -> extract_variants `OneOf items
  | _ ->
      match get_member "anyOf" json with
      | Some (Jsont.Array (items, _)) -> extract_variants `AnyOf items
      | _ -> None

(** Check if a field union has any schema references (which may have ordering issues) *)
let field_union_has_refs (union : field_union_info) : bool =
  List.exists (fun v ->
    match v with
    | Ref_variant _ -> true
    | Prim_variant _ -> false
  ) union.field_variants

(** Generate polymorphic variant type string for inline union.
    For unions with only primitive types, generate proper polymorphic variants.
    For unions with schema references, fall back to Jsont.json to avoid module ordering issues. *)
let poly_variant_type_of_union (union : field_union_info) : string =
  (* If any variant references a schema, we can't reliably generate types
     at analysis time due to module ordering. Use Jsont.json instead. *)
  if field_union_has_refs union then
    "Jsont.json"
  else
    let variants = List.map (fun v ->
      match v with
      | Ref_variant (name, schema_ref) ->
          let prefix, suffix = Name.split_schema_name schema_ref in
          Printf.sprintf "`%s of %s.%s.t" name (Name.to_module_name prefix) (Name.to_module_name suffix)
      | Prim_variant (name, prim_type) ->
          Printf.sprintf "`%s of %s" name prim_type
    ) union.field_variants in
    Printf.sprintf "[ %s ]" (String.concat " | " variants)

(** Type resolution result with full info *)
type type_resolution = {
  resolved_type : string;
  resolved_nullable : bool;
  resolved_constraints : validation_constraints;
  resolved_union : field_union_info option;
}

let rec resolve_type_full (json : Jsont.json) : type_resolution =
  (* Check if the schema is nullable *)
  let parsed = Schema.parse json in
  let is_nullable = Spec.schema_nullable parsed in
  let constraints = extract_constraints json in

  (* Check for oneOf/anyOf first *)
  match analyze_field_union json with
  | Some union ->
      let poly_type = poly_variant_type_of_union union in
      { resolved_type = poly_type; resolved_nullable = is_nullable;
        resolved_constraints = constraints; resolved_union = Some union }
  | None ->
      match get_ref json with
      | Some ref_ ->
          (match schema_name_from_ref ref_ with
           | Some name ->
               let prefix, suffix = Name.split_schema_name name in
               { resolved_type = Printf.sprintf "%s.%s.t" (Name.to_module_name prefix) (Name.to_module_name suffix);
                 resolved_nullable = is_nullable; resolved_constraints = constraints; resolved_union = None }
           | None ->
               { resolved_type = "Jsont.json"; resolved_nullable = is_nullable;
                 resolved_constraints = constraints; resolved_union = None })
      | None ->
          (* Check for allOf with a single $ref - common pattern for type aliasing *)
          (match get_member "allOf" json with
           | Some (Jsont.Array ([item], _)) ->
               (* Single item allOf - try to resolve it *)
               resolve_type_full item
           | Some (Jsont.Array (items, _)) when List.length items > 0 ->
               (* Multiple allOf items - try to find a $ref among them *)
               (match List.find_map (fun item ->
                 match get_ref item with
                 | Some ref_ -> schema_name_from_ref ref_
                 | None -> None
               ) items with
               | Some name ->
                   let prefix, suffix = Name.split_schema_name name in
                   { resolved_type = Printf.sprintf "%s.%s.t" (Name.to_module_name prefix) (Name.to_module_name suffix);
                     resolved_nullable = is_nullable; resolved_constraints = constraints; resolved_union = None }
               | None ->
                   { resolved_type = "Jsont.json"; resolved_nullable = is_nullable;
                     resolved_constraints = constraints; resolved_union = None })
           | _ ->
               let resolved_type = match parsed.type_ with
                 | Some "string" ->
                     (match get_string_member "format" json with
                      | Some "date-time" -> "Ptime.t"
                      | _ -> "string")
                 | Some "integer" ->
                     (match get_string_member "format" json with
                      | Some "int64" -> "int64"
                      | Some "int32" -> "int32"
                      | _ -> "int")
                 | Some "number" -> "float"
                 | Some "boolean" -> "bool"
                 | Some "array" ->
                     (match get_member "items" json with
                      | Some items ->
                          let elem = resolve_type_full items in
                          elem.resolved_type ^ (if elem.resolved_nullable then " option list" else " list")
                      | None -> "Jsont.json list")
                 | Some "object" -> "Jsont.json"
                 | _ -> "Jsont.json"
               in
               { resolved_type; resolved_nullable = is_nullable;
                 resolved_constraints = constraints; resolved_union = None })

(** Simple type resolution for backward compatibility *)
let rec type_of_json_schema (json : Jsont.json) : string * bool =
  let result = resolve_type_full json in
  (result.resolved_type, result.resolved_nullable)

let rec jsont_of_base_type = function
  | "string" -> "Jsont.string"
  | "int" -> "Openapi.Runtime.int_jsont"
  | "int32" -> "Openapi.Runtime.int32_jsont"
  | "int64" -> "Openapi.Runtime.int64_jsont"
  | "float" -> "Openapi.Runtime.number_jsont"
  | "bool" -> "Jsont.bool"
  | "Ptime.t" -> "Openapi.Runtime.ptime_jsont"
  | "Jsont.json" -> "Jsont.json"
  | s when String.ends_with ~suffix:" option" s ->
      let elem = String.sub s 0 (String.length s - 7) in
      Printf.sprintf "(Jsont.option %s)" (jsont_of_base_type elem)
  | s when String.ends_with ~suffix:" list" s ->
      let elem = String.sub s 0 (String.length s - 5) in
      Printf.sprintf "(Jsont.list %s)" (jsont_of_base_type elem)
  | s when String.ends_with ~suffix:".t" s ->
      let module_path = String.sub s 0 (String.length s - 2) in
      module_path ^ ".jsont"
  | _ -> "Jsont.json"

(** Generate a nullable codec wrapper for types that need to handle explicit JSON nulls *)
let nullable_jsont_of_base_type = function
  | "string" -> "Openapi.Runtime.nullable_string"
  | "int" -> "Openapi.Runtime.nullable_int"
  | "float" -> "Openapi.Runtime.nullable_float"
  | "bool" -> "Openapi.Runtime.nullable_bool"
  | "Ptime.t" -> "Openapi.Runtime.nullable_ptime"
  | base_type ->
      (* For other types, wrap with nullable_any *)
      Printf.sprintf "(Openapi.Runtime.nullable_any %s)" (jsont_of_base_type base_type)

(** Format a float value for OCaml code, wrapping negative numbers in parentheses
    and ensuring the value is formatted as a float (with decimal point) *)
let format_float_arg (name : string) (v : float) : string =
  (* Format as float with at least one decimal place *)
  let str = Printf.sprintf "%.17g" v in
  let float_str =
    if String.contains str '.' || String.contains str 'e' || String.contains str 'E' then
      str
    else
      str ^ "."
  in
  if v < 0.0 then
    Printf.sprintf "~%s:(%s)" name float_str
  else
    Printf.sprintf "~%s:%s" name float_str

(** Generate a validated codec wrapper based on constraints *)
let validated_jsont (constraints : validation_constraints) (base_codec : string) (base_type : string) : string =
  if not (has_constraints constraints) then
    base_codec
  else
    match base_type with
    | "string" ->
        let args = List.filter_map Fun.id [
          Option.map (fun v -> Printf.sprintf "~min_length:%d" v) constraints.min_length;
          Option.map (fun v -> Printf.sprintf "~max_length:%d" v) constraints.max_length;
          Option.map (fun v -> Printf.sprintf "~pattern:%S" v) constraints.pattern;
        ] in
        if args = [] then base_codec
        else Printf.sprintf "(Openapi.Runtime.validated_string %s %s)" (String.concat " " args) base_codec
    | "int" | "int32" | "int64" ->
        let args = List.filter_map Fun.id [
          Option.map (format_float_arg "minimum") constraints.minimum;
          Option.map (format_float_arg "maximum") constraints.maximum;
          Option.map (format_float_arg "exclusive_minimum") constraints.exclusive_minimum;
          Option.map (format_float_arg "exclusive_maximum") constraints.exclusive_maximum;
        ] in
        if args = [] then base_codec
        else Printf.sprintf "(Openapi.Runtime.validated_%s %s %s)" base_type (String.concat " " args) base_codec
    | "float" ->
        let args = List.filter_map Fun.id [
          Option.map (format_float_arg "minimum") constraints.minimum;
          Option.map (format_float_arg "maximum") constraints.maximum;
          Option.map (format_float_arg "exclusive_minimum") constraints.exclusive_minimum;
          Option.map (format_float_arg "exclusive_maximum") constraints.exclusive_maximum;
        ] in
        if args = [] then base_codec
        else Printf.sprintf "(Openapi.Runtime.validated_float %s %s)" (String.concat " " args) base_codec
    | s when String.ends_with ~suffix:" list" s ->
        let args = List.filter_map Fun.id [
          Option.map (fun v -> Printf.sprintf "~min_items:%d" v) constraints.min_items;
          Option.map (fun v -> Printf.sprintf "~max_items:%d" v) constraints.max_items;
          (if constraints.unique_items then Some "~unique_items:true" else None);
        ] in
        if args = [] then base_codec
        else
          (* Extract element codec from "(Jsont.list elem_codec)" pattern.
             validated_list takes elem_codec directly, not the wrapped list. *)
          let elem_codec =
            if String.length base_codec > 12 && String.sub base_codec 0 12 = "(Jsont.list " then
              (* Extract from "(Jsont.list X)" -> "X" *)
              String.sub base_codec 12 (String.length base_codec - 13)
            else
              (* Fallback: can't extract, skip validation *)
              ""
          in
          if elem_codec = "" then base_codec
          else Printf.sprintf "(Openapi.Runtime.validated_list %s %s)" (String.concat " " args) elem_codec
    | _ -> base_codec

(** Generate a jsont codec for a polymorphic variant union type.
    For unions with schema refs, returns Jsont.json (matching the fallback type).
    For primitive-only unions, generates a proper polymorphic variant codec. *)
let jsont_of_field_union ~current_prefix:_ (union : field_union_info) : string =
  (* If union has schema refs, we've already fallen back to Jsont.json type *)
  if field_union_has_refs union then
    "Jsont.json"
  else
    (* Primitive-only union - generate polymorphic variant codec *)
    let decoders = List.map (fun v ->
      match v with
      | Ref_variant _ -> failwith "unreachable: ref variant in primitive-only union"
      | Prim_variant (name, prim_type) ->
          let codec = jsont_of_base_type prim_type in
          Printf.sprintf {|(fun json ->
          match Openapi.Runtime.Json.decode_json %s json with
          | Ok v -> Some (`%s v)
          | Error _ -> None)|} codec name
    ) union.field_variants in

    let encoders = List.map (fun v ->
      match v with
      | Ref_variant _ -> failwith "unreachable: ref variant in primitive-only union"
      | Prim_variant (name, prim_type) ->
          let codec = jsont_of_base_type prim_type in
          Printf.sprintf "      | `%s v -> Openapi.Runtime.Json.encode_json %s v" name codec
    ) union.field_variants in

    Printf.sprintf {|(Jsont.map Jsont.json ~kind:"poly_union"
      ~dec:(fun json -> Openapi.Runtime.poly_union_decoder ~exclusive:%b [
        %s
      ] json)
      ~enc:(function
%s))|}
      (union.field_union_style = `OneOf)
      (String.concat ";\n        " decoders)
      (String.concat "\n" encoders)

(** {1 Schema Processing} *)

(** Extract variant name from a schema ref, stripping common prefixes *)
let variant_name_from_ref (ref_ : string) (parent_name : string) : string =
  match schema_name_from_ref ref_ with
  | None -> "Unknown"
  | Some name ->
      (* Try to strip parent prefix for shorter names *)
      let parent_prefix = match String.split_on_char '_' (Name.to_snake_case parent_name) with
        | first :: _ -> Name.to_module_name first
        | [] -> ""
      in
      if String.length name > String.length parent_prefix &&
         String.sub name 0 (String.length parent_prefix) = parent_prefix then
        Name.to_module_name (String.sub name (String.length parent_prefix)
          (String.length name - String.length parent_prefix))
      else
        Name.to_module_name name

(** Analyze oneOf/anyOf schemas to extract union information *)
let analyze_union ~(name : string) (schema : Spec.schema) : union_info option =
  let extract_variants style json_list =
    let variants = List.filter_map (fun json ->
      match get_ref json with
      | Some ref_ ->
          schema_name_from_ref ref_ |> Option.map (fun schema_ref ->
            let variant_name = variant_name_from_ref ref_ name in
            { variant_name; schema_ref })
      | None -> None  (* Skip inline schemas for now *)
    ) json_list in
    if variants = [] || List.length variants <> List.length json_list then None
    else
      let discriminator_field = Option.map (fun (d : Spec.discriminator) ->
        d.property_name) schema.discriminator in
      let discriminator_mapping = Option.fold ~none:[]
        ~some:(fun (d : Spec.discriminator) -> d.mapping) schema.discriminator in
      Some {
        discriminator_field;
        discriminator_mapping;
        variants;
        style;
      }
  in
  match schema.one_of, schema.any_of with
  | Some items, _ -> extract_variants `OneOf items
  | None, Some items -> extract_variants `AnyOf items
  | None, None -> None

let analyze_schema ~(components : Spec.components option) (name : string) (schema : Spec.schema) : schema_info =
  (* First expand allOf composition *)
  let expanded = expand_schema ~components schema in
  let prefix, suffix = Name.split_schema_name name in
  let is_enum = match expanded.enum with
    | Some ((_ :: _) as values) -> List.for_all (function Jsont.String _ -> true | _ -> false) values &&
        not (Spec.schema_nullable expanded)
    | _ -> false in
  (* Determine the base type for enums - integer enums should use int, not string *)
  let enum_base_type = match expanded.type_ with
    | Some "integer" -> "int"
    | _ -> "string"
  in
  let enum_variants = match expanded.enum with
    | Some values ->
        List.filter_map (fun json ->
          match json with
          | Jsont.String (s, _) -> Some (Name.to_variant_name s, s)
          | _ -> None
        ) values
    | None -> []
  in
  (* Check for oneOf/anyOf union types *)
  let dependencies = find_schema_dependencies expanded in
  let rec reaches seen target =
    if target = name then true else if List.mem target seen then false else
    match components with
    | None -> false
    | Some components -> match List.assoc_opt target components.Spec.schemas with
      | None -> false
      | Some schema ->
          let schema = match schema with Spec.Value s -> Some s
            | Spec.Ref r -> resolve_schema_ref ~components:(Some components) r in
          Option.fold ~none:false ~some:(fun schema ->
            List.exists (reaches (target :: seen)) (find_schema_dependencies schema)) schema in
  let union_info = analyze_union ~name expanded in
  let is_opaque = List.exists (fun dep -> dep <> name && reaches [] dep) dependencies ||
    (List.mem name dependencies && Option.is_some union_info) in
  let union_info = if is_opaque then None else union_info in
  let is_union = Option.is_some union_info in
  let object_fields = if is_opaque || Spec.schema_nullable expanded ||
      List.length (Spec.schema_types expanded) > 1 then [] else expanded.properties in
  let fields = List.map (fun (field_name, field_json) ->
    let ocaml_name = Name.to_snake_case field_name in
    let ocaml_name = if List.mem ocaml_name ["v";"jsont"] then ocaml_name ^ "_" else ocaml_name in
    let is_required = List.mem field_name expanded.required in
    let resolved = resolve_type_full field_json in
    let base_type = resolved.resolved_type in
    let is_nullable = resolved.resolved_nullable in
    let default_value =
      if is_required || is_nullable then None else extract_default_value field_json base_type in
    (* Field is optional in record type if:
       - nullable (can be null) OR
       - not required AND no default (may be absent with no fallback)
       Fields with defaults are NOT optional - they always have a value *)
    let has_default = Option.is_some default_value in
    let is_optional = is_nullable || (not is_required && not has_default) in
    let ocaml_type = if is_nullable && not is_required then base_type ^ " option option"
      else if is_optional then base_type ^ " option" else base_type in
    let description = get_string_member "description" field_json in
    { ocaml_name; json_name = field_name; ocaml_type; base_type; is_optional;
      is_required; is_nullable; description;
      constraints = resolved.resolved_constraints;
      field_union = resolved.resolved_union;
      default_value }
  ) object_fields in
  (* Check if schema references itself *)
  let deps = find_schema_dependencies expanded in
  let is_recursive = List.mem name deps in
  { original_name = name; prefix; suffix; schema = expanded; fields; is_enum; enum_variants;
    enum_base_type; description = expanded.description; is_recursive; is_opaque; is_union; union_info }

(** {1 Operation Processing} *)

(** Resolve local component aliases with cycle detection. Unsupported references
    fail at generation time instead of silently dropping required parameters. *)
let resolve_component ~kind entries value =
  let prefix = "#/components/" ^ kind ^ "/" in
  let rec resolve seen = function
    | Spec.Value value -> value
    | Spec.Ref ref_ ->
        if List.mem ref_ seen then invalid_arg ("OpenAPI: cyclic reference " ^ ref_);
        if not (String.starts_with ~prefix ref_) then
          invalid_arg ("OpenAPI: unsupported reference " ^ ref_);
        let name = String.sub ref_ (String.length prefix) (String.length ref_ - String.length prefix) in
        let name = Re.replace_string (Re.compile (Re.str "~1")) ~by:"/" name in
        let name = Re.replace_string (Re.compile (Re.str "~0")) ~by:"~" name in
        match List.assoc_opt name entries with
        | Some next -> resolve (ref_ :: seen) next
        | None -> invalid_arg ("OpenAPI: unresolved reference " ^ ref_)
  in
  resolve [] value

let is_json_media media =
  let media = String.lowercase_ascii (List.hd (String.split_on_char ';' media)) in
  media = "application/json" || String.ends_with ~suffix:"+json" media

let select_content content =
  match List.find_opt (fun (ct, _) -> is_json_media ct) content with
  | Some pair -> Some pair
  | None -> List.nth_opt content 0

let rec wire_type_shape = function
  | None -> Any_json
  | Some (Spec.Ref r) ->
      (match schema_name_from_ref r with Some name -> Reference name | None -> Any_json)
  | Some (Spec.Value (s : Spec.schema)) ->
      let base = match s.type_ with
        | Some "array" ->
            let item = Option.map (fun json ->
              match Jsont.Json.decode Spec.schema_or_ref_jsont json with
              | Ok s -> s | Error e -> invalid_arg e) s.items in
            Array (wire_type_shape item)
        | Some ("string" | "integer" | "number" | "boolean") ->
            let json = match Jsont.Json.encode Spec.schema_jsont s with
              | Ok json -> json | Error e -> invalid_arg e in
            Primitive (resolve_type_full json).resolved_type
        | _ -> Any_json in
      if Spec.schema_nullable s then Nullable base else base

let schema_string schema =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json (Schema.schema_json schema) with
  | Ok raw -> raw | Error e -> invalid_arg e

let wire_type schema = match schema with
  | None -> Any_json
  | Some s -> Checked (schema_string s, wire_type_shape schema)

let rec wire_reference = function
  | Reference name -> Some name
  | Array t | Nullable t | Checked (_,t) -> wire_reference t
  | Any_json | Primitive _ -> None

let analyze_operation ~(spec : Spec.t) ~(path_item_params : Spec.parameter Spec.or_ref list)
    ~path ~method_ (op : Spec.operation) : operation_info =
  let func_name = Name.operation_name ~method_ ~path ~operation_id:op.operation_id in
  let entries get = match spec.components with None -> [] | Some c -> get c in
  let resolve_parameter = resolve_component ~kind:"parameters" (entries (fun c -> c.Spec.parameters)) in
  let resolve_response = resolve_component ~kind:"responses" (entries (fun c -> c.Spec.responses)) in
  let resolve_body = resolve_component ~kind:"requestBodies" (entries (fun c -> c.Spec.request_bodies)) in
  let params = List.fold_left (fun acc p ->
    let p = resolve_parameter p in
    List.filter (fun (old : Spec.parameter) -> old.name <> p.name || old.in_ <> p.in_) acc @ [p]
  ) [] (path_item_params @ op.parameters) in
  let used = ref (StringSet.of_list ["client"; "body"; "__openapi_headers"; "__openapi_body";
    "__openapi_path"; "__openapi_query"; "__openapi_decode"]) in
  let named_params = List.map (fun (p : Spec.parameter) ->
    let rec fresh name = if StringSet.mem name !used then fresh (name ^ "_") else name in
    let name = fresh (Name.to_snake_case p.name) in
    used := StringSet.add name !used;
    p, (name, p.name, p.description, p.required)
  ) params in
  let at location = List.filter_map (fun ((p : Spec.parameter), info) ->
    if p.Spec.in_ = location then Some info else None) named_params in
  let placeholders = Openapi_runtime.Path.parameters path in
  let path_params = at Spec.Path |> List.filter (fun (_, name, _, _) -> List.mem name placeholders) in
  let query_params = at Spec.Query in
  let header_params = at Spec.Header |> List.filter (fun (_, name, _, _) ->
    not (List.mem (String.lowercase_ascii name) ["accept"; "content-type"; "authorization"])) in
  let request_body = Option.map resolve_body op.request_body in
  let request_encoding = Option.bind request_body (fun (rb : Spec.request_body) ->
    Option.map (fun (ct, (media : Spec.media_type)) ->
      if is_json_media ct then Json_body (ct, wire_type media.schema)
      else match String.lowercase_ascii ct with
        | "application/x-www-form-urlencoded" -> Form_body
        | "multipart/form-data" -> Multipart_body
        | _ -> Raw_body ct
    ) (select_content rb.content)) in
  let body_required = match request_body with Some rb -> rb.required | None -> false in
  let body_schema_ref = match request_encoding with
    | Some (Json_body (_, t)) -> wire_reference t | _ -> None in
  let responses = List.map (fun (code, response) -> code, resolve_response response)
    op.responses.responses in
  let successes = List.filter (fun (code, _) -> String.length code = 3 && code.[0] = '2') responses in
  let successes = match successes, op.responses.default with
    | [], Some response -> ["default", resolve_response response]
    | _ -> successes in
  let empty_statuses = List.filter_map (fun (code, (r : Spec.response)) ->
    if r.content = [] || method_ = "HEAD" || code = "204" || code = "205" then Some code else None) successes in
  let nonempty_statuses = List.filter_map (fun (code, _) ->
    if List.mem code empty_statuses then None else Some code) successes in
  let content = List.filter_map (fun (code, (r : Spec.response)) ->
    if List.mem code empty_statuses then None else select_content r.content) successes in
  let response_validation = List.filter_map (fun (code, (r : Spec.response)) ->
    Option.bind (select_content r.content) (fun (media, (m : Spec.media_type)) ->
      if is_json_media media then Option.map (fun schema -> code, schema_string schema) m.schema else None)
  ) successes in
  let response_encoding = match content with
    | [] -> Empty_response
    | (ct, media) :: rest when List.for_all (fun (ct, _) -> is_json_media ct) content ->
        let t = wire_type media.Spec.schema in
        let t = if List.for_all (fun (_, (m : Spec.media_type)) -> wire_type m.schema = t) rest
          then t else Any_json in
        Json_response (List.sort_uniq String.compare (List.map fst content), t)
    | _ -> Raw_response (List.sort_uniq String.compare (List.map fst content)) in
  let response_schema_ref = match response_encoding with
    | Json_response (_, t) -> wire_reference t | _ -> None in
  let errors = List.filter (fun (code, _) -> String.length code = 3 && code.[0] >= '3') responses in
  let errors = errors @ match op.responses.default with
    | None -> [] | Some r -> ["default", resolve_response r] in
  let error_responses = List.map (fun (code, (r : Spec.response)) ->
    let schema_ref = Option.bind (select_content r.content) (fun (ct, (m : Spec.media_type)) ->
      if is_json_media ct then match m.schema with
        | Some (Spec.Ref r) -> schema_name_from_ref r | _ -> None
      else None) in
    { status_code = code; schema_ref; error_description = r.description }) errors in
  { func_name; operation_id = op.operation_id; summary = op.summary;
    description = op.description; tags = op.tags; path; method_;
    path_params; query_params; header_params; request_encoding; body_required;
    response_encoding; response_validation; empty_statuses; nonempty_statuses; body_schema_ref;
    has_request_body = Option.is_some request_encoding;
    response_schema_ref; error_responses }

(** {1 Module Tree Building} *)

(** Extract prefix module dependencies from a schema's fields *)
let schema_prefix_deps (schema : schema_info) : StringSet.t =
  find_schema_dependencies schema.schema
  |> List.map (fun name -> Name.to_module_name (fst (Name.split_schema_name name)))
  |> StringSet.of_list

(** Extract prefix module dependencies from an operation's types *)
let operation_prefix_deps (op : operation_info) : StringSet.t =
  let body_dep = match op.body_schema_ref with
    | Some name ->
        let prefix, _ = Name.split_schema_name name in
        Some (Name.to_module_name prefix)
    | None -> None
  in
  let response_dep = match op.response_schema_ref with
    | Some name ->
        let prefix, _ = Name.split_schema_name name in
        Some (Name.to_module_name prefix)
    | None -> None
  in
  let error_deps = List.filter_map (fun (e : error_response) ->
    Option.map (fun name -> Name.to_module_name (fst (Name.split_schema_name name))) e.schema_ref
  ) op.error_responses in
  StringSet.of_list (List.filter_map Fun.id [body_dep; response_dep] @ error_deps)

let build_module_tree (schemas : schema_info list) (operations : operation_info list) : module_node * string list =
  let root = empty_node "Root" in

  (* Build set of known schema names for validation *)
  let known_schemas = StringSet.of_list (List.map (fun s -> s.original_name) schemas) in

  (* Add schemas to tree and track dependencies *)
  let root = List.fold_left (fun root schema ->
    let prefix_mod = Name.to_module_name schema.prefix in
    let child = match StringMap.find_opt prefix_mod root.children with
      | Some c -> c
      | None -> empty_node prefix_mod
    in
    let schema_deps = schema_prefix_deps schema in
    (* Remove self-dependency *)
    let schema_deps = StringSet.remove prefix_mod schema_deps in
    let child = { child with
      schemas = schema :: child.schemas;
      dependencies = StringSet.union child.dependencies schema_deps
    } in
    { root with children = StringMap.add prefix_mod child root.children }
  ) root schemas in

  (* Add operations to tree based on response type, and track operation dependencies.
     Only use response_schema_ref if the schema actually exists in components/schemas. *)
  let root = List.fold_left (fun root op ->
    (* Check if response schema actually exists *)
    let valid_response_ref = match op.response_schema_ref with
      | Some name when StringSet.mem name known_schemas -> Some name
      | _ -> None
    in
    match valid_response_ref with
    | Some ref_name ->
        let prefix, _ = Name.split_schema_name ref_name in
        let prefix_mod = Name.to_module_name prefix in
        let child = match StringMap.find_opt prefix_mod root.children with
          | Some c -> c
          | None -> empty_node prefix_mod
        in
        let op_deps = operation_prefix_deps op in
        (* Remove self-dependency *)
        let op_deps = StringSet.remove prefix_mod op_deps in
        let child = { child with
          operations = op :: child.operations;
          dependencies = StringSet.union child.dependencies op_deps
        } in
        { root with children = StringMap.add prefix_mod child root.children }
    | None ->
        (* Put in Client module for operations without valid typed response *)
        let child = match StringMap.find_opt "Client" root.children with
          | Some c -> c
          | None -> empty_node "Client"
        in
        let op_deps = operation_prefix_deps op in
        let op_deps = StringSet.remove "Client" op_deps in
        let child = { child with
          operations = op :: child.operations;
          dependencies = StringSet.union child.dependencies op_deps
        } in
        { root with children = StringMap.add "Client" child root.children }
  ) root operations in

  (* Get sorted list of module names (dependencies first) *)
  let module_names = StringMap.fold (fun name _ acc -> name :: acc) root.children [] in
  let deps_of name =
    match StringMap.find_opt name root.children with
    | Some node -> StringSet.elements node.dependencies
    | None -> []
  in
  let sorted = topological_sort module_names deps_of in

  (root, sorted)

(** {1 Code Generation} *)

let gen_enum_intf (schema : schema_info) : string =
  let doc = format_doc schema.description in
  if schema.enum_variants = [] then
    Printf.sprintf "%stype t = %s\n\nval jsont : t Jsont.t" doc schema.enum_base_type
  else
    let type_def = Printf.sprintf "%stype t = [\n%s\n]" doc
      (String.concat "\n" (List.map (fun (v, _) -> "  | `" ^ v) schema.enum_variants))
    in
    Printf.sprintf "%s\n\nval jsont : t Jsont.t" type_def

(** {2 Union Type Generation} *)

(** Format a union variant type reference for code generation *)
let format_union_type_ref ~context ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  if prefix_mod = current_prefix then
    Printf.sprintf "%s.t" suffix_mod
  else if is_forward_ref ~context prefix_mod then
    "Jsont.json"
  else
    Printf.sprintf "%s.%s.t" prefix_mod suffix_mod

(** Format a union variant jsont codec reference *)
let format_union_jsont_ref ~context ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  if prefix_mod <> current_prefix && is_forward_ref ~context prefix_mod then
    "Jsont.json"
  else if prefix_mod = current_prefix then
    Printf.sprintf "%s.jsont" suffix_mod
  else
    Printf.sprintf "%s.%s.jsont" prefix_mod suffix_mod

(** Discriminated unions use whole-value codecs, since schema guards are maps
    around those codecs and cannot be passed to [Jsont.Object.Case.map]. *)
let gen_union_jsont_discriminator ~context ~current_prefix (schema : schema_info) (union : union_info) (field : string) : string =
  let cases = List.map (fun (v : union_variant) ->
    let tag = match List.find_opt (fun (_, reference) ->
      reference = v.schema_ref || schema_name_from_ref reference = Some v.schema_ref
    ) union.discriminator_mapping with
    | Some (tag, _) -> tag | None -> v.schema_ref in
    tag, v.variant_name, format_union_jsont_ref ~context ~current_prefix v.schema_ref
  ) union.variants in
  let decoders = List.map (fun (tag, variant, codec) ->
    Printf.sprintf "      | %S -> (match Openapi.Runtime.Json.decode_json %s json with\n          | Ok v -> %s v | Error e -> Jsont.Error.msg (Jsont.Json.meta json) e)"
      tag codec variant) cases in
  let encoders = List.map (fun (tag, variant, codec) ->
    Printf.sprintf "      | %s v -> let json = Openapi.Runtime.Json.encode_json %s v in\n          if tag json <> %S then Jsont.Error.msg (Jsont.Json.meta json) \"Discriminator does not match union constructor\"; json"
      variant codec tag) cases in
  Printf.sprintf {|let jsont : t Jsont.t =
  let tag = function
    | Jsont.Object (fields, meta) ->
        (match Openapi.Spec.find_member %S fields with
         | Some (Jsont.String (tag, _)) -> tag
         | _ -> Jsont.Error.msg meta "Missing or non-string discriminator")
    | json -> Jsont.Error.msg (Jsont.Json.meta json) "Expected discriminated object"
  in
  Jsont.map Jsont.json ~kind:%S
    ~dec:(fun json -> match tag json with
%s
      | tag -> Jsont.Error.msgf (Jsont.Json.meta json) "Unknown discriminator: %%s" tag)
    ~enc:(function
%s)|} field schema.original_name (String.concat "\n" decoders) (String.concat "\n" encoders)

let gen_union_jsont_try_each ~context ~current_prefix (schema : schema_info) (union : union_info) : string =
  let decoders = List.map (fun (v : union_variant) ->
    Printf.sprintf "(fun json -> match Openapi.Runtime.Json.decode_json %s json with Ok v -> Some (%s v) | Error _ -> None)"
      (format_union_jsont_ref ~context ~current_prefix v.schema_ref) v.variant_name
  ) union.variants in

  let enc_cases = List.map (fun (v : union_variant) ->
    let codec_ref = format_union_jsont_ref ~context ~current_prefix v.schema_ref in
    Printf.sprintf "    | %s v -> Openapi.Runtime.Json.encode_json %s v"
      v.variant_name codec_ref
  ) union.variants in

  Printf.sprintf {|let jsont : t Jsont.t =
  let decode json = Openapi.Runtime.poly_union_decoder ~exclusive:%b [%s] json in
  Jsont.map Jsont.json ~kind:%S
    ~dec:decode
    ~enc:(function
%s)|}
    (union.style = `OneOf) (String.concat ";\n    " decoders)
    schema.original_name
    (String.concat "\n" enc_cases)

(** Generate interface code for a union type schema *)
let gen_union_intf ~context ~current_prefix (schema : schema_info) : string =
  match schema.union_info with
  | None -> failwith "gen_union_intf called on non-union schema"
  | Some union ->
      let doc = format_doc schema.description in
      let type_def = Printf.sprintf "%stype t =\n%s" doc
        (String.concat "\n" (List.map (fun (v : union_variant) ->
          Printf.sprintf "  | %s of %s" v.variant_name
            (format_union_type_ref ~context ~current_prefix v.schema_ref)
        ) union.variants))
      in
      Printf.sprintf "%s\n\nval jsont : t Jsont.t" type_def

(** Localize an OCaml type string by stripping the current_prefix and current_suffix modules.
    When generating code inside a submodule, self-references need to be unqualified. *)
let localize_type ~current_prefix ~current_suffix (type_str : string) : string =
  (* Handle patterns like "User.ResponseDto.t" -> "ResponseDto.t" if current_prefix = "User"
     And further "ResponseDto.t" -> "t" if current_suffix = "ResponseDto" *)
  let prefix_dot = current_prefix ^ "." in
  let suffix_dot = current_suffix ^ "." in
  let full_path = current_prefix ^ "." ^ current_suffix ^ "." in
  let strip_prefix s =
    (* First try to strip full path (Prefix.Suffix.) *)
    if String.length s >= String.length full_path &&
       String.sub s 0 (String.length full_path) = full_path then
      String.sub s (String.length full_path) (String.length s - String.length full_path)
    (* Then try just prefix *)
    else if String.length s >= String.length prefix_dot &&
       String.sub s 0 (String.length prefix_dot) = prefix_dot then
      let rest = String.sub s (String.length prefix_dot) (String.length s - String.length prefix_dot) in
      (* If the rest starts with our suffix, strip that too *)
      if String.length rest >= String.length suffix_dot &&
         String.sub rest 0 (String.length suffix_dot) = suffix_dot then
        String.sub rest (String.length suffix_dot) (String.length rest - String.length suffix_dot)
      else rest
    else s
  in
  (* Handle "X list", "X option", and nested combinations *)
  let rec localize s =
    if String.ends_with ~suffix:" list" s then
      let elem = String.sub s 0 (String.length s - 5) in
      (localize elem) ^ " list"
    else if String.ends_with ~suffix:" option" s then
      let elem = String.sub s 0 (String.length s - 7) in
      (localize elem) ^ " option"
    else
      strip_prefix s
  in
  localize type_str

(** Localize a jsont codec string by stripping the current_prefix and current_suffix modules *)
let rec localize_jsont ~current_prefix ~current_suffix (jsont_str : string) : string =
  let prefix_dot = current_prefix ^ "." in
  let suffix_dot = current_suffix ^ "." in
  let full_path = current_prefix ^ "." ^ current_suffix ^ "." in
  let strip_prefix s =
    (* First try to strip full path (Prefix.Suffix.) *)
    if String.length s >= String.length full_path &&
       String.sub s 0 (String.length full_path) = full_path then
      String.sub s (String.length full_path) (String.length s - String.length full_path)
    (* Then try just prefix *)
    else if String.length s >= String.length prefix_dot &&
       String.sub s 0 (String.length prefix_dot) = prefix_dot then
      let rest = String.sub s (String.length prefix_dot) (String.length s - String.length prefix_dot) in
      (* If the rest starts with our suffix, strip that too *)
      if String.length rest >= String.length suffix_dot &&
         String.sub rest 0 (String.length suffix_dot) = suffix_dot then
        String.sub rest (String.length suffix_dot) (String.length rest - String.length suffix_dot)
      else rest
    else s
  in
  (* Handle patterns like "User.ResponseDto.jsont" -> "ResponseDto.jsont" -> "jsont"
     Also handle "(Jsont.list User.ResponseDto.jsont)" *)
  if String.length jsont_str > 12 && String.sub jsont_str 0 12 = "(Jsont.list " then
    let inner = String.sub jsont_str 12 (String.length jsont_str - 13) in
    "(Jsont.list " ^ localize_jsont ~current_prefix ~current_suffix inner ^ ")"
  else if String.starts_with ~prefix:"(Jsont.option " jsont_str then
    let inner = String.sub jsont_str 14 (String.length jsont_str - 15) in
    "(Jsont.option " ^ localize_jsont ~current_prefix ~current_suffix inner ^ ")"
  else
    strip_prefix jsont_str

let rec map_type_leaf f s =
  if String.ends_with ~suffix:" option" s then map_type_leaf f (String.sub s 0 (String.length s - 7)) ^ " option"
  else if String.ends_with ~suffix:" list" s then map_type_leaf f (String.sub s 0 (String.length s - 5)) ^ " list"
  else f s

let schema_local_type ~context ~current_prefix ~current_suffix schema s =
  map_type_leaf (fun leaf ->
    let localized = localize_type ~current_prefix ~current_suffix leaf in
    let forward = match String.split_on_char '.' leaf with
      | prefix :: _ -> prefix <> current_prefix && is_forward_ref ~context prefix | _ -> false in
    if (schema.is_recursive && localized = "t") || forward then "Jsont.json" else localized) s

let rec schema_local_codec ~context ~current_prefix ~current_suffix schema s =
  let wrap prefix =
    let n = String.length prefix in
    let inner = String.sub s n (String.length s - n - 1) in
    prefix ^ schema_local_codec ~context ~current_prefix ~current_suffix schema inner ^ ")" in
  if String.starts_with ~prefix:"(Jsont.list " s then wrap "(Jsont.list "
  else if String.starts_with ~prefix:"(Jsont.option " s then wrap "(Jsont.option "
  else
    let localized = localize_jsont ~current_prefix ~current_suffix s in
    let forward = match String.split_on_char '.' s with
      | prefix :: _ -> prefix <> current_prefix && is_forward_ref ~context prefix | _ -> false in
    if (schema.is_recursive && localized = "jsont") || forward then "Jsont.json" else localized

let scalar_type schema =
  if schema.is_opaque then "Jsont.json" else
  let resolved = resolve_type_full (Schema.json Spec.schema_jsont schema.schema) in
  resolved.resolved_type ^ (if resolved.resolved_nullable then " option" else "")

let gen_record_intf ~context ~current_prefix ~current_suffix (schema : schema_info) : string =
  (* For recursive schemas, self-referential fields need to use Jsont.json
     to avoid OCaml's let rec restrictions on non-functional values.
     Also handle forward references to modules that come later in the sort order. *)
  let loc_type = schema_local_type ~context ~current_prefix ~current_suffix schema in
  let doc = format_doc schema.description in
  if schema.fields = [] then
    (* Expose that the type is Jsont.json for opaque types - allows users to pattern match *)
    let t = loc_type (scalar_type schema) in
    let v = if t = "Jsont.json" then "val v : unit -> t" else "val v : t -> t" in
    Printf.sprintf "%stype t = %s\n\nval jsont : t Jsont.t\n\n%s" doc t v
  else
    (* Abstract type *)
    let type_decl = Printf.sprintf "%stype t" doc in

    (* Constructor signature
       - Required fields (no default, not optional): field:type
       - Fields with defaults: ?field:type (optional parameter)
       - Optional fields (no default, is_optional): ?field:type *)
    let required_fields = List.filter (fun (f : field_info) ->
      not f.is_optional && Option.is_none f.default_value
    ) schema.fields in
    let default_fields = List.filter (fun (f : field_info) ->
      Option.is_some f.default_value
    ) schema.fields in
    let optional_fields = List.filter (fun (f : field_info) ->
      f.is_optional && Option.is_none f.default_value
    ) schema.fields in
    let v_param_docs = String.concat ""
      ((List.map (fun (f : field_info) -> format_param_doc f.ocaml_name f.description) required_fields) @
       (List.map (fun (f : field_info) -> format_param_doc f.ocaml_name f.description) default_fields) @
       (List.map (fun (f : field_info) -> format_param_doc f.ocaml_name f.description) optional_fields))
    in
    let v_params =
      (List.map (fun (f : field_info) -> Printf.sprintf "%s:%s" f.ocaml_name (loc_type f.base_type)) required_fields) @
      (List.map (fun (f : field_info) -> Printf.sprintf "?%s:%s" f.ocaml_name (loc_type f.ocaml_type)) default_fields) @
      (List.map (fun (f : field_info) -> Printf.sprintf "?%s:%s" f.ocaml_name
        (loc_type (f.base_type ^ (if f.is_nullable && not f.is_required then " option" else "")))) optional_fields) @
      ["unit"; "t"]
    in
    let v_doc = if v_param_docs = "" then "(** Construct a value *)\n"
      else Printf.sprintf "(** Construct a value\n%s*)\n" v_param_docs in
    let v_sig = Printf.sprintf "%sval v : %s" v_doc (String.concat " -> " v_params) in

    (* Accessor signatures *)
    let accessor_sigs = String.concat "\n\n" (List.map (fun (f : field_info) ->
      let acc_doc = match f.description with
        | Some d -> Printf.sprintf "(** %s *)\n" (escape_doc d)
        | None -> ""
      in
      Printf.sprintf "%sval %s : t -> %s" acc_doc f.ocaml_name (loc_type f.ocaml_type)
    ) schema.fields) in

    Printf.sprintf "%s\n\n%s\n\n%s\n\nval jsont : t Jsont.t"
      type_decl v_sig accessor_sigs

(** Format a jsont codec reference, stripping the current_prefix if present.
    Returns Jsont.json for forward references to avoid unbound module errors. *)
let format_jsont_ref ~context ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  (* Check if this is a forward reference to a module that hasn't been defined yet *)
  if prefix_mod <> current_prefix && is_forward_ref ~context prefix_mod then
    "Jsont.json"
  else if prefix_mod = current_prefix then
    Printf.sprintf "%s.jsont" suffix_mod
  else
    Printf.sprintf "%s.%s.jsont" prefix_mod suffix_mod

(** Check if a schema exists - used to validate refs before generating code *)
(** Format a type reference, stripping the current_prefix if present *)
let format_type_ref ~context ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  if prefix_mod = current_prefix then
    (* Local reference - use unqualified name *)
    Printf.sprintf "%s.t" suffix_mod
  else if is_forward_ref ~context prefix_mod then
    (* Forward reference to module not yet defined - use Jsont.json *)
    "Jsont.json"
  else
    Printf.sprintf "%s.%s.t" prefix_mod suffix_mod

let rec wire_codec ~context ~current_prefix = function
  | Checked (schema, t) -> Printf.sprintf "(Openapi.Schema.guard_string __openapi_schemas %S %s)" schema (wire_codec ~context ~current_prefix t)
  | Any_json -> "Jsont.json"
  | Primitive t -> jsont_of_base_type t
  | Reference name -> if StringSet.mem name context.known_schemas then format_jsont_ref ~context ~current_prefix name else "Jsont.json"
  | Array t -> Printf.sprintf "(Jsont.list %s)" (wire_codec ~context ~current_prefix t)
  | Nullable t -> Printf.sprintf "(Jsont.option %s)" (wire_codec ~context ~current_prefix t)

let rec wire_ocaml_type ~context ~current_prefix = function
  | Checked (_,t) -> wire_ocaml_type ~context ~current_prefix t
  | Any_json -> "Jsont.json"
  | Primitive t -> t
  | Reference name -> if StringSet.mem name context.known_schemas then format_type_ref ~context ~current_prefix name else "Jsont.json"
  | Array t -> wire_ocaml_type ~context ~current_prefix t ^ " list"
  | Nullable t -> wire_ocaml_type ~context ~current_prefix t ^ " option"

let operation_doc (op : operation_info) =
  let doc = format_doc_block ~summary:op.summary ?description:op.description () in
  let params = String.concat "" (List.map (fun (n, _, d, _) -> format_param_doc n d)
    (op.path_params @ op.query_params @ op.header_params)) in
  if params = "" then doc
  else if doc = "" then Printf.sprintf "(**\n%s*)\n" params
  else String.sub doc 0 (String.length doc - 3) ^ "\n" ^ params ^ "*)\n"

let gen_operation_impl ~context ~current_prefix (op : operation_info) : string =
  let path_args = List.map (fun (n, _, _, _) -> "~" ^ n) op.path_params in
  let other_args = List.map (fun (n, _, _, req) -> (if req then "~" else "?") ^ n)
    (op.query_params @ op.header_params) in
  let body_args = match op.request_encoding with
    | None -> [] | Some _ -> [if op.body_required then "~body" else "?body"] in
  let path_render = Printf.sprintf "Openapi.Runtime.Path.render ~params:[%s] %S"
    (String.concat "; " (List.map (fun (n, json, _, _) -> Printf.sprintf "(%S, %s)" json n) op.path_params)) op.path in
  let query = Printf.sprintf "Openapi.Runtime.Query.encode (Stdlib.List.concat [%s])"
    (String.concat "; " (List.map (fun (n, json, _, req) ->
      Printf.sprintf "Openapi.Runtime.Query.%s ~key:%S ~value:%s"
        (if req then "singleton" else "optional") json n) op.query_params)) in
  let request_setup = match op.request_encoding with
    | None -> "let __openapi_headers = Fetch.Header.[] in\n  let __openapi_body = None in"
    | Some encoding ->
        let encode = match encoding with
          | Json_body (media, t) -> Printf.sprintf "Fetch.encode (Fetch.Json.v ~media:%S %s) body" media (wire_codec ~context ~current_prefix t)
          | Form_body -> "Fetch.Form.urlencoded body"
          | Multipart_body -> "Fetch.Form.multipart body"
          | Raw_body media -> Printf.sprintf "Fetch.Header.[ content_type, media %S ], body" media in
        if op.body_required then
          Printf.sprintf "let __openapi_headers, __openapi_body = %s in\n  let __openapi_body = Some __openapi_body in" encode
        else Printf.sprintf "let __openapi_headers, __openapi_body = match body with\n    | None -> Fetch.Header.[], None\n    | Some body -> let headers, body = %s in headers, Some body\n  in" encode in
  let header_setup = String.concat "\n  " (List.map (fun (n, json, _, req) ->
    if req then Printf.sprintf "let __openapi_headers = let cell = Fetch.Header.raw %S %s in Fetch.Header.(cell :: __openapi_headers) in" json n
    else Printf.sprintf "let __openapi_headers = match %s with None -> __openapi_headers | Some value -> let cell = Fetch.Header.raw %S value in Fetch.Header.(cell :: __openapi_headers) in" n json
  ) op.header_params) in
  let accept, decode = match op.response_encoding with
    | Empty_response -> [], "()"
    | Json_response (medias, t) ->
        let guards = String.concat "; " (List.map (fun (code, schema) -> Printf.sprintf "(%S, %S)" code schema) op.response_validation) in
        let codec = Printf.sprintf "(Openapi.Schema.guard_response __openapi_schemas [%s] (Fetch.status response) %s)" guards (wire_codec ~context ~current_prefix t) in
        medias, Printf.sprintf "Fetch.decode ~limit (Fetch.Json.v ~media:%S ~accept:%s %s) response"
          (List.hd medias) ("[" ^ String.concat "; " (List.map (Printf.sprintf "%S") medias) ^ "]") codec
    | Raw_response medias ->
        medias, Printf.sprintf "Fetch.decode ~limit (Fetch.Media.of_strings ~accept:[%s] \"application/octet-stream\" ~encode:(fun s -> s) ~decode:(fun s -> Ok s)) response"
          (String.concat "; " (List.map (Printf.sprintf "%S") medias)) in
  let accept_setup = if accept = [] then "" else
    Printf.sprintf "let __openapi_headers = Fetch.Header.((accept, [%s]) :: __openapi_headers) in"
      (String.concat "; " (List.map (fun s -> Printf.sprintf "pref %S" s) accept)) in
  let decode = match op.response_encoding, op.empty_statuses with
    | Empty_response, _ -> "let __openapi_decode ~limit:_ _response = () in"
    | _, [] -> Printf.sprintf "let __openapi_decode ~limit response = %s in" decode
    | _, statuses ->
        let conditions = List.map (fun s ->
          if s = "2XX" || s = "default" then
            let concrete = List.filter (fun s -> s <> "2XX" && s <> "default") op.nonempty_statuses in
            if concrete = [] then "true" else "(" ^ String.concat " && " (List.map (fun s -> "status <> " ^ s) concrete) ^ ")"
          else "status = " ^ s) statuses in
        Printf.sprintf "let __openapi_decode ~limit response =\n    let status = Fetch.status response in\n    if %s then None else Some (%s)\n  in" (String.concat " || " conditions) decode in
  let errors = List.map (fun (e : error_response) ->
    let parser = match e.schema_ref with
      | Some name when StringSet.mem name context.known_schemas -> Printf.sprintf "Openapi.Runtime.Client.typed_error %S %s" name (format_jsont_ref ~context ~current_prefix name)
      | _ -> "(fun _ -> None)" in
    Printf.sprintf "(%S, %s)" e.status_code parser
  ) op.error_responses in
  Printf.sprintf {|%slet %s %s =
  let __openapi_path = %s in
  let __openapi_query = %s in
  %s
  %s
  %s
  %s
  Openapi.Runtime.Client.call ~headers:__openapi_headers ?body:__openapi_body ~errors:[%s]
    ~operation:%S ~path:__openapi_path ~query:__openapi_query ~decode:__openapi_decode client `%s|}
    (operation_doc op) op.func_name (String.concat " " (path_args @ other_args @ body_args @ ["client"; "()"]))
    path_render query request_setup header_setup accept_setup decode (String.concat "; " errors) op.func_name op.method_

let gen_operation_intf ~context ~current_prefix (op : operation_info) : string =
  let path_args = List.map (fun (n, _, _, _) -> n ^ ":string") op.path_params in
  let other_args = List.map (fun (n, _, _, req) -> (if req then "" else "?") ^ n ^ ":string")
    (op.query_params @ op.header_params) in
  let body_args = match op.request_encoding with
    | None -> []
    | Some encoding ->
        let t = match encoding with
          | Json_body (_, t) -> wire_ocaml_type ~context ~current_prefix t
          | Form_body -> "(string * string) list"
          | Multipart_body -> "Fetch.Form.part list"
          | Raw_body _ -> "Fetch.body" in
        [(if op.body_required then "body:" else "?body:") ^ t] in
  let response_type = match op.response_encoding with
    | Empty_response -> "unit"
    | Json_response (_, t) -> wire_ocaml_type ~context ~current_prefix t
    | Raw_response _ -> "string" in
  let response_type = if op.response_encoding <> Empty_response && op.empty_statuses <> []
    then response_type ^ " option" else response_type in
  Printf.sprintf "%sval %s : %s" (operation_doc op) op.func_name
    (String.concat " -> " (path_args @ other_args @ body_args @ ["t"; "unit"; response_type]))

(** {1 Two-Phase Module Generation}

    To solve the module ordering problem for union types that reference multiple
    schemas, we use a two-phase generation approach within each prefix module:

    Phase 1 - Types module: Generate all type definitions first, ordered only by
    TYPE dependencies (A.t contains B.t). No codec dependencies matter here.

    Phase 2 - Full modules: Generate full modules with [include Types.X] plus
    codecs. These are ordered by CODEC dependencies (A.jsont uses B.jsont).
    Since all types exist in the Types module, any type can be referenced.
    Since codecs are ordered by their own dependencies, any needed codec
    exists when referenced.

    This allows union types to reference multiple sibling schemas' codecs
    without forward reference issues. *)

(** {2 Phase 1: Type-Only Generation} *)

(** Generate type-only content for an enum schema (for Types module) *)
let gen_enum_type_only (schema : schema_info) : string =
  let doc = format_doc schema.description in
  if schema.enum_variants = [] then
    Printf.sprintf "%stype t = %s" doc schema.enum_base_type
  else
    Printf.sprintf "%stype t = [\n%s\n]" doc
      (String.concat "\n" (List.map (fun (v, _) -> "  | `" ^ v) schema.enum_variants))

(** Generate type-only content for a union schema (for Types module).
    Type references use Types.Sibling.t format within the Types module. *)
let gen_union_type_only ~context ~current_prefix (schema : schema_info) : string =
  match schema.union_info with
  | None -> failwith "gen_union_type_only called on non-union schema"
  | Some union ->
      let doc = format_doc schema.description in
      (* In Types module, reference siblings as Sibling.t (same namespace) *)
      let format_type_in_types (schema_ref : string) : string =
        let prefix, suffix = Name.split_schema_name schema_ref in
        let prefix_mod = Name.to_module_name prefix in
        let suffix_mod = Name.to_module_name suffix in
        if prefix_mod = current_prefix then
          Printf.sprintf "%s.t" suffix_mod
        else if is_forward_ref ~context prefix_mod then
          "Jsont.json"  (* Cross-prefix forward ref *)
        else
          Printf.sprintf "%s.%s.t" prefix_mod suffix_mod
      in
      Printf.sprintf "%stype t =\n%s" doc
        (String.concat "\n" (List.map (fun (v : union_variant) ->
          Printf.sprintf "  | %s of %s" v.variant_name (format_type_in_types v.schema_ref)
        ) union.variants))

(** Generate type-only content for a record schema (for Types module) *)
let gen_record_type_only ~context ~current_prefix ~current_suffix (schema : schema_info) : string =
  let loc_type = schema_local_type ~context ~current_prefix ~current_suffix schema in
  let doc = format_doc schema.description in
  if schema.fields = [] then
    Printf.sprintf "%stype t = %s" doc (loc_type (scalar_type schema))
  else
    let type_fields = String.concat "\n" (List.map (fun (f : field_info) ->
      let field_doc = match f.description with
        | Some d -> Printf.sprintf "  (** %s *)" (escape_doc d)
        | None -> ""
      in
      Printf.sprintf "  %s : %s;%s" f.ocaml_name (loc_type f.ocaml_type) field_doc
    ) schema.fields) in
    Printf.sprintf "%stype t = {\n%s\n}" doc type_fields

(** Generate a type-only submodule for the Types module *)
let gen_type_only_submodule ~context ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_type_only ~context ~current_prefix schema
    else if schema.is_enum then gen_enum_type_only schema
    else gen_record_type_only ~context ~current_prefix ~current_suffix:suffix_mod schema
  in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "    " ^ l) |> String.concat "\n" in
  Printf.sprintf "  module %s = struct\n%s\n  end" suffix_mod indented

(** {2 Phase 2: Codec-Only Generation (with include Types.X)} *)

(** Generate codec content for an enum schema (includes Types.X) *)
let gen_enum_codec_only (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let jsont_base = jsont_of_base_type schema.enum_base_type in
  if schema.enum_variants = [] then
    Printf.sprintf "include Types.%s\nlet jsont = %s" suffix_mod jsont_base
  else
    let dec_cases = String.concat "\n" (List.map (fun (v, raw) ->
      Printf.sprintf "      | %S -> `%s" raw v
    ) schema.enum_variants) in
    let enc_cases = String.concat "\n" (List.map (fun (v, raw) ->
      Printf.sprintf "      | `%s -> %S" v raw
    ) schema.enum_variants) in
    Printf.sprintf {|include Types.%s

let jsont : t Jsont.t =
  Jsont.map Jsont.string ~kind:%S
    ~dec:(function
%s
      | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %%s" s)
    ~enc:(function
%s)|} suffix_mod schema.original_name dec_cases enc_cases

(** Generate codec content for a union schema (includes Types.X) *)
let gen_union_codec_only ~context ~current_prefix (schema : schema_info) : string =
  match schema.union_info with
  | None -> failwith "gen_union_codec_only called on non-union schema"
  | Some union ->
      let suffix_mod = Name.to_module_name schema.suffix in
      (* Jsont codec - discriminator-based or try-each *)
      let jsont_code = match union.discriminator_field with
        | Some field -> gen_union_jsont_discriminator ~context ~current_prefix schema union field
        | None -> gen_union_jsont_try_each ~context ~current_prefix schema union
      in
      Printf.sprintf "include Types.%s\n\n%s" suffix_mod jsont_code

(** Generate codec content for a record schema (includes Types.X) *)
let gen_record_codec_only ~context ~current_prefix ~current_suffix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  (* Note: loc_type is not needed here since types come from Types.X via include *)
  let loc_jsont = schema_local_codec ~context ~current_prefix ~current_suffix schema in
  if schema.fields = [] then
    let t = scalar_type schema in
    let codec = loc_jsont (jsont_of_base_type t) in
    let v = if t = "Jsont.json" then "let v () = Jsont.Object ([], Jsont.Meta.none)"
      else "let v value = value" in
    Printf.sprintf "include Types.%s\nlet jsont = %s\n%s" suffix_mod codec v
  else
    (* Constructor function v
       - Required fields (no default, not optional): ~field
       - Fields with defaults: ?(field=default)
       - Optional fields (no default, is_optional): ?field *)
    let required_fields = List.filter (fun (f : field_info) ->
      not f.is_optional && Option.is_none f.default_value
    ) schema.fields in
    let default_fields = List.filter (fun (f : field_info) ->
      Option.is_some f.default_value
    ) schema.fields in
    let optional_fields = List.filter (fun (f : field_info) ->
      f.is_optional && Option.is_none f.default_value
    ) schema.fields in
    let v_params =
      (List.map (fun (f : field_info) -> Printf.sprintf "~%s" f.ocaml_name) required_fields) @
      (List.map (fun (f : field_info) ->
        let value = Option.get f.default_value in
        let value = if f.is_nullable && value <> "None" then "Some (" ^ value ^ ")" else value in
        Printf.sprintf "?(%s=%s)" f.ocaml_name value
      ) default_fields) @
      (List.map (fun (f : field_info) -> Printf.sprintf "?%s" f.ocaml_name) optional_fields) @
      ["()"]
    in
    let v_body = String.concat "; " (List.map (fun (f : field_info) -> f.ocaml_name) schema.fields) in
    let v_func = Printf.sprintf "let v %s = { %s }" (String.concat " " v_params) v_body in

    (* Accessor functions *)
    let accessors = String.concat "\n" (List.map (fun (f : field_info) ->
      Printf.sprintf "let %s t = t.%s" f.ocaml_name f.ocaml_name
    ) schema.fields) in

    (* Jsont codec *)
    let make_params = String.concat " " (List.map (fun (f : field_info) -> f.ocaml_name) schema.fields) in
    let jsont_members = String.concat "\n" (List.map (fun (f : field_info) ->
      let base_codec =
        match f.field_union with
        | Some union -> jsont_of_field_union ~current_prefix union
        | None ->
            let raw_codec = jsont_of_base_type f.base_type in
            let localized = loc_jsont raw_codec in
            if has_constraints f.constraints then validated_jsont f.constraints localized f.base_type
            else localized
      in
      if f.is_nullable then
        let nullable_codec = Printf.sprintf "(Jsont.option %s)" base_codec in
        if f.is_required then
          Printf.sprintf "  |> Jsont.Object.mem %S %s ~enc:(fun r -> r.%s)"
            f.json_name nullable_codec f.ocaml_name
        else
          Printf.sprintf "  |> Jsont.Object.opt_mem %S %s ~enc:(fun r -> r.%s)"
            f.json_name nullable_codec f.ocaml_name
      else if f.is_optional then
        (* Optional non-nullable field without default - use opt_mem *)
        Printf.sprintf "  |> Jsont.Object.opt_mem %S %s ~enc:(fun r -> r.%s)"
          f.json_name base_codec f.ocaml_name
      else
        (* Required or has default - use mem, possibly with dec_absent *)
        (match f.default_value with
        | Some def ->
            Printf.sprintf "  |> Jsont.Object.mem %S %s ~dec_absent:(fun () -> %s) ~enc:(fun r -> r.%s)"
              f.json_name base_codec def f.ocaml_name
        | None ->
            Printf.sprintf "  |> Jsont.Object.mem %S %s ~enc:(fun r -> r.%s)"
              f.json_name base_codec f.ocaml_name)
    ) schema.fields) in

    Printf.sprintf {|include Types.%s

%s

%s

let jsont : t Jsont.t =
  Jsont.Object.map ~kind:%S
    (fun %s -> { %s })
%s
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish|}
      suffix_mod v_func accessors schema.original_name make_params v_body jsont_members

(** Generate a codec-only submodule (uses include Types.X) *)
let gen_codec_only_submodule ~context ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_codec_only ~context ~current_prefix schema
    else if schema.is_enum then gen_enum_codec_only schema
    else gen_record_codec_only ~context ~current_prefix ~current_suffix:suffix_mod schema
  in
  let content = content ^ Printf.sprintf "\n\nlet jsont = Openapi.Schema.guard_ref __openapi_schemas %S jsont" schema.original_name in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
  Printf.sprintf "module %s = struct\n%s\nend" suffix_mod indented

(** {2 Codec Dependency Extraction}

    For the two-phase approach, we need to order codecs by their codec dependencies
    (which codecs reference other codecs), separate from type dependencies. *)

(** Extract codec dependencies for a schema - which sibling codecs does this schema's codec reference? *)
let schema_codec_deps ~current_prefix (schema : schema_info) : string list =
  (* For union types, the codec references all variant codecs *)
  let union_deps = match schema.union_info with
    | None -> []
    | Some union ->
        List.filter_map (fun (v : union_variant) ->
          let prefix, suffix = Name.split_schema_name v.schema_ref in
          let prefix_mod = Name.to_module_name prefix in
          if prefix_mod = current_prefix then
            Some (Name.to_module_name suffix)
          else None
        ) union.variants
  in
  (* For records, codecs reference field type codecs *)
  let field_deps = List.filter_map (fun (f : field_info) ->
    if String.contains f.base_type '.' then
      match String.split_on_char '.' f.base_type with
      | prefix :: suffix :: _ when prefix = current_prefix ->
          Some (Name.to_module_name suffix)
      | _ -> None
    else None
  ) schema.fields in
  let schema_deps = find_schema_dependencies schema.schema |> List.filter_map (fun name ->
    let prefix, suffix = Name.split_schema_name name in
    if Name.to_module_name prefix = current_prefix && suffix <> schema.suffix then
      Some (Name.to_module_name suffix) else None) in
  union_deps @ field_deps @ schema_deps |> List.sort_uniq String.compare

(** {1 Full Module Generation} *)

let gen_submodule_intf ~context ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_intf ~context ~current_prefix schema
    else if schema.is_enum then gen_enum_intf schema
    else gen_record_intf ~context ~current_prefix ~current_suffix:suffix_mod schema in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
  Printf.sprintf "module %s : sig\n%s\nend" suffix_mod indented

(** Extract suffix module dependencies within the same prefix *)
let schema_suffix_deps ~current_prefix (schema : schema_info) : string list =
  find_schema_dependencies schema.schema |> List.filter_map (fun name ->
    let prefix, suffix = Name.split_schema_name name in
    if Name.to_module_name prefix = current_prefix && suffix <> schema.suffix then
      Some (Name.to_module_name suffix) else None)

(** Sort schemas within a prefix module by their TYPE dependencies.
    Used for ordering types in the Types module. *)
let sort_schemas_by_type_deps ~current_prefix (schemas : schema_info list) : schema_info list =
  let suffix_of schema = Name.to_module_name schema.suffix in
  let suffix_names = List.map suffix_of schemas in
  let deps_of suffix =
    match List.find_opt (fun s -> suffix_of s = suffix) schemas with
    | Some schema -> schema_suffix_deps ~current_prefix schema |> List.filter (fun d -> List.mem d suffix_names)
    | None -> []
  in
  let sorted = topological_sort suffix_names deps_of in
  List.filter_map (fun suffix ->
    List.find_opt (fun s -> suffix_of s = suffix) schemas
  ) sorted

(** Sort schemas within a prefix module by their CODEC dependencies.
    Used for ordering full modules with codecs. *)
let sort_schemas_by_codec_deps ~current_prefix (schemas : schema_info list) : schema_info list =
  let suffix_of schema = Name.to_module_name schema.suffix in
  let suffix_names = List.map suffix_of schemas in
  let deps_of suffix =
    match List.find_opt (fun s -> suffix_of s = suffix) schemas with
    | Some schema -> schema_codec_deps ~current_prefix schema |> List.filter (fun d -> List.mem d suffix_names)
    | None -> []
  in
  let sorted = topological_sort suffix_names deps_of in
  List.filter_map (fun suffix ->
    List.find_opt (fun s -> suffix_of s = suffix) schemas
  ) sorted

(** Generate a prefix module using two-phase generation:
    Phase 1: Types module with all type definitions
    Phase 2: Full modules with include Types.X + codecs *)
let gen_prefix_module_impl ~context (node : module_node) : string =
  if node.schemas = [] then
    (* No schemas - just generate operations *)
    let op_impls = List.map (gen_operation_impl ~context ~current_prefix:node.name) (List.rev node.operations) in
    if op_impls = [] then
      Printf.sprintf "module %s = struct\nend" node.name
    else
      let content = String.concat "\n\n" op_impls in
      let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
      Printf.sprintf "module %s = struct\n%s\nend" node.name indented
  else
    (* Phase 1: Generate Types module with all type definitions *)
    let type_sorted_schemas = sort_schemas_by_type_deps ~current_prefix:node.name node.schemas in
    let type_mods = List.map (gen_type_only_submodule ~context ~current_prefix:node.name) type_sorted_schemas in
    let types_content = String.concat "\n\n" type_mods in
    let types_module = Printf.sprintf "module Types = struct\n%s\nend" types_content in

    (* Phase 2: Generate full modules with codecs, sorted by codec dependencies *)
    let codec_sorted_schemas = sort_schemas_by_codec_deps ~current_prefix:node.name node.schemas in
    let codec_mods = List.map (gen_codec_only_submodule ~context ~current_prefix:node.name) codec_sorted_schemas in

    (* Operations *)
    let op_impls = List.map (gen_operation_impl ~context ~current_prefix:node.name) (List.rev node.operations) in

    let content = String.concat "\n\n" ([types_module] @ codec_mods @ op_impls) in
    let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
    Printf.sprintf "module %s = struct\n%s\nend" node.name indented

let gen_prefix_module_intf ~context (node : module_node) : string =
  (* For interfaces, we don't need the two-phase approach.
     Just sort by type dependencies and generate full interfaces. *)
  let sorted_schemas = sort_schemas_by_type_deps ~current_prefix:node.name node.schemas in
  let schema_mods = List.map (gen_submodule_intf ~context ~current_prefix:node.name) sorted_schemas in
  let op_intfs = List.map (gen_operation_intf ~context ~current_prefix:node.name) (List.rev node.operations) in
  let content = String.concat "\n\n" (schema_mods @ op_intfs) in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
  Printf.sprintf "module %s : sig\n%s\nend" node.name indented

(** {1 Top-Level Generation} *)

type config = {
  output_dir : string;
  package_name : string;
  spec_path : string option;
}

let schema_definitions spec = match spec.Spec.components with
  | None -> []
  | Some c -> List.map (fun (name, schema) -> name, Schema.schema_json schema) c.schemas

let schema_context spec = Schema.create ~version:spec.Spec.openapi (schema_definitions spec)

let schema_context_source spec =
  let raw = Jsont.Object (List.map (fun (n,v) -> (n,Jsont.Meta.none),v) (schema_definitions spec), Jsont.Meta.none) in
  let raw = match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json raw with
    | Ok raw -> raw | Error error -> invalid_arg error in
  Printf.sprintf "let __openapi_schemas = Openapi.Schema.of_string ~version:%S %S\n\n" spec.Spec.openapi raw

let generate_ml (spec : Spec.t) (package_name : string) : string =
  let api_desc = Option.value ~default:"Generated API client." spec.info.description in

  (* Collect schemas *)
  let schemas = match spec.components with
    | None -> []
    | Some c -> List.filter_map (fun (name, sor) ->
        match sor with
        | Spec.Ref r -> Option.map (analyze_schema ~components:spec.components name)
            (resolve_schema_ref ~components:spec.components r)
        | Spec.Value s -> Some (analyze_schema ~components:spec.components name s)
      ) c.schemas
  in

  (* Set known schemas for validation during code generation *)
  let context = { known_schemas = StringSet.of_list (List.map (fun s -> s.original_name) schemas);
    forward_refs = StringSet.empty } in

  (* Collect operations *)
  let operations = List.concat_map (fun (path, (pi : Spec.path_item)) ->
    let path_item_params = pi.parameters in
    let ops = [
      ("GET", pi.Spec.get); ("POST", pi.post); ("PUT", pi.put);
      ("DELETE", pi.delete); ("PATCH", pi.patch);
      ("HEAD", pi.head); ("OPTIONS", pi.options);
    ] in
    List.filter_map (fun (method_, op_opt) ->
      Option.map (fun op -> analyze_operation ~spec ~path_item_params ~path ~method_ op) op_opt
    ) ops
  ) spec.paths in

  (* Build module tree *)
  let (tree, sorted_modules) = build_module_tree schemas operations in

  (* Generate top-level client type and functions *)
  let client_impl = {|type t = Openapi.Runtime.Client.t

let of_fetch ?max_response_bytes ~base_url session =
  Openapi.Runtime.Client.of_fetch ?max_response_bytes ~base_url session

let create ?session ?max_response_bytes ~sw env ~base_url =
  let session = match session with
    | Some s -> Fetch.restrict s
    | None -> Fetch_curl.std ~sw env
  in
  of_fetch ?max_response_bytes ~base_url session

let base_url = Openapi.Runtime.Client.base_url
let session = Openapi.Runtime.Client.session|} in

  (* Generate prefix modules in dependency order, tracking forward references *)
  let rec gen_with_forward_refs remaining_modules acc =
    match remaining_modules with
    | [] -> List.rev acc
    | name :: rest ->
      (* Set forward refs to modules that come after this one *)
      let context = { context with forward_refs = StringSet.of_list rest } in
      let result = match StringMap.find_opt name tree.children with
        | None -> None
        | Some node ->
            if node.name = "Client" then
              (* Generate Client operations inline *)
              let ops = List.map (gen_operation_impl ~context ~current_prefix:"Client") (List.rev node.operations) in
              if ops = [] then None
              else
                let content = String.concat "\n\n" ops in
                let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
                Some (Printf.sprintf "module Client = struct\n%s\nend" indented)
            else
              Some (gen_prefix_module_impl ~context node)
      in
      gen_with_forward_refs rest (match result with Some r -> r :: acc | None -> acc)
  in
  let prefix_mods = gen_with_forward_refs sorted_modules [] in

  Printf.sprintf {|(** {1 %s}

    %s

    @version %s *)

%s

%s
|}
    (Name.to_module_name package_name) (escape_doc api_desc) (escape_doc spec.info.version)
    (schema_context_source spec ^ client_impl) (String.concat "\n\n" prefix_mods)

let generate_mli (spec : Spec.t) (package_name : string) : string =
  let api_desc = Option.value ~default:"Generated API client." spec.info.description in

  (* Collect schemas *)
  let schemas = match spec.components with
    | None -> []
    | Some c -> List.filter_map (fun (name, sor) ->
        match sor with
        | Spec.Ref r -> Option.map (analyze_schema ~components:spec.components name)
            (resolve_schema_ref ~components:spec.components r)
        | Spec.Value s -> Some (analyze_schema ~components:spec.components name s)
      ) c.schemas
  in

  (* Set known schemas for validation during code generation *)
  let context = { known_schemas = StringSet.of_list (List.map (fun s -> s.original_name) schemas);
    forward_refs = StringSet.empty } in

  (* Collect operations *)
  let operations = List.concat_map (fun (path, (pi : Spec.path_item)) ->
    let path_item_params = pi.parameters in
    let ops = [
      ("GET", pi.Spec.get); ("POST", pi.post); ("PUT", pi.put);
      ("DELETE", pi.delete); ("PATCH", pi.patch);
      ("HEAD", pi.head); ("OPTIONS", pi.options);
    ] in
    List.filter_map (fun (method_, op_opt) ->
      Option.map (fun op -> analyze_operation ~spec ~path_item_params ~path ~method_ op) op_opt
    ) ops
  ) spec.paths in

  (* Build module tree *)
  let (tree, sorted_modules) = build_module_tree schemas operations in

  (* Generate top-level client type and function interfaces *)
  let client_intf = {|type t

val of_fetch : ?max_response_bytes:int -> base_url:string -> _ Fetch.t -> t
(** Use an existing Fetch stack, including scoped credentials, retries and limits.
    [max_response_bytes] defaults to 16 MiB. JSON requires a declared Content-Type.
    Response bodies are closed before returning. Writes do not follow redirects. *)

val create :
  ?session:_ Fetch.t ->
  ?max_response_bytes:int ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  base_url:string ->
  t
(** [create ?session ~sw env ~base_url] is a client rooted at [base_url].
    [session] is the HTTP client to issue requests through, already carrying
    whatever credentials and policy the caller wants; when it is omitted a
    default {!Fetch_curl.std} stack is created under [sw]. *)

val base_url : t -> string
val session : t -> Fetch.plain|} in

  (* Generate prefix modules in dependency order, tracking forward references *)
  let rec gen_with_forward_refs remaining_modules acc =
    match remaining_modules with
    | [] -> List.rev acc
    | name :: rest ->
      (* Set forward refs to modules that come after this one *)
      let context = { context with forward_refs = StringSet.of_list rest } in
      let result = match StringMap.find_opt name tree.children with
        | None -> None
        | Some node ->
            if node.name = "Client" then
              let ops = List.map (gen_operation_intf ~context ~current_prefix:"Client") (List.rev node.operations) in
              if ops = [] then None
              else
                let content = String.concat "\n\n" ops in
                let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
                Some (Printf.sprintf "module Client : sig\n%s\nend" indented)
            else
              Some (gen_prefix_module_intf ~context node)
      in
      gen_with_forward_refs rest (match result with Some r -> r :: acc | None -> acc)
  in
  let prefix_mods = gen_with_forward_refs sorted_modules [] in

  Printf.sprintf {|(** {1 %s}

    %s

    @version %s *)

%s

%s
|}
    (Name.to_module_name package_name) (escape_doc api_desc) (escape_doc spec.info.version)
    client_intf (String.concat "\n\n" prefix_mods)

let generate_dune (package_name : string) : string =
  Printf.sprintf {|(library
 (name %s)
 (public_name %s)
 (libraries openapi jsont jsont.bytesrw fetch fetch-curl ptime eio)
 (wrapped true))

(include dune.inc)
|} package_name package_name

let generate_dune_inc ~(spec_path : string option) (package_name : string) : string =
  match spec_path with
  | None -> "; No spec path provided - regeneration rules not generated\n"
  | Some path ->
      let basename = Filename.basename path in
      Printf.sprintf {|; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets %s.ml %s.mli)
 (deps %S)
 (action
  (run openapi-gen generate --code-only -o . -n %s %%{deps})))
|} package_name package_name basename package_name

let validate_spec spec =
  let context = schema_context spec in
  let unique label names =
    let seen = Hashtbl.create 16 in
    List.iter (fun name -> if Hashtbl.mem seen name then
      invalid_arg ("OpenAPI: duplicate " ^ label ^ " " ^ name);
      Hashtbl.add seen name ()) names in
  let component_entries get = match spec.Spec.components with None -> [] | Some c -> get c in
  let schemas = component_entries (fun c -> c.Spec.schemas) in
  let analyzed = List.filter_map (fun (name, schema) -> match schema with
    | Spec.Value s -> Some (analyze_schema ~components:spec.components name s)
    | Spec.Ref r -> Option.map (analyze_schema ~components:spec.components name) (resolve_schema_ref ~components:spec.components r)) schemas in
  unique "generated schema module" (List.map (fun s -> Name.to_module_name s.prefix ^ "." ^ Name.to_module_name s.suffix) analyzed);
  List.iter (fun s ->
    if List.mem (Name.to_module_name s.prefix) ["Client";"Types";"Openapi";"Fetch";"FetchCurl";"Eio";"Jsont";"Ptime";"Stdlib"] then
      invalid_arg ("OpenAPI: schema name conflicts with generated module " ^ s.original_name);
    List.iter (fun f -> if Option.is_some f.default_value then
      let raw = List.assoc f.json_name s.schema.properties in
      match (Schema.parse raw).default with
      | None -> ()
      | Some value ->
          (match Jsont.Json.decode (Schema.guard context raw Jsont.json) value with
           | Ok _ -> ()
           | Error e -> invalid_arg ("OpenAPI: invalid generated default in " ^ s.original_name ^ "." ^ f.json_name ^ ": " ^ e))
    ) s.fields;
    unique ("field in " ^ s.original_name) (List.map (fun f -> f.ocaml_name) s.fields);
    unique ("enum constructor in " ^ s.original_name) (List.map fst s.enum_variants);
    Option.iter (fun u -> unique ("union constructor in " ^ s.original_name) (List.map (fun v -> v.variant_name) u.variants)) s.union_info
  ) analyzed;
  let check_schema schema = ignore (Schema.guard context (Schema.schema_json schema) Jsont.json) in
  let check_content content = List.iter (fun (media, (m : Spec.media_type)) ->
    if not (Fetch.Media.Syntax.valid_range media ~pos:0 ~len:(String.length media)) then
      invalid_arg ("OpenAPI: invalid media type " ^ media);
    Option.iter check_schema m.schema) content in
  let resolve_parameter = resolve_component ~kind:"parameters" (component_entries (fun c -> c.Spec.parameters)) in
  let resolve_body = resolve_component ~kind:"requestBodies" (component_entries (fun c -> c.Spec.request_bodies)) in
  let resolve_response = resolve_component ~kind:"responses" (component_entries (fun c -> c.Spec.responses)) in
  let check_params refs =
    let params = List.map resolve_parameter refs in
    unique "parameter" (List.map (fun (p : Spec.parameter) ->
      (match p.in_ with Spec.Path -> "path:" | Query -> "query:" | Header -> "header:" | Cookie -> "cookie:") ^ p.name) params);
    List.iter (fun (p : Spec.parameter) ->
      if p.in_ = Spec.Path && not p.required then invalid_arg ("OpenAPI: path parameter must be required: " ^ p.name);
      Option.iter check_schema p.schema; check_content p.content) params;
    params in
  let operation_ids = ref [] and generated_names = ref [] in
  List.iter (fun (path, (pi : Spec.path_item)) ->
    if not (String.starts_with ~prefix:"/" path) || String.contains path '?' || String.contains path '#' then
      invalid_arg ("OpenAPI: invalid path template " ^ path);
    let placeholders = Openapi_runtime.Path.parameters path in
    ignore (Openapi_runtime.Path.render ~params:(List.map (fun name -> name, "parameter") placeholders) path);
    List.iter (fun name -> if name = "" || String.contains name '{' then
      invalid_arg ("OpenAPI: invalid path placeholder in " ^ path)) placeholders;
    let inherited = check_params pi.parameters in
    List.iter (fun (method_, op) -> Option.iter (fun (op : Spec.operation) ->
      Option.iter (fun id -> operation_ids := id :: !operation_ids) op.operation_id;
      let params = check_params op.parameters in
      let all = inherited @ params in
      List.iter (fun (p : Spec.parameter) -> if p.in_ = Spec.Path && not (List.mem p.name placeholders) then
        Logs.warn (fun m -> m "OpenAPI %s %s: ignoring path parameter %s absent from template" method_ path p.name)) all;
      List.iter (fun name -> if not (List.exists (fun (p : Spec.parameter) -> p.in_ = Spec.Path && p.name = name) all) then
        invalid_arg ("OpenAPI: missing path parameter " ^ name)) (Openapi_runtime.Path.parameters path);
      List.iter (fun (code, response) ->
        let valid = String.length code = 3 && code.[0] >= '1' && code.[0] <= '5' &&
          ((code.[1] = 'X' && code.[2] = 'X') ||
           (code.[1] >= '0' && code.[1] <= '9' && code.[2] >= '0' && code.[2] <= '9')) in
        if not valid then invalid_arg ("OpenAPI: invalid response status " ^ code);
        check_content (resolve_response response).content) op.responses.responses;
      Option.iter (fun r -> check_content (resolve_response r).content) op.responses.default;
      Option.iter (fun r -> check_content (resolve_body r).content) op.request_body;
      let info = analyze_operation ~spec ~path_item_params:pi.parameters ~path ~method_ op in
      let prefix = match info.response_schema_ref with
        | None -> "Client" | Some name -> Name.to_module_name (fst (Name.split_schema_name name)) in
      generated_names := (prefix ^ "." ^ info.func_name) :: !generated_names
    ) op) ["GET",pi.get;"POST",pi.post;"PUT",pi.put;"PATCH",pi.patch;"DELETE",pi.delete;"HEAD",pi.head;"OPTIONS",pi.options]
  ) spec.paths;
  unique "operationId" !operation_ids;
  unique "generated operation" !generated_names

let generate ~(config : config) (spec : Spec.t) : (string * string) list =
  validate_spec spec;
  let package_name = config.package_name in
  if package_name = "" || package_name.[0] < 'a' || package_name.[0] > 'z' ||
     not (String.for_all (function 'a'..'z' | '0'..'9' | '_' -> true | _ -> false) package_name)
  then invalid_arg "OpenAPI: package name must be a lowercase OCaml identifier";
  [
    ("dune", generate_dune package_name);
    ("dune.inc", generate_dune_inc ~spec_path:config.spec_path package_name);
    (package_name ^ ".ml", generate_ml spec package_name);
    (package_name ^ ".mli", generate_mli spec package_name);
  ]
  |> List.map (fun (name, content) ->
    let lines = String.split_on_char '\n' content |> List.map (fun line ->
      let rec trim n =
        if n > 0 && (line.[n - 1] = ' ' || line.[n - 1] = '\t' || line.[n - 1] = '\r')
        then trim (n - 1) else n in
      String.sub line 0 (trim (String.length line))) in
    name, String.concat "\n" lines)

let write_files ~output_dir files =
  List.iter (fun (name, _) ->
    if name = "" || Filename.basename name <> name || name = "." || name = ".." then
      invalid_arg ("OpenAPI: invalid output filename " ^ name)) files;
  List.iter (fun (name, content) ->
    let path = Filename.concat output_dir name in
    let temporary, oc = Filename.open_temp_file ~temp_dir:output_dir ("." ^ name ^ ".") ".tmp" in
    Fun.protect ~finally:(fun () ->
      close_out_noerr oc;
      try Sys.remove temporary with Sys_error _ -> ()) (fun () ->
      output_string oc content;
      close_out oc;
      Sys.rename temporary path)
  ) files
