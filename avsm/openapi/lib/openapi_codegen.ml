(** Code generation from OpenAPI specifications.

    This module generates OCaml code from parsed OpenAPI specs:
    - Nested module structure grouped by common schema prefixes
    - Abstract types with accessor and constructor functions
    - Client functions placed in relevant type modules
    - Proper Eio error handling with context
*)

module Spec = Openapi_spec

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
          if i > 0 && not !prev_upper then Buffer.add_char buf '_';
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
    escape_keyword (Buffer.contents buf)

  let to_module_name s =
    let snake = to_snake_case s in
    let parts = String.split_on_char '_' snake in
    String.concat "" (List.map String.capitalize_ascii parts)

  let to_type_name s = String.lowercase_ascii (to_snake_case s)

  let to_variant_name s = String.capitalize_ascii (to_snake_case s)

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
          |> List.filter (fun s -> s <> "" && not (String.length s > 0 && s.[0] = '{'))
        in
        let path_name = String.concat "_" (List.map to_snake_case path_parts) in
        method_name ^ "_" ^ path_name
end

(** {1 OCamldoc Helpers} *)

let escape_doc s =
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

let schema_name_from_ref (ref_ : string) : string option =
  match String.split_on_char '/' ref_ with
  | ["#"; "components"; "schemas"; name] -> Some name
  | _ -> None

(** Resolve a schema reference to its definition *)
let resolve_schema_ref ~(components : Spec.components option) (ref_str : string) : Spec.schema option =
  match schema_name_from_ref ref_str with
  | None -> None
  | Some name ->
    match components with
    | None -> None
    | Some comps ->
      List.find_map (fun (n, s_or_ref) ->
        if n = name then
          match s_or_ref with
          | Spec.Value s -> Some s
          | Spec.Ref _ -> None  (* Nested refs not supported *)
        else None
      ) comps.schemas

(** Flatten allOf composition by merging properties from all schemas *)
let rec flatten_all_of ~(components : Spec.components option) (schemas : Jsont.json list) : (string * Jsont.json) list * string list =
  List.fold_left (fun (props, reqs) json ->
    match get_ref json with
    | Some ref_str ->
      (* Resolve the reference and get its properties *)
      (match resolve_schema_ref ~components ref_str with
       | Some schema ->
         let (nested_props, nested_reqs) =
           match schema.all_of with
           | Some all_of -> flatten_all_of ~components all_of
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
  let from_items = Option.fold ~none:[] ~some:find_refs_in_json schema.items in
  List.sort_uniq String.compare
    (from_properties @ from_items @ refs_from_list schema.all_of
     @ refs_from_list schema.one_of @ refs_from_list schema.any_of)

(** {1 Module Tree Structure} *)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(** {1 Forward Reference Tracking}

    Track which modules come after the current module in the sorted order.
    This is used to detect forward references and replace them with Jsont.json. *)

let forward_refs : StringSet.t ref = ref StringSet.empty

let set_forward_refs mods = forward_refs := StringSet.of_list mods

let is_forward_ref module_name =
  StringSet.mem module_name !forward_refs

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
  is_recursive : bool;  (* true if schema references itself *)
  is_union : bool;  (** true if this is a oneOf/anyOf schema *)
  union_info : union_info option;
}

(** Error response info for typed error handling *)
type error_response = {
  status_code : string;       (** "400", "404", "5XX", "default" *)
  schema_ref : string option; (** Reference to error schema if present *)
  error_description : string;
}

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
  body_schema_ref : string option;
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
      | Jsont.Number (f, _), "int" ->
          Some (Printf.sprintf "%d" (int_of_float f))
      | Jsont.Number (f, _), "int32" ->
          Some (Printf.sprintf "%ldl" (Int32.of_float f))
      | Jsont.Number (f, _), "int64" ->
          Some (Printf.sprintf "%LdL" (Int64.of_float f))
      | Jsont.Number (f, _), "float" ->
          let s = Printf.sprintf "%g" f in
          (* Ensure it's a valid float literal *)
          if String.contains s '.' || String.contains s 'e' then Some s
          else Some (s ^ ".")
      | Jsont.String (s, _), "string" ->
          Some (Printf.sprintf "%S" s)
      | Jsont.String (s, _), t when String.contains t '.' ->
          (* Enum type reference like "AlbumUserRole.T.t" - use backtick variant *)
          Some (Printf.sprintf "`%s" (Name.to_variant_name s))
      | Jsont.Null _, _ ->
          Some "None"  (* For nullable fields *)
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
    if List.length variants >= 2 then
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
  let is_nullable = match get_member "nullable" json with
    | Some (Jsont.Bool (b, _)) -> b
    | _ -> false
  in
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
               let resolved_type = match get_string_member "type" json with
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
                          elem.resolved_type ^ " list"
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
  | "int" -> "Jsont.int"
  | "int32" -> "Jsont.int32"
  | "int64" -> "Jsont.int64"
  | "float" -> "Jsont.number"
  | "bool" -> "Jsont.bool"
  | "Ptime.t" -> "Openapi.Runtime.ptime_jsont"
  | "Jsont.json" -> "Jsont.json"
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
  let str = Printf.sprintf "%g" v in
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
        else Printf.sprintf "(Openapi.Runtime.validated_int %s %s)" (String.concat " " args) base_codec
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
      ~dec:(Openapi.Runtime.poly_union_decoder [
        %s
      ])
      ~enc:(function
%s))|}
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
    if variants = [] then None
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
  let is_enum = Option.is_some expanded.enum in
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
  let union_info = analyze_union ~name expanded in
  let is_union = Option.is_some union_info in
  let fields = List.map (fun (field_name, field_json) ->
    let ocaml_name = Name.to_snake_case field_name in
    let is_required = List.mem field_name expanded.required in
    let resolved = resolve_type_full field_json in
    let base_type = resolved.resolved_type in
    let is_nullable = resolved.resolved_nullable in
    let default_value = extract_default_value field_json base_type in
    (* Field is optional in record type if:
       - nullable (can be null) OR
       - not required AND no default (may be absent with no fallback)
       Fields with defaults are NOT optional - they always have a value *)
    let has_default = Option.is_some default_value in
    let is_optional = is_nullable || (not is_required && not has_default) in
    let ocaml_type = if is_optional then base_type ^ " option" else base_type in
    let description = get_string_member "description" field_json in
    { ocaml_name; json_name = field_name; ocaml_type; base_type; is_optional;
      is_required; is_nullable; description;
      constraints = resolved.resolved_constraints;
      field_union = resolved.resolved_union;
      default_value }
  ) expanded.properties in
  (* Check if schema references itself *)
  let deps = find_schema_dependencies expanded in
  let is_recursive = List.mem name deps in
  { original_name = name; prefix; suffix; schema = expanded; fields; is_enum; enum_variants;
    enum_base_type; description = expanded.description; is_recursive; is_union; union_info }

(** {1 Operation Processing} *)

(** Extract parameter name from a $ref like "#/components/parameters/idOrUUID" *)
let param_name_from_ref ref_str =
  let prefix = "#/components/parameters/" in
  if String.length ref_str > String.length prefix &&
     String.sub ref_str 0 (String.length prefix) = prefix then
    Some (String.sub ref_str (String.length prefix)
            (String.length ref_str - String.length prefix))
  else None

(** Resolve a parameter reference or return inline parameter *)
let resolve_parameter ~(components : Spec.components option) (p : Spec.parameter Spec.or_ref) : Spec.parameter option =
  match p with
  | Spec.Value param -> Some param
  | Spec.Ref ref_str ->
    match param_name_from_ref ref_str with
    | None -> None
    | Some name ->
      match components with
      | None -> None
      | Some comps ->
        List.find_map (fun (n, p_or_ref) ->
          if n = name then
            match p_or_ref with
            | Spec.Value param -> Some param
            | Spec.Ref _ -> None  (* Nested refs not supported *)
          else None
        ) comps.parameters

let analyze_operation ~(spec : Spec.t) ~(path_item_params : Spec.parameter Spec.or_ref list)
    ~path ~method_ (op : Spec.operation) : operation_info =
  let func_name = Name.operation_name ~method_ ~path ~operation_id:op.operation_id in
  (* Merge path_item parameters with operation parameters, operation takes precedence *)
  let all_param_refs = path_item_params @ op.parameters in
  let params = List.filter_map (resolve_parameter ~components:spec.components) all_param_refs in

  let path_params = List.filter_map (fun (p : Spec.parameter) ->
    if p.in_ = Spec.Path then
      Some (Name.to_snake_case p.name, p.name, p.description, p.required)
    else None
  ) params in

  let query_params = List.filter_map (fun (p : Spec.parameter) ->
    if p.in_ = Spec.Query then
      Some (Name.to_snake_case p.name, p.name, p.description, p.required)
    else None
  ) params in

  let body_schema_ref = match op.request_body with
    | Some (Spec.Value (rb : Spec.request_body)) ->
        List.find_map (fun (ct, (media : Spec.media_type)) ->
          if String.length ct >= 16 && String.sub ct 0 16 = "application/json" then
            match media.schema with
            | Some (Spec.Ref r) -> schema_name_from_ref r
            | _ -> None
          else None
        ) rb.content
    | _ -> None
  in

  let find_in_content content =
    List.find_map (fun (ct, (media : Spec.media_type)) ->
      if String.length ct >= 16 && String.sub ct 0 16 = "application/json" then
        match media.schema with
        | Some (Spec.Ref r) -> schema_name_from_ref r
        | Some (Spec.Value s) when s.type_ = Some "array" ->
            Option.bind s.items (fun items -> Option.bind (get_ref items) schema_name_from_ref)
        | _ -> None
      else None
    ) content
  in

  let response_schema_ref =
    let try_status status =
      List.find_map (fun (code, resp) ->
        if code = status then
          match resp with
          | Spec.Value (r : Spec.response) -> find_in_content r.content
          | _ -> None
        else None
      ) op.responses.responses
    in
    match try_status "200" with
    | Some r -> Some r
    | None -> match try_status "201" with
      | Some r -> Some r
      | None -> match op.responses.default with
        | Some (Spec.Value (r : Spec.response)) -> find_in_content r.content
        | _ -> None
  in

  (* Extract error responses (4xx, 5xx, default) *)
  let error_responses =
    let is_error_code code =
      code = "default" ||
      (try int_of_string code >= 400 with _ ->
       String.length code = 3 && (code.[0] = '4' || code.[0] = '5'))
    in
    List.filter_map (fun (code, resp) ->
      if is_error_code code then
        match resp with
        | Spec.Value (r : Spec.response) ->
            let schema_ref = find_in_content r.content in
            Some { status_code = code; schema_ref; error_description = r.description }
        | Spec.Ref _ ->
            Some { status_code = code; schema_ref = None; error_description = "" }
      else None
    ) op.responses.responses
  in

  { func_name; operation_id = op.operation_id; summary = op.summary;
    description = op.description; tags = op.tags; path; method_;
    path_params; query_params; body_schema_ref; response_schema_ref; error_responses }

(** {1 Module Tree Building} *)

(** Extract prefix module dependencies from a schema's fields *)
let schema_prefix_deps (schema : schema_info) : StringSet.t =
  let deps = List.filter_map (fun (f : field_info) ->
    (* Check if the type references another module *)
    if String.contains f.base_type '.' then
      (* Extract first component before the dot *)
      match String.split_on_char '.' f.base_type with
      | prefix :: _ when prefix <> "Jsont" && prefix <> "Ptime" && prefix <> "Openapi" ->
          Some prefix
      | _ -> None
    else None
  ) schema.fields in
  StringSet.of_list deps

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
  StringSet.of_list (List.filter_map Fun.id [body_dep; response_dep])

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

let gen_enum_impl (schema : schema_info) : string =
  let doc = format_doc schema.description in
  let jsont_base = jsont_of_base_type schema.enum_base_type in
  if schema.enum_variants = [] then
    Printf.sprintf "%stype t = %s\n\nlet jsont = %s" doc schema.enum_base_type jsont_base
  else
    let type_def = Printf.sprintf "%stype t = [\n%s\n]" doc
      (String.concat "\n" (List.map (fun (v, _) -> "  | `" ^ v) schema.enum_variants))
    in
    let dec_cases = String.concat "\n" (List.map (fun (v, raw) ->
      Printf.sprintf "      | %S -> `%s" raw v
    ) schema.enum_variants) in
    let enc_cases = String.concat "\n" (List.map (fun (v, raw) ->
      Printf.sprintf "      | `%s -> %S" v raw
    ) schema.enum_variants) in
    Printf.sprintf {|%s

let jsont : t Jsont.t =
  Jsont.map Jsont.string ~kind:%S
    ~dec:(function
%s
      | s -> Jsont.Error.msgf Jsont.Meta.none "Unknown value: %%s" s)
    ~enc:(function
%s)|} type_def schema.original_name dec_cases enc_cases

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
let format_union_type_ref ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  if prefix_mod = current_prefix then
    Printf.sprintf "%s.t" suffix_mod
  else if is_forward_ref prefix_mod then
    "Jsont.json"
  else
    Printf.sprintf "%s.%s.t" prefix_mod suffix_mod

(** Format a union variant jsont codec reference *)
let format_union_jsont_ref ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  if prefix_mod <> current_prefix && is_forward_ref prefix_mod then
    "Jsont.json"
  else if prefix_mod = current_prefix then
    Printf.sprintf "%s.jsont" suffix_mod
  else
    Printf.sprintf "%s.%s.jsont" prefix_mod suffix_mod

(** Generate a discriminator-based jsont codec for union types.
    Uses Jsont.Object.Case for tag-based discrimination. *)
let gen_union_jsont_discriminator ~current_prefix (schema : schema_info) (union : union_info) (field : string) : string =
  (* Generate case definitions *)
  let cases = List.map (fun (v : union_variant) ->
    let codec_ref = format_union_jsont_ref ~current_prefix v.schema_ref in
    (* Look up the tag value in discriminator mapping, or default to snake_case variant name *)
    let tag_value = match List.find_opt (fun (_, ref_) ->
      match schema_name_from_ref ref_ with
      | Some name -> name = v.schema_ref
      | None -> false
    ) union.discriminator_mapping with
    | Some (tag, _) -> tag
    | None -> Name.to_snake_case v.variant_name
    in
    Printf.sprintf {|  let case_%s =
    Jsont.Object.Case.map %S %s ~dec:(fun v -> %s v)
  in|}
      (Name.to_snake_case v.variant_name)
      tag_value
      codec_ref
      v.variant_name
  ) union.variants in

  let enc_cases = List.map (fun (v : union_variant) ->
    Printf.sprintf "    | %s v -> Jsont.Object.Case.value case_%s v"
      v.variant_name (Name.to_snake_case v.variant_name)
  ) union.variants in

  let case_list = List.map (fun (v : union_variant) ->
    Printf.sprintf "make case_%s" (Name.to_snake_case v.variant_name)
  ) union.variants in

  Printf.sprintf {|let jsont : t Jsont.t =
%s
  let enc_case = function
%s
  in
  let cases = Jsont.Object.Case.[%s] in
  Jsont.Object.map ~kind:%S Fun.id
  |> Jsont.Object.case_mem %S Jsont.string ~enc:Fun.id ~enc_case cases
       ~tag_to_string:Fun.id ~tag_compare:String.compare
  |> Jsont.Object.finish|}
    (String.concat "\n" cases)
    (String.concat "\n" enc_cases)
    (String.concat "; " case_list)
    schema.original_name
    field

(** Generate a try-each jsont codec for union types without discriminator.
    Attempts to decode each variant in order until one succeeds. *)
let gen_union_jsont_try_each ~current_prefix (schema : schema_info) (union : union_info) : string =
  let try_cases = List.mapi (fun i (v : union_variant) ->
    let codec_ref = format_union_jsont_ref ~current_prefix v.schema_ref in
    let prefix = if i = 0 then "    " else "        " in
    let error_prefix = if i = List.length union.variants - 1 then
      Printf.sprintf {|%sJsont.Error.msgf Jsont.Meta.none "No variant matched for %s"|} prefix schema.original_name
    else
      ""
    in
    Printf.sprintf {|%smatch Openapi.Runtime.Json.decode_json %s json with
%s| Ok v -> %s v
%s| Error _ ->
%s|}
      prefix codec_ref prefix v.variant_name prefix error_prefix
  ) union.variants in

  let enc_cases = List.map (fun (v : union_variant) ->
    let codec_ref = format_union_jsont_ref ~current_prefix v.schema_ref in
    Printf.sprintf "    | %s v -> Openapi.Runtime.Json.encode_json %s v"
      v.variant_name codec_ref
  ) union.variants in

  Printf.sprintf {|let jsont : t Jsont.t =
  let decode json =
%s
  in
  Jsont.map Jsont.json ~kind:%S
    ~dec:decode
    ~enc:(function
%s)|}
    (String.concat "" try_cases)
    schema.original_name
    (String.concat "\n" enc_cases)

(** Generate implementation code for a union type schema *)
let gen_union_impl ~current_prefix (schema : schema_info) : string =
  match schema.union_info with
  | None -> failwith "gen_union_impl called on non-union schema"
  | Some union ->
      let doc = format_doc schema.description in

      (* Type definition with variant constructors *)
      let type_def = Printf.sprintf "%stype t =\n%s" doc
        (String.concat "\n" (List.map (fun (v : union_variant) ->
          Printf.sprintf "  | %s of %s" v.variant_name
            (format_union_type_ref ~current_prefix v.schema_ref)
        ) union.variants))
      in

      (* Jsont codec - discriminator-based or try-each *)
      let jsont_code = match union.discriminator_field with
        | Some field -> gen_union_jsont_discriminator ~current_prefix schema union field
        | None -> gen_union_jsont_try_each ~current_prefix schema union
      in

      Printf.sprintf "%s\n\n%s" type_def jsont_code

(** Generate interface code for a union type schema *)
let gen_union_intf ~current_prefix (schema : schema_info) : string =
  match schema.union_info with
  | None -> failwith "gen_union_intf called on non-union schema"
  | Some union ->
      let doc = format_doc schema.description in
      let type_def = Printf.sprintf "%stype t =\n%s" doc
        (String.concat "\n" (List.map (fun (v : union_variant) ->
          Printf.sprintf "  | %s of %s" v.variant_name
            (format_union_type_ref ~current_prefix v.schema_ref)
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
  else
    strip_prefix jsont_str

let gen_record_impl ~current_prefix ~current_suffix (schema : schema_info) : string =
  (* For recursive schemas, self-referential fields need to use Jsont.json
     to avoid OCaml's let rec restrictions on non-functional values.
     Also handle forward references to modules that come later in the sort order. *)
  let is_forward_reference type_str =
    (* Extract prefix from type like "People.Update.t" *)
    match String.split_on_char '.' type_str with
    | prefix :: _ when prefix <> current_prefix && is_forward_ref prefix -> true
    | _ -> false
  in
  let loc_type s =
    let localized = localize_type ~current_prefix ~current_suffix s in
    if schema.is_recursive && localized = "t" then "Jsont.json"
    else if schema.is_recursive && localized = "t list" then "Jsont.json list"
    else if schema.is_recursive && localized = "t option" then "Jsont.json option"
    else if schema.is_recursive && localized = "t list option" then "Jsont.json list option"
    (* Handle forward references - use Jsont.json for types from modules not yet defined *)
    else if is_forward_reference s then
      if String.ends_with ~suffix:" option" localized then "Jsont.json option"
      else if String.ends_with ~suffix:" list" localized then "Jsont.json list"
      else if String.ends_with ~suffix:" list option" localized then "Jsont.json list option"
      else "Jsont.json"
    else localized
  in
  let is_forward_jsont_ref jsont_str =
    (* Extract prefix from jsont like "People.Update.jsont", "(Jsont.list People.Update.jsont)",
       or "(Openapi.Runtime.nullable_any People.Update.jsont)" *)
    let s =
      if String.length jsont_str > 12 && String.sub jsont_str 0 12 = "(Jsont.list " then
        String.sub jsont_str 12 (String.length jsont_str - 13)
      else if String.length jsont_str > 31 && String.sub jsont_str 0 31 = "(Openapi.Runtime.nullable_any " then
        String.sub jsont_str 31 (String.length jsont_str - 32)
      else jsont_str
    in
    match String.split_on_char '.' s with
    | prefix :: _ when prefix <> current_prefix && is_forward_ref prefix -> true
    | _ -> false
  in
  let loc_jsont s =
    let localized = localize_jsont ~current_prefix ~current_suffix s in
    if schema.is_recursive && localized = "jsont" then "Jsont.json"
    else if schema.is_recursive && localized = "(Jsont.list jsont)" then
      "(Jsont.list Jsont.json)"
    (* Handle forward references in jsont codecs *)
    else if is_forward_jsont_ref s then
      if String.length localized > 12 && String.sub localized 0 12 = "(Jsont.list " then
        "(Jsont.list Jsont.json)"
      else if String.length s > 31 && String.sub s 0 31 = "(Openapi.Runtime.nullable_any " then
        (* For nullable forward refs, Jsont.json can decode nulls too *)
        "Jsont.json"
      else "Jsont.json"
    else localized
  in
  let doc = format_doc schema.description in
  if schema.fields = [] then
    Printf.sprintf "%stype t = Jsont.json\n\nlet jsont = Jsont.json\n\nlet v () = Jsont.Null ((), Jsont.Meta.none)" doc
  else
    (* Private type definition *)
    let type_fields = String.concat "\n" (List.map (fun (f : field_info) ->
      let field_doc = match f.description with
        | Some d -> Printf.sprintf "  (** %s *)" (escape_doc d)
        | None -> ""
      in
      Printf.sprintf "  %s : %s;%s" f.ocaml_name (loc_type f.ocaml_type) field_doc
    ) schema.fields) in

    let type_def = Printf.sprintf "%stype t = {\n%s\n}" doc type_fields in

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
        Printf.sprintf "?(%s=%s)" f.ocaml_name (Option.get f.default_value)
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
      (* Determine the right codec based on nullable/required/default status:
         - nullable: use nullable codec, dec_absent depends on default
         - optional with default: use mem with dec_absent:(Some default)
         - optional without default: use opt_mem
         - required: use mem
         - field union: use polymorphic variant codec
         - with validation: use validated codec *)
      let base_codec =
        match f.field_union with
        | Some union ->
            (* Field-level union - generate inline polymorphic variant codec *)
            jsont_of_field_union ~current_prefix union
        | None ->
            (* Regular field - may need validation *)
            let raw_codec = jsont_of_base_type f.base_type in
            let localized = loc_jsont raw_codec in
            if has_constraints f.constraints then
              validated_jsont f.constraints localized f.base_type
            else
              localized
      in
      if f.is_nullable then
        let nullable_codec =
          match f.field_union with
          | Some _ -> Printf.sprintf "(Openapi.Runtime.nullable_any %s)" base_codec
          | None -> loc_jsont (nullable_jsont_of_base_type f.base_type)
        in
        (* For nullable fields, dec_absent depends on default:
           - No default: None (absent = null)
           - Default is "None" (JSON null): None
           - Default is a value: (Some value) *)
        let dec_absent = match f.default_value with
          | Some "None" -> "None"  (* Default is null *)
          | Some def -> Printf.sprintf "(Some %s)" def
          | None -> "None"
        in
        Printf.sprintf "  |> Jsont.Object.mem %S %s\n       ~dec_absent:%s ~enc_omit:Option.is_none ~enc:(fun r -> r.%s)"
          f.json_name nullable_codec dec_absent f.ocaml_name
      else if f.is_optional then
        (* Optional non-nullable field without default - use opt_mem *)
        Printf.sprintf "  |> Jsont.Object.opt_mem %S %s ~enc:(fun r -> r.%s)"
          f.json_name base_codec f.ocaml_name
      else
        (* Required or has default - use mem, possibly with dec_absent *)
        (match f.default_value with
        | Some def ->
            Printf.sprintf "  |> Jsont.Object.mem %S %s ~dec_absent:%s ~enc:(fun r -> r.%s)"
              f.json_name base_codec def f.ocaml_name
        | None ->
            Printf.sprintf "  |> Jsont.Object.mem %S %s ~enc:(fun r -> r.%s)"
              f.json_name base_codec f.ocaml_name)
    ) schema.fields) in

    Printf.sprintf {|%s

%s

%s

let jsont : t Jsont.t =
  Jsont.Object.map ~kind:%S
    (fun %s -> { %s })
%s
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish|}
      type_def v_func accessors schema.original_name make_params v_body jsont_members

let gen_record_intf ~current_prefix ~current_suffix (schema : schema_info) : string =
  (* For recursive schemas, self-referential fields need to use Jsont.json
     to avoid OCaml's let rec restrictions on non-functional values.
     Also handle forward references to modules that come later in the sort order. *)
  let is_forward_reference type_str =
    match String.split_on_char '.' type_str with
    | prefix :: _ when prefix <> current_prefix && is_forward_ref prefix -> true
    | _ -> false
  in
  let loc_type s =
    let localized = localize_type ~current_prefix ~current_suffix s in
    if schema.is_recursive && localized = "t" then "Jsont.json"
    else if schema.is_recursive && localized = "t list" then "Jsont.json list"
    else if schema.is_recursive && localized = "t option" then "Jsont.json option"
    else if schema.is_recursive && localized = "t list option" then "Jsont.json list option"
    (* Handle forward references *)
    else if is_forward_reference s then
      if String.ends_with ~suffix:" option" localized then "Jsont.json option"
      else if String.ends_with ~suffix:" list" localized then "Jsont.json list"
      else if String.ends_with ~suffix:" list option" localized then "Jsont.json list option"
      else "Jsont.json"
    else localized
  in
  let doc = format_doc schema.description in
  if schema.fields = [] then
    (* Expose that the type is Jsont.json for opaque types - allows users to pattern match *)
    Printf.sprintf "%stype t = Jsont.json\n\nval jsont : t Jsont.t\n\nval v : unit -> t" doc
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
      (List.map (fun (f : field_info) -> Printf.sprintf "?%s:%s" f.ocaml_name (loc_type f.base_type)) optional_fields) @
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
let format_jsont_ref ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  (* Check if this is a forward reference to a module that hasn't been defined yet *)
  if prefix_mod <> current_prefix && is_forward_ref prefix_mod then
    "Jsont.json"
  else if prefix_mod = current_prefix then
    Printf.sprintf "%s.jsont" suffix_mod
  else
    Printf.sprintf "%s.%s.jsont" prefix_mod suffix_mod

(** Check if a schema exists - used to validate refs before generating code *)
let schema_exists_ref = ref (fun (_ : string) -> true)
let set_known_schemas (schemas : schema_info list) =
  let known = StringSet.of_list (List.map (fun s -> s.original_name) schemas) in
  schema_exists_ref := (fun name -> StringSet.mem name known)

let gen_operation_impl ~current_prefix (op : operation_info) : string =
  let doc = format_doc_block ~summary:op.summary ?description:op.description () in
  let param_docs = String.concat ""
    ((List.map (fun (n, _, d, _) -> format_param_doc n d) op.path_params) @
     (List.map (fun (n, _, d, _) -> format_param_doc n d) op.query_params)) in
  let full_doc = if param_docs = "" then doc
    else if doc = "" then Printf.sprintf "(**\n%s*)\n" param_docs
    else String.sub doc 0 (String.length doc - 3) ^ "\n" ^ param_docs ^ "*)\n" in

  (* Only use body/response refs if schema actually exists *)
  let valid_body_ref = match op.body_schema_ref with
    | Some name when !schema_exists_ref name -> Some name
    | _ -> None
  in
  let valid_response_ref = match op.response_schema_ref with
    | Some name when !schema_exists_ref name -> Some name
    | _ -> None
  in

  let path_args = List.map (fun (n, _, _, _) -> Printf.sprintf "~%s" n) op.path_params in
  let query_args = List.map (fun (n, _, _, req) ->
    if req then Printf.sprintf "~%s" n else Printf.sprintf "?%s" n
  ) op.query_params in
  (* DELETE and HEAD don't support body in the requests library *)
  let method_supports_body = not (List.mem op.method_ ["DELETE"; "HEAD"; "OPTIONS"]) in
  let body_arg = match valid_body_ref, method_supports_body with
    | Some _, true -> ["~body"]
    | _ -> []
  in
  let all_args = path_args @ query_args @ body_arg @ ["client"; "()"] in

  let path_render =
    if op.path_params = [] then Printf.sprintf "%S" op.path
    else
      let bindings = List.map (fun (ocaml, json, _, _) ->
        Printf.sprintf "(%S, %s)" json ocaml
      ) op.path_params in
      Printf.sprintf "Openapi.Runtime.Path.render ~params:[%s] %S"
        (String.concat "; " bindings) op.path
  in

  let query_build =
    if op.query_params = [] then "\"\""
    else
      let parts = List.map (fun (ocaml, json, _, req) ->
        if req then Printf.sprintf "Openapi.Runtime.Query.singleton ~key:%S ~value:%s" json ocaml
        else Printf.sprintf "Openapi.Runtime.Query.optional ~key:%S ~value:%s" json ocaml
      ) op.query_params in
      Printf.sprintf "Openapi.Runtime.Query.encode (List.concat [%s])" (String.concat "; " parts)
  in

  let method_lower = String.lowercase_ascii op.method_ in
  let body_codec = match valid_body_ref with
    | Some name -> format_jsont_ref ~current_prefix name
    | None -> "Jsont.json"
  in
  (* DELETE and HEAD don't support body in the requests library *)
  let method_supports_body' = not (List.mem op.method_ ["DELETE"; "HEAD"; "OPTIONS"]) in
  let http_call = match valid_body_ref, method_supports_body' with
    | Some _, true ->
        Printf.sprintf "Requests.%s client.session ~body:(Requests.Body.json (Openapi.Runtime.Json.encode_json %s body)) url"
          method_lower body_codec
    | Some _, false ->
        (* Method doesn't support body - ignore the body parameter *)
        Printf.sprintf "Requests.%s client.session url" method_lower
    | None, _ ->
        Printf.sprintf "Requests.%s client.session url" method_lower
  in

  let response_codec = match valid_response_ref with
    | Some name -> format_jsont_ref ~current_prefix name
    | None -> "Jsont.json"
  in

  let decode = if response_codec = "Jsont.json" then
    "Requests.Response.json response"
  else
    Printf.sprintf "Openapi.Runtime.Json.decode_json_exn %s (Requests.Response.json response)" response_codec
  in

  (* Generate typed error parsing if we have error schemas *)
  let valid_error_responses = List.filter_map (fun (err : error_response) ->
    match err.schema_ref with
    | Some name when !schema_exists_ref name ->
        let codec = format_jsont_ref ~current_prefix name in
        Some (err.status_code, codec, name)
    | _ -> None
  ) op.error_responses in

  let error_handling =
    if valid_error_responses = [] then
      (* No typed errors - simple error with parsed JSON fallback *)
      {|let body = Requests.Response.text response in
    let parsed_body =
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Ok json -> Some (Openapi.Runtime.Json json)
      | Error _ -> Some (Openapi.Runtime.Raw body)
    in
    raise (Openapi.Runtime.Api_error {
      operation = op_name;
      method_ = |} ^ Printf.sprintf "%S" op.method_ ^ {|;
      url;
      status = Requests.Response.status_code response;
      body;
      parsed_body;
    })|}
    else
      (* Generate try-parse for each error type *)
      let parser_cases = List.map (fun (code, codec, ref_) ->
        Printf.sprintf {|      | %s ->
          (match Openapi.Runtime.Json.decode_json %s (Requests.Response.json response) with
           | Ok v -> Some (Openapi.Runtime.Typed (%S, Openapi.Runtime.Json.encode_json %s v))
           | Error _ -> None)|}
          code codec ref_ codec
      ) valid_error_responses in

      Printf.sprintf {|let body = Requests.Response.text response in
    let status = Requests.Response.status_code response in
    let parsed_body = match status with
%s
      | _ ->
          (match Jsont_bytesrw.decode_string Jsont.json body with
           | Ok json -> Some (Openapi.Runtime.Json json)
           | Error _ -> Some (Openapi.Runtime.Raw body))
    in
    raise (Openapi.Runtime.Api_error {
      operation = op_name;
      method_ = %S;
      url;
      status;
      body;
      parsed_body;
    })|}
        (String.concat "\n" parser_cases)
        op.method_
  in

  Printf.sprintf {|%slet %s %s =
  let op_name = %S in
  let url_path = %s in
  let query = %s in
  let url = client.base_url ^ url_path ^ query in
  let response =
    try %s
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "calling %%s %%s" %S url
  in
  if Requests.Response.ok response then
    %s
  else
    %s|}
    full_doc op.func_name (String.concat " " all_args)
    op.func_name path_render query_build http_call op.method_ decode error_handling

(** Format a type reference, stripping the current_prefix if present *)
let format_type_ref ~current_prefix (schema_ref : string) : string =
  let prefix, suffix = Name.split_schema_name schema_ref in
  let prefix_mod = Name.to_module_name prefix in
  let suffix_mod = Name.to_module_name suffix in
  if prefix_mod = current_prefix then
    (* Local reference - use unqualified name *)
    Printf.sprintf "%s.t" suffix_mod
  else if is_forward_ref prefix_mod then
    (* Forward reference to module not yet defined - use Jsont.json *)
    "Jsont.json"
  else
    Printf.sprintf "%s.%s.t" prefix_mod suffix_mod

let gen_operation_intf ~current_prefix (op : operation_info) : string =
  let doc = format_doc_block ~summary:op.summary ?description:op.description () in
  let param_docs = String.concat ""
    ((List.map (fun (n, _, d, _) -> format_param_doc n d) op.path_params) @
     (List.map (fun (n, _, d, _) -> format_param_doc n d) op.query_params)) in
  let full_doc = if param_docs = "" then doc
    else if doc = "" then Printf.sprintf "(**\n%s*)\n" param_docs
    else String.sub doc 0 (String.length doc - 3) ^ "\n" ^ param_docs ^ "*)\n" in

  (* Only use body/response refs if schema actually exists *)
  let valid_body_ref = match op.body_schema_ref with
    | Some name when !schema_exists_ref name -> Some name
    | _ -> None
  in
  let valid_response_ref = match op.response_schema_ref with
    | Some name when !schema_exists_ref name -> Some name
    | _ -> None
  in

  let path_args = List.map (fun (n, _, _, _) -> Printf.sprintf "%s:string" n) op.path_params in
  let query_args = List.map (fun (n, _, _, req) ->
    if req then Printf.sprintf "%s:string" n else Printf.sprintf "?%s:string" n
  ) op.query_params in
  let method_supports_body = not (List.mem op.method_ ["DELETE"; "HEAD"; "OPTIONS"]) in
  let body_arg = match valid_body_ref, method_supports_body with
    | Some name, true -> [Printf.sprintf "body:%s" (format_type_ref ~current_prefix name)]
    | _ -> []
  in
  let response_type = match valid_response_ref with
    | Some name -> format_type_ref ~current_prefix name
    | None -> "Jsont.json"
  in
  let all_args = path_args @ query_args @ body_arg @ ["t"; "unit"; response_type] in

  Printf.sprintf "%sval %s : %s" full_doc op.func_name (String.concat " -> " all_args)

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
let gen_union_type_only ~current_prefix (schema : schema_info) : string =
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
        else if is_forward_ref prefix_mod then
          "Jsont.json"  (* Cross-prefix forward ref *)
        else
          Printf.sprintf "%s.%s.t" prefix_mod suffix_mod
      in
      Printf.sprintf "%stype t =\n%s" doc
        (String.concat "\n" (List.map (fun (v : union_variant) ->
          Printf.sprintf "  | %s of %s" v.variant_name (format_type_in_types v.schema_ref)
        ) union.variants))

(** Generate type-only content for a record schema (for Types module) *)
let gen_record_type_only ~current_prefix ~current_suffix (schema : schema_info) : string =
  let is_forward_reference type_str =
    match String.split_on_char '.' type_str with
    | prefix :: _ when prefix <> current_prefix && is_forward_ref prefix -> true
    | _ -> false
  in
  let loc_type s =
    let localized = localize_type ~current_prefix ~current_suffix s in
    if schema.is_recursive && localized = "t" then "Jsont.json"
    else if schema.is_recursive && localized = "t list" then "Jsont.json list"
    else if schema.is_recursive && localized = "t option" then "Jsont.json option"
    else if schema.is_recursive && localized = "t list option" then "Jsont.json list option"
    else if is_forward_reference s then
      if String.ends_with ~suffix:" option" localized then "Jsont.json option"
      else if String.ends_with ~suffix:" list" localized then "Jsont.json list"
      else if String.ends_with ~suffix:" list option" localized then "Jsont.json list option"
      else "Jsont.json"
    else localized
  in
  let doc = format_doc schema.description in
  if schema.fields = [] then
    Printf.sprintf "%stype t = Jsont.json" doc
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
let gen_type_only_submodule ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_type_only ~current_prefix schema
    else if schema.is_enum then gen_enum_type_only schema
    else gen_record_type_only ~current_prefix ~current_suffix:suffix_mod schema
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
let gen_union_codec_only ~current_prefix (schema : schema_info) : string =
  match schema.union_info with
  | None -> failwith "gen_union_codec_only called on non-union schema"
  | Some union ->
      let suffix_mod = Name.to_module_name schema.suffix in
      (* Jsont codec - discriminator-based or try-each *)
      let jsont_code = match union.discriminator_field with
        | Some field -> gen_union_jsont_discriminator ~current_prefix schema union field
        | None -> gen_union_jsont_try_each ~current_prefix schema union
      in
      Printf.sprintf "include Types.%s\n\n%s" suffix_mod jsont_code

(** Generate codec content for a record schema (includes Types.X) *)
let gen_record_codec_only ~current_prefix ~current_suffix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  (* Note: loc_type is not needed here since types come from Types.X via include *)
  let is_forward_jsont_ref jsont_str =
    let s =
      if String.length jsont_str > 12 && String.sub jsont_str 0 12 = "(Jsont.list " then
        String.sub jsont_str 12 (String.length jsont_str - 13)
      else if String.length jsont_str > 31 && String.sub jsont_str 0 31 = "(Openapi.Runtime.nullable_any " then
        String.sub jsont_str 31 (String.length jsont_str - 32)
      else jsont_str
    in
    match String.split_on_char '.' s with
    | prefix :: _ when prefix <> current_prefix && is_forward_ref prefix -> true
    | _ -> false
  in
  let loc_jsont s =
    let localized = localize_jsont ~current_prefix ~current_suffix s in
    if schema.is_recursive && localized = "jsont" then "Jsont.json"
    else if schema.is_recursive && localized = "(Jsont.list jsont)" then "(Jsont.list Jsont.json)"
    else if is_forward_jsont_ref s then
      if String.length localized > 12 && String.sub localized 0 12 = "(Jsont.list " then "(Jsont.list Jsont.json)"
      else if String.length s > 31 && String.sub s 0 31 = "(Openapi.Runtime.nullable_any " then "Jsont.json"
      else "Jsont.json"
    else localized
  in
  if schema.fields = [] then
    Printf.sprintf "include Types.%s\nlet jsont = Jsont.json\nlet v () = Jsont.Null ((), Jsont.Meta.none)" suffix_mod
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
        Printf.sprintf "?(%s=%s)" f.ocaml_name (Option.get f.default_value)
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
        let nullable_codec =
          match f.field_union with
          | Some _ -> Printf.sprintf "(Openapi.Runtime.nullable_any %s)" base_codec
          | None -> loc_jsont (nullable_jsont_of_base_type f.base_type)
        in
        (* For nullable fields, dec_absent depends on default:
           - No default: None (absent = null)
           - Default is "None" (JSON null): None
           - Default is a value: (Some value) *)
        let dec_absent = match f.default_value with
          | Some "None" -> "None"  (* Default is null *)
          | Some def -> Printf.sprintf "(Some %s)" def
          | None -> "None"
        in
        Printf.sprintf "  |> Jsont.Object.mem %S %s\n       ~dec_absent:%s ~enc_omit:Option.is_none ~enc:(fun r -> r.%s)"
          f.json_name nullable_codec dec_absent f.ocaml_name
      else if f.is_optional then
        (* Optional non-nullable field without default - use opt_mem *)
        Printf.sprintf "  |> Jsont.Object.opt_mem %S %s ~enc:(fun r -> r.%s)"
          f.json_name base_codec f.ocaml_name
      else
        (* Required or has default - use mem, possibly with dec_absent *)
        (match f.default_value with
        | Some def ->
            Printf.sprintf "  |> Jsont.Object.mem %S %s ~dec_absent:%s ~enc:(fun r -> r.%s)"
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
let gen_codec_only_submodule ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_codec_only ~current_prefix schema
    else if schema.is_enum then gen_enum_codec_only schema
    else gen_record_codec_only ~current_prefix ~current_suffix:suffix_mod schema
  in
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
  union_deps @ field_deps |> List.sort_uniq String.compare

(** {1 Full Module Generation} *)

let gen_submodule_impl ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_impl ~current_prefix schema
    else if schema.is_enum then gen_enum_impl schema
    else gen_record_impl ~current_prefix ~current_suffix:suffix_mod schema in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
  Printf.sprintf "module %s = struct\n%s\nend" suffix_mod indented

let gen_submodule_intf ~current_prefix (schema : schema_info) : string =
  let suffix_mod = Name.to_module_name schema.suffix in
  let content =
    if schema.is_union then gen_union_intf ~current_prefix schema
    else if schema.is_enum then gen_enum_intf schema
    else gen_record_intf ~current_prefix ~current_suffix:suffix_mod schema in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
  Printf.sprintf "module %s : sig\n%s\nend" suffix_mod indented

(** Extract suffix module dependencies within the same prefix *)
let schema_suffix_deps ~current_prefix (schema : schema_info) : string list =
  List.filter_map (fun (f : field_info) ->
    (* Check if the type references a sibling module (same prefix) *)
    if String.contains f.base_type '.' then
      match String.split_on_char '.' f.base_type with
      | prefix :: suffix :: _ when prefix = current_prefix ->
          Some (Name.to_module_name suffix)
      | _ -> None
    else None
  ) schema.fields

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
let gen_prefix_module_impl (node : module_node) : string =
  if node.schemas = [] then
    (* No schemas - just generate operations *)
    let op_impls = List.map (gen_operation_impl ~current_prefix:node.name) (List.rev node.operations) in
    if op_impls = [] then
      Printf.sprintf "module %s = struct\nend" node.name
    else
      let content = String.concat "\n\n" op_impls in
      let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
      Printf.sprintf "module %s = struct\n%s\nend" node.name indented
  else
    (* Phase 1: Generate Types module with all type definitions *)
    let type_sorted_schemas = sort_schemas_by_type_deps ~current_prefix:node.name node.schemas in
    let type_mods = List.map (gen_type_only_submodule ~current_prefix:node.name) type_sorted_schemas in
    let types_content = String.concat "\n\n" type_mods in
    let types_module = Printf.sprintf "module Types = struct\n%s\nend" types_content in

    (* Phase 2: Generate full modules with codecs, sorted by codec dependencies *)
    let codec_sorted_schemas = sort_schemas_by_codec_deps ~current_prefix:node.name node.schemas in
    let codec_mods = List.map (gen_codec_only_submodule ~current_prefix:node.name) codec_sorted_schemas in

    (* Operations *)
    let op_impls = List.map (gen_operation_impl ~current_prefix:node.name) (List.rev node.operations) in

    let content = String.concat "\n\n" ([types_module] @ codec_mods @ op_impls) in
    let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
    Printf.sprintf "module %s = struct\n%s\nend" node.name indented

let gen_prefix_module_intf (node : module_node) : string =
  (* For interfaces, we don't need the two-phase approach.
     Just sort by type dependencies and generate full interfaces. *)
  let sorted_schemas = sort_schemas_by_type_deps ~current_prefix:node.name node.schemas in
  let schema_mods = List.map (gen_submodule_intf ~current_prefix:node.name) sorted_schemas in
  let op_intfs = List.map (gen_operation_intf ~current_prefix:node.name) (List.rev node.operations) in
  let content = String.concat "\n\n" (schema_mods @ op_intfs) in
  let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
  Printf.sprintf "module %s : sig\n%s\nend" node.name indented

(** {1 Top-Level Generation} *)

type config = {
  output_dir : string;
  package_name : string;
  spec_path : string option;
}

let generate_ml (spec : Spec.t) (package_name : string) : string =
  let api_desc = Option.value ~default:"Generated API client." spec.info.description in

  (* Collect schemas *)
  let schemas = match spec.components with
    | None -> []
    | Some c -> List.filter_map (fun (name, sor) ->
        match sor with
        | Spec.Ref _ -> None
        | Spec.Value s -> Some (analyze_schema ~components:spec.components name s)
      ) c.schemas
  in

  (* Set known schemas for validation during code generation *)
  set_known_schemas schemas;

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
  let client_impl = {|type t = {
  session : Requests.t;
  base_url : string;
}

let create ?session ~sw env ~base_url =
  let session = match session with
    | Some s -> s
    | None -> Requests.create ~sw env
  in
  { session; base_url }

let base_url t = t.base_url
let session t = t.session|} in

  (* Generate prefix modules in dependency order, tracking forward references *)
  let rec gen_with_forward_refs remaining_modules acc =
    match remaining_modules with
    | [] -> List.rev acc
    | name :: rest ->
      (* Set forward refs to modules that come after this one *)
      set_forward_refs rest;
      let result = match StringMap.find_opt name tree.children with
        | None -> None
        | Some node ->
            if node.name = "Client" then
              (* Generate Client operations inline *)
              let ops = List.map (gen_operation_impl ~current_prefix:"Client") (List.rev node.operations) in
              if ops = [] then None
              else
                let content = String.concat "\n\n" ops in
                let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
                Some (Printf.sprintf "module Client = struct\n%s\nend" indented)
            else
              Some (gen_prefix_module_impl node)
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
    (Name.to_module_name package_name) (escape_doc api_desc) spec.info.version
    client_impl (String.concat "\n\n" prefix_mods)

let generate_mli (spec : Spec.t) (package_name : string) : string =
  let api_desc = Option.value ~default:"Generated API client." spec.info.description in

  (* Collect schemas *)
  let schemas = match spec.components with
    | None -> []
    | Some c -> List.filter_map (fun (name, sor) ->
        match sor with
        | Spec.Ref _ -> None
        | Spec.Value s -> Some (analyze_schema ~components:spec.components name s)
      ) c.schemas
  in

  (* Set known schemas for validation during code generation *)
  set_known_schemas schemas;

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

val create :
  ?session:Requests.t ->
  sw:Eio.Switch.t ->
  < net : _ Eio.Net.t ; fs : Eio.Fs.dir_ty Eio.Path.t ; clock : _ Eio.Time.clock ; .. > ->
  base_url:string ->
  t

val base_url : t -> string
val session : t -> Requests.t|} in

  (* Generate prefix modules in dependency order, tracking forward references *)
  let rec gen_with_forward_refs remaining_modules acc =
    match remaining_modules with
    | [] -> List.rev acc
    | name :: rest ->
      (* Set forward refs to modules that come after this one *)
      set_forward_refs rest;
      let result = match StringMap.find_opt name tree.children with
        | None -> None
        | Some node ->
            if node.name = "Client" then
              let ops = List.map (gen_operation_intf ~current_prefix:"Client") (List.rev node.operations) in
              if ops = [] then None
              else
                let content = String.concat "\n\n" ops in
                let indented = String.split_on_char '\n' content |> List.map (fun l -> "  " ^ l) |> String.concat "\n" in
                Some (Printf.sprintf "module Client : sig\n%s\nend" indented)
            else
              Some (gen_prefix_module_intf node)
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
    (Name.to_module_name package_name) (escape_doc api_desc) spec.info.version
    client_intf (String.concat "\n\n" prefix_mods)

let generate_dune (package_name : string) : string =
  Printf.sprintf {|(library
 (name %s)
 (public_name %s)
 (libraries openapi jsont jsont.bytesrw requests ptime eio)
 (wrapped true))

(include dune.inc)
|} package_name package_name

let generate_dune_inc ~(spec_path : string option) (package_name : string) : string =
  match spec_path with
  | None -> "; No spec path provided - regeneration rules not generated\n"
  | Some path ->
      Printf.sprintf {|; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets %s.ml %s.mli)
 (deps %s)
 (action
  (run openapi-gen generate -o . -n %s %%{deps})))
|} package_name package_name path package_name

let generate ~(config : config) (spec : Spec.t) : (string * string) list =
  let package_name = config.package_name in
  [
    ("dune", generate_dune package_name);
    ("dune.inc", generate_dune_inc ~spec_path:config.spec_path package_name);
    (package_name ^ ".ml", generate_ml spec package_name);
    (package_name ^ ".mli", generate_mli spec package_name);
  ]

let write_files ~(output_dir : string) (files : (string * string) list) : unit =
  List.iter (fun (filename, content) ->
    let path = Filename.concat output_dir filename in
    let oc = open_out path in
    output_string oc content;
    close_out oc
  ) files
