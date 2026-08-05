(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Full YAML representation with anchors, tags, and aliases *)

type t =
  [ `Scalar of Scalar.t
  | `Alias of string
  | `A of t Sequence.t
  | `O of (t, t) Mapping.t ]

(** Pretty printing *)

let rec pp fmt (v : t) =
  match v with
  | `Scalar s -> Scalar.pp fmt s
  | `Alias name -> Format.fprintf fmt "*%s" name
  | `A seq -> Sequence.pp pp fmt seq
  | `O map -> Mapping.pp pp pp fmt map

(** Equality *)

let rec equal (a : t) (b : t) =
  match (a, b) with
  | `Scalar a, `Scalar b -> Scalar.equal a b
  | `Alias a, `Alias b -> String.equal a b
  | `A a, `A b -> Sequence.equal equal a b
  | `O a, `O b -> Mapping.equal equal equal a b
  | _ -> false

(** Construct from JSON-compatible Value *)

let rec of_value (v : Value.t) : t =
  match v with
  | `Null -> `Scalar (Scalar.make "null")
  | `Bool true -> `Scalar (Scalar.make "true")
  | `Bool false -> `Scalar (Scalar.make "false")
  | `Float f ->
      let s =
        if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
        else Printf.sprintf "%g" f
      in
      `Scalar (Scalar.make s)
  | `String s -> `Scalar (Scalar.make s ~style:`Double_quoted)
  | `A items -> `A (Sequence.make (List.map of_value items))
  | `O pairs ->
      `O
        (Mapping.make
           (List.map
              (fun (k, v) -> (`Scalar (Scalar.make k), of_value v))
              pairs))

(** Default limits for alias expansion (protection against billion laughs
    attack) *)
let default_max_alias_nodes = 10_000_000

let default_max_alias_depth = 100

(** Resolve aliases by replacing them with referenced nodes.

    Processes the tree in document order so that aliases resolve to the anchor
    value that was defined at the point the alias was encountered.

    See
    {{:https://yaml.org/spec/1.2.2/#3222-anchors-and-aliases}Section 3.2.2.2
     (Anchors and Aliases)} of the YAML 1.2.2 specification for details on how
    anchors and aliases work in YAML.

    This implements protection against the "billion laughs attack" (see
    {{:https://yaml.org/spec/1.2.2/#321-processes}Section 3.2.1 (Processes)}) by
    limiting both the total number of nodes and the nesting depth during
    expansion.

    @param max_nodes
      Maximum number of nodes to create during expansion (default 10M)
    @param max_depth
      Maximum depth of alias-within-alias resolution (default 100)
    @raise Error.Yamlrw_error
      with {!type:Error.kind} [Alias_expansion_node_limit] if max_nodes is
      exceeded
    @raise Error.Yamlrw_error
      with {!type:Error.kind} [Alias_expansion_depth_limit] if max_depth is
      exceeded *)
let resolve_aliases ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) (root : t) : t =
  let anchors = Hashtbl.create 16 in
  let node_count = ref 0 in

  (* Check node limit *)
  let check_node_limit () =
    incr node_count;
    if !node_count > max_nodes then
      Error.raise (Alias_expansion_node_limit max_nodes)
  in

  (* Register anchor if present on a node *)
  let register_anchor name resolved_node =
    Hashtbl.replace anchors name resolved_node
  in

  (* Resolve an alias by looking up and expanding the target *)
  let rec expand_alias ~depth name =
    if depth >= max_depth then
      Error.raise (Alias_expansion_depth_limit max_depth);
    match Hashtbl.find_opt anchors name with
    | Some target ->
        (* The target is already resolved, but may contain aliases that
           need expansion if it was registered before those anchors existed *)
        resolve ~depth:(depth + 1) target
    | None -> Error.raise (Undefined_alias name)
  (* Single pass: process in document order, registering anchors and resolving aliases *)
  and resolve ~depth (v : t) : t =
    check_node_limit ();
    match v with
    | `Scalar s ->
        (* Register anchor after we have the resolved node *)
        Option.iter (fun name -> register_anchor name v) (Scalar.anchor s);
        v
    | `Alias name -> expand_alias ~depth name
    | `A seq ->
        (* Register anchor with ORIGINAL node BEFORE resolving members.
           This ensures that when this anchor is expanded later through
           an alias chain, the internal aliases still need resolution,
           allowing the depth counter to properly accumulate. *)
        Option.iter (fun name -> register_anchor name v) (Sequence.anchor seq);
        (* Now resolve all members in order *)
        let resolved_members =
          List.map (resolve ~depth) (Sequence.members seq)
        in
        `A
          (Sequence.make ?anchor:(Sequence.anchor seq) ?tag:(Sequence.tag seq)
             ~implicit:(Sequence.implicit seq) ~style:(Sequence.style seq)
             resolved_members)
    | `O map ->
        (* Register anchor with ORIGINAL node BEFORE resolving members.
           This ensures proper depth tracking for alias chains. *)
        Option.iter (fun name -> register_anchor name v) (Mapping.anchor map);
        (* Process key-value pairs in document order *)
        let resolved_pairs =
          List.map
            (fun (k, v) ->
              let resolved_k = resolve ~depth k in
              let resolved_v = resolve ~depth v in
              (resolved_k, resolved_v))
            (Mapping.members map)
        in
        `O
          (Mapping.make ?anchor:(Mapping.anchor map) ?tag:(Mapping.tag map)
             ~implicit:(Mapping.implicit map) ~style:(Mapping.style map)
             resolved_pairs)
  in
  resolve ~depth:0 root

(** Convert scalar to JSON value based on content *)
let rec scalar_to_value s =
  let value = Scalar.value s in
  let tag = Scalar.tag s in
  let style = Scalar.style s in

  (* If explicitly tagged, respect the tag *)
  match tag with
  | Some "tag:yaml.org,2002:null" | Some "!!null" -> `Null
  | Some "tag:yaml.org,2002:bool" | Some "!!bool" -> (
      match String.lowercase_ascii value with
      | "true" | "yes" | "on" -> `Bool true
      | "false" | "no" | "off" -> `Bool false
      | _ -> Error.raise (Invalid_scalar_conversion (value, "bool")))
  | Some "tag:yaml.org,2002:int" | Some "!!int" -> (
      try `Float (Float.of_string value)
      with _ -> Error.raise (Invalid_scalar_conversion (value, "int")))
  | Some "tag:yaml.org,2002:float" | Some "!!float" -> (
      try `Float (Float.of_string value)
      with _ -> Error.raise (Invalid_scalar_conversion (value, "float")))
  | Some "tag:yaml.org,2002:str" | Some "!!str" -> `String value
  | Some _ ->
      (* Unknown tag - treat as string *)
      `String value
  | None ->
      (* Implicit type resolution for plain scalars *)
      if style <> `Plain then `String value else infer_scalar_type value

(** Infer type from plain scalar value *)
and infer_scalar_type value =
  match String.lowercase_ascii value with
  (* Null *)
  | "" | "null" | "~" -> `Null
  (* Boolean true *)
  | "true" | "yes" | "on" -> `Bool true
  (* Boolean false *)
  | "false" | "no" | "off" -> `Bool false
  (* Special floats *)
  | ".inf" | "+.inf" -> `Float Float.infinity
  | "-.inf" -> `Float Float.neg_infinity
  | ".nan" -> `Float Float.nan
  (* Try numeric *)
  | _ -> try_parse_number value

(** Try to parse as number *)
and try_parse_number value =
  (* Check if value looks like a valid YAML number (not inf/nan without dot)
     This guards against OCaml's Float.of_string accepting "inf", "nan", etc.
     See: https://github.com/avsm/ocaml-yaml/issues/82 *)
  let looks_like_number () =
    let len = String.length value in
    if len = 0 then false
    else
      let first = value.[0] in
      if first >= '0' && first <= '9' then true
      else if (first = '-' || first = '+') && len >= 2 then
        let second = value.[1] in
        (* After sign, must be digit or dot-digit (for +.5, -.5) *)
        (second >= '0' && second <= '9')
        || (second = '.' && len >= 3 && value.[2] >= '0' && value.[2] <= '9')
      else false
  in
  (* Try integer/float *)
  let try_numeric () =
    if looks_like_number () then
      try
        (* Handle octal: 0o prefix or leading 0 *)
        if String.length value > 2 && value.[0] = '0' then
          match value.[1] with
          | 'x' | 'X' ->
              (* Hex *)
              Some (`Float (Float.of_int (int_of_string value)))
          | 'o' | 'O' ->
              (* Octal *)
              Some (`Float (Float.of_int (int_of_string value)))
          | 'b' | 'B' ->
              (* Binary *)
              Some (`Float (Float.of_int (int_of_string value)))
          | _ ->
              (* Decimal with leading zero or octal in YAML 1.1 *)
              Some (`Float (Float.of_string value))
        else Some (`Float (Float.of_string value))
      with _ -> None
    else None
  in
  match try_numeric () with
  | Some v -> v
  | None ->
      (* Try float starting with dot (e.g., .5 for 0.5)
         Note: We must NOT use Float.of_string as a general fallback because
         OCaml accepts "nan", "inf", "infinity" which are NOT valid YAML floats.
         YAML requires the leading dot: .nan, .inf, -.inf
         See: https://github.com/avsm/ocaml-yaml/issues/82 *)
      if
        String.length value >= 2
        && value.[0] = '.'
        && value.[1] >= '0'
        && value.[1] <= '9'
      then try `Float (Float.of_string value) with _ -> `String value
      else `String value

(** Convert to JSON-compatible Value.

    Converts a full YAML representation to a simplified JSON-compatible value
    type. This process implements the representation graph to serialization tree
    conversion described in
    {{:https://yaml.org/spec/1.2.2/#32-processes}Section 3.2 (Processes)} of the
    YAML 1.2.2 specification.

    See also
    {{:https://yaml.org/spec/1.2.2/#10212-json-schema}Section 10.2.1.2 (JSON
     Schema)} for the tag resolution used during conversion.

    @param resolve_aliases_first
      Whether to resolve aliases before conversion (default true)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100)
    @raise Error.Yamlrw_error
      with {!type:Error.kind} [Unresolved_alias] if resolve_aliases_first is
      false and an alias is encountered *)
let to_value ?(resolve_aliases_first = true)
    ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) (v : t) : Value.t =
  let v =
    if resolve_aliases_first then resolve_aliases ~max_nodes ~max_depth v else v
  in
  let rec convert (v : t) : Value.t =
    match v with
    | `Scalar s -> scalar_to_value s
    | `Alias name -> Error.raise (Unresolved_alias name)
    | `A seq -> `A (List.map convert (Sequence.members seq))
    | `O map ->
        `O
          (List.map
             (fun (k, v) ->
               let key =
                 match k with
                 | `Scalar s -> Scalar.value s
                 | _ ->
                     Error.raise (Type_mismatch ("string key", "complex key"))
               in
               (key, convert v))
             (Mapping.members map))
  in
  convert v

(** Get anchor from any node *)
let anchor (v : t) =
  match v with
  | `Scalar s -> Scalar.anchor s
  | `Alias _ -> None
  | `A seq -> Sequence.anchor seq
  | `O map -> Mapping.anchor map

(** Get tag from any node *)
let tag (v : t) =
  match v with
  | `Scalar s -> Scalar.tag s
  | `Alias _ -> None
  | `A seq -> Sequence.tag seq
  | `O map -> Mapping.tag map
