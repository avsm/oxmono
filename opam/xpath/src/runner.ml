open! Core
module Xml = Simple_xml

module Context = struct
  type t =
    { prefixes : string String.Map.t
    ; variables : Value.t Expanded_name.Map.t
    ; functions : Function.t Expanded_name.Map.t
    ; non_standard_qname_name_test_ignores_namespace : bool
    }
  [@@deriving sexp_of]

  let create_exn
    ?(prefixes = String.Map.empty)
    ?(variables = Expanded_name.Map.empty)
    ?(functions = Expanded_name.Map.empty)
    ?(non_standard_qname_name_test_ignores_namespace = false)
    ()
    =
    { prefixes
    ; variables
    ; functions = Map.merge_disjoint_exn functions Predefined_functions.functions
    ; non_standard_qname_name_test_ignores_namespace
    }
  ;;
end

module Outer_context = struct
  type t =
    { prefixes : string String.Map.t
    ; variables : Value.t Expanded_name.Map.t
    ; functions : Function.t Expanded_name.Map.t
    ; non_standard_qname_name_test_ignores_namespace : bool
    ; root : Xml.element
    }
  [@@deriving sexp_of]
end

let evaluate_axis_in_document_order (axis : Axis.t) (trail : Trail.t) (node : Node.t) =
  let parent trail =
    match trail with
    | [] -> None
    | (_specifier, parent) :: parent_trail -> Some (parent_trail, parent)
  in
  let ancestor_rev trail =
    Sequence.unfold ~init:trail ~f:(fun trail ->
      match parent trail with
      | None -> None
      | Some (parent_trail, parent) -> Some ((parent_trail, parent), parent_trail))
  in
  let ancestor trail = ancestor_rev trail |> Sequence.to_list_rev |> Sequence.of_list in
  let ancestor_or_self trail node =
    Sequence.append (ancestor trail) (Sequence.singleton (trail, node))
  in
  let attribute trail (node : Node.t) =
    match node with
    | Element { attributes; _ } ->
      Sequence.of_list attributes
      |> Sequence.mapi ~f:(fun i attribute ->
        (Trail.Specifier.Attribute i, node) :: trail, Node.Attribute attribute)
    | _ -> Sequence.empty
  in
  let child trail (node : Node.t) =
    match node with
    | Root document_node ->
      Sequence.singleton
        ([ Trail.Specifier.Element_child 0, node ], Node.Element document_node)
    | Element { tag = _; children; attributes = _ } ->
      Sequence.of_list children
      |> Sequence.mapi ~f:(fun i child ->
        ( (Trail.Specifier.Element_child i, node) :: trail
        , match child with
          | Element element -> Node.Element element
          | Text text -> Node.Text text ))
    | Attribute _ | Text _ | Namespace _ -> Sequence.empty
  in
  let rec descendant trail node =
    let%bind.Sequence trail, node = child trail node in
    Sequence.append (Sequence.singleton (trail, node)) (descendant trail node)
  in
  let descendant_or_self trail node =
    Sequence.append (Sequence.singleton (trail, node)) (descendant trail node)
  in
  let following_sibling trail (node : Node.t) =
    match node with
    | Namespace _ | Attribute _ -> Sequence.empty (* Per spec *)
    | _ ->
      (match parent trail with
       | None -> Sequence.empty
       | Some (parent_trail, parent) ->
         let specifier, _ = List.hd_exn trail in
         child parent_trail parent
         |> Sequence.drop_while ~f:(fun (trail, _) ->
           let sibling_specifier, _ = List.hd_exn trail in
           Trail.Specifier.( <= ) sibling_specifier specifier))
  in
  let following trail node =
    let%bind.Sequence trail, node =
      Sequence.append (Sequence.singleton (trail, node)) (ancestor_rev trail)
    in
    let%bind.Sequence trail, node = following_sibling trail node in
    descendant_or_self trail node
  in
  let preceding_sibling trail (node : Node.t) =
    match node with
    | Namespace _ | Attribute _ -> Sequence.empty (* Per spec *)
    | _ ->
      (match parent trail with
       | None -> Sequence.empty
       | Some (parent_trail, parent) ->
         let specifier, _ = List.hd_exn trail in
         child parent_trail parent
         |> Sequence.take_while ~f:(fun (trail, _) ->
           let sibling_specifier, _ = List.hd_exn trail in
           Trail.Specifier.( < ) sibling_specifier specifier))
  in
  let preceding trail node =
    let%bind.Sequence trail, node = ancestor_or_self trail node in
    let%bind.Sequence trail, node = preceding_sibling trail node in
    descendant_or_self trail node
  in
  let namespace trail node =
    match node with
    | Node.Element element ->
      Sequence.append
        (Sequence.singleton element)
        (Sequence.of_list trail
         |> Sequence.filter_map ~f:(fun (_trail, node) -> Node.element_val node))
      |> Sequence.folding_map ~init:(String.Map.empty, false) ~f:(fun acc element ->
        let acc, namespaces =
          List.fold_map
            element.Xml.attributes
            ~init:acc
            ~f:(fun (prefixes, has_default) attribute ->
              match attribute with
              | { ns = ""; key = "xmlns"; value } ->
                if has_default
                then
                  ( (prefixes, true)
                  , Some
                      ( Trail.Specifier.Default_namespace
                      , Node.Namespace { prefix = None; value } ) )
                else (prefixes, has_default), None
              | { ns = "xmlns"; key; value } ->
                (match Map.add prefixes ~key ~data:value with
                 | `Ok prefixes ->
                   ( (prefixes, has_default)
                   , Some
                       ( Trail.Specifier.Prefixed_namespace key
                       , Node.Namespace { prefix = Some key; value } ) )
                 | `Duplicate -> (prefixes, has_default), None)
              | _ -> (prefixes, has_default), None)
        in
        acc, Sequence.filter_opt (Sequence.of_list namespaces))
      |> Sequence.concat (* The rest ensures document order. *)
      |> Sequence.to_list_rev
      |> List.sort ~compare:[%compare: Trail.Specifier.t * _]
      |> Sequence.of_list
      |> Sequence.map ~f:(fun (specifier, child) -> (specifier, node) :: trail, child)
    | _ -> Sequence.empty
  in
  match axis with
  | Ancestor -> ancestor trail
  | Ancestor_or_self -> ancestor_or_self trail node
  | Attribute -> attribute trail node
  | Child -> child trail node
  | Descendant -> descendant trail node
  | Descendant_or_self -> descendant_or_self trail node
  | Following -> following trail node
  | Following_sibling -> following_sibling trail node
  | Namespace -> namespace trail node
  | Parent ->
    parent trail |> Option.value_map ~default:Sequence.empty ~f:Sequence.singleton
  | Preceding -> preceding trail node
  | Preceding_sibling -> preceding_sibling trail node
  | Self -> Sequence.return (trail, node)
;;

let evaluate_node_test
  (context : Outer_context.t)
  (node_test : Node_test.t)
  (axis : Axis.t)
  (sequence : (Trail.t * Node.t) Sequence.t)
  =
  match node_test with
  | Processing_instruction _ | Node_type (Comment | Processing_instruction) ->
    Sequence.empty
  | Node_type Node -> sequence
  | Node_type Text ->
    Sequence.filter sequence ~f:(fun (_trail, node) -> Node.is_text node)
  | Name_test test ->
    let is_principal_node =
      match axis with
      | Attribute -> Node.is_attribute
      | Namespace -> Node.is_namespace
      | _ -> Node.is_element
    in
    let test =
      match test with
      | Any -> `Any
      | Any_in_namespace { prefix } ->
        `Any_in_namespace (Map.find_exn context.prefixes prefix)
      | Name { prefix; name } ->
        `Name
          { Expanded_name.namespace = Option.map ~f:(Map.find_exn context.prefixes) prefix
          ; name
          }
    in
    Sequence.filter sequence ~f:(fun (_trail, node) ->
      is_principal_node node
      &&
      match test with
      | `Any -> true
      | (`Any_in_namespace _ | `Name _) as test ->
        let expanded_name = Node.expanded_name node |> Option.value_exn in
        (match test with
         | `Any_in_namespace ns ->
           Option.mem ~equal:String.equal expanded_name.namespace ns
         | `Name name ->
           if context.non_standard_qname_name_test_ignores_namespace
           then String.equal name.name expanded_name.name
           else Expanded_name.equal name expanded_name))
;;

let trail_map_of_sequence sequence =
  Sequence.fold sequence ~init:Trail.Map.empty ~f:(fun node_set (trail, node) ->
    (* Add is better than set because set can allocate in case of duplicate keys. *)
    match Map.add node_set ~key:trail ~data:node with
    | `Ok node_set -> node_set
    | `Duplicate -> node_set)
;;

let merge_trail_maps map1 map2 =
  let map1, map2 = if Map.length map1 < Map.length map2 then map1, map2 else map2, map1 in
  Map.fold map1 ~init:map2 ~f:(fun ~key ~data acc -> Map.set acc ~key ~data)
;;

(** The step is an injective function. *)
let step_requires_deduplication_if_more_than_one_value
  { Types.Step.axis; node_test = _; predicates = _ }
  =
  match axis with
  | Ancestor
  | Ancestor_or_self
  | Parent
  | Preceding
  | Preceding_sibling
  | Descendant
  | Descendant_or_self
  | Following
  | Following_sibling -> true
  | Attribute | Child | Self | Namespace -> false
;;

let rec evaluate_path
  (context : Outer_context.t)
  (context_node : Trail.t * Node.t)
  { Types.Path.kind; path }
  =
  let trail, node =
    match kind with
    | Relative -> context_node
    | Absolute -> [], Root context.root
  in
  List.fold
    ~init:(Sequence.return (trail, node), false)
    path
    ~f:(fun (node_set, deduplicate) step ->
      let node_set =
        if deduplicate
        then trail_map_of_sequence node_set |> Map.to_sequence
        else node_set
      in
      let should_deduplicate_next_step =
        not
          (step_requires_deduplication_if_more_than_one_value step
           || Sequence.length_is_bounded_by ~max:1 node_set)
      in
      let node_set =
        Sequence.concat_map node_set ~f:(fun (trail, node) ->
          evaluate_step context trail node step)
      in
      node_set, should_deduplicate_next_step)
  |> fst
  |> trail_map_of_sequence

and evaluate_step
  (context : Outer_context.t)
  trail
  node
  { Types.Step.axis; node_test; predicates }
  =
  let initial_node_sequence =
    evaluate_axis_in_document_order axis trail node
    |> evaluate_node_test context node_test axis
  in
  List.fold ~init:initial_node_sequence predicates ~f:(fun node_set predicate ->
    evaluate_predicate context predicate node_set)

and evaluate_predicate (context : Outer_context.t) predicate node_set =
  let size = lazy (Sequence.length node_set) in
  Sequence.filteri node_set ~f:(fun position (trail, node) ->
    let position = position + 1 in
    let node_context = { Context_node.node; trail; position; size } in
    let (T (value_type, value)) = evaluate_expression context node_context predicate in
    let result : bool =
      match value_type with
      | Boolean -> value
      | Number -> Float.of_int position |> Float.equal value
      | String -> String.length value > 0
      | Node_set -> Map.is_empty value |> not
    in
    result)

and evaluate_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (expression : Types.Expression.t)
  : Value.t
  =
  match expression with
  | Or [ expression ] | And [ expression ] ->
    evaluate_expression context node_context expression
  | Or expressions ->
    List.exists expressions ~f:(fun expression ->
      evaluate_expression context node_context expression |> Value.Cast.to_boolean)
    |> Value.boolean
  | And expressions ->
    List.for_all expressions ~f:(fun expression ->
      evaluate_expression context node_context expression |> Value.Cast.to_boolean)
    |> Value.boolean
  | Expression equality_expression ->
    evaluate_equality_expression context node_context equality_expression

and evaluate_equality_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (equality_expression : Types.Equality_expression.t)
  =
  match equality_expression with
  | Value relational_expression ->
    evaluate_relational_expression context node_context relational_expression
  | Operation (op, left, right) ->
    let left = evaluate_equality_expression context node_context left in
    let right = evaluate_relational_expression context node_context right in
    Value.Xpath_comparison.equality left right ~op |> Value.boolean

and evaluate_relational_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (relational_expression : Types.Relational_expression.t)
  =
  match relational_expression with
  | Value additive_expression ->
    evaluate_additive_expression context node_context additive_expression
  | Operation (op, left, right) ->
    let left = evaluate_relational_expression context node_context left in
    let right = evaluate_additive_expression context node_context right in
    Value.Xpath_comparison.relational left right ~op |> Value.boolean

and evaluate_additive_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (additive_expression : Types.Additive_expression.t)
  =
  match additive_expression with
  | Value multiplicative_expression ->
    evaluate_multiplicative_expression context node_context multiplicative_expression
  | Operation (op, left, right) ->
    let left =
      evaluate_additive_expression context node_context left |> Value.Cast.to_number
    in
    let right =
      evaluate_multiplicative_expression context node_context right
      |> Value.Cast.to_number
    in
    (match op with
     | Add -> left +. right
     | Subtract -> left -. right)
    |> Value.number

and evaluate_multiplicative_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (multiplicative_expression : Types.Multiplicative_expression.t)
  =
  match multiplicative_expression with
  | Value unary_expression ->
    evaluate_unary_expression context node_context unary_expression
  | Operation (op, left, right) ->
    let left =
      evaluate_multiplicative_expression context node_context left |> Value.Cast.to_number
    in
    let right =
      evaluate_unary_expression context node_context right |> Value.Cast.to_number
    in
    (match op with
     | Multiply -> left *. right
     | Divide -> left /. right
     | Modulo -> Float.mod_float left right)
    |> Value.number

and evaluate_unary_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (unary_expression : Types.Unary_expression.t)
  =
  match unary_expression with
  | Value union_expression ->
    evaluate_union_expression context node_context union_expression
  | Negation unary_expression ->
    evaluate_unary_expression context node_context unary_expression
    |> Value.Cast.to_number
    |> Float.neg
    |> Value.number

and evaluate_union_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (union_expression : Types.Union_expression.t)
  =
  match union_expression with
  | Value path_expression -> evaluate_path_expression context node_context path_expression
  | Union (left_expr, right_expr) ->
    let left = evaluate_union_expression context node_context left_expr in
    let right = evaluate_path_expression context node_context right_expr in
    (match Value.Cast.to_node_set left, Value.Cast.to_node_set right with
     | Some left, Some right -> merge_trail_maps left right |> Value.node_set
     | None, _ ->
       raise_s
         [%message
           "left operand in a union expression is not a node set"
             (left : Value.t)
             ~expression:(Types.Union_expression.to_string_abbreviated left_expr)]
     | _, None ->
       raise_s
         [%message
           "right operand in a union expression is not a node set"
             (right : Value.t)
             ~expression:(Types.Path_expression.to_string_abbreviated right_expr)])

and evaluate_path_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (path_expression : Types.Path_expression.t)
  =
  match path_expression with
  | Path path ->
    evaluate_path context (node_context.trail, node_context.node) path |> Value.node_set
  | Filter (filter_expression, relative_path) ->
    let value = evaluate_filter_expression context node_context filter_expression in
    (match relative_path with
     | None -> value
     | Some path ->
       (match Value.Cast.to_node_set value with
        | None ->
          raise_s
            [%message
              "applying a path to a filter expression that does not evaluate to a node \
               set"
                (value : Value.t)
                ~expression:
                  (Types.Filter_expression.to_string_abbreviated filter_expression)]
        | Some node_set ->
          Map.to_sequence node_set
          |> Sequence.map ~f:(fun node ->
            evaluate_path context node { path; kind = Relative })
          |> Sequence.reduce ~f:merge_trail_maps
          |> Option.value ~default:Trail.Map.empty
          |> Value.node_set))

and evaluate_filter_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  ((primary_expression, predicates) : Types.Filter_expression.t)
  =
  let value = evaluate_primary_expression context node_context primary_expression in
  match predicates with
  | [] -> value
  | _ ->
    (match Value.Cast.to_node_set value with
     | None ->
       raise_s
         [%message
           "primary expression does not evaluate to a node set, filtered by predicates"
             (value : Value.t)
             ~expression:
               (Types.Primary_expression.to_string_abbreviated primary_expression)]
     | Some value ->
       List.fold predicates ~init:(Map.to_sequence value) ~f:(fun node_set predicate ->
         evaluate_predicate context predicate node_set)
       |> trail_map_of_sequence
       |> Value.node_set)

and evaluate_primary_expression
  (context : Outer_context.t)
  (node_context : Context_node.t)
  (primary_expression : Types.Primary_expression.t)
  =
  match primary_expression with
  | Expression expression -> evaluate_expression context node_context expression
  | Literal s -> Value.string (String_literal.to_string s)
  | Variable_reference reference ->
    (match
       Map.find
         context.variables
         (Expanded_name.of_qualified_name_exn reference ~prefixes:context.prefixes)
     with
     | None -> raise_s [%message "variable not found" (reference : Qualified_name.t)]
     | Some value -> value)
  | Number number -> Value.number number
  | Function_call (function_name, arguments) ->
    (match
       Map.find
         context.functions
         (Expanded_name.of_qualified_name_exn function_name ~prefixes:context.prefixes)
     with
     | None -> raise_s [%message "function not found" (function_name : Qualified_name.t)]
     | Some (F (signature, function_)) ->
       (* TODO: specialize some cases to avoid allocating multiple closures *)
       evaluate_function_to_call
         context
         function_name
         signature
         function_
         arguments
         node_context)

and evaluate_function_to_call
  : type f return.
    Outer_context.t
    -> Qualified_name.t
    -> (f, return) Function.Signature.t
    -> (Context_node.t -> f)
    -> Types.Expression.t list
    -> Context_node.t
    -> Value.t
  =
  fun context function_name signature function_ arguments ->
  let extract_exn expected_value_type argument =
    match Function.Type.extract expected_value_type argument with
    | None ->
      raise_s
        [%message
          "wrong argument type"
            (function_name : Qualified_name.t)
            (argument : Value.t)
            (expected_value_type : _ Function.Type.t)]
    | Some value -> value
  in
  match signature, arguments with
  | Abstract (Required _, _), [] ->
    raise_s [%message "too few arguments" (function_name : Qualified_name.t)]
  | Abstract (Required expected_value_type, signature), argument :: arguments ->
    let f node_context =
      evaluate_expression context node_context argument
      |> extract_exn expected_value_type
      |> function_ node_context
    in
    evaluate_function_to_call context function_name signature f arguments
  | Abstract (Nullable expected_value_type, signature), argument :: arguments ->
    let f node_context =
      evaluate_expression context node_context argument
      |> extract_exn expected_value_type
      |> Some
      |> function_ node_context
    in
    evaluate_function_to_call context function_name signature f arguments
  | Abstract (Nullable _, signature), [] ->
    let f node_context = function_ node_context None in
    evaluate_function_to_call context function_name signature f []
  | Abstract (Variadic expected_value_type, signature), arguments ->
    let f node_context =
      List.map arguments ~f:(fun argument ->
        evaluate_expression context node_context argument
        |> extract_exn expected_value_type)
      |> function_ node_context
    in
    evaluate_function_to_call context function_name signature f []
  | Return value_type, [] ->
    let f node_context = function_ node_context |> Function.Type.wrap value_type in
    f
  | Return _, _ :: _ ->
    raise_s [%message "too many arguments" (function_name : Qualified_name.t)]
;;

let run_exn
  { Context.prefixes
  ; functions
  ; variables
  ; non_standard_qname_name_test_ignores_namespace
  }
  root
  expression
  =
  evaluate_expression
    { prefixes
    ; functions
    ; variables
    ; non_standard_qname_name_test_ignores_namespace
    ; root
    }
    { Context_node.node = Root root; trail = []; size = lazy 1; position = 1 }
    expression
;;

module Private = struct
  let evaluate_axis_in_document_order = evaluate_axis_in_document_order
end
