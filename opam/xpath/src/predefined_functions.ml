open! Core

let last { Context_node.size; _ } = Lazy.force size |> Float.of_int
let position { Context_node.position; _ } = Float.of_int position
let count _ node_set = Float.of_int (Map.length node_set)

(* We don't support DID ids yet *)
let id _ _ = Trail.Map.empty

let local_name { Context_node.node; _ } node_set =
  let node =
    Option.bind node_set ~f:(fun node_set -> Map.min_elt node_set)
    |> Option.value_map ~default:node ~f:snd
  in
  match Node.expanded_name node with
  | None -> ""
  | Some name -> name.name
;;

let namespace_uri { Context_node.node; _ } node_set =
  let node =
    Option.bind node_set ~f:(fun node_set -> Map.min_elt node_set)
    |> Option.value_map ~default:node ~f:snd
  in
  match Node.expanded_name node with
  | None -> ""
  | Some name -> Option.value name.namespace ~default:""
;;

let name { Context_node.node; trail; _ } node_set =
  let trail, node =
    Option.bind node_set ~f:(fun node_set -> Map.min_elt node_set)
    |> Option.value ~default:(trail, node)
  in
  match Node.expanded_name node with
  | None -> ""
  | Some name ->
    let qualified_name : Qualified_name.t =
      match name.namespace with
      | None -> { prefix = None; name = name.name }
      | Some namespace ->
        let nodes_to_search =
          Sequence.append
            (Sequence.singleton node)
            (Sequence.of_list trail |> Sequence.map ~f:snd)
        in
        (match
           Sequence.find_map nodes_to_search ~f:(Node.find_prefix_definition ~namespace)
         with
         | None ->
           raise_s
             [%message
               "Could not find qualified name for node" (node : Node.t) (trail : Trail.t)]
         | Some `Default -> { prefix = None; name = name.name }
         | Some (`Prefixed prefix) -> { prefix = Some prefix; name = name.name })
    in
    Qualified_name.to_string qualified_name
;;

let string { Context_node.node; trail; _ } value =
  let value =
    match value with
    | None -> Value.node_set (Trail.Map.singleton trail node)
    | Some value -> value
  in
  Value.Cast.to_string value
;;

let concat _ a b c = String.concat (a :: b :: c)
let starts_with _ s prefix = String.is_prefix ~prefix s
let contains _ s substring = String.is_substring ~substring s

let substring_before _ s pattern =
  match String.substr_index s ~pattern with
  | None -> ""
  | Some index -> String.subo s ~len:index
;;

let substring_after _ s pattern =
  match String.substr_index s ~pattern with
  | None -> ""
  | Some index -> String.subo s ~pos:(index + String.length pattern)
;;

let substring _ s start len =
  match Float.round_nearest start |> Int.of_float |> Int.pred with
  | exception _ -> ""
  | start ->
    let len =
      match len with
      | None -> None
      | Some len ->
        (match Float.round_nearest len |> Int.of_float with
         | len -> Some (Int.max 0 len)
         | exception _ ->
           (match Float.classify len with
            | Infinite when Float.is_positive len -> None
            | _ -> Some 0))
    in
    let pos, len =
      if start >= 0
      then start, len
      else (
        match len with
        | None -> 0, None
        | Some len -> 0, Some (Int.max 0 (start + len)))
    in
    String.subo s ~pos ?len
;;

(* These are provided in the standard. *)
let%expect_test "substring unusual cases" =
  let unusual_cases =
    [ 1.5, 2.6
    ; 0., 3.
    ; Float.nan, 3.
    ; 1., Float.nan
    ; -42., 1. /. 0.
    ; -1. /. 0., 1. /. 0.
    ]
  in
  List.iter unusual_cases ~f:(fun (start, len) ->
    let s = "12345" in
    let result = substring () s start (Some len) in
    print_s [%message (result : string) (start : float) (len : float)]);
  [%expect
    {|
    ((result 234) (start 1.5) (len 2.6))
    ((result 12) (start 0) (len 3))
    ((result "") (start NAN) (len 3))
    ((result "") (start 1) (len NAN))
    ((result 12345) (start -42) (len INF))
    ((result "") (start -INF) (len INF))
    |}]
;;

let string_length { Context_node.node; _ } value =
  let value =
    match value with
    | None -> Node.string_value node
    | Some value -> value
  in
  String.length value |> Float.of_int
;;

let normalize_space { Context_node.node; _ } value =
  let value =
    match value with
    | None -> Node.string_value node
    | Some value -> value
  in
  let stripped = String.strip value in
  let buffer = Buffer.create (String.length stripped) in
  String.fold stripped ~init:false ~f:(fun added_whitespace c ->
    if Char.is_whitespace c
    then (
      if not added_whitespace then Buffer.add_char buffer ' ';
      true)
    else (
      Buffer.add_char buffer c;
      false))
  |> (ignore : bool -> unit);
  Buffer.contents buffer
;;

let translate _ s from to_ =
  let mapping =
    Sequence.zip (String.to_sequence from) (String.to_sequence to_)
    |> Char.Map.of_sequence_reduce ~f:(fun a _ -> a)
  in
  String.map s ~f:(fun c -> Map.find mapping c |> Option.value ~default:c)
;;

let boolean _ value = Value.Cast.to_boolean value
let not_ _ value = not value
let true_ _ = true
let false_ _ = false

let lang { Context_node.node; trail; _ } lang =
  let nodes_to_search =
    Sequence.append
      (Sequence.singleton node)
      (Sequence.of_list trail |> Sequence.map ~f:snd)
  in
  Sequence.find_map nodes_to_search ~f:(fun node ->
    let%map.Option node_lang = Node.lang node in
    String.equal node_lang lang)
  |> Option.value ~default:false
;;

let number { Context_node.node; trail; _ } value =
  let value =
    match value with
    | None -> Value.node_set (Trail.Map.singleton trail node)
    | Some value -> value
  in
  Value.Cast.to_number value
;;

let sum _ node_set =
  Map.sum (module Float) node_set ~f:(fun node ->
    Node.string_value node |> Value.string |> Value.Cast.to_number)
;;

let floor _ value = Float.round_down value
let ceiling _ value = Float.round_up value
let round _ value = Float.round_nearest value

let wrap name signature implementation =
  { Expanded_name.namespace = None; name }, Function.F (signature, implementation)
;;

let functions =
  let open Function.Signature.O in
  [ wrap "last" (return Number) last
  ; wrap "position" (return Number) position
  ; wrap "count" (Node_set @-> return Number) count
  ; wrap "id" (Any @-> return Node_set) id
  ; wrap "local-name" (Node_set @?-> return String) local_name
  ; wrap "namespace-uri" (Node_set @?-> return String) namespace_uri
  ; wrap "name" (Node_set @?-> return String) name
  ; wrap "string" (Any @?-> return String) string
  ; wrap "concat" (String @-> String @-> String @*-> return String) concat
  ; wrap "starts-with" (String @-> String @-> return Boolean) starts_with
  ; wrap "contains" (String @-> String @-> return Boolean) contains
  ; wrap "substring-before" (String @-> String @-> return String) substring_before
  ; wrap "substring-after" (String @-> String @-> return String) substring_after
  ; wrap "substring" (String @-> Number @-> Number @?-> return String) substring
  ; wrap "string-length" (String @?-> return Number) string_length
  ; wrap "normalize-space" (String @?-> return String) normalize_space
  ; wrap "translate" (String @-> String @-> String @-> return String) translate
  ; wrap "boolean" (Any @-> return Boolean) boolean
  ; wrap "not" (Boolean @-> return Boolean) not_
  ; wrap "true" (return Boolean) true_
  ; wrap "false" (return Boolean) false_
  ; wrap "lang" (String @-> return Boolean) lang
  ; wrap "number" (Any @?-> return Number) number
  ; wrap "sum" (Node_set @-> return Number) sum
  ; wrap "floor" (Number @-> return Number) floor
  ; wrap "ceiling" (Number @-> return Number) ceiling
  ; wrap "round" (Number @-> return Number) round
  ]
  |> Expanded_name.Map.of_alist_exn
;;
