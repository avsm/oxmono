open! Core

module Type = struct
  type 'a t =
    | String : string t
    | Number : float t
    | Boolean : bool t
    | Node_set : Node.t Trail.Map.t t
  [@@deriving sexp_of]
end

type t = T : 'a Type.t * 'a -> t

let sexp_of_t (T (value_type, value)) =
  match value_type with
  | String -> [%message "String" ~_:(value : string)]
  | Number -> [%message "Number" ~_:(value : float)]
  | Boolean -> [%message "Boolean" ~_:(value : bool)]
  | Node_set -> [%message "Node_set" ~_:(value : Node.t Trail.Map.t)]
;;

let boolean value = T (Boolean, value)
let number value = T (Number, value)
let string value = T (String, value)
let node_set value = T (Node_set, value)

let string_to_number value =
  let value = String.strip value in
  if String.for_all value ~f:(function
       | '0' .. '9' | '.' | '-' -> true
       | _ -> false)
  then Float.of_string value
  else Float.nan
;;

module Cast = struct
  let to_node_set (T (value_type, value)) : Node.t Trail.Map.t option =
    match value_type with
    | Node_set -> Some value
    | _ -> None
  ;;

  (* boolean function per spec *)
  let to_boolean (T (value_type, value)) : bool =
    match value_type with
    | Boolean -> value
    | Number ->
      (match Float.classify value with
       | Zero | Nan -> false
       | _ -> true)
    | String -> not (String.is_empty value)
    | Node_set -> not (Map.is_empty value)
  ;;

  let to_string (T (value_type, value)) : string =
    match value_type with
    | String -> value
    | Number -> Float_serializer.serialize value
    | Boolean -> if value then "true" else "false"
    | Node_set ->
      Map.min_elt value
      |> Option.value_map ~f:(fun (_, node) -> Node.string_value node) ~default:""
  ;;

  let to_number (T (value_type, value)) : float =
    match value_type with
    | Number -> value
    | String -> string_to_number value
    | Boolean -> Float.of_int (if value then 1 else 0)
    | Node_set -> Float.of_int (Map.length value)
  ;;
end

let node_set_string_set set =
  Map.to_sequence set
  |> Sequence.map ~f:(fun (_, node) -> Node.string_value node)
  |> String.Set.of_sequence
;;

module Xpath_comparison = struct
  let equality (T (type1, value1)) (T (type2, value2)) ~(op : Types.Equality_op.t) =
    let f =
      match op with
      | Equal -> Fn.id
      | Not_equal -> not
    in
    let node_set_equal_number set number =
      Map.exists set ~f:(fun node ->
        Float.equal (Node.string_value node |> string_to_number) number |> f)
    in
    let node_set_equal_string set string =
      Map.exists set ~f:(fun node -> String.equal (Node.string_value node) string |> f)
    in
    match type1, type2 with
    | Node_set, Node_set ->
      let smallest_set, largest_set =
        if Map.length value1 < Map.length value2 then value1, value2 else value2, value1
      in
      let smallest_set_string_values = node_set_string_set smallest_set in
      Map.exists largest_set ~f:(fun node ->
        Set.mem smallest_set_string_values (Node.string_value node) |> f)
    | Node_set, Number -> node_set_equal_number value1 value2
    | Number, Node_set -> node_set_equal_number value2 value1
    | Node_set, String -> node_set_equal_string value1 value2
    | String, Node_set -> node_set_equal_string value2 value1
    | Boolean, _ | _, Boolean ->
      Bool.equal
        (Cast.to_boolean (T (type1, value1)))
        (Cast.to_boolean (T (type2, value2)))
      |> f
    | Number, _ | _, Number ->
      Float.equal
        (Cast.to_number (T (type1, value1)))
        (Cast.to_number (T (type2, value2)))
      |> f
    | String, String -> String.equal value1 value2 |> f
  ;;

  let relational (T (type1, value1)) (T (type2, value2)) ~(op : Types.Relational_op.t) =
    let node_set_min_and_max set =
      Map.to_sequence set
      |> Sequence.fold ~init:(Float.nan, Float.nan) ~f:(fun (min, max) (_, node) ->
        let value = Node.string_value node |> string_to_number in
        let min = if Float.is_nan min then value else Float.min min value in
        let max = if Float.is_nan max then value else Float.max max value in
        min, max)
    in
    let compare_node_set_and_value (op : Types.Relational_op.t) set other_value =
      let set_min, set_max = node_set_min_and_max set in
      let other_value = Cast.to_number other_value in
      match op with
      | Less_than -> Float.(set_min < other_value)
      | Less_than_or_equal -> Float.(set_min <= other_value)
      | Greater_than -> Float.(set_max > other_value)
      | Greater_than_or_equal -> Float.(set_max >= other_value)
    in
    match type1, type2 with
    | Node_set, Node_set ->
      let min1, max1 = node_set_min_and_max value1 in
      let min2, max2 = node_set_min_and_max value2 in
      (* Nan will always be false for these functions. if operation is <, then we care
         about min1 < max2. if operation is >, then we care about max1 > min2. Same things
         apply if equality is included.
      *)
      (match op with
       | Less_than -> Float.(min1 < max2)
       | Less_than_or_equal -> Float.(min1 <= max2)
       | Greater_than -> Float.(max1 > min2)
       | Greater_than_or_equal -> Float.(max1 >= min2))
    | Node_set, _ -> compare_node_set_and_value op value1 (T (type2, value2))
    | _, Node_set ->
      (* Symmetric inverse *)
      let op : Types.Relational_op.t =
        match op with
        | Less_than -> Greater_than
        | Less_than_or_equal -> Greater_than_or_equal
        | Greater_than -> Less_than
        | Greater_than_or_equal -> Less_than_or_equal
      in
      compare_node_set_and_value op value2 (T (type1, value1))
    | _, _ ->
      let f =
        match op with
        | Less_than -> Float.( < )
        | Less_than_or_equal -> Float.( <= )
        | Greater_than -> Float.( > )
        | Greater_than_or_equal -> Float.( >= )
      in
      f (Cast.to_number (T (type1, value1))) (Cast.to_number (T (type2, value2)))
  ;;
end

let to_string_hum ?ns_prefix (T (value_type, value)) =
  match value_type with
  | String -> [%string "String: %{value}"]
  | Number -> [%string "Number: %{Float_serializer.serialize value}"]
  | Boolean -> [%string "Boolean: %{value#Bool}"]
  | Node_set ->
    let value =
      Map.to_alist value
      |> List.map ~f:(fun (_, node) -> Node.to_string_hum ?ns_prefix node)
    in
    String.concat_lines ("Node_set:" :: value)
;;
