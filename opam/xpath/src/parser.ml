open! Core
open! Angstrom
open! Angstrom.Let_syntax

let whitespace = satisfy Char.is_whitespace |> many

let axis =
  let choices =
    (char '@' *> return Axis.Attribute)
    :: List.map Axis.all ~f:(fun axis ->
      string (Axis.to_string axis) *> whitespace *> string "::" *> return axis)
  in
  whitespace *> choice choices ~failure_msg:"expected axis"
;;

let axis_specifier = option Axis.Child axis

let node_type =
  let choices =
    List.map Node_type.all ~f:(fun node_type ->
      string (Node_type.to_string node_type) *> return node_type)
  in
  whitespace *> choice choices ~failure_msg:"expected node type"
;;

module Encoding = struct
  (* Ascii is faster *)
  type t =
    | Utf8
    | Ascii
  [@@deriving sexp_of, compare]
end

module Utf8 = struct
  let trailing =
    let%bind c = any_uint8 in
    if c land 0xc0 = 0x80 then return (c land 0x3f) else fail "invalid utf8 character"
  ;;

  let character =
    let%bind c = any_uint8 in
    match c land 0x80, c land 0xe0, c land 0xf0, c land 0xf8 with
    | 0, _, _, _ -> return c
    | _, 0xc0, _, _ ->
      let%map next = trailing in
      ((c land 0x1f) lsl 6) lor next
    | _, _, 0xe0, _ ->
      let%mapn next1 = trailing
      and next2 = trailing in
      ((c land 0x0f) lsl 12) lor (next1 lsl 6) lor next2
    | _, _, _, 0xf0 ->
      let%mapn next1 = trailing
      and next2 = trailing
      and next3 = trailing in
      ((c land 0x07) lsl 18) lor (next1 lsl 12) lor (next2 lsl 6) lor next3
    | _, _, _, _ -> fail "invalid utf8 character"
  ;;
end

module Name_parser = struct
  module Utf8 = struct
    (* Name start character *)
    (*=[A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF] *)

    (* Name chars additional character *)
    (*=| "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040] *)

    (* We split ascii and non-ascii ranges for speed *)
    let start_ranges_ascii =
      [| Char.to_int 'A', Char.to_int 'Z'
       ; Char.to_int '_', Char.to_int '_'
       ; Char.to_int 'a', Char.to_int 'z'
      |]
    ;;

    let start_ranges_non_ascii =
      [| 0xC0, 0xD6
       ; 0xD8, 0xF6
       ; 0xF8, 0x2FF
       ; 0x370, 0x37D
       ; 0x37F, 0x1FFF
       ; 0x200C, 0x200D
       ; 0x2070, 0x218F
       ; 0x2C00, 0x2FEF
       ; 0x3001, 0xD7FF
       ; 0xF900, 0xFDCF
       ; 0xFDF0, 0xFFFD
       ; 0x10000, 0xEFFFF
      |]
    ;;

    let start_ranges = Array.concat [ start_ranges_ascii; start_ranges_non_ascii ]

    let additional_ranges_ascii =
      [| Char.to_int '-', Char.to_int '-'
       ; Char.to_int '.', Char.to_int '.'
       ; Char.to_int '0', Char.to_int '9'
      |]
    ;;

    let additional_ranges_non_ascii =
      [| 0xB7, 0xB7; 0xB7, 0xB7; 0x0300, 0x036F; 0x203F, 0x2040 |]
    ;;

    let all_ranges =
      Array.concat
        [ start_ranges
        ; additional_ranges_ascii
        ; start_ranges_non_ascii
        ; additional_ranges_non_ascii
        ]
    ;;

    let character ~is_start =
      let%bind c = Utf8.character in
      if is_start
      then
        if Array.exists start_ranges ~f:(fun (low, high) -> Int.between c ~low ~high)
        then return ()
        else fail "invalid name start character"
      else if Array.exists all_ranges ~f:(fun (low, high) -> Int.between c ~low ~high)
      then return ()
      else fail "invalid name character"
    ;;
  end

  module Ascii = struct
    let start_character = function
      | 'A' .. 'Z' | '_' | 'a' .. 'z' -> true
      | _ -> false
    ;;

    let non_start_character = function
      | 'A' .. 'Z' | '_' | 'a' .. 'z' | '-' | '.' | '0' .. '9' -> true
      | _ -> false
    ;;

    let character ~is_start =
      (if is_start then satisfy start_character else satisfy non_start_character)
      *> return ()
    ;;
  end

  let character = function
    | Encoding.Utf8 -> Utf8.character
    | Encoding.Ascii -> Ascii.character
  ;;

  let name encoding =
    consumed
      (character encoding ~is_start:true *> skip_many (character encoding ~is_start:false))
  ;;
end

let qualified_name encoding =
  let name = Name_parser.name encoding in
  let%mapn prefix = option None (map (name <* char ':') ~f:Option.return)
  and name in
  { Qualified_name.prefix; name }
;;

let%expect_test _ =
  let input1 = "foo:bar" in
  let input2 = "baz0" in
  let input3 = "中国" in
  let input4 = "foo:中国" in
  let input5 = "foo:bar:中国" in
  let input6 = "0a" in
  let test input =
    parse_string (qualified_name Utf8) ~consume:All input
    |> [%sexp_of: (Qualified_name.t, string) Result.t]
    |> print_s
  in
  test input1;
  [%expect {| (Ok foo:bar) |}];
  test input2;
  [%expect {| (Ok baz0) |}];
  test input3;
  [%expect {| (Ok "\228\184\173\229\155\189") |}];
  test input4;
  [%expect {| (Ok "foo:\228\184\173\229\155\189") |}];
  test input5;
  [%expect {| (Error ": end_of_input") |}];
  test input6;
  [%expect {| (Error ": invalid name start character") |}]
;;

let name_test encoding =
  let name = Name_parser.name encoding in
  let qualified_name = qualified_name encoding in
  whitespace
  *> choice
       [ char '*' *> return Name_test.Any
       ; name
         <* string ":*"
         |> map ~f:(fun prefix -> Name_test.Any_in_namespace { prefix })
       ; map qualified_name ~f:(fun name -> Name_test.Name name)
       ]
       ~failure_msg:"expected name test"
;;

let literal =
  whitespace
  *> choice
       [ char '"' *> take_while (fun c -> Char.( <> ) c '"') <* char '"'
       ; char '\'' *> take_while (fun c -> Char.( <> ) c '\'') <* char '\''
       ]
       ~failure_msg:"expected literal"
  |> map ~f:String_literal.create_exn
;;

let node_test encoding =
  let name_test = name_test encoding in
  whitespace
  *> choice
       [ node_type
         <* whitespace
         <* char '('
         <* whitespace
         <* char ')'
         |> map ~f:(fun node_type -> Node_test.Node_type node_type)
       ; string "processing-instruction"
         *> whitespace
         *> char '('
         *> whitespace
         *> literal
         <* whitespace
         <* char ')'
         |> map ~f:(fun literal -> Node_test.Processing_instruction literal)
       ; name_test |> map ~f:(fun name_test -> Node_test.Name_test name_test)
       ]
       ~failure_msg:"expected node test"
;;

let number =
  whitespace
  *> (take_while1 Char.is_digit *> option "" (char '.' *> take_while Char.is_digit)
      <|> char '.' *> take_while1 Char.is_digit
      |> Angstrom.consumed
      |> map ~f:Float.of_string)
;;

let abbreviated_any =
  { Types.axis = Descendant_or_self; node_test = Node_type Node; predicates = [] }
;;

let abbreviated_parent =
  { Types.axis = Parent; node_test = Node_type Node; predicates = [] }
;;

let abbreviated_self = { Types.axis = Self; node_test = Node_type Node; predicates = [] }

let left_associative_expression sub_expression ~operator ~base_value ~combine =
  let%mapn first_expression = whitespace *> sub_expression
  and rest = many (both (whitespace *> operator) (whitespace *> sub_expression)) in
  (* This is left associative, so fold is fine. *)
  List.fold rest ~init:(base_value first_expression) ~f:(fun acc (operator, next) ->
    combine operator acc next)
;;

let function_call encoding expression =
  let function_name =
    let%bind qualified_name = qualified_name encoding in
    match qualified_name with
    | { prefix = None; name } ->
      (match Node_type.of_string name with
       | _ -> fail (sprintf "invalid function name: %s" name)
       | exception _ -> return qualified_name)
    | _ -> return qualified_name
  in
  let%mapn name = whitespace *> function_name
  and arguments =
    whitespace
    *> char '('
    *> whitespace
    *> sep_by (whitespace *> char ',' *> whitespace) expression
    <* whitespace
    <* char ')'
  in
  Types.Primary_expression.Function_call (name, arguments)
;;

let primary_expression encoding expression =
  let function_call = function_call encoding expression in
  whitespace
  *> choice
       [ char '$' *> qualified_name encoding
         |> map ~f:Types.Primary_expression.variable_reference
       ; char '(' *> whitespace *> expression
         <* whitespace
         <* char ')'
         |> map ~f:Types.Primary_expression.expression
       ; literal |> map ~f:Types.Primary_expression.literal
       ; number |> map ~f:Types.Primary_expression.number
       ; function_call
       ]
       ~failure_msg:"expected primary expression"
;;

let filter_expression encoding (predicate, expression) =
  let primary_expression = primary_expression encoding expression in
  let%mapn primary_expression = whitespace *> primary_expression
  and predicate = many (whitespace *> predicate) in
  ((primary_expression, predicate) : Types.Filter_expression.t)
;;

let path_expression encoding (any_path, absolute_path, predicate, expression) =
  let filter_expression = filter_expression encoding (predicate, expression) in
  whitespace
  *> choice
       [ (let%mapn filter = filter_expression <* whitespace
          and path = option None (map absolute_path ~f:Option.some) in
          Types.Path_expression.filter filter path)
       ; any_path |> map ~f:Types.Path_expression.path
       ]
       ~failure_msg:"expected path expression"
;;

let union_expression encoding path =
  let path_expression = path_expression encoding path in
  left_associative_expression
    path_expression
    ~operator:(char '|' <* whitespace)
    ~base_value:Types.Union_expression.value
    ~combine:(const Types.Union_expression.union)
;;

let unary_expression encoding path =
  let union_expression = union_expression encoding path in
  fix (fun unary_expression ->
    whitespace
    *> choice
         [ char '-' *> whitespace *> unary_expression
           |> map ~f:Types.Unary_expression.negation
         ; union_expression |> map ~f:Types.Unary_expression.value
         ]
         ~failure_msg:"expected unary expression")
;;

let multiplicative_expression encoding path =
  let unary_expression = unary_expression encoding path in
  let operator =
    choice
      [ char '*' *> return Types.Multiplicative_op.Multiply
      ; string "div" *> return Types.Multiplicative_op.Divide
      ; string "mod" *> return Types.Multiplicative_op.Modulo
      ]
      ~failure_msg:"expected multiplicative operator"
  in
  left_associative_expression
    unary_expression
    ~operator
    ~base_value:Types.Multiplicative_expression.value
    ~combine:Types.Multiplicative_expression.operation
;;

let additive_expression encoding path =
  let multiplicative_expression = multiplicative_expression encoding path in
  let operator =
    choice
      [ char '+' *> return Types.Additive_op.Add
      ; char '-' *> return Types.Additive_op.Subtract
      ]
      ~failure_msg:"expected additive operator"
  in
  left_associative_expression
    multiplicative_expression
    ~operator
    ~base_value:Types.Additive_expression.value
    ~combine:Types.Additive_expression.operation
;;

let relational_expression encoding path =
  let additive_expression = additive_expression encoding path in
  let operator =
    choice
      [ string "<=" *> return Types.Relational_op.Less_than_or_equal
      ; string ">=" *> return Types.Relational_op.Greater_than_or_equal
      ; char '<' *> return Types.Relational_op.Less_than
      ; char '>' *> return Types.Relational_op.Greater_than
      ]
      ~failure_msg:"expected relational operator"
  in
  left_associative_expression
    additive_expression
    ~operator
    ~base_value:Types.Relational_expression.value
    ~combine:Types.Relational_expression.operation
;;

let equality_expression encoding path =
  let relational_expression = relational_expression encoding path in
  let operator =
    choice
      [ string "!=" *> return Types.Equality_op.Not_equal
      ; char '=' *> return Types.Equality_op.Equal
      ]
      ~failure_msg:"expected equality operator"
  in
  left_associative_expression
    relational_expression
    ~operator
    ~base_value:Types.Equality_expression.value
    ~combine:Types.Equality_expression.operation
;;

let predicate expression =
  whitespace *> char '[' *> whitespace *> expression <* whitespace <* char ']'
;;

let step encoding expression =
  let predicate = predicate expression in
  let unabbreviated =
    let%mapn axis = axis_specifier <* whitespace
    and node_test = node_test encoding <* whitespace
    and predicates = many predicate in
    { Types.axis; node_test; predicates }
  in
  whitespace
  *> choice
       [ string ".." *> return abbreviated_parent
       ; char '.' *> return abbreviated_self
       ; unabbreviated
       ]
       ~failure_msg:"expected step"
;;

let relative_path encoding expression =
  let step = step encoding expression in
  Angstrom.fix (fun relative_path ->
    let subsequent_steps =
      choice
        [ string "//" *> whitespace *> relative_path
          |> map ~f:(fun path -> abbreviated_any :: path)
        ; char '/' *> whitespace *> relative_path
        ]
      |> option []
    in
    let%mapn first_step = whitespace *> step
    and subsequent_steps in
    first_step :: subsequent_steps)
  <?> "expected relative path"
;;

let absolute_path encoding expression =
  let relative_path = relative_path encoding expression in
  whitespace
  *> choice
       [ string "//" *> whitespace *> relative_path
         |> map ~f:(fun path -> abbreviated_any :: path)
       ; char '/' *> option [] relative_path
       ]
       ~failure_msg:"expected absolute path"
;;

let path encoding expression =
  let absolute_path = absolute_path encoding expression in
  let relative_path = relative_path encoding expression in
  choice
    [ map absolute_path ~f:(fun path -> { Types.path; kind = Absolute })
    ; map relative_path ~f:(fun path -> { Types.path; kind = Relative })
    ]
;;

let expression encoding =
  Angstrom.fix (fun expression ->
    let any_path = path encoding expression in
    let absolute_path = absolute_path encoding expression in
    let predicate = predicate expression in
    let equality_expression =
      equality_expression encoding (any_path, absolute_path, predicate, expression)
    in
    let and_expression =
      sep_by1 (whitespace *> string "and" *> whitespace) equality_expression
      |> map ~f:(fun expressions ->
        List.map expressions ~f:Types.Expression.expression |> Types.Expression.and_)
    in
    whitespace *> sep_by1 (whitespace *> string "or" *> whitespace) and_expression
    |> map ~f:Types.Expression.or_)
  <* whitespace
;;

let parse_ascii_exn =
  let expression = expression Ascii in
  fun s -> parse_string ~consume:All expression s |> Result.ok_or_failwith
;;

let parse_utf8_exn =
  let expression = expression Utf8 in
  fun s -> parse_string ~consume:All expression s |> Result.ok_or_failwith
;;
