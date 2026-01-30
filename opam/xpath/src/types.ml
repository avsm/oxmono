open! Core

module Path_kind = struct
  type t =
    | Absolute
    | Relative
  [@@deriving sexp_of, compare, variants]
end

module Equality_op = struct
  type t =
    | Equal
    | Not_equal
  [@@deriving sexp_of, compare, variants]
end

module Relational_op = struct
  type t =
    | Less_than
    | Less_than_or_equal
    | Greater_than
    | Greater_than_or_equal
  [@@deriving sexp_of, compare, variants]
end

module Additive_op = struct
  type t =
    | Add
    | Subtract
  [@@deriving sexp_of, compare, variants]
end

module Multiplicative_op = struct
  type t =
    | Multiply
    | Divide
    | Modulo
  [@@deriving sexp_of, compare, variants]
end

type step =
  { axis : Axis.t
  ; node_test : Node_test.t
  ; predicates : predicate list [@sexp.list]
  }

and predicate = expression

and expression =
  | Or of expression list
  | And of expression list
  | Expression of equality_expression

and equality_expression =
  | Value of relational_expression
  | Operation of Equality_op.t * equality_expression * relational_expression

and relational_expression =
  | Value of additive_expression
  | Operation of Relational_op.t * relational_expression * additive_expression

and additive_expression =
  | Value of multiplicative_expression
  | Operation of Additive_op.t * additive_expression * multiplicative_expression

and multiplicative_expression =
  | Value of unary_expression
  | Operation of Multiplicative_op.t * multiplicative_expression * unary_expression

and unary_expression =
  | Value of union_expression
  | Negation of unary_expression

and union_expression =
  | Value of path_expression
  | Union of union_expression * path_expression

and path_expression =
  | Path of path
  | Filter of filter_expression * relative_path option

and filter_expression = primary_expression * predicate list

and primary_expression =
  | Expression of expression
  | Literal of String_literal.t
  | Variable_reference of Qualified_name.t
  | Number of float
  | Function_call of Qualified_name.t * expression list

and relative_path = step list

and path =
  { path : relative_path
  ; kind : Path_kind.t
  }
[@@deriving sexp_of, compare]

module To_string = struct
  let to_string_with_separator ~sep ~f ~buffer list =
    List.iteri list ~f:(fun i element ->
      if i > 0 then Buffer.add_string buffer sep;
      f element ~buffer)
  ;;

  let rec step_to_string { axis; node_test; predicates } ~buffer ~abbreviate =
    match abbreviate, axis, node_test, predicates with
    | true, Self, Node_type Node, [] -> Buffer.add_string buffer "."
    | true, Parent, Node_type Node, [] -> Buffer.add_string buffer ".."
    | _ ->
      let axis =
        if abbreviate
        then (
          match axis with
          | Axis.Attribute -> "@"
          | Axis.Child -> ""
          | _ -> [%string "%{axis#Axis}::"])
        else [%string "%{axis#Axis}::"]
      in
      Buffer.add_string buffer axis;
      let node_test = Node_test.to_string node_test in
      Buffer.add_string buffer node_test;
      List.iter predicates ~f:(predicate_to_string ~buffer ~abbreviate)

  and predicate_to_string predicate ~buffer ~abbreviate =
    Buffer.add_char buffer '[';
    expression_to_string predicate ~buffer ~abbreviate;
    Buffer.add_char buffer ']'

  and expression_to_string expression ~buffer ~abbreviate =
    match expression with
    | Or expressions ->
      to_string_with_separator
        ~sep:" or "
        ~f:(expression_to_string ~abbreviate)
        expressions
        ~buffer
    | And expressions ->
      to_string_with_separator
        ~sep:" or "
        ~f:(expression_to_string ~abbreviate)
        expressions
        ~buffer
    | Expression equality_expression ->
      equality_expression_to_string equality_expression ~buffer ~abbreviate

  and equality_expression_to_string equality_expression ~buffer ~abbreviate =
    match equality_expression with
    | Value relational_expression ->
      relational_expression_to_string relational_expression ~buffer ~abbreviate
    | Operation (op, left, right) ->
      equality_expression_to_string left ~buffer ~abbreviate;
      Buffer.add_string
        buffer
        (match op with
         | Equal -> " = "
         | Not_equal -> " != ");
      relational_expression_to_string right ~buffer ~abbreviate

  and relational_expression_to_string relational_expression ~buffer ~abbreviate =
    match relational_expression with
    | Value additive_expression ->
      additive_expression_to_string additive_expression ~buffer ~abbreviate
    | Operation (op, left, right) ->
      relational_expression_to_string left ~buffer ~abbreviate;
      Buffer.add_string
        buffer
        (match op with
         | Less_than -> " < "
         | Less_than_or_equal -> " <= "
         | Greater_than -> " > "
         | Greater_than_or_equal -> " >= ");
      additive_expression_to_string right ~buffer ~abbreviate

  and additive_expression_to_string additive_expression ~buffer ~abbreviate =
    match additive_expression with
    | Value multiplicative_expression ->
      multiplicative_expression_to_string multiplicative_expression ~buffer ~abbreviate
    | Operation (op, left, right) ->
      additive_expression_to_string left ~buffer ~abbreviate;
      Buffer.add_string
        buffer
        (match op with
         | Add -> " + "
         | Subtract -> " - ");
      multiplicative_expression_to_string right ~buffer ~abbreviate

  and multiplicative_expression_to_string multiplicative_expression ~buffer ~abbreviate =
    match multiplicative_expression with
    | Value unary_expression ->
      unary_expression_to_string unary_expression ~buffer ~abbreviate
    | Operation (op, left, right) ->
      multiplicative_expression_to_string left ~buffer ~abbreviate;
      Buffer.add_string
        buffer
        (match op with
         | Multiply -> " * "
         | Divide -> " div "
         | Modulo -> " mod ");
      unary_expression_to_string right ~buffer ~abbreviate

  and unary_expression_to_string unary_expression ~buffer ~abbreviate =
    match unary_expression with
    | Value union_expression ->
      union_expression_to_string union_expression ~buffer ~abbreviate
    | Negation unary_expression ->
      Buffer.add_char buffer '-';
      unary_expression_to_string unary_expression ~buffer ~abbreviate

  and union_expression_to_string union_expression ~buffer ~abbreviate =
    match union_expression with
    | Value path_expression ->
      path_expression_to_string path_expression ~buffer ~abbreviate
    | Union (left, right) ->
      union_expression_to_string left ~buffer ~abbreviate;
      Buffer.add_string buffer " | ";
      path_expression_to_string right ~buffer ~abbreviate

  and path_expression_to_string path_expression ~buffer ~abbreviate =
    match path_expression with
    | Path path -> path_to_string path ~buffer ~abbreviate
    | Filter (filter_expression, relative_path) ->
      filter_expression_to_string filter_expression ~buffer ~abbreviate;
      Option.iter relative_path ~f:(fun relative_path ->
        path_to_string { path = relative_path; kind = Absolute } ~buffer ~abbreviate)

  and filter_expression_to_string (primary_expression, predicates) ~buffer ~abbreviate =
    primary_expression_to_string primary_expression ~buffer ~abbreviate;
    List.iter predicates ~f:(predicate_to_string ~buffer ~abbreviate)

  and primary_expression_to_string primary_expression ~buffer ~abbreviate =
    match primary_expression with
    | Expression expression ->
      Buffer.add_char buffer '(';
      expression_to_string expression ~buffer ~abbreviate;
      Buffer.add_char buffer ')'
    | Literal literal ->
      String_literal.to_quoted_string literal |> Buffer.add_string buffer
    | Variable_reference variable_reference ->
      Buffer.add_char buffer '$';
      Buffer.add_string buffer (Qualified_name.to_string variable_reference)
    | Number number -> Float_serializer.serialize number |> Buffer.add_string buffer
    | Function_call (function_name, arguments) ->
      Buffer.add_string buffer (Qualified_name.to_string function_name);
      Buffer.add_char buffer '(';
      to_string_with_separator
        ~sep:", "
        ~f:(expression_to_string ~abbreviate)
        arguments
        ~buffer;
      Buffer.add_char buffer ')'

  and path_to_string { path; kind } ~buffer ~abbreviate =
    (match kind with
     | Absolute -> Buffer.add_string buffer "/"
     | Relative -> ());
    if abbreviate
    then (
      (* Complicated rules: We can use '//' to abbreviate descendant-or-self::node(), but
         not twice in a row. If descendant-or-self::node() cannot be abbreviated if it's
         the last thing, or if it's the first thing in a relative path (you have to do
         .//..., which we shouldn't do in a serialization function).
      *)
      let step_count = List.length path in
      List.foldi path ~init:false ~f:(fun i used_abbreviation step ->
        if i > 0 then Buffer.add_string buffer "/";
        let used_abbreviation =
          match kind, step, used_abbreviation, i < step_count - 1 with
          | ( Absolute
            , { axis = Descendant_or_self; node_test = Node_type Node; predicates = [] }
            , false
            , true ) -> true
          | ( Relative
            , { axis = Descendant_or_self; node_test = Node_type Node; predicates = [] }
            , false
            , true )
            when i > 0 -> true
          | _ ->
            step_to_string ~abbreviate step ~buffer;
            false
        in
        used_abbreviation)
      |> (ignore : bool -> unit))
    else to_string_with_separator ~sep:"/" ~f:(step_to_string ~abbreviate) path ~buffer
  ;;

  module type S = sig
    type t

    val to_string : t -> string
    val to_string_abbreviated : t -> string
  end

  let make_signature (type a) (f : a -> buffer:Buffer.t -> abbreviate:bool -> unit)
    : (module S with type t = a)
    =
    (module struct
      type t = a

      let to_string t =
        let buffer = Buffer.create 128 in
        f t ~buffer ~abbreviate:false;
        Buffer.contents buffer
      ;;

      let to_string_abbreviated t =
        let buffer = Buffer.create 128 in
        f t ~buffer ~abbreviate:true;
        Buffer.contents buffer
      ;;
    end)
  ;;
end

module Step = struct
  include (val To_string.(make_signature step_to_string))

  type t = step =
    { axis : Axis.t
    ; node_test : Node_test.t
    ; predicates : predicate list [@sexp.list]
    }
  [@@deriving sexp_of, compare]
end

module Predicate = struct
  include (val To_string.(make_signature predicate_to_string))

  type t = predicate [@@deriving sexp_of, compare]
end

module Expression = struct
  include (val To_string.(make_signature expression_to_string))

  type t = expression =
    | Or of t list
    | And of t list
    | Expression of equality_expression
  [@@deriving sexp_of, compare, variants]
end

module Equality_expression = struct
  module Op = Equality_op
  include (val To_string.(make_signature equality_expression_to_string))

  type t = equality_expression =
    | Value of relational_expression
    | Operation of Op.t * t * relational_expression
  [@@deriving sexp_of, compare, variants]
end

module Relational_expression = struct
  module Op = Relational_op
  include (val To_string.(make_signature relational_expression_to_string))

  type t = relational_expression =
    | Value of additive_expression
    | Operation of Op.t * t * additive_expression
  [@@deriving sexp_of, compare, variants]
end

module Additive_expression = struct
  module Op = Additive_op
  include (val To_string.(make_signature additive_expression_to_string))

  type t = additive_expression =
    | Value of multiplicative_expression
    | Operation of Op.t * t * multiplicative_expression
  [@@deriving sexp_of, compare, variants]
end

module Multiplicative_expression = struct
  module Op = Multiplicative_op
  include (val To_string.(make_signature multiplicative_expression_to_string))

  type t = multiplicative_expression =
    | Value of unary_expression
    | Operation of Op.t * t * unary_expression
  [@@deriving sexp_of, compare, variants]
end

module Unary_expression = struct
  include (val To_string.(make_signature unary_expression_to_string))

  type t = unary_expression =
    | Value of union_expression
    | Negation of t
  [@@deriving sexp_of, compare, variants]
end

module Union_expression = struct
  include (val To_string.(make_signature union_expression_to_string))

  type t = union_expression =
    | Value of path_expression
    | Union of t * path_expression
  [@@deriving sexp_of, compare, variants]
end

module Path_expression = struct
  include (val To_string.(make_signature path_expression_to_string))

  type t = path_expression =
    | Path of path
    | Filter of filter_expression * relative_path option
  [@@deriving sexp_of, compare, variants]
end

module Filter_expression = struct
  include (val To_string.(make_signature filter_expression_to_string))

  type t = filter_expression [@@deriving sexp_of, compare]
end

module Primary_expression = struct
  include (val To_string.(make_signature primary_expression_to_string))

  type t = primary_expression =
    | Expression of expression
    | Literal of String_literal.t
    | Variable_reference of Qualified_name.t
    | Number of float
    | Function_call of Qualified_name.t * expression list
  [@@deriving sexp_of, compare, variants]
end

module Relative_path = struct
  type t = step list [@@deriving sexp_of, compare]
end

module Path = struct
  module Kind = Path_kind
  include (val To_string.(make_signature path_to_string))

  type t = path =
    { path : relative_path
    ; kind : Path_kind.t
    }
  [@@deriving sexp_of, compare]
end
