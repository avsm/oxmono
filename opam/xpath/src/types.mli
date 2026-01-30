open! Core

module Path_kind : sig
  type t =
    | Absolute
    | Relative
  [@@deriving sexp_of, compare, variants]
end

module Equality_op : sig
  type t =
    | Equal
    | Not_equal
  [@@deriving sexp_of, compare, variants]
end

module Relational_op : sig
  type t =
    | Less_than
    | Less_than_or_equal
    | Greater_than
    | Greater_than_or_equal
  [@@deriving sexp_of, compare, variants]
end

module Additive_op : sig
  type t =
    | Add
    | Subtract
  [@@deriving sexp_of, compare, variants]
end

module Multiplicative_op : sig
  type t =
    | Multiply
    | Divide
    | Modulo
  [@@deriving sexp_of, compare, variants]
end

type step =
  { axis : Axis.t
  ; node_test : Node_test.t
  ; predicates : predicate list
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

module To_string : sig
  module type S = sig
    type t

    val to_string : t -> string
    val to_string_abbreviated : t -> string
  end
end

module Step : sig
  type t = step =
    { axis : Axis.t
    ; node_test : Node_test.t
    ; predicates : predicate list
    }
  [@@deriving sexp_of, compare]

  include To_string.S with type t := t
end

module Predicate : sig
  type t = predicate [@@deriving sexp_of, compare]

  include To_string.S with type t := t
end

module Expression : sig
  type t = expression =
    | Or of t list
    | And of t list
    | Expression of equality_expression
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Equality_expression : sig
  module Op = Equality_op

  type t = equality_expression =
    | Value of relational_expression
    | Operation of Op.t * t * relational_expression
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Relational_expression : sig
  module Op = Relational_op

  type t = relational_expression =
    | Value of additive_expression
    | Operation of Op.t * t * additive_expression
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Additive_expression : sig
  module Op = Additive_op

  type t = additive_expression =
    | Value of multiplicative_expression
    | Operation of Op.t * t * multiplicative_expression
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Multiplicative_expression : sig
  module Op = Multiplicative_op

  type t = multiplicative_expression =
    | Value of unary_expression
    | Operation of Op.t * t * unary_expression
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Unary_expression : sig
  type t = unary_expression =
    | Value of union_expression
    | Negation of t
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Union_expression : sig
  type t = union_expression =
    | Value of path_expression
    | Union of t * path_expression
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Path_expression : sig
  type t = path_expression =
    | Path of path
    | Filter of filter_expression * relative_path option
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Filter_expression : sig
  type t = filter_expression [@@deriving sexp_of, compare]

  include To_string.S with type t := t
end

module Primary_expression : sig
  type t = primary_expression =
    | Expression of expression
    | Literal of String_literal.t
    | Variable_reference of Qualified_name.t
    | Number of float
    | Function_call of Qualified_name.t * expression list
  [@@deriving sexp_of, compare, variants]

  include To_string.S with type t := t
end

module Relative_path : sig
  type t = step list [@@deriving sexp_of, compare]
end

module Path : sig
  module Kind = Path_kind

  type t = path =
    { path : relative_path
    ; kind : Path_kind.t
    }
  [@@deriving sexp_of, compare]

  include To_string.S with type t := t
end
