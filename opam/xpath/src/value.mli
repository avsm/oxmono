open! Core

module Type : sig
  type 'a t =
    | String : string t
    | Number : float t
    | Boolean : bool t
    | Node_set : Node.t Trail.Map.t t
  [@@deriving sexp_of]
end

type t = T : 'a Type.t * 'a -> t [@@deriving sexp_of]

val string : string -> t
val number : float -> t
val boolean : bool -> t
val node_set : Node.t Trail.Map.t -> t

module Cast : sig
  val to_string : t -> string
  val to_number : t -> float
  val to_boolean : t -> bool
  val to_node_set : t -> Node.t Trail.Map.t option
end

module Xpath_comparison : sig
  val equality : t -> t -> op:Types.Equality_expression.Op.t -> bool
  val relational : t -> t -> op:Types.Relational_expression.Op.t -> bool
end

val to_string_hum : ?ns_prefix:(string -> string option) -> t -> string
