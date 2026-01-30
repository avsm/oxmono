open! Core

(** This represents function types and their implementations. See [Predefined_functions]
    module for examples. *)

module Type : sig
  type 'a t =
    | Any : Value.t t
    | Boolean : bool t
    | Number : float t
    | String : string t
    | Node_set : Node.t Trail.Map.t t
  [@@deriving sexp_of]

  val extract : 'a t -> Value.t -> 'a option
  val wrap : 'a t -> 'a -> Value.t
end

module With_count : sig
  type 'a t =
    | Required : 'a Type.t -> 'a t
    | Nullable : 'a Type.t -> 'a option t
    | Variadic : 'a Type.t -> 'a list t
  [@@deriving sexp_of]
end

module Signature : sig
  type ('a, 'return) t =
    | Return : 'a Type.t -> ('a, 'a) t
    | Abstract : 'a With_count.t * ('b, 'return) t -> ('a -> 'b, 'return) t
  [@@deriving sexp_of]

  module O : sig
    val ( @-> ) : 'a Type.t -> ('b, 'c) t -> ('a -> 'b, 'c) t
    val ( @?-> ) : 'a Type.t -> ('b, 'c) t -> ('a option -> 'b, 'c) t
    val ( @*-> ) : 'a Type.t -> ('b, 'c) t -> ('a list -> 'b, 'c) t
    val return : 'a Type.t -> ('a, 'a) t
  end
end

type t = F : ('a, 'b) Signature.t * (Context_node.t -> 'a) -> t [@@deriving sexp_of]
