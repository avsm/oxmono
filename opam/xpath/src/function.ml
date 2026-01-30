open! Core

module Type = struct
  type 'a t =
    | Any : Value.t t
    | Boolean : bool t
    | Number : float t
    | String : string t
    | Node_set : Node.t Trail.Map.t t
  [@@deriving sexp_of]

  let extract (type a) (t : a t) (value : Value.t) : a option =
    match t with
    | Any -> Some value
    | Node_set -> Value.Cast.to_node_set value
    | Boolean -> Value.Cast.to_boolean value |> Some
    | Number -> Value.Cast.to_number value |> Some
    | String -> Value.Cast.to_string value |> Some
  ;;

  let wrap (type a) (t : a t) (value : a) : Value.t =
    match t with
    | Any -> value
    | Boolean -> Value.T (Boolean, value)
    | Number -> Value.T (Number, value)
    | String -> Value.T (String, value)
    | Node_set -> Value.T (Node_set, value)
  ;;
end

module With_count = struct
  type 'a t =
    | Required : 'a Type.t -> 'a t
    | Nullable : 'a Type.t -> 'a option t
    | Variadic : 'a Type.t -> 'a list t

  let sexp_of_t : type a. _ -> a t -> Sexp.t =
    fun _ -> function
    | Required t -> Type.sexp_of_t sexp_of_opaque t
    | Nullable t -> Sexp.List [ Type.sexp_of_t sexp_of_opaque t; Atom "?" ]
    | Variadic t -> Sexp.List [ Type.sexp_of_t sexp_of_opaque t; Atom "*" ]
  ;;
end

module Signature = struct
  type ('a, 'return) t =
    | Return : 'a Type.t -> ('a, 'a) t
    | Abstract : 'a With_count.t * ('b, 'return) t -> ('a -> 'b, 'return) t

  let rec sexp_list_of_t : type a b. (a, b) t -> Sexp.t list = function
    | Return t -> [ Type.sexp_of_t sexp_of_opaque t ]
    | Abstract (t, f) ->
      With_count.sexp_of_t sexp_of_opaque t :: Atom "->" :: sexp_list_of_t f
  ;;

  let sexp_of_t _ _ t = Sexp.List (sexp_list_of_t t)

  module O = struct
    let ( @-> ) type_ rest = Abstract (Required type_, rest)
    let ( @?-> ) type_ rest = Abstract (Nullable type_, rest)
    let ( @*-> ) type_ rest = Abstract (Variadic type_, rest)
    let return type_ = Return type_
  end
end

type t = F : ('a, _) Signature.t * (Context_node.t -> 'a) -> t

let sexp_of_t (F (signature, _)) = [%sexp_of: (_, _) Signature.t] signature
