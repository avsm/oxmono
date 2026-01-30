open! Ppxlib

type t =
  | Base
  | Deriving of core_type

(** If your context is [Base], return the label as an expression. Otherwise prepend with
    [Base.Array]. *)
val runtime_fun : t -> location -> label -> expression

val how_to_vary_kinds
  :  t
  -> input:expression
  -> output:expression option
  -> How_to_vary_kinds.t
