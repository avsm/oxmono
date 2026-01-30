open! Core

type t =
  | Ancestor
  | Ancestor_or_self
  | Attribute
  | Child
  | Descendant
  | Descendant_or_self
  | Following
  | Following_sibling
  | Namespace
  | Parent
  | Preceding
  | Preceding_sibling
  | Self
[@@deriving sexp_of, compare, enumerate, variants, string]
