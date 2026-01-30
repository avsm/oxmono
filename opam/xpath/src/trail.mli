open! Core

module Specifier : sig
  type t =
    | Default_namespace
    | Prefixed_namespace of string
    | Attribute of int
    | Element_child of int
  [@@deriving sexp_of, compare]

  include Comparable.S_plain with type t := t
end

(** This is the trail from the current node to the root node. The comparison ensures that
    it is done in xpath document order.

    Note that the comparison does not include the node, and it is *not* safe to mix path
    comparisons from different source xml documents. *)
type t = (Specifier.t * Node.t) list [@@deriving sexp_of, compare]

include Comparable.S_plain with type t := t
