open! Core
module Xml := Simple_xml

(* We do not support comments or processing instructions *)
type t =
  | Root of Xml.element
  | Element of Xml.element
  | Attribute of Xml.Attribute.t
  | Namespace of
      { prefix : string option
      ; value : string
      }
  | Text of string
[@@deriving sexp_of, variants]

(** Find the prefix definition for a given namespace for an element. *)
val find_prefix_definition
  :  namespace:string
  -> t
  -> [ `Default | `Prefixed of string ] option

(** Find the language (xml:lang attribute) for an element. *)
val lang : t -> string option

(** Find the xpath string value for a node. *)
val string_value : t -> string

(** Find the xpath expanded name for a node if it exists. *)
val expanded_name : t -> Expanded_name.t option

val to_string_hum : ?ns_prefix:(string -> string option) -> t -> string
