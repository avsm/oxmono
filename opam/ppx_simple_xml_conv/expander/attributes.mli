open! Base
open! Ppxlib

module Namespace : sig
  type t =
    | Do_not_care
    | Assert_no_namespace
    | Namespace of expression

  val to_parser : loc:Location.t -> t -> expression
  val to_value : loc:Location.t -> t -> expression

  val of_parameters
    :  loc:Location.t
    -> assert_no_namespace:bool
    -> namespace:expression option
    -> t
end

module Constant_or_variable : sig
  type t =
    | Constant of string
    | Variable of Longident.t
end

(** This reads a leaf xml element for its string contents. *)
module Leaf : sig
  type t =
    { tag : Constant_or_variable.t
    ; namespace : Namespace.t
    ; of_string : expression option
    ; to_string : expression option
    ; ignore_attributes : bool
    ; preserve_space : bool
    }
end

module Empty : sig
  type t =
    { tag : Constant_or_variable.t
    ; namespace : Namespace.t
    ; ignore_children : bool
    ; ignore_attributes : bool
    }
end

module Boolean : sig
  module Values : sig
    type t =
      { true_ : expression
      ; false_ : expression
      }
  end

  type t =
    { tag : Constant_or_variable.t
    ; namespace : Namespace.t
    ; ignore_children : bool
    ; ignore_attributes : bool
    ; values : Values.t
    }
end

(** For use in errors. *)
val content_attribute_name : string

module Core_type : sig
  type t =
    | From_type
    | Leaf of Leaf.t (** This reads the [@xml.leaf] attribute on a core type. *)
    | Empty of Empty.t (** This reads the [@xml.empty] attribute on a core type. *)

  val handle : core_type -> loc:Location.t -> t
end

module Record : sig
  module Element_count : sig
    type t =
      | Required
      | Option
      | List
      | Default of expression
  end

  module Attribute_count : sig
    type t =
      | Required
      | Option
      | Default of expression
  end

  module Attribute_string_fn : sig
    type t =
      | Custom of expression
      | Derive_from_type of core_type
  end

  type t =
    | Element of
        { count : Element_count.t
        ; type_ : core_type
        }
    | Boolean_element of Boolean.t
    | Inlined of core_type
    | Content of core_type
    | Attribute of
        { count : Attribute_count.t
        ; key : Constant_or_variable.t
        ; namespace : Namespace.t
        ; of_string : Attribute_string_fn.t
        ; to_string : Attribute_string_fn.t
        }

  val handle : label_declaration -> loc:Location.t -> t
end

module Variant : sig
  type t =
    | Empty of Empty.t
    | From_type

  val handle : constructor_declaration -> loc:Location.t -> t
end
