open! Ppxlib

(** This modules defines the expander to be registered by the ppx, which takes derivers
    and returns code to add to the file. *)

module Namespace : sig
  type t

  val of_parameters
    :  loc:Location.t
    -> assert_no_namespace:bool
    -> namespace:expression option
    -> t
end

module Parameters : sig
  module Tag_deserializer : sig
    type t =
      { tag : expression
      ; namespace : Namespace.t
      ; allow_extra_elements : bool
      ; allow_extra_attributes : bool
      }
  end

  module Tag_serializer : sig
    type t =
      { tag : expression
      ; namespace : expression option
      ; prefix_declarations : expression option
      }
  end

  type 'tag t =
    | Tag of 'tag
    | Inlined
    | No_parameters

  val create_deserializer
    :  Location.t
    -> tag:expression option
    -> inlined:bool
    -> allow_extra_elements:bool
    -> allow_extra_attributes:bool
    -> namespace:Namespace.t
    -> Tag_deserializer.t t

  val create_serializer
    :  Location.t
    -> tag:expression option
    -> inlined:bool
    -> prefix_declarations:expression option
    -> namespace:expression option
    -> Tag_serializer.t t
end

module Requested : sig
  type t =
    { of_xml : bool
    ; to_xml : bool
    }
end

module Serializer : sig
  (** The implementation side deriver. The deriver should take arguments, which will only
      be used when deriving on records and fail otherwise: a tag for the element to parse,
      and whether to allow extra elements/extra attributes. *)
  val str_type_decl
    :  loc:location
    -> rec_flag * type_declaration list
    -> requested:Requested.t
    -> parameters:Parameters.Tag_serializer.t Parameters.t
    -> structure

  (** The signature side deriver. *)
  val sig_type_decl
    :  loc:location
    -> rec_flag * type_declaration list
    -> inlined:bool
    -> signature_item list
end

module Deserializer : sig
  (** The implementation side deriver. The deriver should take arguments, which will only
      be used when deriving on records and fail otherwise: a tag for the element to parse,
      and whether to allow extra elements/extra attributes. *)
  val str_type_decl
    :  loc:location
    -> rec_flag * type_declaration list
    -> requested:Requested.t
    -> parameters:Parameters.Tag_deserializer.t Parameters.t
    -> structure

  (** The signature side deriver. *)
  val sig_type_decl
    :  loc:location
    -> rec_flag * type_declaration list
    -> inlined:bool
    -> signature_item list
end

module Extensions : sig
  val of_xml_description : core_type -> expression
  val of_xml : core_type -> expression
  val xml_of : core_type -> expression
end

(** This generates an open statement that's required by code that uses the PPX in the
    derivation. *)
val top_level_open : location -> structure
