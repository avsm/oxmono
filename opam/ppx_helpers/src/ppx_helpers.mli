open! Stdppx
open Ppxlib
module Ox = Ox

(** Common utility functions for PPXs that we may eventually upstream to ppxlib *)

(** Ast-mapper that ghosts all locations *)
val ghoster : Ast_traverse.map

(** Utilities for manipulating doc comments *)

module Docs : sig
  type t =
    | Toggle (** [(**/**)] *)
    | Inline (** [(** @inline *)] *)
    | Doc of string (** [(** <doc> *)] *)

  (** Check whether this attribute is a doc comment and characterize it if so *)
  val of_attribute : attribute -> t option

  (** [hide] surrounds a list of signature items with the [(**/**)] comment, which toggles
      odoc generation. *)
  val hide : loc:Location.t -> signature_item list -> signature_item list

  (** [simplify] removes consecutive pairs of [(**/**)] attributes in a signature *)
  val simplify : signature_item list -> signature_item list
end

(** [demangle_template] separates an identifier on the first ["__"], which by our
    conventions corresponds to the mangling suffix added by [ppx_template]. Other ppxs use
    this to recognize names mangled by [ppx_template] and treat them specially.

    If the identifier is a hash name (like [t__bits64#]), the hash is mangled with
    [mangle_unboxed] first. For example, [demangle_template "t__bits64#"] is
    ["t_u", "__bits64"]. *)
val demangle_template : string -> string * string

(** [type_constr_conv_expr] is the standard way to map (long) identifiers to conversion
    functions, for preprocessors that creates values that follow the structure of types.

    [f] produces the last part of a long identifier (the part after the module path) for
    the conversion function, given the name of the functor module in the type (if there is
    one) and the type name itself.

    For example, the arguments to [f] are:
    - [None, "t"] if the type is [t]
    - [None, "foo"] if the type is [A.B.foo]
    - [Some "f", "foo"] if the type is [A.B.F(A1)(A2).foo]

    [type_constr_conv_expr] assembles the module path from the type, the conversion
    function basename produced by [f], any modules passed as arguments to the functor in
    the type, and the list of additional expression arguments into an expression
    representing the fully-applied conversion function call.

    For example,
    {[
      type_constr_conv_expr
        ~loc
        lident
        (fun ?functor_ t ->
          sprintf
            "sexp_of_%s%s"
            (Option.value_map functor_ ~f:(fun functor_ -> functor_ ^ "__") ~default:"")
            t)
        [ [%expr arg] ]
    ]}
    is:

    - [sexp_of_t arg] if [lident] is [t]
    - [A.B.sexp_of_foo arg] if [lident] is [A.B.foo]
    - [A.B.sexp_of_f__foo (module A1) (module A2) arg] if [lident] is [A.B.F(A1)(A2).foo] *)
val type_constr_conv_expr
  :  loc:Location.t (** [loc] to use for the application expression *)
  -> Longident.t loc (** Identifier of type for which to generate a conversion *)
  -> f:(?functor_:string -> string -> string)
       (** How to build up the conversion function name *)
  -> expression list (** Additional arguments to the conversion *)
  -> expression

(** Like [type_constr_conv_expr], but generates a [pattern] for defining a type
    constructor conversion function. [type_constr_conv_pat] only works on that are a
    simple identifier, without any dots or functors. This is because e.g. [sexp_of_t] is a
    valid pattern, but [A.B.sexp_of_foo] is not a pattern. *)
val type_constr_conv_pat
  :  loc:Location.t
  -> Longident.t loc
  -> f:(?functor_:string -> string -> string)
  -> pattern

(** Given a typename, determine if it is implicit unboxed (e.g. [t#]) *)
val is_implicit_unboxed : String.t -> bool

(** Converts implicit unboxed type names (e.g. [t#]) into names that can be used as
    identifiers (e.g. [t_u]). Leaves all other type names alone.

    If the identifier was mangled by ppx_template, the unboxed mangling happens on the
    prefix of the identifier. For example, [mangle_unboxed "t__bits64#"] is "t_u__bits64". *)
val mangle_unboxed : String.t -> String.t

(** Reports whether a type has the [[@@ocaml.unboxed]] or [[@@unboxed]] attribute. *)
val has_unboxed_attribute : type_declaration -> bool

(** Returns the implicit unboxed version of a type declaration. Produces [None] if a type
    declaration doesn't have an implicit unboxed version. *)
val implicit_unboxed_record : type_declaration -> type_declaration option

(** For derivers that take an [~unboxed] flag. If [unboxed], each [td] is followed by
    [implicit_unboxed_record td]. If [not unboxed], returns the input unchanged.

    Raises if [unboxed] but no type has an implicit unboxed version. *)
val with_implicit_unboxed_records
  :  loc:Location.t
  -> unboxed:bool
  -> type_declaration list
  -> type_declaration list

module Polytype : sig
  type t =
    { loc : Location.t
    ; vars : (string loc * Ppxlib_jane.jkind_annotation option) list
    ; body : core_type
    }

  val to_core_type : ?universally_quantify_only_if_jkind_annotation:bool -> t -> core_type
end

(** A wrapper over [Ppxlib.combinator_type_of_type_declaration] that makes it harder to
    forget to universally-quantify the type variables and supports type parameters being
    marked as phantom.

    If [phantom_attribute] is provided, then parameters with that attribute do not appear
    in the combinator type. *)
val combinator_type_of_type_declaration
  :  ?phantom_attr:(core_type, unit) Attribute.t
  -> type_declaration
  -> f:(loc:Location.t -> core_type -> core_type)
  -> Polytype.t
