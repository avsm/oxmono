open! Stdppx
open! Import
open Language.Typed
module Type = Language.Type

module Definitions = struct
  module Poly = struct
    type 'mangle binding = Binding : (_, 'mangle) Binding.with_mangle -> 'mangle binding

    type t =
      | Poly : 'mangle Type.non_tuple Axis.t * 'mangle Type.non_tuple binding list -> t
  end

  module Mono = struct
    type t = Expression.Basic.packed Loc.t list Maybe_explicit.t Axis.Map.t
  end

  type poly_w =
    [ `value_binding
    | `value_description
    | `module_binding
    | `module_declaration
    | `type_declaration
    | `module_type_declaration
    | `include_infos
    ]

  type mono_w =
    [ `expression
    | `module_expr
    | `core_type
    | `module_type
    ]

  type exclave_if_w = [ `expression ]

  type zero_alloc_if_w =
    [ `expression
    | `value_binding
    | `value_description
    ]

  type any_w =
    [ poly_w
    | mono_w
    | zero_alloc_if_w
    ]

  module Context = struct
    type ('a, 'w) t =
      | Expression : (expression, [> `expression ]) t
      | Module_expr : (module_expr, [> `module_expr ]) t
      | Core_type : (core_type, [> `core_type ]) t
      | Module_type : (module_type, [> `module_type ]) t
      | Value_binding : (value_binding, [> `value_binding ]) t
      | Value_description : (value_description, [> `value_description ]) t
      | Module_binding : (module_binding, [> `module_binding ]) t
      | Module_declaration : (module_declaration, [> `module_declaration ]) t
      | Type_declaration : (type_declaration, [> `type_declaration ]) t
      | Module_type_declaration :
          (module_type_declaration, [> `module_type_declaration ]) t
      | Include_infos :
          ((module_expr, module_type) Either.t include_infos, [> `include_infos ]) t

    type 'a poly = ('a, poly_w) t
    type 'a mono = ('a, mono_w) t
    type 'a zero_alloc_if = ('a, zero_alloc_if_w) t
    type 'a any = ('a, any_w) t
  end

  module Exclave_if = struct
    module Reason = struct
      type t =
        | May_return_local
        | May_return_regional
        | Will_return_unboxed
    end

    (** Represents attributes that introduce conditional exclaves into the code. [loc] is
        the location of the attribute's payload. [expr] is the expression (mode, alloc,
        etc.) to compare against to determine whether to apply [[@@exclave_if_*]].
        [reasons] is the list of reasons for applying unconditionally, out of a list of
        pre-defined reasons. *)
    type ('typ, 'reasons) t =
      { expr : ('typ, Expression.singleton) Expression.t Loc.t
      ; reasons : 'reasons
      }
  end

  module Zero_alloc_if = struct
    (** Represents attributes that optionally annotate code as zero-alloc. [loc] is the
        location of the attribute's payload. [expr] is the expression (mode, alloc, etc.)
        to compare against to determine whether to apply [[@@zero_alloc]]. [args] is the
        arguments to be given to the [[@@zero_alloc]] attribute. *)
    type 'a t =
      { loc : location
      ; expr : ('a, Expression.singleton) Expression.t Loc.t
      ; args : expression list
      }
  end
end

module type Attributes = sig
  include module type of struct
    include Definitions
  end

  module Context : sig
    include module type of struct
      include Context
    end

    type 'w packed = private T : (_, 'w) t -> 'w packed [@@unboxed]

    val poly_to_any : 'a poly -> 'a any
    val mono_to_any : 'a mono -> 'a any
    val zero_alloc_if_to_any : 'a zero_alloc_if -> 'a any
    val location : ('a, 'b) t -> 'a -> Location.t
  end

  (** A [('w, 'b) t] is a handler that knows how to consume a particular attribute on ['w]
      syntax items, and produce back ['b] values. Note: if the attribute isn't present,
      the handler will return some default ['b], whether that be [None] (if ['b] is
      [_ option]) or some semantic default for the given ['b]. *)
  type ('w, 'b) t

  (** [consume t ctx item] runs the handler [t] in the context [ctx] on [item]. The
      handler [t] strips its corresponding attributes from [item] in addition to producing
      its output. *)
  val consume : ('w, 'b) t -> ('a, 'w) Context.t -> 'a -> ('a * 'b, Syntax_error.t) result

  (** A map from [('a, 'w) Context.t] to [('a, 'b) Attribute.t]. *)
  module Attribute_map : sig
    type ('w, 'b) t

    val find_exn
      :  ('w, 'b) t
      -> ('a, 'w) Context.t
      -> ('a, 'b) Attribute.t Maybe_explicit.Both.t
  end

  module Poly : module type of struct
    include Poly
  end

  (** A handler for attributes that make definitions/declarations polymorphic. Might
      return an [Error _] if the attribute's payload is malformed. Defaults to
      [Ok { kinds = None; modes = None }]. *)
  val poly : (poly_w, Poly.t Maybe_explicit.t list) t

  module Mono : sig
    include module type of struct
      include Mono
    end

    val contexts : mono_w Context.packed list

    type attr :=
      ( mono_w
        , (Expression.Basic.packed Loc.t list, Syntax_error.t) result )
        Attribute_map.t

    val kind_attr : attr
    val kind_set_attr : attr
    val mode_attr : attr
    val modality_attr : attr
    val alloc_attr : attr
  end

  (** A handler for attributes that mangle identifiers to the correct monomorphized name.
      Defaults to [{ kinds = []; modes = [] }]. *)
  val mono : (mono_w, Mono.t) t

  module Exclave_if : sig
    include module type of struct
      include Exclave_if
    end

    module Reason : sig
      include module type of struct
        include Exclave_if.Reason
      end
    end
  end

  (** A handler for attributes that optionally insert [exclave_] markers. We expect to
      replace these attributes with mode-polymorphic tailcalls and/or unboxed types. *)
  val exclave_if_local
    : (exclave_if_w, (Type.mode, Exclave_if.Reason.t list) Exclave_if.t option) t

  (** Like {!exclave_if_local}, but for allocation identifiers. *)
  val exclave_if_stack : (exclave_if_w, (Type.alloc, unit) Exclave_if.t option) t

  module Zero_alloc_if : sig
    include module type of struct
      include Zero_alloc_if
    end
  end

  (** A handler for attributes that optionally annotate code as zero-alloc. *)
  val zero_alloc_if_local : (zero_alloc_if_w, Type.mode Zero_alloc_if.t option) t

  (** Like {!zero_alloc_if_local}, but for allocation identifiers. *)
  val zero_alloc_if_stack : (zero_alloc_if_w, Type.alloc Zero_alloc_if.t option) t

  val with_ : ([ `module_type ], signature option) t
  val with_attr : (module_type, (signature, Syntax_error.t) result) Attribute.t

  val error_you_can_only_use_one_attribute_per_axis
    :  loc:location
    -> (_, Syntax_error.t) result

  module Floating : sig
    type poly :=
      [ `structure_item
      | `signature_item
      ]

    module Context : sig
      type ('a, 'w) t =
        | Structure_item : (structure_item, [> `structure_item ]) t
        | Signature_item : (signature_item, [> `signature_item ]) t

      type nonrec 'a poly = ('a, poly) t

      val location : ('a, 'b) t -> 'a -> Location.t
    end

    module Define : sig
      type t = Define : 'a Type.non_tuple Binding.t list -> t
    end

    module Poly : sig
      type kind =
        | Never_add_mangler
        | Always_add_mangler
        | Add_mangler_if_more_than_one_elt
        (** Behaves like [Always_add_mangler] if the set on the RHS of the binding, when
            evaluated with [Expand_atoms_bound_to_sets], has more than one element, and
            like [Never_add_mangler] otherwise.

            For [Add_mangler_if_more_than_one_elt] specifically, we only permit exactly
            one [(_, singleton) Expression.t] on the RHS. Loosely speaking, if the
            expression on the RHS contains a [Union], it is probably true that it always
            evaluates to a set with more than one element, and the [Always_add_mangler]
            version of the attribute should be used instead. *)

      type t =
        { bindings : Poly.t
        ; kind : kind
        }

      (** Check if the provided ast node is a floating poly template attribute. Does not
          mark the attribute as seen. *)
      val is_present : ('a, poly) Context.t -> 'a -> bool
    end

    type t =
      | Define of Define.t
      | Poly of Poly.t Maybe_explicit.t

    (** Check if the provided ast node is a floating template attribute, and evaluate its
        contents. Marks the attribute as seen. Returns an [Error _] if the payload of the
        attribute is malformed or inconsistent. *)
    val convert : ('a, poly) Context.t -> 'a -> (t option, Syntax_error.t) result
  end
end
