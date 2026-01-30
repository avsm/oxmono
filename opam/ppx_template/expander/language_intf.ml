open! Stdppx
open! Import

(** This module defines the underlying DSL of [ppx_template]. Below, we provide an
    overview of the semantics of the DSL, as well as a grammar for how the surface syntax
    corresponds to the DSL.

    While inside of a [[%template]] node, an environment ([Env.t]) is accumulated that
    maps identifiers ([Identifier.t]) to values ([Value.t]). Each identifier and value is
    associated with a type ([Type.t]) and identifiers are namespaced by their type.

    The environment is updated by two types of attributes:

    1. poly-attributes; a poly-attribute is either:
       a. floating (in which case it updates the environment for all subsequent items in
          the current structure/signature), e.g. [[@@@kind k = bits64]] or
       b. attached to an item (in which case it updates the environment for the item it is
          directly attached to), e.g. [[@@kind k = bits64]]

    2. define-attributes; a define attribute is always floating attribute, and is used to
       bind a set in the namespace [[@@@kind_set.define ks = (value, bits64)]]

    Poly-attributes and define-attributes both consist of a list of bindings; if there are
    multiple such attributes, the lists of bindings can conceptually be concatenated to
    form a single list of bindings. Bindings are evaluated sequentially. An individual
    binding maps a pattern ([Pattern.t]) to a single expression ([Expression.t]). For a
    pattern of type ['a], the associated expression evaluates to a set of values
    ([Value.t]) of type ['a].
    - For a poly-attribute, evaluation is split into a branch for each value in the set.
      The value is combined with the pattern to form new (identifier x expression) entries
      which are added to the environment for the branch.
    - For a define-attribute, no branching occurs; rather, the whole set is bound to the
      name in the environment.

    Note the distinction between:
    - [[@@@kind_set ks = (value, bits64)]], which is a poly-attribute that causes the
      following code to be templated with the two singleton sets [ks = value] and
      [ks = bits64] and
    - [[@@@kind_set.define ks = (value, bits64)]], which is a define-attribute and binds
      the name [ks] to the set [value, bits64] once.

    The environment is used to update the OCaml AST in two ways:

    1) Each type is associated with a node in the OCaml AST (via [Node.t]), and when such
       a node is reached within a [[%template]] body, any OCaml identifiers within that
       node will be looked up in the current environment and, if found, replaced with
       their associated value.

    2) If a mono-attribute is encountered (a [[@kind]], [[@mode]], [[@modality]], or
       [[@alloc]] attribute attached to an identifier), its payload (a list of
       expressions) is evaluated in the current environment, and is used to mangle the
       attached identifier. If there are multiple mono-attributes, the lists of
       expressions are concatenated. *)

module Definitions = struct
  module Type = struct
    (*_ Note: we only provide concrete definitions for these types so that the compiler
        knows they are distinct. They are [private] so that they can only ever be used as
        phantom types. *)
    type kind_ = private Kind
    type mode_ = private Mode
    type modality_ = private Modality
    type alloc_ = private Alloc
    type synchro_ = private Synchro

    type _ non_tuple =
      | Kind : kind_ non_tuple
      | Mode : mode_ non_tuple
      | Modality : modality_ non_tuple
      | Alloc : alloc_ non_tuple
      | Synchro : synchro_ non_tuple

    type kind = kind_ non_tuple
    type mode = mode_ non_tuple
    type modality = modality_ non_tuple
    type alloc = alloc_ non_tuple
    type synchro = synchro_ non_tuple

    type _ t =
      | Non_tuple : 'a non_tuple -> 'a non_tuple t
      | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

    and _ tuple =
      | [] : unit tuple
      | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

    type packed = P : 'a t -> packed [@@unboxed]

    module Non_tuple = struct
      type packed = P : 'a non_tuple t -> packed [@@unboxed]
    end
  end

  module Untyped = struct
    module Axis = struct
      type t =
        | Kind
        | Mode
        | Modality
        | Alloc
        | Synchro
        | Set of t
    end

    module Identifier = struct
      type t = { ident : string }
    end

    module Expression = struct
      type t =
        | Identifier of Identifier.t
        | Kind_product of t Nonempty_list.t
        | Kind_mod of t * t Nonempty_list.t
        | Kind_coercion of t * t
        | Comma_separated of t Nonempty_list.t
        | Typed of t * Type.packed
    end

    module Value = struct
      type t =
        | Identifier of Identifier.t
        | Kind_product of t Nonempty_list.t
        | Kind_mod of t * t Nonempty_list.t
        | Tuple of t Nonempty_list.t
    end

    module Pattern = struct
      type t =
        | Wildcard
        | Identifier of Identifier.t
        | Tuple of t Nonempty_list.t
    end
  end

  module Typed = struct
    module Axis = struct
      type _ singleton =
        | Kind : Type.kind singleton
        | Mode : Type.mode singleton
        | Modality : Type.modality singleton
        | Alloc : Type.alloc singleton
        | Synchro : Type.synchro singleton

      type _ t =
        | Singleton : 'a singleton -> 'a t
        | Set : 'a singleton -> 'a t

      type packed = P : _ t -> packed [@@unboxed]

      module Sub_axis = struct
        module Modal = struct
          type t =
            | Locality
            | Portability
            | Contention
            | Statefulness
            | Visibility
            | Linearity
            | Uniqueness
            | Yielding
            | Forkable
        end

        module Mode = struct
          type t = Mode of Modal.t [@@unboxed]
        end

        module Modality = struct
          type t = Modality of Modal.t [@@unboxed]
        end

        module Or_unrecognized = struct
          type 'a t =
            | Known of 'a
            | Unrecognized
        end

        type _ t =
          | Kind : Type.kind t
          | Mode : Mode.t Or_unrecognized.t -> Type.mode t
          | Modality : Modality.t Or_unrecognized.t -> Type.modality t
          | Alloc : Type.alloc t
          | Synchro : Type.synchro t

        type packed = P : _ t -> packed [@@unboxed]
      end

      module Namespace = struct
        type _ t =
          | Singleton : 'a Sub_axis.t -> 'a t
          | Set : 'a Sub_axis.t -> 'a t
          | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

        and _ tuple =
          | [] : unit tuple
          | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple
      end
    end

    module Identifier = struct
      type 'a t =
        { type_ : 'a Type.t
        ; ident : string
        }
    end

    module Expression = struct
      (** Expressions that recursively do not contain any syntactic unions, e.g.
          [k mod m], [base_with_imm], and [heap @ global]. These expressions can still be
          interpreted as sets when identifiers bound to sets are expanded. *)
      type singleton = private Singleton

      (** Expressions that might contain a syntactic union, such as any singleton, and
          also expressions like [(k1, k2) & k3]. *)
      type set = private Set

      type ('err, 'which) allow_set =
        | Singleton_only : { why_no_set : 'err } -> ('err, singleton) allow_set
        (** Error hint for expressions that contain illegal unions *)
        | Set_or_singleton : (_, set) allow_set

      (** ['a] is the type of [Value]s to which an [Expression] evaluates. ['s] is whether
          the expression is allowed to syntactically contain sets (corresponding to the
          [Union] constructor). If an [('a, singleton) t] is evaluated without expanding
          identifiers bound to sets, the result is just one ['a Value.t]. If a
          [('a, set) t] is evaluated, or when any [('a, _) t] is evaluated by expanding
          identifiers bound to sets, the result of evaluation is a list of ['a Value.t]s. *)
      type ('a, 's) t =
        | Identifier : 'a Identifier.t -> ('a, _) t
        | Kind_product : (Type.kind, 's) t Nonempty_list.t -> (Type.kind, 's) t
        | Kind_mod :
            (Type.kind, 's) t * (Type.modality, singleton) t Nonempty_list.t
            -> (Type.kind, 's) t
        | Kind_coercion : (Type.kind, 's) t * (Type.kind, set) t -> (Type.kind, 's) t
        | Tuple :
            ('a * 'b) tuple
            -> ('a * 'b, _) t (*_ Tuples have at least one element *)
        | Union : ('a, _) t Nonempty_list.t -> ('a, set) t

      and 'a tuple =
        | [] : unit tuple
        | ( :: ) : ('a, singleton) t * 'b tuple -> ('a * 'b) tuple

      type packed = P : ('a, 's) t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : ('a Type.non_tuple, singleton) t -> packed [@@unboxed]
      end
    end

    module Value = struct
      type 'a t =
        | Identifier : 'a Type.non_tuple Identifier.t -> 'a Type.non_tuple t
        | Kind_product : Type.kind t Nonempty_list.t -> Type.kind t
        | Kind_mod : Type.kind t * Type.modality t Nonempty_list.t -> Type.kind t
        | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

      type packed = P : 'a t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : 'a Type.non_tuple t -> packed [@@unboxed]
      end
    end

    module Pattern = struct
      type 'a t =
        | Wildcard : 'a t
        | Identifier : 'a Identifier.t -> 'a t
        | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple
    end

    module Env = struct
      (** An association list mapping identifiers to values. Entries are added to the
          front of the association list, and looked up from front to back, so newer
          entries shadow older ones. *)

      type lookup =
        | Preserve_atoms
        (** When evaluating, do not expand identifiers into the set they represent. This
            form of lookup is used when evaluating set mono and poly attributes for the
            purposes of mangling, because we mangle by _names_ of sets, not canonical
            representations of their contents. *)
        | Expand_atoms_bound_to_sets
        (** When evaluating, fully expand all identifiers. This form of lookup is used
            when evaluating the right hand side of a non-set binding, since here we wish
            to bind the pattern on the left against all values in the set on the right *)

      module Entry = struct
        type 'a entry =
          { ident : 'a Identifier.t
          ; preserve_atoms : 'a Value.t
          (** A non-union-containing "alias" of this name. e.g. in
              [[@kind_set ks = value_with_imm]], the ident [value_with_imm] *)
          ; expand_atoms_bound_to_sets : 'a Value.t Nonempty_list.t
          (** The full set. e.g. in [[@kind_set ks = value_with_imm]], the list of idents
              [value], [immediate], [immediate64]. For names that are not obviously bound
              to sets, this is just the singleton set containing [preserve_atoms] *)
          ; namespace : 'a Axis.Namespace.t
          }

        type t = Entry : _ entry -> t
      end

      type t = Entry.t list
    end

    module Binding = struct
      type 'a t =
        { pattern : 'a Pattern.t
        ; expression : ('a, Expression.set) Expression.t Loc.t
        ; lookup : Env.lookup
        }

      type ('a, 'mangle) with_mangle =
        { binding : 'a t
        ; mangle : 'a Value.t -> 'mangle Value.t
        (** When an item [let lhs = rhs [@@attr pat = (expr1, expr2)]] is evaluated e.g.
            on [expr1], the expression gets evaluated to a [Value.t], which is then passed
            to [mangle] to create a [Mangler.t] that is used to mangle [lhs]. This is used
            to enable [[@@alloc (a @ m) = ...]] to mangle only based on [a]. *)
        ; mangle_axis : 'mangle Axis.t
        }
        constraint 'mangle = _ Type.non_tuple
    end

    module Node = struct
      (** A type used as the output of [Value.to_node]. *)
      type 'a t =
        | Jkind_annotation : jkind_annotation -> Type.kind t
        | Mode : mode loc -> Type.mode t
        | Modality : modality loc -> Type.modality t
        | Alloc : Type.alloc t
        | Synchro : Type.synchro t
    end
  end

  module Type_error = struct
    type t =
      | Type_mismatch :
          { kind : string
          ; sexp_of_kind : 'value -> Sexp.t
          ; value : 'value
          ; expected_type : _ Type.t
          ; expected_sets : (string, _) Typed.Expression.allow_set option
          ; hint : string option
          }
          -> t
      | Tuple_length_mismatch :
          { kind : string
          ; sexp_of_kind : 'value -> Sexp.t
          ; value : 'value
          ; expected_type : _ Type.t
          }
          -> t
  end
end

module type Language = sig
  include module type of struct
    include Definitions
  end

  module Type : sig
    include module type of struct
      include Type
    end

    module Non_tuple : sig
      include module type of struct
        include Non_tuple
      end
    end

    val sexp_of_t : _ t -> Sexp.t
    val kind : kind t
    val mode : mode t
    val modality : modality t
    val alloc : alloc t
    val synchro : synchro t
    val tuple2 : 'a t -> 'b t -> ('a * ('b * unit)) t
  end

  module Untyped : sig
    include module type of struct
      include Untyped
    end

    module Axis : sig
      include module type of struct
        include Axis
      end

      module Map : Map.S with type key = t
    end

    module Identifier : sig
      include module type of struct
        include Identifier
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end

    module Expression : sig
      include module type of struct
        include Expression
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end

    module Value : sig
      include module type of struct
        include Value
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end

    module Pattern : sig
      include module type of struct
        include Pattern
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end
  end

  module Typed : sig
    include module type of struct
      include Typed
    end

    module Axis : sig
      include module type of struct
        include Axis
      end

      module Map : Map.S with type key := packed

      val of_type : 'a Type.non_tuple Type.t -> 'a Type.non_tuple t
      val is_set : 'a t -> bool

      module Sub_axis : sig
        include module type of struct
          include Sub_axis
        end

        module Modal : sig
          include module type of struct
            include Modal
          end
        end

        module Mode : sig
          include module type of struct
            include Mode
          end
        end

        module Modality : sig
          include module type of struct
            include Modality
          end
        end

        module Or_unrecognized : sig
          include module type of struct
            include Or_unrecognized
          end
        end

        module Map : Stdlib.Map.S with type key := packed

        val of_identifier : 'a Type.non_tuple Identifier.t -> 'a Type.non_tuple t
        val of_value : 'a Type.non_tuple Value.t -> 'a Type.non_tuple t
      end

      module Namespace : sig
        include module type of struct
          include Namespace
        end

        val of_value : is_set:bool -> 'a Value.t -> 'a t
      end
    end

    module Identifier : sig
      include module type of struct
        include Identifier
      end
    end

    module Value : sig
      include module type of struct
        include Value
      end

      val compare : 'a t -> 'a t -> int

      (** Checks whether a value is a default value for its axis. *)
      val is_default : 'a Type.non_tuple t -> bool

      (** Convert a value in the template language to a concrete OCaml AST node. *)
      val to_node : 'a Type.non_tuple t -> loc:location -> 'a Type.non_tuple Node.t
    end

    module Pattern : sig
      include module type of struct
        include Pattern
      end

      val untype : 'a t -> Untyped.Pattern.t

      val type_check
        :  Untyped.Pattern.t
        -> expected:'a Type.t
        -> ('a t, Type_error.t) result
    end

    module Expression : sig
      include module type of struct
        include Expression
      end

      val to_set : ('a, _) t -> ('a, set) t
      val untype : ('a, 'allow_set) t -> Untyped.Expression.t

      val type_check
        :  Untyped.Expression.t
        -> expected:'a Type.t
        -> allow_set:(string, 's) allow_set
        -> (('a, 's) t, Type_error.t) result
    end

    module Env : sig
      include module type of struct
        include Env
      end

      (** An [Env.t] populated with initial bindings for [heap] and [stack] *)
      val initial : t

      val find : t -> 'a Identifier.t -> 'a Value.t option
      val find_expanding_sets : t -> 'a Identifier.t -> 'a Value.t Nonempty_list.t option

      (** [bind env ~loc ~is_set pat value] adds a new binding to [pat] in [env]. [value]
          is used for the [preserve_atoms] entry, and its fully expanded version for the
          [expand_atoms_bound_to_sets] entry.

          Produces an [Error] if binding against this pattern would shadow an identifier
          from a different namespace, or if evaluating value while expanding identifiers
          produces an error. *)
      val bind
        :  t
        -> loc:location
        -> is_set:bool
        -> 'a Pattern.t
        -> 'a Value.t
        -> (t, Syntax_error.t) result

      (** [bind_set env ~loc ident values] adds a new set binding to [ident] in [env].
          [ident] is used as the [preserve_atoms] entry, and [values] as the
          [expand_atoms_bound_to_sets] entry. *)
      val bind_set
        :  t
        -> loc:location
        -> 'a Type.non_tuple Pattern.t
        -> 'a Type.non_tuple Value.t Nonempty_list.t
        -> (t, Syntax_error.t) result

      (** Evaluate an expression in the given environment. Unbound
          [Expression.Identifier]s are evaluated as an equivalent [Value.Identifier] under
          the assumption that the identifier will be interpreted by the OCaml compiler; if
          it is not, we let the compiler report the error to the user. *)

      (** Evaluate a singleton expression without expanding identifiers bound to sets,
          producing a single value. *)
      val eval_singleton
        :  t
        -> ('a, Expression.singleton) Expression.t Loc.t
        -> ('a Value.t, Syntax_error.t) result

      (** Evaluate an expression that may contain unions, treating identifiers as
          determined by [lookup]. Always produces a set of values. *)
      val eval
        :  t
        -> lookup
        -> ('a, Expression.set) Expression.t Loc.t
        -> ('a Value.t Nonempty_list.t, Syntax_error.t) result
    end
  end

  module Type_error : sig
    include module type of struct
      include Type_error
    end

    val to_error : loc:Location.t -> t -> Syntax_error.t

    val lift_to_error_result
      :  loc:Location.t
      -> ('a, t) result
      -> ('a, Syntax_error.t) result
  end
end
