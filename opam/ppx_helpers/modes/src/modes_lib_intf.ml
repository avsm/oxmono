module Definitions = struct
  type monadicity =
    | Comonadic
    | Monadic
  [@@deriving_inline sexp_of]

  let _ = fun (_ : monadicity) -> ()

  let sexp_of_monadicity =
    (function
     | Comonadic -> Sexplib0.Sexp.Atom "Comonadic"
     | Monadic -> Sexplib0.Sexp.Atom "Monadic"
     : monadicity -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_monadicity

  [@@@end]

  (** Modal axes *)

  type locality =
    | Global
    | Local
  [@@deriving_inline sexp_of]

  let _ = fun (_ : locality) -> ()

  let sexp_of_locality =
    (function
     | Global -> Sexplib0.Sexp.Atom "Global"
     | Local -> Sexplib0.Sexp.Atom "Local"
     : locality -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_locality

  [@@@end]

  type portability =
    | Portable
    | Shareable
    | Nonportable
  [@@deriving_inline sexp_of]

  let _ = fun (_ : portability) -> ()

  let sexp_of_portability =
    (function
     | Portable -> Sexplib0.Sexp.Atom "Portable"
     | Shareable -> Sexplib0.Sexp.Atom "Shareable"
     | Nonportable -> Sexplib0.Sexp.Atom "Nonportable"
     : portability -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_portability

  [@@@end]

  type contention =
    | Uncontended
    | Shared
    | Contended
  [@@deriving_inline sexp_of]

  let _ = fun (_ : contention) -> ()

  let sexp_of_contention =
    (function
     | Uncontended -> Sexplib0.Sexp.Atom "Uncontended"
     | Shared -> Sexplib0.Sexp.Atom "Shared"
     | Contended -> Sexplib0.Sexp.Atom "Contended"
     : contention -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_contention

  [@@@end]

  type statefulness =
    | Stateless
    | Observing
    | Stateful
  [@@deriving_inline sexp_of]

  let _ = fun (_ : statefulness) -> ()

  let sexp_of_statefulness =
    (function
     | Stateless -> Sexplib0.Sexp.Atom "Stateless"
     | Observing -> Sexplib0.Sexp.Atom "Observing"
     | Stateful -> Sexplib0.Sexp.Atom "Stateful"
     : statefulness -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_statefulness

  [@@@end]

  type visibility =
    | Read_write
    | Read
    | Immutable
  [@@deriving_inline sexp_of]

  let _ = fun (_ : visibility) -> ()

  let sexp_of_visibility =
    (function
     | Read_write -> Sexplib0.Sexp.Atom "Read_write"
     | Read -> Sexplib0.Sexp.Atom "Read"
     | Immutable -> Sexplib0.Sexp.Atom "Immutable"
     : visibility -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_visibility

  [@@@end]

  type linearity =
    | Many
    | Once
  [@@deriving_inline sexp_of]

  let _ = fun (_ : linearity) -> ()

  let sexp_of_linearity =
    (function
     | Many -> Sexplib0.Sexp.Atom "Many"
     | Once -> Sexplib0.Sexp.Atom "Once"
     : linearity -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_linearity

  [@@@end]

  type uniqueness =
    | Unique
    | Aliased
  [@@deriving_inline sexp_of]

  let _ = fun (_ : uniqueness) -> ()

  let sexp_of_uniqueness =
    (function
     | Unique -> Sexplib0.Sexp.Atom "Unique"
     | Aliased -> Sexplib0.Sexp.Atom "Aliased"
     : uniqueness -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_uniqueness

  [@@@end]

  type yielding =
    | Unyielding
    | Yielding
  [@@deriving_inline sexp_of]

  let _ = fun (_ : yielding) -> ()

  let sexp_of_yielding =
    (function
     | Unyielding -> Sexplib0.Sexp.Atom "Unyielding"
     | Yielding -> Sexplib0.Sexp.Atom "Yielding"
     : yielding -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_yielding

  [@@@end]

  type forkable =
    | Forkable
    | Unforkable
  [@@deriving_inline sexp_of]

  let _ = fun (_ : forkable) -> ()

  let sexp_of_forkable =
    (function
     | Forkable -> Sexplib0.Sexp.Atom "Forkable"
     | Unforkable -> Sexplib0.Sexp.Atom "Unforkable"
     : forkable -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_forkable

  [@@@end]

  type staticity =
    | Static
    | Dynamic
  [@@deriving_inline sexp_of]

  let _ = fun (_ : staticity) -> ()

  let sexp_of_staticity =
    (function
     | Static -> Sexplib0.Sexp.Atom "Static"
     | Dynamic -> Sexplib0.Sexp.Atom "Dynamic"
     : staticity -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_staticity

  [@@@end]

  (** Nonmodal jkind axes (soon to be nontrivialities) *)

  type externality =
    | External_
    | External64
    | Internal
  [@@deriving_inline sexp_of]

  let _ = fun (_ : externality) -> ()

  let sexp_of_externality =
    (function
     | External_ -> Sexplib0.Sexp.Atom "External_"
     | External64 -> Sexplib0.Sexp.Atom "External64"
     | Internal -> Sexplib0.Sexp.Atom "Internal"
     : externality -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_externality

  [@@@end]

  type nullability =
    | Non_null
    | Maybe_null
  [@@deriving_inline sexp_of]

  let _ = fun (_ : nullability) -> ()

  let sexp_of_nullability =
    (function
     | Non_null -> Sexplib0.Sexp.Atom "Non_null"
     | Maybe_null -> Sexplib0.Sexp.Atom "Maybe_null"
     : nullability -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_nullability

  [@@@end]

  type separability =
    | Non_float
    | Separable
    | Maybe_separable
  [@@deriving_inline sexp_of]

  let _ = fun (_ : separability) -> ()

  let sexp_of_separability =
    (function
     | Non_float -> Sexplib0.Sexp.Atom "Non_float"
     | Separable -> Sexplib0.Sexp.Atom "Separable"
     | Maybe_separable -> Sexplib0.Sexp.Atom "Maybe_separable"
     : separability -> Sexplib0.Sexp.t)
  ;;

  let _ = sexp_of_separability

  [@@@end]
  [@@@ocamlformat "disable"]

  type !'a[@phantom] axis =
    | Locality : locality axis
    | Portability : portability axis
    | Contention : contention axis
    | Statefulness : statefulness axis
    | Visibility : visibility axis
    | Linearity : linearity axis
    | Uniqueness : uniqueness axis
    | Yielding : yielding axis
    | Forkable : forkable axis
    | Staticity : staticity axis
  [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a axis) -> ()

  let sexp_of_axis : 'a . 'a axis -> Sexplib0.Sexp.t =
    fun (type a__002_) ->
    (function
      | Locality -> Sexplib0.Sexp.Atom "Locality"
      | Portability -> Sexplib0.Sexp.Atom "Portability"
      | Contention -> Sexplib0.Sexp.Atom "Contention"
      | Statefulness -> Sexplib0.Sexp.Atom "Statefulness"
      | Visibility -> Sexplib0.Sexp.Atom "Visibility"
      | Linearity -> Sexplib0.Sexp.Atom "Linearity"
      | Uniqueness -> Sexplib0.Sexp.Atom "Uniqueness"
      | Yielding -> Sexplib0.Sexp.Atom "Yielding"
      | Forkable -> Sexplib0.Sexp.Atom "Forkable"
      | Staticity -> Sexplib0.Sexp.Atom "Staticity"
                      : a__002_ axis -> Sexplib0.Sexp.t)

  let _ = sexp_of_axis

  [@@@end]
  [@@@ocamlformat "enable"]

  type !'a nonmodal_axis =
    | Externality : externality nonmodal_axis
    | Nullability : nullability nonmodal_axis
    | Separability : separability nonmodal_axis

  type !'a jkind_axis =
    | Modal of 'a axis
    | Nonmodal of 'a nonmodal_axis

  module Modal = struct
    type t =
      | Global
      | Local
      | Portable
      | Shareable
      | Nonportable
      | Uncontended
      | Shared
      | Contended
      | Stateless
      | Observing
      | Stateful
      | Read_write
      | Read
      | Immutable
      | Many
      | Once
      | Unique
      | Aliased
      | Unyielding
      | Yielding
      | Forkable
      | Unforkable
      | Static
      | Dynamic
    [@@deriving_inline enumerate, sexp]

    let _ = fun (_ : t) -> ()

    let all =
      ([ Global
       ; Local
       ; Portable
       ; Shareable
       ; Nonportable
       ; Uncontended
       ; Shared
       ; Contended
       ; Stateless
       ; Observing
       ; Stateful
       ; Read_write
       ; Read
       ; Immutable
       ; Many
       ; Once
       ; Unique
       ; Aliased
       ; Unyielding
       ; Yielding
       ; Forkable
       ; Unforkable
       ; Static
       ; Dynamic
       ]
       : t list)
    ;;

    let _ = all

    let t_of_sexp =
      (let error_source__005_ = "modes_lib_intf.ml.Definitions.Modal.t" in
       function
       | Sexplib0.Sexp.Atom ("global" | "Global") -> Global
       | Sexplib0.Sexp.Atom ("local" | "Local") -> Local
       | Sexplib0.Sexp.Atom ("portable" | "Portable") -> Portable
       | Sexplib0.Sexp.Atom ("shareable" | "Shareable") -> Shareable
       | Sexplib0.Sexp.Atom ("nonportable" | "Nonportable") -> Nonportable
       | Sexplib0.Sexp.Atom ("uncontended" | "Uncontended") -> Uncontended
       | Sexplib0.Sexp.Atom ("shared" | "Shared") -> Shared
       | Sexplib0.Sexp.Atom ("contended" | "Contended") -> Contended
       | Sexplib0.Sexp.Atom ("stateless" | "Stateless") -> Stateless
       | Sexplib0.Sexp.Atom ("observing" | "Observing") -> Observing
       | Sexplib0.Sexp.Atom ("stateful" | "Stateful") -> Stateful
       | Sexplib0.Sexp.Atom ("read_write" | "Read_write") -> Read_write
       | Sexplib0.Sexp.Atom ("read" | "Read") -> Read
       | Sexplib0.Sexp.Atom ("immutable" | "Immutable") -> Immutable
       | Sexplib0.Sexp.Atom ("many" | "Many") -> Many
       | Sexplib0.Sexp.Atom ("once" | "Once") -> Once
       | Sexplib0.Sexp.Atom ("unique" | "Unique") -> Unique
       | Sexplib0.Sexp.Atom ("aliased" | "Aliased") -> Aliased
       | Sexplib0.Sexp.Atom ("unyielding" | "Unyielding") -> Unyielding
       | Sexplib0.Sexp.Atom ("yielding" | "Yielding") -> Yielding
       | Sexplib0.Sexp.Atom ("forkable" | "Forkable") -> Forkable
       | Sexplib0.Sexp.Atom ("unforkable" | "Unforkable") -> Unforkable
       | Sexplib0.Sexp.Atom ("static" | "Static") -> Static
       | Sexplib0.Sexp.Atom ("dynamic" | "Dynamic") -> Dynamic
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom
              ( "global"
              | "Global"
              | "local"
              | "Local"
              | "portable"
              | "Portable"
              | "shareable"
              | "Shareable"
              | "nonportable"
              | "Nonportable"
              | "uncontended"
              | "Uncontended"
              | "shared"
              | "Shared"
              | "contended"
              | "Contended"
              | "stateless"
              | "Stateless"
              | "observing"
              | "Observing"
              | "stateful"
              | "Stateful"
              | "read_write"
              | "Read_write"
              | "read"
              | "Read"
              | "immutable"
              | "Immutable"
              | "many"
              | "Many"
              | "once"
              | "Once"
              | "unique"
              | "Unique"
              | "aliased"
              | "Aliased"
              | "unyielding"
              | "Unyielding"
              | "yielding"
              | "Yielding"
              | "forkable"
              | "Forkable"
              | "unforkable"
              | "Unforkable"
              | "static"
              | "Static"
              | "dynamic"
              | "Dynamic" )
           :: _) as sexp__006_ ->
         Sexplib0.Sexp_conv_error.stag_no_args error_source__005_ sexp__006_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__004_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__005_ sexp__004_
       | Sexplib0.Sexp.List [] as sexp__004_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__005_ sexp__004_
       | sexp__004_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__005_
           [ "Global"
           ; "Local"
           ; "Portable"
           ; "Shareable"
           ; "Nonportable"
           ; "Uncontended"
           ; "Shared"
           ; "Contended"
           ; "Stateless"
           ; "Observing"
           ; "Stateful"
           ; "Read_write"
           ; "Read"
           ; "Immutable"
           ; "Many"
           ; "Once"
           ; "Unique"
           ; "Aliased"
           ; "Unyielding"
           ; "Yielding"
           ; "Forkable"
           ; "Unforkable"
           ; "Static"
           ; "Dynamic"
           ]
           sexp__004_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | Global -> Sexplib0.Sexp.Atom "Global"
       | Local -> Sexplib0.Sexp.Atom "Local"
       | Portable -> Sexplib0.Sexp.Atom "Portable"
       | Shareable -> Sexplib0.Sexp.Atom "Shareable"
       | Nonportable -> Sexplib0.Sexp.Atom "Nonportable"
       | Uncontended -> Sexplib0.Sexp.Atom "Uncontended"
       | Shared -> Sexplib0.Sexp.Atom "Shared"
       | Contended -> Sexplib0.Sexp.Atom "Contended"
       | Stateless -> Sexplib0.Sexp.Atom "Stateless"
       | Observing -> Sexplib0.Sexp.Atom "Observing"
       | Stateful -> Sexplib0.Sexp.Atom "Stateful"
       | Read_write -> Sexplib0.Sexp.Atom "Read_write"
       | Read -> Sexplib0.Sexp.Atom "Read"
       | Immutable -> Sexplib0.Sexp.Atom "Immutable"
       | Many -> Sexplib0.Sexp.Atom "Many"
       | Once -> Sexplib0.Sexp.Atom "Once"
       | Unique -> Sexplib0.Sexp.Atom "Unique"
       | Aliased -> Sexplib0.Sexp.Atom "Aliased"
       | Unyielding -> Sexplib0.Sexp.Atom "Unyielding"
       | Yielding -> Sexplib0.Sexp.Atom "Yielding"
       | Forkable -> Sexplib0.Sexp.Atom "Forkable"
       | Unforkable -> Sexplib0.Sexp.Atom "Unforkable"
       | Static -> Sexplib0.Sexp.Atom "Static"
       | Dynamic -> Sexplib0.Sexp.Atom "Dynamic"
       : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module type Wrapper = sig
    type 'a t = private 'a

    val mk : 'a -> 'a t
    val unwrap : 'a t -> 'a
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end

  module Nonmodal = struct
    type t =
      | External_
      | External64
      | Internal
      | Non_null
      | Maybe_null
      | Non_float
      | Separable
      | Maybe_separable
    [@@deriving_inline enumerate, sexp]

    let _ = fun (_ : t) -> ()

    let all =
      ([ External_
       ; External64
       ; Internal
       ; Non_null
       ; Maybe_null
       ; Non_float
       ; Separable
       ; Maybe_separable
       ]
       : t list)
    ;;

    let _ = all

    let t_of_sexp =
      (let error_source__009_ = "modes_lib_intf.ml.Definitions.Nonmodal.t" in
       function
       | Sexplib0.Sexp.Atom ("external_" | "External_") -> External_
       | Sexplib0.Sexp.Atom ("external64" | "External64") -> External64
       | Sexplib0.Sexp.Atom ("internal" | "Internal") -> Internal
       | Sexplib0.Sexp.Atom ("non_null" | "Non_null") -> Non_null
       | Sexplib0.Sexp.Atom ("maybe_null" | "Maybe_null") -> Maybe_null
       | Sexplib0.Sexp.Atom ("non_float" | "Non_float") -> Non_float
       | Sexplib0.Sexp.Atom ("separable" | "Separable") -> Separable
       | Sexplib0.Sexp.Atom ("maybe_separable" | "Maybe_separable") -> Maybe_separable
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom
              ( "external_"
              | "External_"
              | "external64"
              | "External64"
              | "internal"
              | "Internal"
              | "non_null"
              | "Non_null"
              | "maybe_null"
              | "Maybe_null"
              | "non_float"
              | "Non_float"
              | "separable"
              | "Separable"
              | "maybe_separable"
              | "Maybe_separable" )
           :: _) as sexp__010_ ->
         Sexplib0.Sexp_conv_error.stag_no_args error_source__009_ sexp__010_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__008_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__009_ sexp__008_
       | Sexplib0.Sexp.List [] as sexp__008_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__009_ sexp__008_
       | sexp__008_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__009_
           [ "External_"
           ; "External64"
           ; "Internal"
           ; "Non_null"
           ; "Maybe_null"
           ; "Non_float"
           ; "Separable"
           ; "Maybe_separable"
           ]
           sexp__008_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | External_ -> Sexplib0.Sexp.Atom "External_"
       | External64 -> Sexplib0.Sexp.Atom "External64"
       | Internal -> Sexplib0.Sexp.Atom "Internal"
       | Non_null -> Sexplib0.Sexp.Atom "Non_null"
       | Maybe_null -> Sexplib0.Sexp.Atom "Maybe_null"
       | Non_float -> Sexplib0.Sexp.Atom "Non_float"
       | Separable -> Sexplib0.Sexp.Atom "Separable"
       | Maybe_separable -> Sexplib0.Sexp.Atom "Maybe_separable"
       : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Jkind_mod = struct
    type t =
      | Modal of Modal.t
      | Nonmodal of Nonmodal.t
  end

  module type Lattice = sig
    type t

    val top : t
    val bottom : t
    val join : t -> t -> t
    val meet : t -> t -> t
    val le : t -> t -> bool
    val equal : t -> t -> bool
  end

  module type Axis = sig
    include Lattice

    val all : t list
    val monadicity : monadicity
    val legacy : t
    val of_modal : Modal.t -> t option
    val to_modal : t -> Modal.t

    (** Implementations of [Axis] must provide a total order over its elements. [compare]
        should agree naturally with [le] and [equal]. *)
    val compare : t -> t -> int

    val sexp_of_t : t -> Sexplib0.Sexp.t

    (*_ filled in later with concrete types *)
    type 'a mode
    type 'a modality
    type 'a crossing

    val apply_modality : t modality -> t mode -> t mode

    (** [cross crossing mode] strengthens [mode] as it would on a variable of kind
        [_ mod crossing].

        In the compiler, this is known as [cross_left]. *)
    val cross : t crossing -> t mode -> t mode

    (** [uncross crossing mode] is the weakest mode [mode'] such that
        [cross crossing mode'] is at least as strong as [mode]. It acts as a sort of
        inverse to [cross].

        In the compiler, this is known as [cross_right]. *)
    val uncross : t crossing -> t mode -> t mode
  end

  module type Per_axis = sig
    type 'a wrapped

    type t =
      { locality : locality wrapped
      ; portability : portability wrapped
      ; contention : contention wrapped
      ; statefulness : statefulness wrapped
      ; visibility : visibility wrapped
      ; linearity : linearity wrapped
      ; uniqueness : uniqueness wrapped
      ; yielding : yielding wrapped
      ; forkable : forkable wrapped
      ; staticity : staticity wrapped
      }

    val sexp_of_t : t -> Sexplib0.Sexp.t

    type make_f = { f : 'a. 'a axis -> 'a wrapped }

    val make : make_f -> t
    val get : t -> 'a axis -> 'a wrapped
    val set : t -> 'a axis -> 'a wrapped -> t
  end

  module type Nonmodal_axis = sig
    include Lattice

    val all : t list
    val of_nonmodal : Nonmodal.t -> t option
    val to_nonmodal : t -> Nonmodal.t
  end

  module Nonmodals = struct
    type t =
      { externality : externality
      ; nullability : nullability
      ; separability : separability
      }
    [@@deriving_inline sexp_of]

    let _ = fun (_ : t) -> ()

    let sexp_of_t =
      (fun { externality = externality__012_
           ; nullability = nullability__014_
           ; separability = separability__016_
           } ->
         let bnds__011_ = ([] : _ Stdlib.List.t) in
         let bnds__011_ =
           let arg__017_ = sexp_of_separability separability__016_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "separability"; arg__017_ ]
            :: bnds__011_
            : _ Stdlib.List.t)
         in
         let bnds__011_ =
           let arg__015_ = sexp_of_nullability nullability__014_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "nullability"; arg__015_ ]
            :: bnds__011_
            : _ Stdlib.List.t)
         in
         let bnds__011_ =
           let arg__013_ = sexp_of_externality externality__012_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "externality"; arg__013_ ]
            :: bnds__011_
            : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__011_
       : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Base_sort = struct
    type t =
      | Void
      | Value
      | Untagged_immediate
      | Float64
      | Float32
      | Word
      | Bits8
      | Bits16
      | Bits32
      | Bits64
      | Vec128
      | Vec256
      | Vec512
    [@@deriving_inline sexp]

    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__020_ = "modes_lib_intf.ml.Definitions.Base_sort.t" in
       function
       | Sexplib0.Sexp.Atom ("void" | "Void") -> Void
       | Sexplib0.Sexp.Atom ("value" | "Value") -> Value
       | Sexplib0.Sexp.Atom ("untagged_immediate" | "Untagged_immediate") ->
         Untagged_immediate
       | Sexplib0.Sexp.Atom ("float64" | "Float64") -> Float64
       | Sexplib0.Sexp.Atom ("float32" | "Float32") -> Float32
       | Sexplib0.Sexp.Atom ("word" | "Word") -> Word
       | Sexplib0.Sexp.Atom ("bits8" | "Bits8") -> Bits8
       | Sexplib0.Sexp.Atom ("bits16" | "Bits16") -> Bits16
       | Sexplib0.Sexp.Atom ("bits32" | "Bits32") -> Bits32
       | Sexplib0.Sexp.Atom ("bits64" | "Bits64") -> Bits64
       | Sexplib0.Sexp.Atom ("vec128" | "Vec128") -> Vec128
       | Sexplib0.Sexp.Atom ("vec256" | "Vec256") -> Vec256
       | Sexplib0.Sexp.Atom ("vec512" | "Vec512") -> Vec512
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom
              ( "void"
              | "Void"
              | "value"
              | "Value"
              | "untagged_immediate"
              | "Untagged_immediate"
              | "float64"
              | "Float64"
              | "float32"
              | "Float32"
              | "word"
              | "Word"
              | "bits8"
              | "Bits8"
              | "bits16"
              | "Bits16"
              | "bits32"
              | "Bits32"
              | "bits64"
              | "Bits64"
              | "vec128"
              | "Vec128"
              | "vec256"
              | "Vec256"
              | "vec512"
              | "Vec512" )
           :: _) as sexp__021_ ->
         Sexplib0.Sexp_conv_error.stag_no_args error_source__020_ sexp__021_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__019_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__020_ sexp__019_
       | Sexplib0.Sexp.List [] as sexp__019_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__020_ sexp__019_
       | sexp__019_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__020_
           [ "Void"
           ; "Value"
           ; "Untagged_immediate"
           ; "Float64"
           ; "Float32"
           ; "Word"
           ; "Bits8"
           ; "Bits16"
           ; "Bits32"
           ; "Bits64"
           ; "Vec128"
           ; "Vec256"
           ; "Vec512"
           ]
           sexp__019_
       : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | Void -> Sexplib0.Sexp.Atom "Void"
       | Value -> Sexplib0.Sexp.Atom "Value"
       | Untagged_immediate -> Sexplib0.Sexp.Atom "Untagged_immediate"
       | Float64 -> Sexplib0.Sexp.Atom "Float64"
       | Float32 -> Sexplib0.Sexp.Atom "Float32"
       | Word -> Sexplib0.Sexp.Atom "Word"
       | Bits8 -> Sexplib0.Sexp.Atom "Bits8"
       | Bits16 -> Sexplib0.Sexp.Atom "Bits16"
       | Bits32 -> Sexplib0.Sexp.Atom "Bits32"
       | Bits64 -> Sexplib0.Sexp.Atom "Bits64"
       | Vec128 -> Sexplib0.Sexp.Atom "Vec128"
       | Vec256 -> Sexplib0.Sexp.Atom "Vec256"
       | Vec512 -> Sexplib0.Sexp.Atom "Vec512"
       : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    [@@@end]
  end

  module Layout = struct
    type t =
      | Base of Base_sort.t
      | Any
  end
end

module type Modes_lib = sig
  include module type of struct
    include Definitions
  end

  module Mode : Wrapper
  module Modality : Wrapper
  module Crossing : Wrapper

  module Modal : sig
    include module type of struct
      include Modal
    end

    (** Accepts both [local] and [Local] capitalization. *)
    val of_string : string -> t

    (** Produces [local] capitalization. *)
    val to_string : t -> string
  end

  module Nonmodal : sig
    include module type of struct
      include Nonmodal
    end

    (** Accepts both [separable] and [Separable] capitalization. *)
    val of_string : string -> t

    (** Produces [separable] capitalization. *)
    val to_string : t -> string
  end

  module Jkind_mod : sig
    include module type of struct
      include Jkind_mod
    end

    val all : t list
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t

    (** Accepts both [separable] and [Separable] capitalization. *)
    val of_string : string -> t

    (** Produces [separable] capitalization. *)
    val to_string : t -> string
  end

  module Layout : sig
    include module type of struct
      include Layout
    end

    val compare : t -> t -> int
    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t

    (** Accepts both [value] and [Value] capitalization. *)
    val of_string : string -> t

    (** Produces [value] capitalization. *)
    val to_string : t -> string
  end

  module type Axis =
    Axis
    with type 'a mode := 'a Mode.t
     and type 'a modality := 'a Modality.t
     and type 'a crossing := 'a Crossing.t

  module Axis : sig
    type 'a t = 'a axis
    type packed = P : 'a t -> packed [@@unboxed]

    val all : packed list
    val module_ : 'a t -> (module Axis with type t = 'a)
  end

  module Nonmodal_axis : sig
    type 'a t = 'a nonmodal_axis
    type packed = P : 'a t -> packed [@@unboxed]

    val all : packed list
    val module_ : 'a t -> (module Nonmodal_axis with type t = 'a)
  end

  module Jkind_axis : sig
    type 'a t = 'a jkind_axis
    type packed = P : 'a t -> packed [@@unboxed]

    val all : packed list
  end

  (** Modal axes *)

  module Locality : Axis with type t = locality
  module Portability : Axis with type t = portability
  module Contention : Axis with type t = contention
  module Statefulness : Axis with type t = statefulness
  module Visibility : Axis with type t = visibility
  module Linearity : Axis with type t = linearity
  module Uniqueness : Axis with type t = uniqueness
  module Yielding : Axis with type t = yielding

  (** Nonmodal jkind axes (soon to be nontrivialities) *)

  module Externality : Nonmodal_axis with type t = externality
  module Nullability : Nonmodal_axis with type t = nullability
  module Separability : Nonmodal_axis with type t = separability

  module Optional : sig
    include Per_axis with type 'a wrapped := 'a option

    (** When omitted, axis is filled in with [None]. *)
    val of_modals : Modal.t list -> t
  end

  module Modes : sig
    include Per_axis with type 'a wrapped := 'a Mode.t
    include Lattice with type t := t

    (** When omitted, axis is filled in with default or implied mode. *)
    val of_modals : Modal.t list -> t

    val to_modals : t -> Modal.t list
    val to_modals_explicit : t -> Modal.t list
  end

  module Modalities : sig
    include Per_axis with type 'a wrapped := 'a Modality.t
    include Lattice with type t := t

    (** When omitted, axis is filled in with default or implied modality, taking into
        account whether the default is for immutable or mutable values. *)
    val of_modals : mutable_implied:bool -> Modal.t list -> t

    val to_modals : mutable_implied:bool -> t -> Modal.t list
    val to_modals_explicit : t -> Modal.t list
  end

  module Crossings : sig
    include Per_axis with type 'a wrapped := 'a Crossing.t
    include Lattice with type t := t

    (** When omitted, axis is filled in with default or implied crossing. *)
    val of_modals : Modal.t list -> t

    val to_modals : t -> Modal.t list
    val to_modals_explicit : t -> Modal.t list
  end

  module Nonmodals : sig
    include module type of struct
      include Nonmodals
    end

    include Lattice with type t := t

    (** When omitted, axis is filled in with default value on that jkind axis. *)
    val of_nonmodals : Nonmodal.t list -> t

    val to_nonmodals : t -> Nonmodal.t list
    val to_nonmodals_explicit : t -> Nonmodal.t list

    type make_f = { f : 'a. 'a nonmodal_axis -> 'a }

    val make : make_f -> t
    val get : t -> 'a nonmodal_axis -> 'a
    val set : t -> 'a nonmodal_axis -> 'a -> t
  end

  module Jkind_modifiers : sig
    type 'a wrapper =
      | Modal : 'a Crossing.t * 'a Axis.t -> 'a wrapper
      | Nonmodal : 'a * 'a Nonmodal_axis.t -> 'a wrapper

    type t =
      { modals : Crossings.t
      ; nonmodals : Nonmodals.t
      }

    type make_f = { f : 'a. 'a jkind_axis -> 'a wrapper }

    (** When omitted, axis is filled in with default value for that modifier. *)
    val of_jkind_mods : Jkind_mod.t list -> t

    val to_jkind_mods : t -> Jkind_mod.t list
    val to_jkind_mods_explicit : t -> Jkind_mod.t list
    val make : make_f -> t
    val get : t -> 'a jkind_axis -> 'a

    (*_ Since [wrapper] includes a witness of the axis, we do not need to require the axis
        separately. *)
    val set : t -> 'a wrapper -> t
  end

  module Kind : sig
    type t =
      { layout : Layout.t
      ; bounds : Jkind_modifiers.t
      }

    (** Interprets [ident] as a kind, expanding abbreviations known to the compiler (e.g.
        [mutable_data]).

        Raises if [ident] does not correspond to a know layout or kind abbreviation. *)
    val of_ident_exn : ident:string -> t

    (** Applies the given modifiers [mods] to the kind [k] --- produces the kind
        [(k) mod (mods)]. This can only strengthen the kind, and so will not necessarily
        set the bounds to be exactly the modifiers provided. *)
    val apply_mods : t -> Jkind_modifiers.t -> t

    val is_subkind : t -> of_:t -> bool

    (**/**)

    (*_ Exposed for testing *)
    module Private : sig
      val interpreted_abbreviations : string list
    end
  end

  val apply_modalities : Modalities.t -> Modes.t -> Modes.t

  (** [cross crossing mode] strengthens [mode] as it would on a variable of kind
      [_ mod crossing].

      In the compiler, this is known as [cross_left]. *)
  val cross : Crossings.t -> Modes.t -> Modes.t

  (** [uncross crossing mode] is the weakest mode [mode'] such that [cross crossing mode']
      is at least as strong as [mode]. It acts as a sort of inverse to [cross].

      In the compiler, this is known as [cross_right]. *)
  val uncross : Crossings.t -> Modes.t -> Modes.t
end
