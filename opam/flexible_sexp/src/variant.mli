@@ portable

open! Core
open! Import

module Other : sig
  (** enumerate behaves as if this is [Nothing.t]. *)
  type t : immutable_data
  [@@deriving compare ~localize, enumerate, equal ~localize, globalize, hash, sexp_of]

  (** A simple generator to help you derive quickcheck on flexible-sexp types. It's your
      responsibility to pick [other_constructor_names] that do not already exist in the
      variant type in question.

      For an example, see the "Quickcheck" section of the README for this library. *)
  val quickcheck_generator
    :  other_constructor_names:string list
    -> t Quickcheck.Generator.t @ portable

  val quickcheck_observer : t Quickcheck.Observer.t
  val quickcheck_shrinker : t Quickcheck.Shrinker.t

  module Private__for_flexible_sexp_unit_tests_only : sig
    val of_sexp : Sexp.t -> t
  end
end

module Stable : sig
  module Other : sig
    module V1 : sig
      (** enumerate behaves as if this is [Nothing.t]. *)
      type nonrec t : immutable_data = Other.t
      [@@deriving
        compare ~localize
        , enumerate
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]

      (** See unstable documentation. *)
      val quickcheck_generator
        :  other_constructor_names:string list
        -> t Quickcheck.Generator.t @ portable

      val quickcheck_observer : t Quickcheck.Observer.t
      val quickcheck_shrinker : t Quickcheck.Shrinker.t
    end
  end

  (** For usage, please see the [doc] directory.

      Notes:

      - The variant constructor [Other] is special, and must be present. This is enforced
        both by the functor signature, and at runtime. *)
  module Make : sig
    module%template.portable
      [@mode m = (global, local)] [@modality p] V1
        (T : sig
           type t [@@deriving (compare [@mode m]), sexp]

           module Variants : sig
             val descriptions : (string * int) list
             val other : (Other.V1.t -> t) Variantslib.Variant.t
           end
         end)
        () : sig
      include Flexible_sexp_intf.S [@modality p] with type t := T.t
      include Comparator.S [@modality p] with type t := T.t
    end
  end

  module Make_without_comparator : sig
    module%template
      [@modality p = (portable, nonportable)] V1
        (T : sig
         @@ p
           type t [@@deriving sexp]

           module Variants : sig
             val descriptions : (string * int) list
             val other : (Other.V1.t -> t) Variantslib.Variant.t
           end
         end)
        () : sig
      @@ p
      include Flexible_sexp_intf.S [@modality p] with type t := T.t
    end
  end
end
