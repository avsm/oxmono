@@ portable

open! Core
open! Import

module (Tags @@ nonportable) : sig @@ portable
  type t : value mod contended portable
  [@@deriving compare ~localize, equal ~localize, hash, quickcheck, sexp_of]

  (** A simple generator to help you derive quickcheck on flexible-sexp types. It's your
      responsibility to pick [field_names] that do not already exist in the record type in
      question.

      For an example, see the "Quickcheck" section of the README for this library. *)
  val quickcheck_generator
    :  other_field_names:string list
    -> t Quickcheck.Generator.t @ portable

  val empty : t
  val is_empty : t -> bool

  (** The list of fields that were present in the sexp, but not included in the record
      definition. *)
  val field_names : t -> string list

  module Private__for_flexible_sexp_unit_tests_only : sig
    val of_map : Sexp.t String.Map.t -> t
    val to_map : t -> Sexp.t String.Map.t
  end
end

module (Stable @@ nonportable) : sig
  module Tags : sig
    module V1 : sig @@ portable
      type nonrec t = Tags.t
      [@@deriving
        compare ~localize
        , equal ~localize
        , hash
        , quickcheck
        , sexp
        , sexp_grammar
        , stable_witness]

      (** See unstable documentation. *)
      val quickcheck_generator
        :  other_field_names:string list
        -> t Quickcheck.Generator.t @ portable

      val empty : t
    end
  end

  (** For usage, please see the [doc] directory.

      Notes:

      - The field [tags] is special, and must be present. This is enforced both by the
        functor signature, and at runtime.

      - The underlying sexp representation of [T] must be "record-like". [@sexp.option]
        fields and similar are supported fine. *)
  module Make : sig
    module%template
      [@modality p = (portable, nonportable)] V1
        (T : sig
         @@ p
           type t [@@deriving compare, sexp]

           module Fields : sig
             val names : string list
             val tags : ([ `Read | `Set_and_create ], t, Tags.V1.t) Field.t_with_perm
           end
         end)
        () : sig
      @@ p
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

           module Fields : sig
             val names : string list
             val tags : ([ `Read | `Set_and_create ], t, Tags.V1.t) Field.t_with_perm
           end
         end)
        () : sig
      @@ p
      include Flexible_sexp_intf.S [@modality p] with type t := T.t
    end
  end
end
