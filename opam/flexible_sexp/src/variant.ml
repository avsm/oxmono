module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Other = struct
    module V1 = struct
      type t = (Sexp.V1.t[@sexp_grammar.any "Flexible_sexp.Variant.Other.t"])
      [@@deriving
        compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]

      open! Core
      open! Import

      let all = []

      let quickcheck_generator ~other_constructor_names =
        if List.is_empty other_constructor_names
        then
          raise_s
            [%sexp
              "You can't create a generator that never generates the [Other] constructor \
               by passing an empty list to this function. Instead, annotate the [Other] \
               constructor with [@quickcheck.do_not_generate]."];
        let open Base_quickcheck.Generator.Portable.Let_syntax in
        let%bind_open other_constructor_name = of_list other_constructor_names in
        (Quickcheck.Generator.union [@mode portable])
          [ return (Sexp.Atom other_constructor_name)
          ; (let%map args =
               (List.gen_non_empty [@mode portable]) Sexp.quickcheck_generator
             in
             Sexp.List (Atom other_constructor_name :: args))
          ]
      ;;

      let quickcheck_shrinker = Sexp.quickcheck_shrinker
      let quickcheck_observer = Sexp.quickcheck_observer

      module Private__for_flexible_sexp_unit_tests_only = struct
        let of_sexp = Fn.id
      end
    end

    module Current = V1
  end

  module Make_without_comparator = struct
    module%template.portable
      [@modality p] V1
        (T : sig
           open! Core
           open! Import

           type t [@@deriving sexp]

           module Variants : sig
             val descriptions : (string * int) list
             val other : (Other.V1.t -> t) Variantslib.Variant.t
           end
         end)
        () =
    struct
      module T_impl = struct
        open! Core
        open! Import

        type t = T.t [@@deriving sexp]

        module Variant_names : sig @@ portable
          val lookup : string -> [ `Variant | `Special_other_variant ] option
        end = struct
          let normalise name =
            if String.is_empty name
            then name
            else String.of_char (Char.uppercase name.[0]) ^ String.subo name ~pos:1
          ;;

          let variant_names : [ `Special_other_variant | `Variant ] String_dict.t =
            T.Variants.descriptions
            |> List.map ~f:(fun (name, _) ->
              let name = normalise name in
              if String.equal name T.Variants.other.name
              then name, `Special_other_variant
              else name, `Variant)
            |> String_dict.of_alist_exn
          ;;

          let lookup name = String_dict.find variant_names (normalise name)
        end

        (* It shouldn't actually break anything if the [Other] variant is called something
           different to "Other". However, we currently don't think it's good form to allow
           this field to be called anything different. So, forbid it. *)
        let () =
          let other_actual_variant_name = T.Variants.other.name in
          if String.( <> ) other_actual_variant_name "Other"
          then
            raise_s
              [%sexp
                "Expected the \"Other\" variant to actually be called \"Other\""
                , { other_actual_variant_name : string }
                , ([%here].Source_code_position.pos_fname : string)]
        ;;

        let variant_name_of_sexp_exn (sexp : Sexp.t) =
          match sexp with
          | Atom variant_name | List (Atom variant_name :: _) -> variant_name
          | _ ->
            raise_s
              [%sexp
                "Not a valid variant format; expected either an atom or a list with \
                 first element an atom"
                , ([%here].Source_code_position.pos_fname : string)
                , { sexp : Sexp.t }]
        ;;

        let t_of_sexp_gen ~flexible sexp =
          let flexible = flexible && not (Deep_inflexible_context.am_i_within ()) in
          let variant_name = variant_name_of_sexp_exn sexp in
          match Variant_names.lookup variant_name with
          | Some `Special_other_variant ->
            raise_s
              [%sexp
                "\"Other\" is not a valid variant name"
                , ([%here].Source_code_position.pos_fname : string)
                , { sexp : Sexp.t }]
          | Some `Variant -> T.t_of_sexp sexp
          | None ->
            if flexible
            then T.Variants.other.constructor sexp
            else
              raise_s
                [%sexp
                  "unknown variant name"
                  , ([%here].Source_code_position.pos_fname : string)
                  , { sexp : Sexp.t }]
        ;;

        let t_of_sexp x = t_of_sexp_gen ~flexible:true x

        let sexp_of_t t =
          let sexp = T.sexp_of_t t in
          match sexp with
          | Sexp.List [ Sexp.Atom variant_name; contained_sexp ]
            when String.equal variant_name T.Variants.other.name ->
            let variant_name = variant_name_of_sexp_exn contained_sexp in
            (match Variant_names.lookup variant_name with
             | Some `Special_other_variant ->
               raise_s
                 [%sexp
                   "\"Other\" is not a valid variant name"
                   , ([%here].Source_code_position.pos_fname : string)
                   , { sexp : Sexp.t }]
             | Some `Variant ->
               raise_s
                 [%sexp
                   "\"Other\" variant has same name as a known variant"
                   , ([%here].Source_code_position.pos_fname : string)
                   , { sexp : Sexp.t; variant_name : string }]
             | None -> contained_sexp)
          | _ -> sexp
        ;;
      end

      include T_impl
      include Binable.Of_sexpable.V1 [@modality p] [@alert "-legacy"] (T_impl)

      module Shallow_inflexible = struct
        let t_of_sexp sexp = t_of_sexp_gen ~flexible:false sexp
      end

      module Deep_inflexible = struct
        let t_of_sexp sexp =
          Deep_inflexible_context.run_within ~f:(fun () ->
            t_of_sexp_gen ~flexible:false sexp)
        ;;
      end
    end
  end

  module Make = struct
    module%template.portable
      [@mode m = (global, local)] [@modality p] V1
        (T : sig
           open! Core
           open! Import

           type t [@@deriving (compare [@mode m]), sexp]

           module Variants : sig
             val descriptions : (string * int) list
             val other : (Other.V1.t -> t) Variantslib.Variant.t
           end
         end)
        () =
    struct
      module T_impl' = struct
        include Make_without_comparator.V1 [@modality p] (T) ()

        let compare = T.compare
      end

      include T_impl'
      include Comparator.V1.Make [@modality p] (T_impl')
    end
  end
end

open! Core
open! Import
module Other = Stable.Other.Current
