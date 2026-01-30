module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Tags = struct
    module V1 = struct
      type t = Sexp.V1.t Map.V1.M(String.V1).t
      [@@deriving compare ~localize, sexp, sexp_grammar, stable_witness]

      open! Core
      open! Import

      let quickcheck_generator ~other_field_names =
        let open Base_quickcheck.Generator.Portable.Let_syntax in
        let empty = (Map.empty (module String) : t) in
        if List.is_empty other_field_names
        then return empty
        else (
          let other_field_names = String.Set.of_list other_field_names in
          (Base_quickcheck.Generator.weighted_union [@mode portable])
            [ 4., return empty
            ; ( 1.
              , (Base_quickcheck.Generator.map_t_m [@mode portable])
                  (module String)
                  ((Base_quickcheck.Generator.of_list [@mode portable])
                     (Set.to_list other_field_names))
                  Sexp.quickcheck_generator )
            ])
      ;;

      let quickcheck_shrinker =
        (Base_quickcheck.Shrinker.map_t [@mode portable])
          String.quickcheck_shrinker
          Sexp.quickcheck_shrinker
      ;;

      let quickcheck_observer =
        (Base_quickcheck.Observer.map_t [@mode portable])
          String.quickcheck_observer
          Sexp.quickcheck_observer
      ;;

      let%template equal = ([%compare.equal: Sexp.t String.Map.t] [@mode local])
      [@@mode __ = (local, global)]
      ;;

      let empty = String.Map.empty
      let is_empty = Map.is_empty

      include Map.Provide_hash [@modality portable] (String)

      let hash_fold_t state t = hash_fold_t Sexp.hash_fold_t state t
      let hash t = Hash.of_fold hash_fold_t t
      let field_names = Map.keys

      module Private__for_flexible_sexp_unit_tests_only = struct
        let of_map = Fn.id
        let to_map = Fn.id
      end
    end

    module Current = V1
  end

  module Make_without_comparator = struct
    module%template
      [@modality p = (portable, nonportable)] V1
        (T : sig
         @@ p
           open! Core
           open! Import

           type t [@@deriving sexp]

           module Fields : sig
             val names : string list
             val tags : ([ `Read | `Set_and_create ], t, Tags.V1.t) Field.t_with_perm
           end
         end)
        () =
    struct
      module T_impl = struct
        open! Core
        open! Import

        type t = T.t

        (* It shouldn't actually break anything if the [tags] field is called something
           different to "tags". However, we currently don't think it's good form to allow
           this field to be called anything different. So, forbid it. *)
        let () =
          let tags_actual_field_name = Field.name T.Fields.tags in
          if String.( <> ) tags_actual_field_name "tags"
          then
            raise_s
              [%sexp
                "Expected the \"tags\" field to actually be called \"tags\""
                , { tags_actual_field_name : string }
                , ([%here].Source_code_position.pos_fname : string)]
        ;;

        let field_names : [ `Field | `Special_tags_field ] String_dict.t =
          T.Fields.names
          |> List.map ~f:(fun name ->
            if String.equal name (Field.name T.Fields.tags)
            then name, `Special_tags_field
            else name, `Field)
          |> String_dict.of_alist_exn
        ;;

        let t_of_sexp_gen ~flexible (sexp : Sexp.t) : t =
          let flexible = flexible && not (Deep_inflexible_context.am_i_within ()) in
          match sexp with
          | Sexp.Atom _ ->
            raise_s
              [%sexp
                "parsing top-level record; expecting a list, got unexpected atom at \
                 top-level"
                , ([%here].Source_code_position.pos_fname : string)
                , { sexp : Sexp.t }]
          | Sexp.List field_sexps ->
            let fields, tags_name_and_values =
              List.partition_map field_sexps ~f:(fun field_sexp ->
                match field_sexp with
                | Sexp.List [ Sexp.Atom field_name; _ ] ->
                  (match String_dict.find field_names field_name with
                   | Some `Special_tags_field ->
                     raise_s
                       [%sexp
                         "\"tags\" is an illegal field or tag name"
                         , ([%here].Source_code_position.pos_fname : string)
                         , { field_sexp : Sexp.t }]
                   | Some `Field -> First field_sexp
                   | None -> Second (field_name, field_sexp))
                | _ ->
                  raise_s
                    [%sexp
                      "parsing record field; expecting a 2-list with first element an \
                       atom"
                      , ([%here].Source_code_position.pos_fname : string)
                      , { field_sexp : Sexp.t }])
            in
            let tags_names, tags_values = List.unzip tags_name_and_values in
            if (not flexible) && not (List.is_empty tags_names)
            then
              raise_s
                [%sexp
                  "extra fields"
                  , ([%here].Source_code_position.pos_fname : string)
                  , (tags_names : string list)];
            let tags_sexp =
              Sexp.List [ Sexp.Atom (Field.name T.Fields.tags); Sexp.List tags_values ]
            in
            T.t_of_sexp (Sexp.List (tags_sexp :: fields))
        ;;

        let t_of_sexp = t_of_sexp_gen ~flexible:true

        let sexp_of_t (t : t) : Sexp.t =
          let sexp_with_empty_tags =
            T.sexp_of_t (Field.fset T.Fields.tags t String.Map.empty)
          in
          match sexp_with_empty_tags with
          | Sexp.Atom _ ->
            raise_s
              [%sexp
                "parsing top-level record; expecting a list, got unexpected atom at \
                 top-level"
                , ([%here].Source_code_position.pos_fname : string)
                , { sexp_with_empty_tags : Sexp.t }]
          | Sexp.List field_sexps ->
            let sexp_without_tags =
              List.rev_filter field_sexps ~f:(fun field_sexp ->
                match field_sexp with
                | Sexp.List [ Sexp.Atom field_name; _ ] ->
                  not (String.equal field_name (Field.name T.Fields.tags))
                | _ ->
                  raise_s
                    [%sexp
                      "parsing record field; expecting a 2-list with first element an \
                       atom"
                      , ([%here].Source_code_position.pos_fname : string)
                      , { field_sexp : Sexp.t }])
            in
            let tags_sexp =
              Map.fold_right
                (Field.get T.Fields.tags t : _ String.Map.t)
                ~init:[]
                ~f:(fun ~key:tag_name ~data:tag_value accum ->
                  match String_dict.find field_names tag_name with
                  | Some (`Special_tags_field | `Field) ->
                    raise_s
                      [%sexp
                        "tag has same name as a field"
                        , ([%here].Source_code_position.pos_fname : string)
                        , { tag_name : string
                          ; field_names : [ `Field | `Special_tags_field ] String_dict.t
                          ; tag_value : Sexp.t
                          }]
                  | None -> Sexp.List [ Sexp.Atom tag_name; tag_value ] :: accum)
            in
            Sexp.List (List.rev_append sexp_without_tags tags_sexp)
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
    module%template
      [@modality p = (portable, nonportable)] V1
        (T : sig
         @@ p
           open! Core
           open! Import

           type t [@@deriving compare, sexp]

           module Fields : sig
             val names : string list
             val tags : ([ `Read | `Set_and_create ], t, Tags.V1.t) Field.t_with_perm
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
module Tags = Stable.Tags.Current
