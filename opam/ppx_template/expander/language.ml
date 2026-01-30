open! Stdppx
open! Import
include Language_intf.Definitions
open Result.Let_syntax

module Type = struct
  include Type

  let equal_witness_non_tuple
    : type a b. a non_tuple -> b non_tuple -> (a, b) Stdlib.Type.eq option
    =
    fun t1 t2 ->
    match t1, t2 with
    | Kind, Kind -> Some Equal
    | Mode, Mode -> Some Equal
    | Modality, Modality -> Some Equal
    | Alloc, Alloc -> Some Equal
    | Synchro, Synchro -> Some Equal
    | (Kind | Mode | Modality | Alloc | Synchro), _ -> None
  ;;

  let sexp_of_non_tuple : type a. a non_tuple -> Sexp.t = function
    | Kind -> Atom "Kind"
    | Mode -> Atom "Mode"
    | Modality -> Atom "Modality"
    | Alloc -> Atom "Alloc"
    | Synchro -> Atom "Synchro"
  ;;

  let rec equal_witness : type a b. a t -> b t -> (a, b) Stdlib.Type.eq option =
    fun t1 t2 ->
    match t1, t2 with
    | Non_tuple b1, Non_tuple b2 ->
      (match equal_witness_non_tuple b1 b2 with
       | Some Equal -> Some Equal
       | None -> None)
    | Tuple tp1, Tuple tp2 ->
      (match equal_tuple_witness tp1 tp2 with
       | None -> None
       | Some Equal -> Some Equal)
    | (Non_tuple _ | Tuple _), _ -> None

  and equal_tuple_witness : type a b. a tuple -> b tuple -> (a, b) Stdlib.Type.eq option =
    fun tp1 tp2 ->
    match tp1, tp2 with
    | [], [] -> Some Equal
    | hd1 :: tl1, hd2 :: tl2 ->
      (match equal_witness hd1 hd2 with
       | None -> None
       | Some Equal ->
         (match equal_tuple_witness tl1 tl2 with
          | None -> None
          | Some Equal -> Some Equal))
    | [], _ :: _ | _ :: _, [] -> None
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Non_tuple non_tuple -> sexp_of_non_tuple non_tuple
    | Tuple [ (Non_tuple Alloc as t1); (Non_tuple Mode as t2) ] ->
      List [ sexp_of_t t1; Atom "@"; sexp_of_t t2 ]
    | Tuple [ (Non_tuple Synchro as t1); (Non_tuple Mode as t2) ] ->
      List [ sexp_of_t t1; Atom "@"; sexp_of_t t2 ]
    | Tuple tp -> List (Atom "Tuple" :: sexp_of_tuple tp)

  and sexp_of_tuple : type a. a tuple -> Sexp.t list = function
    | [] -> []
    | hd :: tl -> sexp_of_t hd :: sexp_of_tuple tl
  ;;

  let compare_packed : packed -> packed -> int = Poly.compare
  let sexp_of_packed (P t) = sexp_of_t t
  let kind = Non_tuple Kind
  let mode = Non_tuple Mode
  let modality = Non_tuple Modality
  let alloc = Non_tuple Alloc
  let synchro = Non_tuple Synchro
  let tuple2 t1 t2 = Tuple [ t1; t2 ]
end

module Untyped = struct
  include Untyped

  module Axis = struct
    include Axis

    let compare : t -> t -> int = Poly.compare

    module Map = Map.Make (struct
        type nonrec t = t

        let compare = compare
      end)
  end

  module Identifier = struct
    include Identifier

    let compare t1 t2 = String.compare t1.ident t2.ident
    let sexp_of_t t = Atom t.ident
  end

  module Value = struct
    include Value

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Kind_product kinds1, Kind_product kinds2 ->
        Nonempty_list.compare kinds1 kinds2 ~cmp:compare
      | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
        (match compare kind1 kind2 with
         | 0 ->
           let mods1 = Nonempty_list.sort_uniq ~cmp:compare mods1 in
           let mods2 = Nonempty_list.sort_uniq ~cmp:compare mods2 in
           Nonempty_list.compare ~cmp:compare mods1 mods2
         | n -> n)
      | Tuple ts1, Tuple ts2 -> Nonempty_list.compare ~cmp:compare ts1 ts2
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Kind_product _, _ -> -1
      | _, Kind_product _ -> 1
      | Kind_mod _, _ -> -1
      | _, Kind_mod _ -> 1
      | Tuple _, _ -> .
      | _, Tuple _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Identifier ident -> Identifier.sexp_of_t ident
      | Kind_product kinds ->
        List [ Atom "Product"; kinds |> Nonempty_list.to_list |> sexp_of_list sexp_of_t ]
      | Kind_mod (kind, mods) ->
        List
          [ Atom "Mod"
          ; sexp_of_t kind
          ; mods
            |> Nonempty_list.to_list
            |> List.sort_uniq ~cmp:compare
            |> sexp_of_list sexp_of_t
          ]
      | Tuple ts ->
        List (Atom "Tuple" :: (ts |> Nonempty_list.to_list |> List.map ~f:sexp_of_t))
    ;;
  end

  module Pattern = struct
    include Pattern

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Wildcard, Wildcard -> 0
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Tuple ts1, Tuple ts2 -> Nonempty_list.compare ~cmp:compare ts1 ts2
      | Wildcard, _ -> -1
      | _, Wildcard -> 1
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Tuple _, _ -> .
      | _, Tuple _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Wildcard -> Atom "Wildcard"
      | Identifier ident -> Identifier.sexp_of_t ident
      | Tuple ts ->
        List (Atom "Tuple" :: (ts |> Nonempty_list.to_list |> List.map ~f:sexp_of_t))
    ;;
  end

  module Expression = struct
    include Expression

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Kind_product kinds1, Kind_product kinds2 ->
        Nonempty_list.compare kinds1 kinds2 ~cmp:compare
      | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
        (match compare kind1 kind2 with
         | 0 ->
           let mods1 = Nonempty_list.sort_uniq ~cmp:compare mods1 in
           let mods2 = Nonempty_list.sort_uniq ~cmp:compare mods2 in
           Nonempty_list.compare ~cmp:compare mods1 mods2
         | n -> n)
      | Kind_coercion (kind1, coerce_to1), Kind_coercion (kind2, coerce_to2) ->
        (match compare kind1 kind2 with
         | 0 -> compare coerce_to1 coerce_to2
         | n -> n)
      | Comma_separated ts1, Comma_separated ts2 ->
        Nonempty_list.compare ~cmp:compare ts1 ts2
      | Typed (t1, typ1), Typed (t2, typ2) ->
        (match compare t1 t2 with
         | 0 -> Type.compare_packed typ1 typ2
         | n -> n)
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Kind_product _, _ -> -1
      | _, Kind_product _ -> 1
      | Kind_mod _, _ -> -1
      | _, Kind_mod _ -> 1
      | Kind_coercion _, _ -> -1
      | _, Kind_coercion _ -> 1
      | Comma_separated _, _ -> -1
      | _, Comma_separated _ -> 1
      | Typed _, _ -> .
      | _, Typed _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Identifier ident -> Identifier.sexp_of_t ident
      | Kind_product kinds ->
        List (Atom "Product" :: (kinds |> Nonempty_list.to_list |> List.map ~f:sexp_of_t))
      | Kind_mod (kind, mods) ->
        List
          [ Atom "Mod"
          ; sexp_of_t kind
          ; mods
            |> Nonempty_list.to_list
            |> List.sort_uniq ~cmp:compare
            |> sexp_of_list sexp_of_t
          ]
      | Kind_coercion (kind, coerce_to) ->
        List [ Atom "Kind_coercion"; sexp_of_t kind; sexp_of_t coerce_to ]
      | Comma_separated ts ->
        List
          (Atom "Comma_separated" :: (ts |> Nonempty_list.to_list |> List.map ~f:sexp_of_t)
          )
      | Typed (t, typ) -> List [ sexp_of_t t; Atom ":"; Type.sexp_of_packed typ ]
    ;;
  end
end

module Typed = struct
  include Typed

  module Vec = struct
    (* Used in some logic below to make error handling easier. *)

    type ('item, 'length) t =
      | [] : (_, unit) t
      | ( :: ) : 'item * ('item, 'length) t -> ('item, _ * 'length) t

    type wrong_length = Wrong_length

    let rec of_list_with_length
      : type item length.
        item list -> length Type.tuple -> ((item, length) t, wrong_length) result
      =
      fun items length ->
      match items, length with
      | [], [] -> Ok []
      | hd_items :: tl_items, _ :: tl_length ->
        let* tl = of_list_with_length tl_items tl_length in
        Ok (hd_items :: tl)
      | _ :: _, [] | [], _ :: _ -> Error Wrong_length
    ;;
  end

  module Axis = struct
    include Axis

    let compare_packed : packed -> packed -> int = Poly.compare

    let of_type : type a. a Type.non_tuple Type.t -> a Type.non_tuple t = function
      | Non_tuple Kind -> Singleton Kind
      | Non_tuple Mode -> Singleton Mode
      | Non_tuple Modality -> Singleton Modality
      | Non_tuple Alloc -> Singleton Alloc
      | Non_tuple Synchro -> Singleton Synchro
    ;;

    let is_set : type a. a t -> bool = function
      | Set _ -> true
      | Singleton _ -> false
    ;;

    module Map = Map.Make (struct
        type t = packed

        let compare = compare_packed
      end)

    module Sub_axis = struct
      include Sub_axis

      module Modal = struct
        include Modal

        let of_mode_identifier : string -> t Or_unrecognized.t = function
          | "global" | "local" -> Known Locality
          | "nonportable" | "portable" -> Known Portability
          | "uncontended" | "contended" | "shared" -> Known Contention
          | "stateful" | "observing" | "stateless" -> Known Statefulness
          | "read_write" | "read" | "immutable" -> Known Visibility
          | "many" | "once" -> Known Linearity
          | "aliased" | "unique" -> Known Uniqueness
          | "unyielding" | "yielding" -> Known Yielding
          | "forkable" | "unforkable" -> Known Forkable
          | _ -> Unrecognized
        ;;

        let sexp_of_t = function
          | Locality -> Atom "Locality"
          | Portability -> Atom "Portability"
          | Contention -> Atom "Contention"
          | Statefulness -> Atom "Statefulness"
          | Visibility -> Atom "Visibility"
          | Linearity -> Atom "Linearity"
          | Uniqueness -> Atom "Uniqueness"
          | Yielding -> Atom "Yielding"
          | Forkable -> Atom "Forkable"
        ;;
      end

      module Mode = struct
        include Mode

        let default (Mode m) =
          match m with
          | Locality -> "global"
          | Portability -> "nonportable"
          | Contention -> "uncontended"
          | Statefulness -> "stateful"
          | Visibility -> "read_write"
          | Linearity -> "many"
          | Uniqueness -> "aliased"
          | Yielding -> "unyielding"
          | Forkable ->
            (* Above comment also applies to [forkable]. *)
            "forkable"
        ;;

        let of_mode_identifier str : _ Or_unrecognized.t =
          match Modal.of_mode_identifier str with
          | Known m -> Known (Mode m)
          | Unrecognized -> Unrecognized
        ;;

        let sexp_of_t (Mode m) = Modal.sexp_of_t m
      end

      module Modality = struct
        include Modality

        let default (Modality m) =
          match m with
          | Locality -> "local"
          | Portability -> "nonportable"
          | Contention -> "uncontended"
          | Statefulness -> "stateful"
          | Visibility -> "read_write"
          | Linearity -> "once"
          | Uniqueness -> "unique"
          | Yielding -> "yielding"
          | Forkable -> "unforkable"
        ;;

        let of_mode_identifier str : _ Or_unrecognized.t =
          match Modal.of_mode_identifier str with
          | Known m -> Known (Modality m)
          | Unrecognized -> Unrecognized
        ;;

        let sexp_of_t (Modality m) = Modal.sexp_of_t m
      end

      module Or_unrecognized = struct
        include Or_unrecognized

        let sexp_of_t sexp_of_a = function
          | Known a -> sexp_of_a a
          | Unrecognized -> Atom "unrecognized"
        ;;
      end

      let compare_packed : packed -> packed -> int = Poly.compare

      module Map = Stdlib.Map.Make (struct
          type t = packed

          let compare = compare_packed
        end)

      let of_identifier : type a. a Type.non_tuple Identifier.t -> a Type.non_tuple t =
        fun { ident; type_ = Non_tuple type_ } ->
        match type_ with
        | Kind -> Kind
        | Mode -> Mode (Mode.of_mode_identifier ident)
        | Modality -> Modality (Modality.of_mode_identifier ident)
        | Alloc -> Alloc
        | Synchro -> Synchro
      ;;

      let of_value : type a. a Type.non_tuple Value.t -> a Type.non_tuple t = function
        | Identifier ident -> of_identifier ident
        | Kind_product _ -> Kind
        | Kind_mod _ -> Kind
      ;;

      let sexp_of_t : type a. a t -> Sexp.t = function
        | Kind -> Atom "Kind"
        | Mode m -> List [ Atom "Mode"; Or_unrecognized.sexp_of_t Mode.sexp_of_t m ]
        | Modality m ->
          List [ Atom "Modality"; Or_unrecognized.sexp_of_t Modality.sexp_of_t m ]
        | Alloc -> Atom "Alloc"
        | Synchro -> Atom "Synchro"
      ;;
    end

    module Namespace = struct
      include Namespace

      let rec of_value : type a. is_set:bool -> a Value.t -> a t =
        fun ~is_set val_ ->
        let to_namespace sub_axis = if is_set then Set sub_axis else Singleton sub_axis in
        match val_ with
        | Identifier _ -> to_namespace (Sub_axis.of_value val_)
        | Kind_product _ -> to_namespace (Sub_axis.of_value val_)
        | Kind_mod _ -> to_namespace (Sub_axis.of_value val_)
        | Tuple tp -> Tuple (of_value_tuple ~is_set tp)

      and of_value_tuple : type a. is_set:bool -> a Value.tuple -> a tuple =
        fun ~is_set tp ->
        match tp with
        | [] -> []
        | hd :: tl -> of_value ~is_set hd :: of_value_tuple ~is_set tl
      ;;

      (* equality modulo mode/modality conflation *)
      let rec same_namespace : type a b. a t -> b t -> bool =
        fun tp1 tp2 ->
        match tp1, tp2 with
        | Singleton tp1, Singleton tp2 | Set tp1, Set tp2 ->
          (* We conflate modalities and modes *)
          (match tp1, tp2 with
           | Mode (Known (Mode m1)), Modality (Known (Modality m2)) -> m1 = m2
           | Modality (Known (Modality m1)), Mode (Known (Mode m2)) -> m1 = m2
           | Mode Unrecognized, Modality Unrecognized
           | Modality Unrecognized, Mode Unrecognized -> true
           | _, _ -> Sub_axis.compare_packed (P tp1) (P tp2) = 0)
        | Tuple tp1, Tuple tp2 -> same_namespace_tuple tp1 tp2
        | (Singleton _ | Set _), Tuple _
        | Tuple _, (Singleton _ | Set _)
        | Singleton _, Set _
        | Set _, Singleton _ -> false

      and same_namespace_tuple : type a b. a tuple -> b tuple -> bool =
        fun tp1 tp2 ->
        match tp1, tp2 with
        | [], [] -> true
        | hd1 :: tl1, hd2 :: tl2 -> same_namespace hd1 hd2 && same_namespace_tuple tl1 tl2
        | [], _ :: _ | _ :: _, [] -> false
      ;;

      let rec sexp_of_t : type a. a t -> Sexp.t = function
        | Singleton t -> Sub_axis.sexp_of_t t
        | Set t -> List [ Atom "Set"; Sub_axis.sexp_of_t t ]
        | Tuple tp -> List (Atom "Tuple" :: sexp_of_tuple tp)

      and sexp_of_tuple : type a. a tuple -> Sexp.t list = function
        | [] -> []
        | hd :: tl -> sexp_of_t hd :: sexp_of_tuple tl
      ;;
    end
  end

  module Identifier = struct
    include Identifier

    let equal_witness { type_; ident } t =
      match Type.equal_witness type_ t.type_ with
      | Some _ as eq when String.equal ident t.ident -> eq
      | _ -> None
    ;;
  end

  module Value = struct
    include Value

    let rec untype : type a. a t -> Untyped.Value.t = function
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:untype)
      | Kind_mod (kind, mods) -> Kind_mod (untype kind, Nonempty_list.map mods ~f:untype)
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a b. (a * b) tuple -> Untyped.Value.t Nonempty_list.t
      = function
      | [ hd ] -> [ untype hd ]
      | hd :: (_ :: _ as tl) -> untype hd :: Nonempty_list.to_list (untype_tuple tl)
    ;;

    let compare t1 t2 = Untyped.Value.compare (untype t1) (untype t2)

    let rec type_ : type a. a t -> a Type.t = function
      | Identifier ident -> ident.type_
      | Kind_mod _ -> Type.kind
      | Kind_product _ -> Type.kind
      | Tuple tp -> Tuple (type_tuple tp)

    and type_tuple : type a. a tuple -> a Type.tuple = function
      | [] -> []
      | hd :: tl -> type_ hd :: type_tuple tl
    ;;

    (* Translate a value back into the expression that most directly corresponds to it.
       More exactly, [eval (as_expression t) === t] when evaluating in an empty
       environment. *)
    let rec as_expression : type a. a t -> (a, Expression.singleton) Expression.t
      = function
      | Identifier ident -> Identifier ident
      | Kind_mod (kind, mods) ->
        Kind_mod (as_expression kind, Nonempty_list.map mods ~f:as_expression)
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:as_expression)
      | Tuple tp -> Tuple (uneval_tuple tp)

    and uneval_tuple : type a. a tuple -> a Expression.tuple = function
      | [] -> []
      | hd :: tl -> as_expression hd :: uneval_tuple tl
    ;;

    let is_default : type a. a Type.non_tuple t -> bool =
      fun value ->
      match type_ value, value with
      | Non_tuple Kind, Identifier { ident = "value" | "value_or_null"; _ } -> true
      | Non_tuple Kind, _ -> false
      | Non_tuple Mode, Identifier ident ->
        (match Axis.Sub_axis.of_identifier ident with
         | Mode Unrecognized -> false
         | Mode (Known axis) -> String.equal ident.ident (Axis.Sub_axis.Mode.default axis))
      | Non_tuple Modality, Identifier ident ->
        (match Axis.Sub_axis.of_identifier ident with
         | Modality Unrecognized -> false
         | Modality (Known axis) ->
           String.equal ident.ident (Axis.Sub_axis.Modality.default axis))
      | Non_tuple Alloc, Identifier { ident; _ } -> String.equal ident "heap"
      | Non_tuple Synchro, Identifier { ident; _ } -> String.equal ident "unsync"
    ;;

    let rec to_node
      : type a. a Type.non_tuple t -> loc:location -> a Type.non_tuple Node.t
      =
      fun t ~loc ->
      match t with
      | Identifier { ident; type_ } ->
        (match type_ with
         | Non_tuple Mode -> Mode { txt = Mode ident; loc }
         | Non_tuple Modality -> Modality { txt = Modality ident; loc }
         | Non_tuple Kind ->
           Jkind_annotation { pjkind_desc = Pjk_abbreviation ident; pjkind_loc = loc }
         | Non_tuple Alloc -> Alloc
         | Non_tuple Synchro -> Synchro)
      | Kind_product kinds ->
        let kinds =
          kinds
          |> Nonempty_list.to_list
          |> List.map ~f:(fun kind ->
            let (Jkind_annotation kind) = to_node ~loc kind in
            kind)
        in
        Jkind_annotation { pjkind_desc = Pjk_product kinds; pjkind_loc = loc }
      | Kind_mod (kind, mods) ->
        let (Jkind_annotation kind) = to_node ~loc kind in
        let mods =
          mods
          |> Nonempty_list.to_list
          |> List.sort_uniq ~cmp:compare
          |> List.map ~f:(fun (Identifier { ident; type_ = Non_tuple Modality }) ->
            { txt = Mode ident; loc })
        in
        Jkind_annotation { pjkind_desc = Pjk_mod (kind, mods); pjkind_loc = loc }
    ;;
  end

  module Pattern = struct
    include Pattern

    let rec untype : type a. a t -> Untyped.Pattern.t = function
      | Wildcard -> Wildcard
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a b. (a * b) tuple -> Untyped.Pattern.t Nonempty_list.t
      = function
      | [ hd ] -> [ untype hd ]
      | hd :: (_ :: _ as tl) -> untype hd :: Nonempty_list.to_list (untype_tuple tl)
    ;;

    let rec type_check
      : type a. Untyped.Pattern.t -> expected:a Type.t -> (a t, Type_error.t) result
      =
      fun untyped ~expected ->
      match untyped, expected with
      | Wildcard, _ -> Ok Wildcard
      | Identifier { ident }, type_ -> Ok (Identifier { ident; type_ })
      | Tuple untyped_tp, Tuple type_tp ->
        let* vec =
          match Vec.of_list_with_length (Nonempty_list.to_list untyped_tp) type_tp with
          | Ok vec -> Ok vec
          | Error Wrong_length ->
            Error
              (Type_error.Tuple_length_mismatch
                 { kind = "pattern"
                 ; sexp_of_kind = Untyped.Pattern.sexp_of_t
                 ; value = Tuple untyped_tp
                 ; expected_type = Tuple type_tp
                 })
        in
        let+ tuple = type_check_tuple vec ~expected:type_tp in
        Tuple tuple
      | (Tuple _ as node), (Non_tuple _ as type_) ->
        Error
          (Type_mismatch
             { kind = "pattern"
             ; sexp_of_kind = Untyped.Pattern.sexp_of_t
             ; value = node
             ; expected_type = type_
             ; expected_sets = None
             ; hint = None
             })

    and type_check_tuple
      : type a.
        (Untyped.Pattern.t, a) Vec.t
        -> expected:a Type.tuple
        -> (a tuple, Type_error.t) result
      =
      fun untyped ~expected ->
      match untyped, expected with
      | [], [] -> Ok []
      | untyped_hd :: untyped_tl, type_hd :: type_tl ->
        let* hd = type_check untyped_hd ~expected:type_hd in
        let+ tl = type_check_tuple untyped_tl ~expected:type_tl in
        hd :: tl
    ;;
  end

  module Expression = struct
    include Expression

    let sexp_of_sets : type s. (string, s) allow_set -> Sexp.t = function
      | Singleton_only _hint -> Atom "singleton"
      | Set_or_singleton -> Atom "set with unions"
    ;;

    let rec type_ : type a is_set. (a, is_set) t -> a Type.t = function
      | Identifier { type_; _ } -> type_
      | Kind_mod _ -> Type.kind
      | Kind_product _ -> Type.kind
      | Kind_coercion _ -> Type.kind
      | Tuple tp -> Tuple (type_tuple tp)
      | Union (hd :: _) -> type_ hd

    and type_tuple : type a b. (a * b) tuple -> (a * b) Type.tuple = function
      | [ hd ] -> [ type_ hd ]
      | hd :: (_ :: _ as tl) -> type_ hd :: type_tuple tl
    ;;

    let rec untype : type a is_set. (a, is_set) t -> Untyped.Expression.t = function
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Kind_mod (kind, mods) -> Kind_mod (untype kind, Nonempty_list.map mods ~f:untype)
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:untype)
      | Kind_coercion (kind, coerce_to) -> Kind_coercion (untype kind, untype coerce_to)
      | Tuple tp -> Comma_separated (untype_tuple tp)
      | Union ts -> Comma_separated (Nonempty_list.map ts ~f:untype)

    and untype_tuple : type a b. (a * b) tuple -> Untyped.Expression.t Nonempty_list.t
      = function
      | [ hd ] -> [ untype hd ]
      | hd :: (_ :: _ as tl) -> untype hd :: Nonempty_list.to_list (untype_tuple tl)
    ;;

    let rec to_set : type a is_set. (a, is_set) t -> (a, set) t = function
      | Identifier ident -> Identifier ident
      | Kind_mod (kind, mods) -> Kind_mod (to_set kind, mods)
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:to_set)
      | Kind_coercion (kind, coerce_to) -> Kind_coercion (to_set kind, coerce_to)
      | Tuple tp -> Tuple tp
      | Union ts -> Union (Nonempty_list.map ts ~f:to_set)
    ;;

    let type_mismatch
      (type s)
      ?hint
      ~value
      ~expected
      ~(allow_set : (string, s) Expression.allow_set)
      ()
      =
      Error
        (Type_error.Type_mismatch
           { kind = "expression"
           ; sexp_of_kind = Untyped.Expression.sexp_of_t
           ; value
           ; expected_type = expected
           ; expected_sets = Some allow_set
           ; hint
           })
    ;;

    let rec type_check
      : type a s.
        Untyped.Expression.t
        -> expected:a Type.t
        -> allow_set:(string, s) allow_set
        -> ((a, s) t, Type_error.t) result
      =
      fun untyped ~expected ~allow_set ->
      match untyped, expected with
      | Identifier { ident }, type_ -> Ok (Identifier { ident; type_ })
      | Kind_product kinds, Non_tuple Kind ->
        let+ kinds =
          Nonempty_list.map_result
            kinds
            ~f:(type_check ~expected:(Non_tuple Kind) ~allow_set)
        in
        Kind_product kinds
      | (Kind_product _ as value), expected ->
        type_mismatch ~value ~expected ~allow_set ()
      | Kind_mod (kind, mods), Non_tuple Kind ->
        let* kind = type_check kind ~expected:(Non_tuple Kind) ~allow_set in
        let+ mods =
          Nonempty_list.map_result
            mods
            ~f:
              (type_check
                 ~expected:(Non_tuple Modality)
                 ~allow_set:
                   (Singleton_only
                      { why_no_set = "sets not allowed inside [kind mod] modalities" }))
        in
        Kind_mod (kind, mods)
      | Kind_coercion (kind, coerce_to), Non_tuple Kind ->
        let* kind = type_check kind ~expected ~allow_set in
        let+ coerce_to = type_check coerce_to ~expected ~allow_set:Set_or_singleton in
        Kind_coercion (kind, coerce_to)
      | (Kind_coercion _ as value), expected ->
        type_mismatch ~value ~expected ~allow_set ()
      | (Kind_mod _ as value), expected -> type_mismatch ~value ~expected ~allow_set ()
      | Comma_separated untyped_tp, Tuple type_tp ->
        let* vec =
          match Vec.of_list_with_length (Nonempty_list.to_list untyped_tp) type_tp with
          | Ok vec -> Ok vec
          | Error Wrong_length ->
            Error
              (Type_error.Tuple_length_mismatch
                 { kind = "expression"
                 ; sexp_of_kind = Untyped.Expression.sexp_of_t
                 ; value = untyped
                 ; expected_type = expected
                 })
        in
        let+ tuple = type_check_tuple vec ~expected:type_tp in
        Tuple tuple
      | Comma_separated untyped_union, Non_tuple _ ->
        (match allow_set with
         | Singleton_only { why_no_set = hint } ->
           type_mismatch ~hint ~value:untyped ~expected ~allow_set ()
         | Set_or_singleton ->
           let+ subsets =
             Nonempty_list.map_result
               untyped_union
               ~f:(type_check ~expected ~allow_set:Set_or_singleton)
           in
           Union (subsets :> (_, set) t Nonempty_list.t))
      | (Typed (untyped, P typ) as value), expected ->
        (match Type.equal_witness typ expected with
         | None -> type_mismatch ~value ~expected ~allow_set ()
         | Some Equal -> type_check untyped ~expected ~allow_set)

    and type_check_tuple
      : type a.
        (Untyped.Expression.t, a) Vec.t
        -> expected:a Type.tuple
        -> (a tuple, Type_error.t) result
      =
      fun untyped ~expected ->
      match untyped, expected with
      | [], [] -> Ok []
      | untyped_hd :: untyped_tl, type_hd :: type_tl ->
        let* hd =
          type_check
            untyped_hd
            ~expected:type_hd
            ~allow_set:(Singleton_only { why_no_set = "sets not allowed inside tuples" })
        in
        let+ tl = type_check_tuple untyped_tl ~expected:type_tl in
        hd :: tl
    ;;
  end

  module Env = struct
    include Env

    let value_entry ident value ~is_set : Entry.t =
      Entry
        { ident
        ; preserve_atoms = value
        ; expand_atoms_bound_to_sets = [ value ]
        ; namespace = Axis.Namespace.of_value ~is_set value
        }
    ;;

    let set_entry ident (values : _ Nonempty_list.t) : Entry.t =
      let namespace =
        let (value :: _) =
          (* We make the simplifying assumption that all values in the set have have the
             same namespace *)
          values
        in
        Axis.Namespace.of_value ~is_set:true value
      in
      Entry
        { ident
        ; preserve_atoms = Identifier ident
        ; expand_atoms_bound_to_sets = values
        ; namespace
        }
    ;;

    let initial : t =
      let open struct
        type 'a _identifier = 'a Identifier.t =
          { type_ : 'a Type.t
          ; ident : string
          }
      end in
      let value_entry ?(is_set = true) ident value = value_entry ident value ~is_set in
      let alloc_mode_entries : t =
        let heap_alloc = { ident = "heap"; type_ = Type.alloc } in
        let stack_alloc = { ident = "stack"; type_ = Type.alloc } in
        let global_mode = { ident = "global"; type_ = Type.mode } in
        let local_mode = { ident = "local"; type_ = Type.mode } in
        let heap_alloc_mode =
          { ident = "heap_global"; type_ = Type.(tuple2 alloc mode) }
        in
        let stack_alloc_mode =
          { ident = "stack_local"; type_ = Type.(tuple2 alloc mode) }
        in
        [ value_entry heap_alloc (Identifier heap_alloc)
        ; value_entry stack_alloc (Identifier stack_alloc)
        ; value_entry
            ~is_set:false
            heap_alloc_mode
            (Tuple [ Identifier heap_alloc; Identifier global_mode ])
        ; value_entry
            ~is_set:false
            stack_alloc_mode
            (Tuple [ Identifier stack_alloc; Identifier local_mode ])
        ]
      in
      let synchro_mode_entries : t =
        let unsync = { ident = "unsync"; type_ = Type.synchro } in
        let sync = { ident = "sync"; type_ = Type.synchro } in
        let uncontended = { ident = "uncontended"; type_ = Type.mode } in
        let shared = { ident = "shared"; type_ = Type.mode } in
        let contended = { ident = "contended"; type_ = Type.mode } in
        let unsync_uncontended =
          { ident = "unsync_uncontended"; type_ = Type.(tuple2 synchro mode) }
        in
        let sync_shared = { ident = "sync_shared"; type_ = Type.(tuple2 synchro mode) } in
        let sync_contended =
          { ident = "sync_contended"; type_ = Type.(tuple2 synchro mode) }
        in
        [ value_entry unsync (Identifier unsync)
        ; value_entry sync (Identifier sync)
        ; value_entry
            ~is_set:false
            unsync_uncontended
            (Tuple [ Identifier unsync; Identifier uncontended ])
        ; value_entry
            ~is_set:false
            sync_shared
            (Tuple [ Identifier sync; Identifier shared ])
        ; value_entry
            ~is_set:false
            sync_contended
            (Tuple [ Identifier sync; Identifier contended ])
        ]
      in
      let kind_set_entries : t =
        let kind_ident k = { ident = k; type_ = Type.kind } in
        let kind k : _ Value.t = Identifier (kind_ident k) in
        let base_non_value : _ Nonempty_list.t =
          [ kind "bits64"; kind "bits32"; kind "word"; kind "float64"; kind "float32" ]
        in
        (* [value] is last so that, when templating record or variant types, the fields
           and constructors for the unmangled type are the ones in scope. *)
        let value_with_imm : _ Nonempty_list.t =
          [ kind "immediate"; kind "immediate64"; kind "value" ]
        in
        let value_or_null_with_imm : _ Nonempty_list.t =
          [ kind "immediate"; kind "immediate64"; kind "value_or_null" ]
        in
        let kind_set_ident = kind_ident in
        [ set_entry (kind_set_ident "base_non_value") base_non_value
        ; set_entry (kind_set_ident "value_with_imm") value_with_imm
        ; set_entry (kind_set_ident "value_or_null_with_imm") value_or_null_with_imm
        ; set_entry
            (kind_set_ident "base")
            (Nonempty_list.concat [ base_non_value; [ kind "value" ] ])
        ; set_entry
            (kind_set_ident "base_with_imm")
            (Nonempty_list.concat [ base_non_value; value_with_imm ])
        ; set_entry
            (kind_set_ident "base_or_null")
            (Nonempty_list.concat [ base_non_value; [ kind "value_or_null" ] ])
        ; set_entry
            (kind_set_ident "base_or_null_with_imm")
            (Nonempty_list.concat [ base_non_value; value_or_null_with_imm ])
        ]
      in
      alloc_mode_entries @ synchro_mode_entries @ kind_set_entries
    ;;

    let find (type a) (t : t) (ident : a Identifier.t) =
      List.find_map
        t
        ~f:
          (fun
            (Entry
              { ident = ident'
              ; preserve_atoms = value
              ; expand_atoms_bound_to_sets = _
              ; namespace = _
              })
          ->
          Option.map (Identifier.equal_witness ident ident') ~f:(fun Equal : a Value.t ->
            value))
    ;;

    let find_expanding_sets (type a) (t : t) (ident : a Identifier.t) =
      List.find_map
        t
        ~f:
          (fun
            (Entry
              { ident = ident'
              ; preserve_atoms = _
              ; expand_atoms_bound_to_sets = values
              ; namespace = _
              })
          ->
          Option.map
            (Identifier.equal_witness ident ident')
            ~f:(fun Equal : a Value.t Nonempty_list.t -> values))
    ;;

    module Eval_result = struct
      type (_, _) t =
        | Singleton : 'a -> ('a, [ `one ]) t
        | Set : 'a Nonempty_list.t -> ('a, [ `many ]) t

      let map_res
        : type a b r e. (a, r) t -> f:(a -> (b, e) result) -> ((b, r) t, e) result
        =
        fun t ~f ->
        match t with
        | Singleton value ->
          let+ value = f value in
          Singleton value
        | Set value ->
          let+ value = Nonempty_list.map_result value ~f in
          Set value
      ;;

      let map : type a b r. (a, r) t -> f:(a -> b) -> (b, r) t =
        fun t ~f ->
        match t with
        | Singleton value -> Singleton (f value)
        | Set values -> Set (Nonempty_list.map values ~f)
      ;;

      let bind_res
        : type a b r e. (a, r) t -> f:(a -> ((b, r) t, e) result) -> ((b, r) t, e) result
        =
        fun t ~f ->
        match t with
        | Singleton value -> f value
        | Set values ->
          let+ values =
            Nonempty_list.map_result values ~f:(fun value ->
              let+ (Set values) = f value in
              values)
          in
          Set (Nonempty_list.concat values)
      ;;

      let product : type a r. (a, r) t Nonempty_list.t -> (a Nonempty_list.t, r) t =
        fun (hd :: tl) ->
        match hd with
        | Singleton hd ->
          let tl = List.map tl ~f:(fun (Singleton tl) -> tl) in
          Singleton (hd :: tl)
        | Set hd ->
          let tl = List.map tl ~f:(fun (Set tl) -> tl) in
          Set (Nonempty_list.product (hd :: tl))
      ;;

      module Witness = struct
        type _ size =
          | One : [ `one ] size
          | Many : [ `many ] size

        type (_, _, _) size_reason =
          | Simple : (Expression.singleton, [ `singleton_lookup ], [ `one ]) size_reason
          | Expr_has_unions : (Expression.set, _, [ `many ]) size_reason
          | Lookup_is_expanding_identifiers : (_, [ `set_lookup ], [ `many ]) size_reason

        type _ lookup =
          | Preserve_atoms : [ `singleton_lookup ] lookup
          | Expand_atoms_bound_to_sets : [ `set_lookup ] lookup

        type ('s, 'r) t =
          | Eval_witness :
              { is_set : (unit, 's) Expression.allow_set
              ; lookup : 'l lookup
              ; result_size : 'r size
              ; because : ('s, 'l, 'r) size_reason
              }
              -> ('s, 'r) t

        let singleton =
          Eval_witness
            { is_set = Singleton_only { why_no_set = () }
            ; lookup = Preserve_atoms
            ; result_size = One
            ; because = Simple
            }
        ;;

        let explicit_set ~lookup =
          Eval_witness
            { is_set = Set_or_singleton
            ; lookup
            ; result_size = Many
            ; because = Expr_has_unions
            }
        ;;

        let expand_atoms ~is_set =
          Eval_witness
            { is_set
            ; lookup = Expand_atoms_bound_to_sets
            ; result_size = Many
            ; because = Lookup_is_expanding_identifiers
            }
        ;;
      end
    end

    include struct
      open Ppx_helpers.Ox

      let rec interpret_kind
        : loc:location -> Type.kind Value.t -> (Kind.t, Syntax_error.t) result
        =
        fun ~loc -> function
        | Identifier { ident; type_ = _ } ->
          (try Ok (Kind.of_ident_exn ~ident) with
           | _ ->
             Error (Syntax_error.createf ~loc "Unrecognized kind identifier: %s" ident))
        | Kind_product _ ->
          Error
            (Syntax_error.createf
               ~loc
               "Interpreting kind products in coercions is not supported")
        | Kind_mod (kind, mods) ->
          let* kind = interpret_kind ~loc kind in
          let+ mods =
            Nonempty_list.map_result mods ~f:(fun (Identifier { ident; type_ = _ }) ->
              try Ok (Jkind_mod.of_string ident) with
              | _ -> Error (Syntax_error.createf ~loc "Unrecognized crossing: %s" ident))
            >>| Nonempty_list.to_list
          in
          Kind.apply_mods kind (Jkind_modifiers.of_jkind_mods mods)
      ;;
    end

    let rec eval_general
      : type a s r.
        t
        -> loc:location
        -> (s, r) Eval_result.Witness.t
        -> (a, s) Expression.t
        -> ((a Value.t, r) Eval_result.t, Syntax_error.t) result
      =
      fun env ~loc eval_witness expr ->
      let (Eval_witness { is_set; lookup; result_size; because = result_size_reason }) =
        eval_witness
      in
      match expr with
      | Kind_product kinds ->
        kinds
        |> Nonempty_list.map_result ~f:(eval_general env ~loc eval_witness)
        >>| Eval_result.product
        >>| Eval_result.map ~f:(fun kinds : _ Value.t -> Kind_product kinds)
      | Kind_mod (kind, mods) ->
        let* kind = eval_general env ~loc eval_witness kind in
        let+ (Singleton mods) =
          Nonempty_list.map_result
            ~f:(eval_general env ~loc Eval_result.Witness.singleton)
            mods
          >>| Eval_result.product
        in
        let mods = Nonempty_list.sort_uniq ~cmp:Value.compare mods in
        Eval_result.map kind ~f:(fun kind : _ Value.t -> Kind_mod (kind, mods))
      | Kind_coercion (kind, coerce_to) ->
        let* kind = eval_general env ~loc eval_witness kind in
        let* (Set coerce_to) =
          eval_general
            env
            ~loc
            (Eval_result.Witness.explicit_set ~lookup:Expand_atoms_bound_to_sets)
            coerce_to
        in
        let* coerce_to =
          coerce_to
          |> Nonempty_list.map_result ~f:(fun kind ->
            let+ interpreted = interpret_kind ~loc kind in
            kind, interpreted)
          >>| Nonempty_list.to_list
        in
        Eval_result.map_res kind ~f:(fun kind ->
          match interpret_kind ~loc kind with
          | Error _ ->
            (* Permit invalid kinds on the LHS of the coercion *)
            Ok kind
          | Ok interpreted ->
            (match
               List.filter coerce_to ~f:(fun (_, coerce_to) ->
                 Ppx_helpers.Ox.Kind.is_subkind interpreted ~of_:coerce_to)
             with
             | [] -> Ok kind
             | hd :: tl as coerced_to ->
               let+ () =
                 (* If there is more than one coercion result, we want to assert that they
                    are all comparable, then take the most specific. If the coercions are
                    not comparable, we error. *)
                 List.map coerced_to ~f:(fun (val1, k1) ->
                   (* We do double the work here, but that's fine, these lists are always
                      small *)
                   List.map coerced_to ~f:(fun (val2, k2) ->
                     if not
                          Ppx_helpers.Ox.Kind.(
                            is_subkind k1 ~of_:k2 || is_subkind k2 ~of_:k1)
                     then (
                       let to_string kind =
                         kind
                         |> Value.untype
                         |> Untyped.Value.sexp_of_t
                         |> Sexplib0.Sexp.to_string_hum
                       in
                       Error
                         (Syntax_error.createf
                            ~loc
                            "Kind is a subkind of two incomparable coercion options:\n\
                            \  %s\n\
                            \  is subkind of %s\n\
                            \  and           %s\n\
                            \  But neither is a subkind of the other\n"
                            (to_string kind)
                            (to_string val1)
                            (to_string val2)))
                     else Ok ())
                   |> Result.all_unit)
                 |> Result.all_unit
               in
               (* Take the most specific kind *)
               let res, _ =
                 List.fold_left tl ~init:hd ~f:(fun ((_, k1) as min) ((_, k2) as el) ->
                   if Ppx_helpers.Ox.Kind.is_subkind k1 ~of_:k2 then min else el)
               in
               res))
      | Tuple tp ->
        let+ tp = eval_tuple env ~loc tp in
        let value : _ Value.t = Tuple tp in
        (match result_size with
         | One -> (Singleton value : (_, r) Eval_result.t)
         | Many -> Set [ value ])
      | Union exprs ->
        let Set_or_singleton, Many, _ = is_set, result_size, result_size_reason in
        Eval_result.bind_res (Set exprs) ~f:(fun expr ->
          expr
          |> Expression.to_set
          |> eval_general env ~loc (Eval_result.Witness.explicit_set ~lookup))
      | Identifier ident as expr ->
        let found : (_, r) Eval_result.t option =
          match lookup, result_size, result_size_reason with
          | Preserve_atoms, One, _ ->
            Option.map (find env ident) ~f:(fun value -> Eval_result.Singleton value)
          | Preserve_atoms, Many, _ ->
            Option.map (find env ident) ~f:(fun value -> Eval_result.Set [ value ])
          | Expand_atoms_bound_to_sets, Many, _ ->
            Option.map (find_expanding_sets env ident) ~f:(fun values ->
              Eval_result.Set values)
          | _ -> .
        in
        (match found with
         | Some value -> Ok value
         | None ->
           let typ = Expression.type_ expr in
           (match typ with
            | Non_tuple (Mode | Modality | Kind) ->
              (* We assume the identifier is a built-in and let the compiler provide an
                 error message if not. *)
              Ok
                (match result_size with
                 | One -> Singleton (Identifier ident)
                 | Many -> Set [ Identifier ident ])
            | Tuple _ | Non_tuple Alloc | Non_tuple Synchro ->
              let hint =
                match typ, ident.ident with
                (* Alloc typos *)
                | Non_tuple Alloc, "heap_global" -> Some "Did you mean [heap]?"
                | Non_tuple Alloc, "stack_local" -> Some "Did you mean [stack]?"
                | Tuple [ Non_tuple Alloc; Non_tuple Mode ], "heap" ->
                  Some "Did you mean [heap_global]?"
                | Tuple [ Non_tuple Alloc; Non_tuple Mode ], "stack" ->
                  Some "Did you mean [stack_local]?"
                (* Synchro typos *)
                | Non_tuple Synchro, "unsync_uncontended" -> Some "Did you mean [unsync]?"
                | Non_tuple Synchro, ("sync_shared" | "sync_contended") ->
                  Some "Did you mean [sync]?"
                | Tuple [ Non_tuple Synchro; Non_tuple Mode ], "unsync" ->
                  Some "Did you mean [unsync_uncontended]?"
                | Tuple [ Non_tuple Synchro; Non_tuple Mode ], "sync" ->
                  Some "Did you mean [sync_shared] or [sync_contended]?"
                (* Type mismatch *)
                | _ ->
                  (match
                     List.find_map env ~f:(fun (Entry { ident = ident'; _ }) ->
                       if String.equal ident'.ident ident.ident
                       then Some (Type.sexp_of_t ident'.type_)
                       else None)
                   with
                   | None -> None
                   | Some type_ ->
                     Some
                       (Printf.sprintf
                          "There is a template identifier [%s] in scope with type [%s]."
                          ident.ident
                          (Sexp.to_string type_)))
              in
              let hint_string =
                match hint with
                | None -> ""
                | Some hint -> "\nHint: " ^ hint
              in
              Error
                (Syntax_error.createf
                   ~loc
                   "Unbound template identifier [%s] of type [%a].%s"
                   ident.ident
                   Sexplib0.Sexp.pp_hum
                   (Type.sexp_of_t typ)
                   hint_string)))

    and eval_tuple
      : type a.
        t -> loc:location -> a Expression.tuple -> (a Value.tuple, Syntax_error.t) result
      =
      fun env ~loc tuple ->
      match tuple with
      | [] -> Ok []
      | hd :: tl ->
        let* (Singleton hd) = eval_general env ~loc Eval_result.Witness.singleton hd in
        let+ tl = eval_tuple env ~loc tl in
        (hd :: tl : _ Value.tuple)
    ;;

    let eval_singleton
      : type a.
        t
        -> (a, Expression.singleton) Expression.t Loc.t
        -> (a Value.t, Syntax_error.t) result
      =
      fun env { txt = expr; loc } ->
      let+ (Singleton res) = eval_general env ~loc Eval_result.Witness.singleton expr in
      res
    ;;

    let eval
      : type a.
        t
        -> lookup
        -> (a, Expression.set) Expression.t Loc.t
        -> (a Value.t Nonempty_list.t, Syntax_error.t) result
      =
      fun env lookup { txt = expr; loc } ->
      let[@inline] eval_lookup lookup =
        let+ (Set values) =
          eval_general env ~loc (Eval_result.Witness.explicit_set ~lookup) expr
        in
        values
      in
      match lookup with
      | Preserve_atoms -> eval_lookup Preserve_atoms
      | Expand_atoms_bound_to_sets -> eval_lookup Expand_atoms_bound_to_sets
    ;;

    (* We prohibit shadowing a variable that is already bound to values for a different
       sub-axis. Shadowing a variable in the same sub-axis is permitted. *)
    let check_ident_shadowing env ~loc ~is_set (pat : _ Identifier.t) value =
      match
        List.find_opt env ~f:(fun (Entry.Entry { ident; _ }) ->
          String.equal ident.ident pat.ident)
      with
      | None ->
        (* this identifier is not shadowing *)
        Ok ()
      | Some
          (Entry
            { ident = _
            ; preserve_atoms = old_value
            ; expand_atoms_bound_to_sets = set_value
            ; namespace = old_namespace
            }) ->
        let namespace = Axis.Namespace.of_value ~is_set value in
        if Axis.Namespace.same_namespace old_namespace namespace
        then Ok ()
        else
          Error
            (Syntax_error.createf
               ~loc
               "shadowing variables from a different namespace is prohibited\n\
                attempting to bind\n\
               \  identifier '%s'\n\
               \  in namespace '%a'\n\
                but it is already bound\n\
               \  in namespace '%a'\n\
               \  to the value '%a'\n\
               \  and when fully expanded '%a'"
               pat.ident
               Sexplib0.Sexp.pp
               (Axis.Namespace.sexp_of_t namespace)
               Sexplib0.Sexp.pp
               (Axis.Namespace.sexp_of_t old_namespace)
               Sexplib0.Sexp.pp
               (old_value |> Value.untype |> Untyped.Value.sexp_of_t)
               Sexplib0.Sexp.pp
               (List
                  (set_value
                   |> Nonempty_list.to_list
                   |> List.map ~f:(fun val_ ->
                     val_ |> Value.untype |> Untyped.Value.sexp_of_t))))
    ;;

    (* we only cast modes and modalities *)
    type 'a castable_witness =
      | Mode : Type.mode castable_witness
      | Modality : Type.modality castable_witness

    let cast_entry
      (type a b)
      (entry : a Entry.entry)
      (type_ : b Type.non_tuple Type.t)
      (castable : a castable_witness)
      ~is_set
      : Entry.t
      =
      let { ident; preserve_atoms; expand_atoms_bound_to_sets; namespace = _ }
        : _ Entry.entry
        =
        entry
      in
      let cast_ident (ident : a Identifier.t) : b Type.non_tuple Identifier.t =
        { type_; ident = ident.ident }
      in
      let cast_value (value : a Value.t) : b Type.non_tuple Value.t =
        match value, castable with
        | Identifier ident, _ -> Identifier (cast_ident ident)
        | _, _ -> .
      in
      let preserve_atoms = cast_value preserve_atoms in
      Entry
        { ident = cast_ident ident
        ; preserve_atoms
        ; expand_atoms_bound_to_sets =
            Nonempty_list.map expand_atoms_bound_to_sets ~f:cast_value
        ; namespace = Axis.Namespace.of_value ~is_set preserve_atoms
        }
    ;;

    let rec bind
      : type a.
        t
        -> loc:location
        -> is_set:bool
        -> a Pattern.t
        -> a Value.t
        -> (t, Syntax_error.t) result
      =
      fun env ~loc ~is_set pat value ->
      match pat, value with
      | Wildcard, _ ->
        (* Don't put wildcard patterns [_] into the environment, just like OCaml. *)
        Ok env
      | Identifier pat, value ->
        let* () = check_ident_shadowing env ~loc ~is_set pat value in
        let+ (Set values) =
          value
          |> Value.as_expression
          |> eval_general
               env
               ~loc
               (Eval_result.Witness.expand_atoms
                  ~is_set:(Singleton_only { why_no_set = () }))
        in
        let entry : _ Entry.entry =
          { ident = pat
          ; preserve_atoms = value
          ; expand_atoms_bound_to_sets = values
          ; namespace = Axis.Namespace.of_value ~is_set value
          }
        in
        let entries : Entry.t list =
          (* See comment about conflating axes at bottom of file. *)
          let (value :: _) = values in
          match value with
          | Identifier _ as value ->
            (match Axis.Sub_axis.of_value value with
             | Mode (Known (Mode (Portability | Contention | Statefulness | Visibility)))
               -> [ Entry entry; cast_entry entry Type.modality Mode ~is_set ]
             | Modality
                 (Known (Modality (Portability | Contention | Statefulness | Visibility)))
               -> [ Entry entry; cast_entry entry Type.mode Modality ~is_set ]
             | _ -> [ Entry entry ])
          | _ -> [ Entry entry ]
        in
        entries @ env
      | Tuple pat_tp, Tuple value_tp -> bind_tuple ~loc ~is_set env pat_tp value_tp
      | Tuple _, _ -> .

    and bind_tuple
      : type a.
        t
        -> loc:location
        -> is_set:bool
        -> a Pattern.tuple
        -> a Value.tuple
        -> (t, Syntax_error.t) result
      =
      fun env ~loc ~is_set pat_tp value_tp ->
      match pat_tp, value_tp with
      | [], [] -> Ok env
      | pat_hd :: pat_tl, value_hd :: value_tl ->
        let* env = bind env ~loc ~is_set pat_hd value_hd in
        bind_tuple env ~loc ~is_set pat_tl value_tl
    ;;

    let bind_set
      : type a.
        t
        -> loc:location
        -> a Type.non_tuple Pattern.t
        -> a Type.non_tuple Value.t Nonempty_list.t
        -> (t, Syntax_error.t) result
      =
      fun env ~loc pattern values ->
      match pattern with
      | Wildcard ->
        (* Don't put wildcard patterns [_] into the environment, just like OCaml. *)
        Ok env
      | Identifier ident ->
        let+ () =
          (* we sample one value from the set to check *)
          let (value :: _) = values in
          check_ident_shadowing env ~loc ~is_set:true ident value
        in
        set_entry ident values :: env
    ;;
  end
end

module Type_error = struct
  include Type_error

  let sexp_of_t : t -> Sexp.t = function
    | Type_mismatch
        { kind; sexp_of_kind; value; expected_type = type_; expected_sets = is_set; hint }
      ->
      Sexplib0.Sexp.message
        "Type mismatch"
        ([ "kind", Atom kind
         ; "value", sexp_of_kind value
         ; "expected type", Type.sexp_of_t type_
         ]
         @ (match is_set with
            | Some is_set -> [ "expected sets", Typed.Expression.sexp_of_sets is_set ]
            | None -> [])
         @
         match hint with
         | Some hint -> [ "hint", Atom hint ]
         | None -> [])
    | Tuple_length_mismatch { kind; sexp_of_kind; value; expected_type = type_ } ->
      Sexplib0.Sexp.message
        "Tuple length mismatch"
        [ "kind", Atom kind
        ; "value", sexp_of_kind value
        ; "expected type", Type.sexp_of_t type_
        ]
  ;;

  let to_error ~loc t =
    Syntax_error.createf
      ~loc
      "%s"
      (Sexp.to_string_hum (List [ Atom "[%template]"; sexp_of_t t ]))
  ;;

  let lift_to_error_result ~loc = function
    | Ok _ as ok -> ok
    | Error type_error -> Error (to_error ~loc type_error)
  ;;
end

(* Note about conflating axes (e.g. in [Env.bind]):

   We always conflate modes and modalities for the portability and contention axes. These
   axes have the property that the legacy mode coincides with the top mode for comonadic
   axes and bottom mode for monadic axes[^0]. When this holds, ['a @ m -> 'b @ n] is
   equivalent to ['a @@ m -> 'b @@ n].

   More thoroughly: let [t @@ m] be a type whenever [t] is a type and [m] is a modality,
   such that [t @@ m] behaves like
   {[
     type t_atat_m = { inner : t @@ m } [@@unboxed]
   ]}
   i.e. is a zero-cost modality box around the type. Note that we define [t @@ m] even
   when [m] is a modality that does nothing; for example, [t @@ local] behaves just as [t]
   (since the [local] modality does nothing[^1]).

   Then, for all modes/modalities [m] and [n] on comonadic (resp. monadic) axes, if we let
   [ext_m] and [ext_n] be the top (resp. bottom) mode of the corresponding axes ([ext] as
   in "extremum"), then ['a @ m -> 'b @ n] is equivalent to
   ['a @@ m @ ext_m -> 'b @@ n @ ext_n]. For example, ['a @ local -> 'b @ global] is
   equivalent to ['a @@ local @ local -> 'b @@ global @ local], and (since ['a @@ local]
   is just ['a]) also ['a @ local -> 'b @@ global @ local].

   To make conflating a mode with its corresponding modality act in unsurprising ways, we
   want ['a @ m -> 'b @ n] to be equivalent to ['a @@ m -> 'b @@ n], which is implicitly
   ['a @@ m @ legacy_m -> 'b @@ n @ legacy_n]. This holds exactly when [ext_m = legacy_m]
   and [ext_n = legacy_n].

   Going through all of the currently supported axes:
   {v
      +-------------+-------------+-------------+-------------+
      |    axis     |  direction  |   [ext_m]   |   legacy    |
      +-------------+-------------+-------------+-------------+
      | locality    | comonadic   | local       | global      |
      +-------------+-------------+-------------+-------------+
      | portability | comonadic   | nonportable | nonportable |
      +-------------+-------------+-------------+-------------+
      | contention  |   monadic   | uncontended | uncontended |
      +-------------+-------------+-------------+-------------+
      | visibility  | comonadic   | stateful    | stateful    |
      +-------------+-------------+-------------+-------------+
      | access      |   monadic   | read_write  | read_write  |
      +-------------+-------------+-------------+-------------+
      | affinity    | comonadic   | once        | many        |
      +-------------+-------------+-------------+-------------+
      | uniqueness  |   monadic   | unique      | aliased     |
      +-------------+-------------+-------------+-------------+
      | yielding    | comonadic   | yielding    | unyielding  |
      +-------------+-------------+-------------+-------------+
      | forkable    | comonadic   | unforkable  | forkable    |
      +-------------+-------------+-------------+-------------+
   v}

   The only axes for which the right two columns align are portability and contention. For
   this reason, we always conflate modes and modalities on these two axes, and never on
   the other axes.

   [^0] The "top" (resp. "bottom") mode of an axis is the mode which is a super-mode
   (resp. sub-mode) of all other modes on the axis.

   [^1] Each modality acts as meet (min) for comonadic axes and join (max) for monadic
   axes; e.g. the [global] modality acts as [fun m -> meet global m]. This means the
   [local] modality acts as [fun m -> meet local m], but since [local] is top for the
   locality axis, [meet local m = m], so [local] just acts as [fun m -> m], i.e. does
   nothing.
*)
