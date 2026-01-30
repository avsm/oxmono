open! Base
open Base_quickcheck.Export
open Expect_test_helpers_base

module Key = struct
  type t = int [@@deriving compare, equal, hash, sexp]

  include (val Comparator.make ~compare ~sexp_of_t)

  let quickcheck_generator = Base_quickcheck.Generator.small_positive_or_zero_int
  let quickcheck_observer = Base_quickcheck.Observer.int
  let quickcheck_shrinker = Base_quickcheck.Shrinker.int
end

module T = struct
  type t = Bidirectional_multimap.M(Key)(Key).t
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module _ : module type of Bidirectional_multimap = struct
  (** Type *)

  module Binding = Bidirectional_multimap.Binding

  type ('l, 'lc, 'r, 'rc) t = ('l, 'lc, 'r, 'rc) Bidirectional_multimap.t

  type 'a workaround_to_make_the_above_typecheck =
    'a Bidirectional_multimap.workaround_to_make_the_above_typecheck

  (** Accessors *)

  let to_alist = Bidirectional_multimap.to_alist

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let alist = Bidirectional_multimap.to_alist t in
        (match List.find_a_dup alist ~compare:[%compare: int * int] with
         | None -> ()
         | Some (left_key, right_key) ->
           print_cr [%message "duplicate binding" (left_key : Key.t) (right_key : Key.t)]);
        require (List.is_sorted alist ~compare:[%compare: Key.t * Key.t]))
  ;;

  let length = Bidirectional_multimap.length

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Int)
          (Bidirectional_multimap.length t)
          (List.length (Bidirectional_multimap.to_alist t)))
  ;;

  let number_of_left_keys = Bidirectional_multimap.number_of_left_keys

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Int)
          (Bidirectional_multimap.number_of_left_keys t)
          (Bidirectional_multimap.to_alist t
           |> List.map ~f:fst
           |> List.dedup_and_sort ~compare:Key.compare
           |> (List.length :> _ -> _)))
  ;;

  let number_of_right_keys = Bidirectional_multimap.number_of_right_keys

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Int)
          (Bidirectional_multimap.number_of_right_keys t)
          (Bidirectional_multimap.to_alist t
           |> List.map ~f:snd
           |> List.dedup_and_sort ~compare:Key.compare
           |> (List.length :> _ -> _)))
  ;;

  let is_empty = Bidirectional_multimap.is_empty

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Bool)
          (Bidirectional_multimap.is_empty t)
          (List.is_empty (Bidirectional_multimap.to_alist t)))
  ;;

  let mem_binding = Bidirectional_multimap.mem_binding

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        let alist = to_alist t in
        require_equal
          (module Bool)
          (Bidirectional_multimap.mem_binding t l r)
          (List.mem alist (l, r) ~equal:[%equal: Key.t * Key.t]))
  ;;

  let number_of_bindings_for_left_key =
    Bidirectional_multimap.number_of_bindings_for_left_key
  ;;

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let alist = to_alist t in
        require_equal
          (module Int)
          (Bidirectional_multimap.number_of_bindings_for_left_key t l)
          (List.count alist ~f:(fun (key, _) -> Key.equal key l)))
  ;;

  let number_of_bindings_for_right_key =
    Bidirectional_multimap.number_of_bindings_for_right_key
  ;;

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let alist = to_alist t in
        require_equal
          (module Int)
          (Bidirectional_multimap.number_of_bindings_for_right_key t r)
          (List.count alist ~f:(fun (_, key) -> Key.equal key r)))
  ;;

  let mem_left = Bidirectional_multimap.mem_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let alist = to_alist t in
        require_equal
          (module Bool)
          (Bidirectional_multimap.mem_left t l)
          (List.exists alist ~f:(fun (key, _) -> Key.equal key l)))
  ;;

  let mem_right = Bidirectional_multimap.mem_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let alist = to_alist t in
        require_equal
          (module Bool)
          (Bidirectional_multimap.mem_right t r)
          (List.exists alist ~f:(fun (_, key) -> Key.equal key r)))
  ;;

  let find_left = Bidirectional_multimap.find_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let alist = to_alist t in
        require_equal
          (module struct
            type t = Set.M(Key).t [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.find_left t l)
          (List.filter_map alist ~f:(fun (other_l, r) ->
             if Key.equal l other_l then Some r else None)
           |> Set.of_list (module Key)))
  ;;

  let find_right = Bidirectional_multimap.find_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let alist = to_alist t in
        require_equal
          (module struct
            type t = Set.M(Key).t [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.find_right t r)
          (List.filter_map alist ~f:(fun (l, other_r) ->
             if Key.equal r other_r then Some l else None)
           |> Set.of_list (module Key)))
  ;;

  let find_left_if_nonempty = Bidirectional_multimap.find_left_if_nonempty

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let alist = to_alist t in
        require_equal
          (module struct
            type t = Set.M(Key).t option [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.find_left_if_nonempty t l)
          (let set =
             List.filter_map alist ~f:(fun (other_l, r) ->
               if Key.equal l other_l then Some r else None)
             |> Set.of_list (module Key)
           in
           Option.some_if (not (Set.is_empty set)) set))
  ;;

  let find_right_if_nonempty = Bidirectional_multimap.find_right_if_nonempty

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let alist = to_alist t in
        require_equal
          (module struct
            type t = Set.M(Key).t option [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.find_right_if_nonempty t r)
          (let set =
             List.filter_map alist ~f:(fun (l, other_r) ->
               if Key.equal r other_r then Some l else None)
             |> Set.of_list (module Key)
           in
           Option.some_if (not (Set.is_empty set)) set))
  ;;

  let lefts = Bidirectional_multimap.lefts

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Key.t list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.lefts t)
          (Bidirectional_multimap.to_alist t
           |> List.map ~f:fst
           |> List.remove_consecutive_duplicates ~equal:Key.equal))
  ;;

  let rights = Bidirectional_multimap.rights

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Key.t list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.rights t)
          (List.map ~f:snd (Bidirectional_multimap.to_alist t)
           |> List.dedup_and_sort ~compare:Key.compare))
  ;;

  let left_to_right = Bidirectional_multimap.left_to_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Set.M(Key).t Map.M(Key).t [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.left_to_right t)
          (Bidirectional_multimap.to_alist t
           |> Map.of_alist_multi (module Key)
           |> Map.map ~f:(Set.of_list (module Key))))
  ;;

  let right_to_left = Bidirectional_multimap.right_to_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Set.M(Key).t Map.M(Key).t [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.right_to_left t)
          (Bidirectional_multimap.to_alist t
           |> List.map ~f:(fun (l, r) -> r, l)
           |> Map.of_alist_multi (module Key)
           |> Map.map ~f:(Set.of_list (module Key))))
  ;;

  let iter = Bidirectional_multimap.iter

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let queue = Queue.create () in
        Bidirectional_multimap.iter t ~f:(fun l r -> Queue.enqueue queue (l, r));
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Queue.to_list queue)
          (Bidirectional_multimap.to_alist t))
  ;;

  let fold = Bidirectional_multimap.fold

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.fold t ~init:[] ~f:(fun l r acc -> (l, r) :: acc))
          (Bidirectional_multimap.to_alist t |> List.rev))
  ;;

  let for_all = Bidirectional_multimap.for_all

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Bool)
          (Bidirectional_multimap.for_all t ~f:(fun l r -> l <= r))
          (List.for_all (Bidirectional_multimap.to_alist t) ~f:(fun (l, r) -> l <= r)))
  ;;

  let exists = Bidirectional_multimap.exists

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Bool)
          (Bidirectional_multimap.exists t ~f:(fun l r -> l <= r))
          (List.exists (Bidirectional_multimap.to_alist t) ~f:(fun (l, r) -> l <= r)))
  ;;

  let invariant = Bidirectional_multimap.invariant

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t -> Bidirectional_multimap.invariant ignore ignore t)
  ;;

  (** Constructors *)

  let empty = Bidirectional_multimap.empty

  let%expect_test _ =
    let t = Bidirectional_multimap.empty (module Key) (module Key) in
    print_s [%sexp (t : T.t)];
    require (Bidirectional_multimap.is_empty t);
    require_equal (module Int) (Bidirectional_multimap.length t) 0;
    require_does_not_raise (fun () -> Bidirectional_multimap.invariant ignore ignore t);
    [%expect {| () |}]
  ;;

  let singleton = Bidirectional_multimap.singleton

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (l, r) ->
        let t = Bidirectional_multimap.singleton (module Key) (module Key) l r in
        require (not (Bidirectional_multimap.is_empty t));
        require_equal (module Int) (Bidirectional_multimap.length t) 1;
        require
          (Bidirectional_multimap.mem_left t l && Bidirectional_multimap.mem_right t r);
        require_does_not_raise (fun () ->
          Bidirectional_multimap.invariant ignore ignore t))
  ;;

  let of_alist = Bidirectional_multimap.of_alist

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = (Key.t * Key.t) list [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun alist ->
        let t = Bidirectional_multimap.of_alist (module Key) (module Key) alist in
        Bidirectional_multimap.invariant ignore ignore t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist t)
          (List.dedup_and_sort alist ~compare:[%compare: int * int]))
  ;;

  let add = Bidirectional_multimap.add

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        match Bidirectional_multimap.add t l r with
        | None -> require (Bidirectional_multimap.mem_binding t l r)
        | Some modified_t ->
          Bidirectional_multimap.invariant ignore ignore modified_t;
          require_equal
            (module struct
              type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
            end)
            (Bidirectional_multimap.to_alist modified_t)
            (Bidirectional_multimap.to_alist t
             |> List.merge [ l, r ] ~compare:[%compare: int * int]))
  ;;

  let set = Bidirectional_multimap.set

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        let modified_t = Bidirectional_multimap.set t l r in
        Bidirectional_multimap.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist modified_t)
          (Bidirectional_multimap.to_alist t
           |> List.filter ~f:(fun (other_l, other_r) -> other_l <> l || other_r <> r)
           |> List.merge [ l, r ] ~compare:[%compare: int * int]))
  ;;

  let remove_left = Bidirectional_multimap.remove_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let modified_t = Bidirectional_multimap.remove_left t l in
        Bidirectional_multimap.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist modified_t)
          (Bidirectional_multimap.to_alist t
           |> List.filter ~f:(fun (other_l, _) -> other_l <> l)))
  ;;

  let remove_right = Bidirectional_multimap.remove_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let modified_t = Bidirectional_multimap.remove_right t r in
        Bidirectional_multimap.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist modified_t)
          (Bidirectional_multimap.to_alist t
           |> List.filter ~f:(fun (_, other_r) -> other_r <> r)))
  ;;

  let remove_binding = Bidirectional_multimap.remove_binding

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        let modified_t = Bidirectional_multimap.remove_binding t l r in
        Bidirectional_multimap.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist modified_t)
          (Bidirectional_multimap.to_alist t
           |> List.filter ~f:(fun (other_l, other_r) -> other_l <> l || other_r <> r)))
  ;;

  let filter = Bidirectional_multimap.filter

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let modified_t = Bidirectional_multimap.filter t ~f:(fun l r -> l <= r) in
        Bidirectional_multimap.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist modified_t)
          (Bidirectional_multimap.to_alist t
           |> List.filter ~f:(fun (other_l, other_r) -> other_l <= other_r)))
  ;;

  let partition_tf = Bidirectional_multimap.partition_tf

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let keeps, drops = Bidirectional_multimap.partition_tf t ~f:(fun l r -> l <= r) in
        Bidirectional_multimap.invariant ignore ignore keeps;
        Bidirectional_multimap.invariant ignore ignore drops;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list * (Key.t * Key.t) list
            [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist keeps, Bidirectional_multimap.to_alist drops)
          (Bidirectional_multimap.to_alist t
           |> List.partition_tf ~f:(fun (other_l, other_r) -> other_l <= other_r)))
  ;;

  let merge = Bidirectional_multimap.merge

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        let t = Bidirectional_multimap.merge t1 t2 in
        Bidirectional_multimap.invariant ignore ignore t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_multimap.to_alist t)
          (List.concat
             [ Bidirectional_multimap.to_alist t1; Bidirectional_multimap.to_alist t2 ]
           |> List.dedup_and_sort ~compare:[%compare: int * int]))
  ;;

  (** Deriving *)

  module M = Bidirectional_multimap.M

  let sexp_of_m__t = Bidirectional_multimap.sexp_of_m__t
  let m__t_of_sexp = Bidirectional_multimap.m__t_of_sexp

  let%expect_test _ =
    quickcheck_m (module T) ~f:(fun t ->
      let sexp = T.sexp_of_t t in
      let round_trip = T.t_of_sexp sexp in
      require_equal
        (module struct
          type t = T.t [@@deriving equal, sexp_of]
        end)
        round_trip
        t)
  ;;

  let equal_m__t = Bidirectional_multimap.equal_m__t

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        require (T.equal t t);
        require_equal
          (module Bool)
          (T.equal t (Bidirectional_multimap.set t l r))
          (Bidirectional_multimap.mem_binding t l r);
        require_equal
          (module Bool)
          (T.equal t (Bidirectional_multimap.remove_binding t l r))
          (not (Bidirectional_multimap.mem_binding t l r)))
  ;;

  let compare_m__t = Bidirectional_multimap.compare_m__t

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        require_equal
          (module Ordering)
          (Ordering.of_int (T.compare t1 t2))
          (Ordering.of_int
             ([%compare: (Key.t * Key.t list) list]
                (Bidirectional_multimap.to_alist t1
                 |> List.Assoc.sort_and_group ~compare:Key.compare)
                (Bidirectional_multimap.to_alist t2
                 |> List.Assoc.sort_and_group ~compare:Key.compare))))
  ;;

  let hash_fold_m__t = Bidirectional_multimap.hash_fold_m__t
  let hash_m__t = Bidirectional_multimap.hash_m__t

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        require_equal (module Int) (T.hash t) (T.hash t);
        require_equal
          (module Bool)
          (T.hash t = T.hash (Bidirectional_multimap.set t l r))
          (Bidirectional_multimap.mem_binding t l r);
        require_equal
          (module Bool)
          (T.hash t = T.hash (Bidirectional_multimap.remove_binding t l r))
          (not (Bidirectional_multimap.mem_binding t l r)))
  ;;

  let quickcheck_generator_m__t = Bidirectional_multimap.quickcheck_generator_m__t
  let quickcheck_observer_m__t = Bidirectional_multimap.quickcheck_observer_m__t
  let quickcheck_shrinker_m__t = Bidirectional_multimap.quickcheck_shrinker_m__t
end
