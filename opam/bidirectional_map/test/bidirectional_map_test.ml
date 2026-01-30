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
  type t = Bidirectional_map.M(Key)(Key).t
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

module _ : module type of Bidirectional_map = struct
  (** Type *)

  module Binding = Bidirectional_map.Binding

  type ('l, 'lc, 'r, 'rc) t = ('l, 'lc, 'r, 'rc) Bidirectional_map.t

  type 'a workaround_to_make_the_above_typecheck =
    'a Bidirectional_map.workaround_to_make_the_above_typecheck

  (** Accessors *)

  let to_alist = Bidirectional_map.to_alist

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let alist = Bidirectional_map.to_alist t in
        (match List.find_a_dup alist ~compare:[%compare: int * _] with
         | None -> ()
         | Some (key, _) -> print_cr [%message "duplicate left key" (key : Key.t)]);
        (match List.find_a_dup alist ~compare:[%compare: _ * int] with
         | None -> ()
         | Some (_, key) -> print_cr [%message "duplicate right key" (key : Key.t)]);
        require (List.is_sorted_strictly alist ~compare:[%compare: Key.t * Key.t]))
  ;;

  let length = Bidirectional_map.length

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Int)
          (Bidirectional_map.length t)
          (List.length (Bidirectional_map.to_alist t)))
  ;;

  let is_empty = Bidirectional_map.is_empty

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Bool)
          (Bidirectional_map.is_empty t)
          (List.is_empty (Bidirectional_map.to_alist t)))
  ;;

  let mem_binding = Bidirectional_map.mem_binding

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        let alist = to_alist t in
        require_equal
          (module Bool)
          (Bidirectional_map.mem_binding t l r)
          (List.mem alist (l, r) ~equal:[%equal: Key.t * Key.t]))
  ;;

  let mem_left = Bidirectional_map.mem_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let alist = to_alist t in
        require_equal
          (module Bool)
          (Bidirectional_map.mem_left t l)
          (List.exists alist ~f:(fun (key, _) -> Key.equal key l)))
  ;;

  let mem_right = Bidirectional_map.mem_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let alist = to_alist t in
        require_equal
          (module Bool)
          (Bidirectional_map.mem_right t r)
          (List.exists alist ~f:(fun (_, key) -> Key.equal key r)))
  ;;

  let find_left = Bidirectional_map.find_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let alist = to_alist t in
        require_equal
          (module struct
            type t = Key.t option [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.find_left t l)
          (List.Assoc.find alist l ~equal:Key.equal))
  ;;

  let find_right = Bidirectional_map.find_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let alist = to_alist t in
        require_equal
          (module struct
            type t = Key.t option [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.find_right t r)
          (List.Assoc.find (List.map alist ~f:(fun (l, r) -> r, l)) r ~equal:Key.equal))
  ;;

  let lefts = Bidirectional_map.lefts

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
          (Bidirectional_map.lefts t)
          (List.map ~f:fst (Bidirectional_map.to_alist t)))
  ;;

  let rights = Bidirectional_map.rights

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
          (Bidirectional_map.rights t)
          (List.map ~f:snd (Bidirectional_map.to_alist t)
           |> List.sort ~compare:Key.compare))
  ;;

  let left_to_right = Bidirectional_map.left_to_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Key.t Map.M(Key).t [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.left_to_right t)
          (Map.of_alist_exn (module Key) (Bidirectional_map.to_alist t)))
  ;;

  let right_to_left = Bidirectional_map.right_to_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module struct
            type t = Key.t Map.M(Key).t [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.right_to_left t)
          (Map.of_alist_exn
             (module Key)
             (List.map ~f:(fun (l, r) -> r, l) (Bidirectional_map.to_alist t))))
  ;;

  let iter = Bidirectional_map.iter

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let queue = Queue.create () in
        Bidirectional_map.iter t ~f:(fun l r -> Queue.enqueue queue (l, r));
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Queue.to_list queue)
          (Bidirectional_map.to_alist t))
  ;;

  let fold = Bidirectional_map.fold

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
          (Bidirectional_map.fold t ~init:[] ~f:(fun l r acc -> (l, r) :: acc))
          (Bidirectional_map.to_alist t |> List.rev))
  ;;

  let for_all = Bidirectional_map.for_all

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Bool)
          (Bidirectional_map.for_all t ~f:(fun l r -> l <= r))
          (List.for_all (Bidirectional_map.to_alist t) ~f:(fun (l, r) -> l <= r)))
  ;;

  let exists = Bidirectional_map.exists

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        require_equal
          (module Bool)
          (Bidirectional_map.exists t ~f:(fun l r -> l <= r))
          (List.exists (Bidirectional_map.to_alist t) ~f:(fun (l, r) -> l <= r)))
  ;;

  let invariant = Bidirectional_map.invariant

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t -> Bidirectional_map.invariant ignore ignore t)
  ;;

  (** Constructors *)

  let empty = Bidirectional_map.empty

  let%expect_test _ =
    let t = Bidirectional_map.empty (module Key) (module Key) in
    print_s [%sexp (t : T.t)];
    require (Bidirectional_map.is_empty t);
    require_equal (module Int) (Bidirectional_map.length t) 0;
    require_does_not_raise (fun () -> Bidirectional_map.invariant ignore ignore t);
    [%expect {| () |}]
  ;;

  let singleton = Bidirectional_map.singleton

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (l, r) ->
        let t = Bidirectional_map.singleton (module Key) (module Key) l r in
        require (not (Bidirectional_map.is_empty t));
        require_equal (module Int) (Bidirectional_map.length t) 1;
        require (Bidirectional_map.mem_left t l && Bidirectional_map.mem_right t r);
        require_does_not_raise (fun () -> Bidirectional_map.invariant ignore ignore t))
  ;;

  let of_alist_or_error = Bidirectional_map.of_alist_or_error

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = (Key.t * Key.t) list [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun alist ->
        match Bidirectional_map.of_alist_or_error (module Key) (module Key) alist with
        | Error _ ->
          require
            (List.contains_dup alist ~compare:[%compare: int * _]
             || List.contains_dup alist ~compare:[%compare: _ * int])
        | Ok t ->
          Bidirectional_map.invariant ignore ignore t;
          require_equal
            (module struct
              type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
            end)
            (Bidirectional_map.to_alist t)
            (List.sort alist ~compare:[%compare: int * int]))
  ;;

  let add = Bidirectional_map.add

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        match Bidirectional_map.add t l r with
        | None ->
          require (Bidirectional_map.mem_left t l || Bidirectional_map.mem_right t r)
        | Some modified_t ->
          Bidirectional_map.invariant ignore ignore modified_t;
          require_equal
            (module struct
              type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
            end)
            (Bidirectional_map.to_alist modified_t)
            (Bidirectional_map.to_alist t
             |> List.merge [ l, r ] ~compare:[%compare: int * int]))
  ;;

  let add_or_keep = Bidirectional_map.add_or_keep

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        match Bidirectional_map.add_or_keep t l r with
        | None ->
          require
            ((Bidirectional_map.mem_left t l || Bidirectional_map.mem_right t r)
             && not (Bidirectional_map.mem_binding t l r))
        | Some modified_t ->
          Bidirectional_map.invariant ignore ignore modified_t;
          require_equal
            (module struct
              type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
            end)
            (Bidirectional_map.to_alist modified_t)
            (Bidirectional_map.to_alist t
             |> List.merge [ l, r ] ~compare:[%compare: int * int]
             |> List.remove_consecutive_duplicates ~equal:[%equal: int * int]))
  ;;

  let set = Bidirectional_map.set

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        let modified_t = Bidirectional_map.set t l r in
        Bidirectional_map.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.to_alist modified_t)
          (Bidirectional_map.to_alist t
           |> List.filter ~f:(fun (other_l, other_r) -> other_l <> l && other_r <> r)
           |> List.merge [ l, r ] ~compare:[%compare: int * int]))
  ;;

  let remove_left = Bidirectional_map.remove_left

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l) ->
        let modified_t = Bidirectional_map.remove_left t l in
        Bidirectional_map.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.to_alist modified_t)
          (Bidirectional_map.to_alist t
           |> List.filter ~f:(fun (other_l, _) -> other_l <> l)))
  ;;

  let remove_right = Bidirectional_map.remove_right

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, r) ->
        let modified_t = Bidirectional_map.remove_right t r in
        Bidirectional_map.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.to_alist modified_t)
          (Bidirectional_map.to_alist t
           |> List.filter ~f:(fun (_, other_r) -> other_r <> r)))
  ;;

  let remove_binding = Bidirectional_map.remove_binding

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        let modified_t = Bidirectional_map.remove_binding t l r in
        Bidirectional_map.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.to_alist modified_t)
          (Bidirectional_map.to_alist t
           |> List.filter ~f:(fun (other_l, other_r) -> other_l <> l || other_r <> r)))
  ;;

  let filter = Bidirectional_map.filter

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let modified_t = Bidirectional_map.filter t ~f:(fun l r -> l <= r) in
        Bidirectional_map.invariant ignore ignore modified_t;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.to_alist modified_t)
          (Bidirectional_map.to_alist t
           |> List.filter ~f:(fun (other_l, other_r) -> other_l <= other_r)))
  ;;

  let partition_tf = Bidirectional_map.partition_tf

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun t ->
        let keeps, drops = Bidirectional_map.partition_tf t ~f:(fun l r -> l <= r) in
        Bidirectional_map.invariant ignore ignore keeps;
        Bidirectional_map.invariant ignore ignore drops;
        require_equal
          (module struct
            type t = (Key.t * Key.t) list * (Key.t * Key.t) list
            [@@deriving equal, sexp_of]
          end)
          (Bidirectional_map.to_alist keeps, Bidirectional_map.to_alist drops)
          (Bidirectional_map.to_alist t
           |> List.partition_tf ~f:(fun (other_l, other_r) -> other_l <= other_r)))
  ;;

  let merge = Bidirectional_map.merge

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * T.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t1, t2) ->
        match Bidirectional_map.merge t1 t2 with
        | Error _ ->
          require_error
            T.sexp_of_t
            (Bidirectional_map.of_alist_or_error
               (module Key)
               (module Key)
               (List.concat
                  [ Bidirectional_map.to_alist t1; Bidirectional_map.to_alist t2 ]))
        | Ok t ->
          Bidirectional_map.invariant ignore ignore t;
          require_equal
            (module struct
              type t = (Key.t * Key.t) list [@@deriving equal, sexp_of]
            end)
            (Bidirectional_map.to_alist t)
            (List.concat [ Bidirectional_map.to_alist t1; Bidirectional_map.to_alist t2 ]
             |> List.dedup_and_sort ~compare:[%compare: int * int]))
  ;;

  (** Deriving *)

  module M = Bidirectional_map.M

  let sexp_of_m__t = Bidirectional_map.sexp_of_m__t
  let m__t_of_sexp = Bidirectional_map.m__t_of_sexp

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

  let equal_m__t = Bidirectional_map.equal_m__t

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        require (T.equal t t);
        require_equal
          (module Bool)
          (T.equal t (Bidirectional_map.set t l r))
          (Bidirectional_map.mem_binding t l r);
        require_equal
          (module Bool)
          (T.equal t (Bidirectional_map.remove_binding t l r))
          (not (Bidirectional_map.mem_binding t l r)))
  ;;

  let compare_m__t = Bidirectional_map.compare_m__t

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
             ([%compare: (Key.t * Key.t) list]
                (Bidirectional_map.to_alist t1)
                (Bidirectional_map.to_alist t2))))
  ;;

  let hash_fold_m__t = Bidirectional_map.hash_fold_m__t
  let hash_m__t = Bidirectional_map.hash_m__t

  let%expect_test _ =
    quickcheck_m
      (module struct
        type t = T.t * Key.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (t, l, r) ->
        require_equal (module Int) (T.hash t) (T.hash t);
        require_equal
          (module Bool)
          (T.hash t = T.hash (Bidirectional_map.set t l r))
          (Bidirectional_map.mem_binding t l r);
        require_equal
          (module Bool)
          (T.hash t = T.hash (Bidirectional_map.remove_binding t l r))
          (not (Bidirectional_map.mem_binding t l r)))
  ;;

  let quickcheck_generator_m__t = Bidirectional_map.quickcheck_generator_m__t
  let quickcheck_observer_m__t = Bidirectional_map.quickcheck_observer_m__t
  let quickcheck_shrinker_m__t = Bidirectional_map.quickcheck_shrinker_m__t
end
