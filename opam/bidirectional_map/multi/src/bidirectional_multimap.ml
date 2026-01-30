open! Base

module Binding = struct
  type ('a, 'b) t = 'a * 'b

  include Comparator.Derived2 [@modality portable] (struct
      type ('a, 'b) t = 'a * 'b [@@deriving compare ~localize, sexp_of]
    end)
end

let binding_comparator_m (type l lc r rc) lm rm : _ Comparator.Module.t =
  let module L = (val (lm : (l, lc) Comparator.Module.t)) in
  let module R = (val (rm : (r, rc) Comparator.Module.t)) in
  (module struct
    type t = L.t * R.t

    type comparator_witness =
      (L.comparator_witness, R.comparator_witness) Binding.comparator_witness

    let comparator = Binding.comparator L.comparator R.comparator
  end)
;;

(** Indexes bindings three ways: by both keys, by left keys, and by right keys. *)
type ('l, 'lc, 'r, 'rc) t =
  { bindings : ('l * 'r, ('lc, 'rc) Binding.comparator_witness) Set.t
  ; left_to_right : ('l, ('r, 'rc) Set.t, 'lc) Map.t
  ; right_to_left : ('r, ('l, 'lc) Set.t, 'rc) Map.t
  }

type 'a workaround_to_make_the_above_typecheck

let invariant l_invariant r_invariant { bindings; left_to_right; right_to_left } =
  assert (Set.invariants bindings);
  assert (Map.invariants left_to_right);
  assert (Map.invariants right_to_left);
  Map.iter_keys left_to_right ~f:l_invariant;
  Map.iter_keys right_to_left ~f:r_invariant;
  Map.iteri left_to_right ~f:(fun ~key:l ~data:rs ->
    assert (not (Set.is_empty rs));
    Set.iter rs ~f:(fun r -> assert (Set.mem bindings (l, r))));
  Map.iteri right_to_left ~f:(fun ~key:r ~data:ls ->
    assert (not (Set.is_empty ls));
    Set.iter ls ~f:(fun l -> assert (Set.mem bindings (l, r))));
  Set.iter bindings ~f:(fun (l, r) ->
    assert (Set.mem (Map.find_exn left_to_right l) r);
    assert (Set.mem (Map.find_exn right_to_left r) l))
;;

let is_empty t = Set.is_empty t.bindings
let length t = Set.length t.bindings
let number_of_left_keys t = Map.length t.left_to_right
let number_of_right_keys t = Map.length t.right_to_left

let number_of_bindings_for_left_key t l =
  match Map.find t.left_to_right l with
  | None -> 0
  | Some rs -> Set.length rs
;;

let number_of_bindings_for_right_key t r =
  match Map.find t.right_to_left r with
  | None -> 0
  | Some ls -> Set.length ls
;;

let mem_binding t l r = Set.mem t.bindings (l, r)
let mem_left t l = Map.mem t.left_to_right l
let mem_right t r = Map.mem t.right_to_left r
let find_left_if_nonempty t l = Map.find t.left_to_right l
let find_right_if_nonempty t r = Map.find t.right_to_left r

let find_left t l =
  match find_left_if_nonempty t l with
  | Some rs -> rs
  | None -> Set.empty (Map.comparator_s t.right_to_left)
;;

let find_right t r =
  match find_right_if_nonempty t r with
  | Some ls -> ls
  | None -> Set.empty (Map.comparator_s t.left_to_right)
;;

let lefts t = Map.keys t.left_to_right
let rights t = Map.keys t.right_to_left
let left_to_right t = t.left_to_right
let right_to_left t = t.right_to_left
let to_alist t = Set.to_list t.bindings

let of_alist lm rm lr_alist =
  let rl_alist = List.map lr_alist ~f:(fun (l, r) -> r, l) in
  let bindings = Set.of_list (binding_comparator_m lm rm) lr_alist in
  let left_to_right = lr_alist |> Map.of_alist_multi lm |> Map.map ~f:(Set.of_list rm) in
  let right_to_left = rl_alist |> Map.of_alist_multi rm |> Map.map ~f:(Set.of_list lm) in
  { bindings; left_to_right; right_to_left }
;;

let empty lm rm =
  { bindings = Set.empty (binding_comparator_m lm rm)
  ; left_to_right = Map.empty lm
  ; right_to_left = Map.empty rm
  }
;;

let add_to_map map m key elt =
  Map.update map key ~f:(function
    | None -> Set.singleton m elt
    | Some set -> Set.add set elt)
;;

let remove_from_map map key elt =
  Map.change map key ~f:(function
    | None -> None
    | Some set ->
      let set = Set.remove set elt in
      if Set.is_empty set then None else Some set)
;;

let add_unchecked t l r =
  { bindings = Set.add t.bindings (l, r)
  ; left_to_right = add_to_map t.left_to_right (Map.comparator_s t.right_to_left) l r
  ; right_to_left = add_to_map t.right_to_left (Map.comparator_s t.left_to_right) r l
  }
;;

let add t l r = if mem_binding t l r then None else Some (add_unchecked t l r)

let remove_unchecked t l r =
  { bindings = Set.remove t.bindings (l, r)
  ; left_to_right = remove_from_map t.left_to_right l r
  ; right_to_left = remove_from_map t.right_to_left r l
  }
;;

let remove_binding t l r = if mem_binding t l r then remove_unchecked t l r else t

let remove_left t l =
  match find_left_if_nonempty t l with
  | None -> t
  | Some rs -> Set.fold rs ~init:t ~f:(fun t r -> remove_binding t l r)
;;

let remove_right t r =
  match find_right_if_nonempty t r with
  | None -> t
  | Some ls -> Set.fold ls ~init:t ~f:(fun t l -> remove_binding t l r)
;;

let set t l r = if mem_binding t l r then t else add_unchecked t l r
let singleton lm rm l r = add_unchecked (empty lm rm) l r

let iter t ~f =
  Map.iteri t.left_to_right ~f:(fun ~key:l ~data:rs -> Set.iter rs ~f:(fun r -> f l r))
;;

let fold t ~init ~f =
  Map.fold t.left_to_right ~init ~f:(fun ~key:l ~data:rs acc ->
    Set.fold rs ~init:acc ~f:(fun acc r -> f l r acc))
;;

let for_all t ~f =
  Map.for_alli t.left_to_right ~f:(fun ~key:l ~data:rs ->
    Set.for_all rs ~f:(fun r -> f l r))
;;

let exists t ~f =
  Map.existsi t.left_to_right ~f:(fun ~key:l ~data:rs ->
    Set.exists rs ~f:(fun r -> f l r))
;;

let partition_tf t ~f =
  fold t ~init:(t, t) ~f:(fun l r (t_true, t_false) ->
    match f l r with
    | true -> t_true, remove_unchecked t_false l r
    | false -> remove_unchecked t_true l r, t_false)
;;

let filter t ~f =
  fold t ~init:t ~f:(fun l r t -> if f l r then t else remove_unchecked t l r)
;;

let merge t1 t2 =
  let small, large = if length t1 <= length t2 then t1, t2 else t2, t1 in
  fold small ~init:large ~f:(fun l r t -> set t l r)
;;

include struct
  open Bidirectional_map_interfaces

  module M (Left : With_comparator_witness) (Right : With_comparator_witness) = struct
    type nonrec t = (Left.t, Left.comparator_witness, Right.t, Right.comparator_witness) t
  end

  let sexp_of_m__t
    (type l r)
    (module L : With_sexp_of with type t = l)
    (module R : With_sexp_of with type t = r)
    t
    =
    to_alist t |> [%sexp_of: (L.t * R.t) list]
  ;;

  let m__t_of_sexp
    (type l lc r rc)
    (module L : With_of_sexp with type t = l and type comparator_witness = lc)
    (module R : With_of_sexp with type t = r and type comparator_witness = rc)
    sexp
    =
    [%of_sexp: (L.t * R.t) list] sexp |> of_alist (module L) (module R)
  ;;

  let compare_m__t
    (type l r)
    (module L : With_compare with type t = l)
    (module R : With_compare with type t = r)
    t1
    t2
    =
    Map.compare_m__t
      (module L)
      (Set.compare_m__t (module R))
      t1.left_to_right
      t2.left_to_right
  ;;

  let equal_m__t
    (type l r)
    (module L : With_equal with type t = l)
    (module R : With_equal with type t = r)
    t1
    t2
    =
    Map.equal_m__t
      (module L)
      (Set.equal_m__t (module R))
      t1.left_to_right
      t2.left_to_right
  ;;

  let hash_fold_m__t
    (type l r)
    (module L : With_hash_fold with type t = l)
    (module R : With_hash_fold with type t = r)
    hash_state
    t
    =
    Map.hash_fold_m__t
      (module L)
      (Set.hash_fold_m__t (module R))
      hash_state
      t.left_to_right
  ;;

  let hash_m__t lm rm t = Hash.get_hash_value (hash_fold_m__t lm rm (Hash.create ()) t)

  include struct
    open Base_quickcheck

    let quickcheck_generator_m__t
      (type l lc r rc)
      (module L : With_quickcheck_generator
        with type t = l
         and type comparator_witness = lc)
      (module R : With_quickcheck_generator
        with type t = r
         and type comparator_witness = rc)
      =
      Generator.list (Generator.both L.quickcheck_generator R.quickcheck_generator)
      |> Generator.map ~f:(fun alist ->
        (* Generate a [t] with as many bindings from [alist] as possible, rather than
           choosing a new alist if some bindings overlap. *)
        List.fold
          alist
          ~init:(empty (module L) (module R))
          ~f:(fun t (l, r) -> add t l r |> Option.value ~default:t))
    ;;

    let quickcheck_observer_m__t
      (type l r)
      (module L : With_quickcheck_observer with type t = l)
      (module R : With_quickcheck_observer with type t = r)
      =
      Observer.list (Observer.both L.quickcheck_observer R.quickcheck_observer)
      |> Observer.unmap ~f:to_alist
    ;;

    let quickcheck_shrinker_m__t
      (type l r)
      (module L : With_quickcheck_shrinker with type t = l)
      (module R : With_quickcheck_shrinker with type t = r)
      =
      Shrinker.both
        Shrinker.atomic
        (Shrinker.list (Shrinker.both L.quickcheck_shrinker R.quickcheck_shrinker))
      |> Shrinker.map
           ~f:(fun ((lc, rc), alist) -> of_alist lc rc alist)
           ~f_inverse:(fun t ->
             ( (Map.comparator_s t.left_to_right, Map.comparator_s t.right_to_left)
             , to_alist t ))
    ;;
  end
end
