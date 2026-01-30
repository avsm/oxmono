open Core
module T = Stored_reversed
open T

module Elem : sig
  type t [@@deriving sexp_of, compare, equal]

  include Quickcheckable with type t := t
end =
  Int

let%test_unit "empty to_list" = [%test_result: Elem.t list] ~expect:[] (to_list empty)

let%test_unit "snoc to_list" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list t * Elem.t]
    ~sexp_of:[%sexp_of: Elem.t list t * Elem.t]
    ~f:(fun (xs, x) ->
      [%test_result: Elem.t list] ~expect:(to_list xs @ [ x ]) (to_list (snoc xs x)))
;;

let%test_unit "singleton" =
  Quickcheck.test Elem.quickcheck_generator ~sexp_of:[%sexp_of: Elem.t] ~f:(fun x ->
    [%test_result: Elem.t list] ~expect:(List.return x) (to_list (singleton x));
    [%test_result: Elem.t list t] ~expect:(snoc empty x) (singleton x))
;;

let%test_unit "of_list -> to_list" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list]
    ~sexp_of:[%sexp_of: Elem.t list]
    ~f:(fun x -> [%test_result: Elem.t list] ~expect:x (to_list (of_list x)))
;;

let%test_unit "to_list -> of_list" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list t]
    ~sexp_of:[%sexp_of: Elem.t list t]
    ~f:(fun x -> [%test_result: Elem.t list t] ~expect:x (of_list (to_list x)))
;;

let%test_unit "of_list_rev -> to_list_rev" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list]
    ~sexp_of:[%sexp_of: Elem.t list]
    ~f:(fun x -> [%test_result: Elem.t list] ~expect:x (to_list_rev (of_list_rev x)))
;;

let%test_unit "to_list_rev -> of_list_rev" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list t]
    ~sexp_of:[%sexp_of: Elem.t list t]
    ~f:(fun x -> [%test_result: Elem.t list t] ~expect:x (of_list_rev (to_list_rev x)))
;;

let%test_unit "of_list -> to_list_rev" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list]
    ~sexp_of:[%sexp_of: Elem.t list]
    ~f:(fun x ->
      [%test_result: Elem.t list] ~expect:(List.rev x) (to_list_rev (of_list x)))
;;

let%test_unit "of_list_rev -> to_list" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list]
    ~sexp_of:[%sexp_of: Elem.t list]
    ~f:(fun x ->
      [%test_result: Elem.t list] ~expect:(List.rev x) (to_list (of_list_rev x)))
;;

let mapping_f x = `F x

let%test_unit "map_to_list" =
  Quickcheck.test
    [%quickcheck.generator: Elem.t list t * [ `F of Elem.t ] list option]
    ~sexp_of:[%sexp_of: Elem.t list t * [ `F of Elem.t ] list option]
    ~f:(fun (x, tail) ->
      [%test_result: [ `F of Elem.t ] list]
        ~expect:(List.map (to_list x) ~f:mapping_f @ Option.value tail ~default:[])
        (map_to_list ?tail x ~f:mapping_f))
;;

let%test_unit "map_append" =
  Quickcheck.test
    [%quickcheck.generator: [ `F of Elem.t ] list t * Elem.t list]
    ~sexp_of:[%sexp_of: [ `F of Elem.t ] list t * Elem.t list]
    ~f:(fun (x, y) ->
      [%test_result: [ `F of Elem.t ] list t]
        ~expect:(List.fold (List.map y ~f:mapping_f) ~init:x ~f:snoc)
        (map_append x y ~f:mapping_f))
;;

let%test_unit "compare" =
  (* Comparison is just passed through. *)
  let check_comparison x y =
    [%test_result: int]
      ~expect:([%compare: Elem.t list] (to_list_rev x) (to_list_rev y))
      ([%compare: Elem.t list t] x y)
  in
  Quickcheck.test
    [%quickcheck.generator: Elem.t list t * Elem.t list t]
    ~sexp_of:[%sexp_of: Elem.t list t * Elem.t list t]
    ~f:(fun (x, y) -> check_comparison x y);
  Quickcheck.test
    [%quickcheck.generator: [ `Refl of Elem.t list t ]]
    ~sexp_of:[%sexp_of: [ `Refl of Elem.t list t ]]
    ~f:(fun (`Refl x) -> check_comparison x x)
;;

let%test_unit "equal" =
  (* Equal is just passed through. *)
  let check_comparison x y =
    [%test_result: bool]
      ~expect:([%equal: Elem.t list] (to_list_rev x) (to_list_rev y))
      ([%equal: Elem.t list t] x y)
  in
  Quickcheck.test
    [%quickcheck.generator: Elem.t list t * Elem.t list t]
    ~sexp_of:[%sexp_of: Elem.t list t * Elem.t list t]
    ~f:(fun (x, y) -> check_comparison x y);
  Quickcheck.test
    [%quickcheck.generator: [ `Refl of Elem.t list t ]]
    ~sexp_of:[%sexp_of: [ `Refl of Elem.t list t ]]
    ~f:(fun (`Refl x) -> check_comparison x x)
;;
