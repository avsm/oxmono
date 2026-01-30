open! Core
open! Import
open! Lru_cache

let foreach n f = List.init n ~f:(( + ) 1) |> List.iter ~f

(* The signature reminds us to update tests after adding a function. *)
module _ : S with type key := int = struct
  module M = Make (struct
      type t = int [@@deriving compare, hash, sexp_of]

      let invariant = ignore
    end)

  type 'a t = 'a M.t [@@deriving sexp_of]

  let invariant = M.invariant
  let create = M.create
  let set = M.set
  let stats = M.stats
  let hit_rate = M.hit_rate

  let%expect_test "[create], [set], [stats], [max_size]" =
    let test keys =
      let t = create ~max_size:2 () in
      List.iter keys ~f:(fun key ->
        set t ~key ~data:();
        print_s [%message "set" (key : int) ~stats:(stats t : Sexp.t)])
    in
    test [ 1; 2; 3; 1 ];
    [%expect
      {|
      (set
        (key 1)
        (stats (
          (max_size 2)
          (length   1)
          (hit_rate 0)
          (keys (1)))))
      (set
        (key 2)
        (stats (
          (max_size 2)
          (length   2)
          (hit_rate 0)
          (keys (1 2)))))
      (set
        (key 3)
        (stats (
          (max_size 2)
          (length   2)
          (hit_rate 0)
          (keys (2 3)))))
      (set
        (key 1)
        (stats (
          (max_size 2)
          (length   2)
          (hit_rate 0)
          (keys (3 1)))))
      |}]
  ;;

  let to_alist = M.to_alist

  let%expect_test "[to_alist]" =
    let test keys =
      let t = create ~max_size:2 () in
      List.iter keys ~f:(fun key -> set t ~key ~data:());
      print_s [%sexp (to_alist t : (int * unit) list)]
    in
    test [ 1; 2; 3 ];
    [%expect
      {|
      ((2 ())
       (3 ()))
      |}];
    test [ 1; 2; 1 ];
    [%expect
      {|
      ((2 ())
       (1 ()))
      |}]
  ;;

  let clear = M.clear
  let length = M.length
  let is_empty = M.is_empty

  let%expect_test "[clear], [length], [is_empty]" =
    let t = create ~max_size:2 () in
    let show () =
      print_s [%message "" ~length:(length t : int) ~is_empty:(is_empty t : bool)]
    in
    show ();
    [%expect
      {|
      ((length   0)
       (is_empty true))
      |}];
    foreach 3 (fun key ->
      set t ~key ~data:();
      show ());
    [%expect
      {|
      ((length   1)
       (is_empty false))
      ((length   2)
       (is_empty false))
      ((length   2)
       (is_empty false))
      |}];
    let (`Dropped dropped) = clear t in
    print_s [%message (dropped : int)];
    show ();
    [%expect
      {|
      (dropped 2)
      ((length   0)
       (is_empty true))
      |}]
  ;;

  let find = M.find
  let mem = M.mem
  let remove = M.remove

  let%expect_test "[find], [mem], [remove]" =
    let t = create ~max_size:2 () in
    let show key =
      print_s [%message "" ~find:(find t key : int option) ~mem:(mem t key : bool)]
    in
    show 1;
    [%expect {| ((find ()) (mem false)) |}];
    set t ~key:1 ~data:1;
    show 1;
    [%expect {| ((find (1)) (mem true)) |}];
    print_s [%message "" ~remove:(remove t 1 : [ `Ok | `No_such_key ])];
    show 1;
    [%expect
      {|
      (remove Ok)
      ((find ()) (mem false))
      |}];
    print_s [%message "" ~remove:(remove t 1 : [ `Ok | `No_such_key ])];
    [%expect {| (remove No_such_key) |}];
    print_s [%message (stats t : Sexp.t)];
    [%expect
      {|
      ("stats t" (
        (max_size 2)
        (length   0)
        (hit_rate 0.33333333333333331)
        (keys ())))
      |}]
  ;;

  let find_or_add = M.find_or_add

  let%expect_test "[find_or_add]" =
    let t = create ~max_size:2 () in
    let test key =
      find_or_add t key ~default:(fun () -> print_s [%message "Added"]);
      print_s (stats t)
    in
    test 1;
    [%expect
      {|
      Added
      ((max_size 2)
       (length   1)
       (hit_rate 0)
       (keys (1)))
      |}];
    test 2;
    [%expect
      {|
      Added
      ((max_size 2)
       (length   2)
       (hit_rate 0)
       (keys (1 2)))
      |}];
    test 1;
    [%expect
      {|
      ((max_size 2)
       (length   2)
       (hit_rate 0.33333333333333331)
       (keys (2 1)))
      |}];
    test 2;
    [%expect
      {|
      ((max_size 2)
       (length   2)
       (hit_rate 0.5)
       (keys (1 2)))
      |}]
  ;;

  let find_and_remove = M.find_and_remove

  let%expect_test "[find_and_remove]" =
    let t = create ~max_size:2 () in
    let show () = print_s [%sexp (M.to_alist t : (int * int) list)] in
    let find_and_remove key = print_s [%sexp (M.find_and_remove t key : int option)] in
    set t ~key:1 ~data:1;
    set t ~key:2 ~data:2;
    show ();
    [%expect
      {|
      ((1 1)
       (2 2))
      |}];
    find_and_remove 1;
    [%expect {| (1) |}];
    show ();
    [%expect {| ((2 2)) |}];
    find_and_remove 1;
    [%expect {| () |}];
    set t ~key:1 ~data:1;
    show ();
    [%expect
      {|
      ((2 2)
       (1 1))
      |}];
    find_and_remove 2;
    find_and_remove 1;
    find_and_remove 1;
    [%expect
      {|
      (2)
      (1)
      ()
      |}];
    print_s [%sexp (hit_rate t : float)];
    [%expect {| 0.6 |}]
  ;;

  let max_size = M.max_size
  let set_max_size = M.set_max_size

  let%expect_test _ =
    let t = create ~max_size:2 () in
    set t ~key:1 ~data:();
    set t ~key:2 ~data:();
    print_s (stats t);
    [%expect
      {|
      ((max_size 2)
       (length   2)
       (hit_rate 0)
       (keys (1 2)))
      |}];
    let test n =
      let (`Dropped dropped) = set_max_size t ~max_size:n in
      assert (max_size t = n);
      print_s [%message (dropped : int) ~_:(stats t : Sexp.t)]
    in
    test 1;
    [%expect
      {|
      ((dropped 1)
       ((max_size 1)
        (length   1)
        (hit_rate 0)
        (keys (2))))
      |}];
    test 0;
    [%expect
      {|
      ((dropped 1)
       ((max_size 0)
        (length   0)
        (hit_rate 0)
        (keys ())))
      |}];
    show_raise (fun () -> set_max_size t ~max_size:(-1));
    [%expect
      {|
      (raised (
        "invalid Lru.max_size argument" (
          (requested_max_size     -1)
          (smallest_value_allowed 0))))
      |}]
  ;;

  let destruct queue = print_s [%message "destructing" ~_:(queue : (int * int) Queue.t)]

  let%expect_test "[set], [remove] with [destruct]" =
    let t = create ~destruct ~max_size:3 () in
    List.iteri [ 1; 2; 2; 3; 4; 6; 5; 7 ] ~f:(fun idx key -> set t ~key ~data:idx);
    [%expect
      {|
      (destructing ((2 1)))
      (destructing ((1 0)))
      (destructing ((2 2)))
      (destructing ((3 3)))
      (destructing ((4 4)))
      |}];
    print_s (stats t);
    [%expect
      {|
      ((max_size 3)
       (length   3)
       (hit_rate 0)
       (keys (6 5 7)))
      |}];
    print_s [%sexp (remove t 7 : [ `Ok | `No_such_key ])];
    [%expect
      {|
      (destructing ((7 7)))
      Ok
      |}];
    print_s (stats t);
    [%expect
      {|
      ((max_size 3)
       (length   2)
       (hit_rate 0)
       (keys (6 5)))
      |}];
    print_s [%sexp (find_and_remove t 6 : int option)];
    [%expect
      {|
      (destructing ((6 5)))
      (5)
      |}]
  ;;

  let%expect_test "[set_max_size], [clear] with [destruct]" =
    let t = create ~destruct ~max_size:4 () in
    List.iteri [ 1; 2; 3; 4 ] ~f:(fun idx key -> set t ~key ~data:idx);
    let test_set_max_size n =
      let (`Dropped dropped) = set_max_size t ~max_size:n in
      print_s [%message (dropped : int) ~_:(stats t : Sexp.t)]
    in
    let test_clear () =
      let (`Dropped dropped) = clear t in
      print_s [%message (dropped : int) ~_:(stats t : Sexp.t)]
    in
    test_set_max_size 5;
    [%expect
      {|
      ((dropped 0)
       ((max_size 5)
        (length   4)
        (hit_rate 0)
        (keys (1 2 3 4))))
      |}];
    test_set_max_size 2;
    [%expect
      {|
      (destructing (
        (1 0)
        (2 1)))
      ((dropped 2)
       ((max_size 2)
        (length   2)
        (hit_rate 0)
        (keys (3 4))))
      |}];
    test_clear ();
    [%expect
      {|
      (destructing (
        (3 2)
        (4 3)))
      ((dropped 2)
       ((max_size 2)
        (length   0)
        (hit_rate 0)
        (keys ())))
      |}]
  ;;

  let destruct queue = raise_s [%message "destructing" ~_:(queue : (int * int) Queue.t)]

  let%expect_test "consistency with a raising [destruct]" =
    let t = create ~destruct ~max_size:4 () in
    List.iteri [ 1; 2; 3; 4 ] ~f:(fun idx key -> set t ~key ~data:idx);
    [%expect {| |}];
    List.iteri [ 4; 5; 6; 7; 8 ] ~f:(fun idx key ->
      let data = idx + 100 in
      printf "adding %d -> %d\n" key data;
      show_raise (fun () -> set t ~key ~data));
    [%expect
      {|
      adding 4 -> 100
      (raised (destructing ((4 3))))
      adding 5 -> 101
      (raised (destructing ((1 0))))
      adding 6 -> 102
      (raised (destructing ((2 1))))
      adding 7 -> 103
      (raised (destructing ((3 2))))
      adding 8 -> 104
      (raised (destructing ((4 100))))
      |}];
    print_s [%message "" ~_:(stats t : Sexp.t)];
    [%expect
      {|
      ((max_size 4)
       (length   4)
       (hit_rate 0)
       (keys (5 6 7 8)))
      |}];
    List.iter [ 9; 8 ] ~f:(fun key ->
      show_raise (fun () ->
        let result = remove t key in
        print_s [%message "removed" (key : int) (result : [ `Ok | `No_such_key ])]));
    [%expect
      {|
      (removed
        (key    9)
        (result No_such_key))
      "did not raise"
      (raised (destructing ((8 104))))
      |}];
    print_s [%message "" ~_:(stats t : Sexp.t)];
    [%expect
      {|
      ((max_size 4)
       (length   3)
       (hit_rate 0)
       (keys (5 6 7)))
      |}];
    set t ~key:8 ~data:1000;
    show_raise (fun () -> set_max_size t ~max_size:2);
    [%expect
      {|
      (raised (
        destructing (
          (5 101)
          (6 102))))
      |}];
    print_s [%message "" ~_:(stats t : Sexp.t)];
    [%expect
      {|
      ((max_size 2)
       (length   2)
       (hit_rate 0)
       (keys (7 8)))
      |}];
    show_raise (fun () -> clear t);
    [%expect
      {|
      (raised (
        destructing (
          (7 103)
          (8 1000))))
      |}];
    print_s [%message "" ~_:(stats t : Sexp.t)];
    [%expect
      {|
      ((max_size 2)
       (length   0)
       (hit_rate 0)
       (keys ()))
      |}]
  ;;

  let%expect_test "[find_or_add] with a raising [destruct]" =
    let t = create ~destruct ~max_size:1 () in
    let test_find_or_add ~key ~data =
      show_raise (fun () ->
        let elem = find_or_add t key ~default:(fun () -> data) in
        print_s [%message "found" (elem : int)]);
      print_s [%message "" ~_:(stats t : Sexp.t)]
    in
    test_find_or_add ~key:1 ~data:10;
    [%expect
      {|
      (found (elem 10))
      "did not raise"
      ((max_size 1)
       (length   1)
       (hit_rate 0)
       (keys (1)))
      |}];
    test_find_or_add ~key:2 ~data:11;
    [%expect
      {|
      (raised (destructing ((1 10))))
      ((max_size 1)
       (length   1)
       (hit_rate 0)
       (keys (2)))
      |}];
    test_find_or_add ~key:2 ~data:12;
    [%expect
      {|
      (found (elem 11))
      "did not raise"
      ((max_size 1)
       (length   1)
       (hit_rate 0.33333333333333331)
       (keys (2)))
      |}]
  ;;
end
