open! Core
open Portable
open Await
open Base_quickcheck
open Expect_test_helpers_base
module Ws_deque = Portable_ws_deque

let print_lines_sorted s =
  s
  |> String.split_lines
  |> List.sort ~compare:[%compare: string]
  |> String.concat_lines
  |> print_endline
;;

let steal' (deque : int Ws_deque.t) =
  let res = Ws_deque.steal deque in
  if Or_null.is_null res then Basement.Stdlib_shim.Domain.cpu_relax ();
  res
;;

let pop' (deque : int Ws_deque.t) =
  let res = Ws_deque.pop deque in
  if Or_null.is_null res then Basement.Stdlib_shim.Domain.cpu_relax ();
  res
;;

let create_owner_and_stealer () =
  let owner = Capsule.Isolated.create Ws_deque.create in
  Capsule.Isolated.get_id owner
;;

let owner_and_stealer_of_list l =
  let owner = Capsule.Isolated.create (fun () -> Ws_deque.of_list l) in
  Capsule.Isolated.get_id owner
;;

let%expect_test "empty" =
  let t = Ws_deque.create () in
  require_does_raise (fun () : int -> Ws_deque.pop_exn t);
  [%expect {| (Failure "Ws_deque.pop_exn called on empty deque") |}]
;;

let%expect_test "push and pop" =
  let t = Ws_deque.create () in
  Ws_deque.push t 1;
  Ws_deque.push t 10;
  Ws_deque.push t 100;
  print_s [%sexp (Ws_deque.pop_exn t : int)];
  [%expect {| 100 |}];
  print_s [%sexp (Ws_deque.pop_exn t : int)];
  [%expect {| 10 |}];
  print_s [%sexp (Ws_deque.pop_exn t : int)];
  [%expect {| 1 |}]
;;

let%expect_test "push and steal" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  let t = Ws_deque.create () in
  Ws_deque.push t 1;
  Ws_deque.push t 10;
  Ws_deque.push t 100;
  let stealers = 3 in
  Concurrent.with_scope conc () ~f:(fun s ->
    for _ = 0 to stealers - 1 do
      Concurrent.spawn s ~f:(fun _ _ _ ->
        let v = Ws_deque.steal_exn t in
        print_s [%message (v : int)])
    done);
  print_lines_sorted [%expect.output];
  [%expect
    {|
    (v 1)
    (v 10)
    (v 100)
    |}]
;;

let%expect_test "concurrent workload" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  (* The desired number of push events. *)
  let n = 100000 in
  (* The desired number of steal attempts per thief. *)
  let attempts = 100000 in
  (* The number of thieves. *)
  let thieves = if Sys.word_size_in_bits >= 64 then 16 else 2 in
  (* The queue. *)
  let #(owner, { aliased = stealer }) = create_owner_and_stealer () in
  (* A generator of fresh elements. *)
  let c = Atomic.make 0 in
  let fresh () = Atomic.fetch_and_add c 1 in
  (* A history of pushed elements. *)
  let pushed = Capsule.With_mutex.create (fun () -> ref [])
  (* A history of popped elements. *)
  and popped = Capsule.With_mutex.create (fun () -> ref [])
  (* Histories of stolen elements. *)
  and stolen = Capsule.With_mutex.create (fun () -> Array.create ~len:thieves []) in
  let push_back w v x =
    Capsule.With_mutex.iter w v ~f:(fun l -> l := Modes.Contended.cross x :: !l)
  in
  Concurrent.with_scope conc () ~f:(fun s ->
    (* The owner thread. *)
    Concurrent.spawn s ~f:(fun _ _ conc ->
      let owner = Capsule.Isolated.unwrap owner in
      let n = ref n in
      let push () =
        let x : int = fresh () in
        Ws_deque.push owner x;
        push_back (Concurrent.await conc) pushed x;
        Int.decr n
      and pop () =
        match pop' owner with
        | Null -> false
        | This x ->
          push_back (Concurrent.await conc) popped x;
          true
      in
      let rec loop () =
        if !n > 0
        then (
          (* More pushes are allowed. *)
          (* Choose between pushing and popping; then continue. *)
          if Random.bool () then push () else ignore (pop () : bool);
          loop ())
        else if (* No more pushes are allowed. Pop and continue. *)
                pop ()
        then loop ()
      in
      loop () [@nontail]);
    (* The thief threads. *)
    for i = 0 to thieves - 1 do
      Concurrent.spawn s ~f:(fun _ _ conc ->
        let steal () =
          match Ws_deque.steal stealer with
          | Null -> Basement.Stdlib_shim.Domain.cpu_relax ()
          | This x ->
            Capsule.With_mutex.iter (Concurrent.await conc) stolen ~f:(fun stolen ->
              stolen.(i) <- x :: stolen.(i))
            [@nontail]
        in
        for _ = 1 to attempts do
          steal ()
        done)
    done);
  (* Check that the elements that have been popped or stolen are exactly the elements that
     have been pushed. Thus, no element is lost, duplicated, or created out of thin air. *)
  let pushed =
    Capsule.With_mutex.with_lock (Concurrent.await conc) pushed ~f:(fun p -> !p)
  and popped =
    Capsule.With_mutex.with_lock (Concurrent.await conc) popped ~f:(fun p -> !p)
  in
  let npushed = List.length pushed
  and npopped = List.length popped
  and nstolen =
    Capsule.With_mutex.with_lock (Concurrent.await conc) stolen ~f:(fun stolen ->
      Array.fold ~init:0 ~f:(fun accu stolen -> accu + List.length stolen) stolen)
  in
  print_s [%message (npushed : int) (npopped + nstolen : int)];
  [%expect
    {|
    ((npushed             100000)
     ("npopped + nstolen" 100000))
    |}];
  let stolen =
    Capsule.With_mutex.with_lock (Concurrent.await conc) stolen ~f:(fun stolen ->
      Array.fold ~init:[] ~f:( @ ) stolen)
  in
  require_sets_are_equal
    (Core.Int.Set.of_list pushed)
    (Core.Int.Set.of_list (popped @ stolen));
  [%expect {| |}]
;;

module%test One_producer = struct
  let%expect_test "pops are in order" =
    let%quick_test prop (l : int list) (l' : int Nonempty_list.t) =
      let l' = Nonempty_list.to_list l' in
      let q = l @ l' |> Ws_deque.of_list in
      let pop_list = List.init (List.length l') ~f:(fun _ : int -> Ws_deque.pop_exn q) in
      [%test_result: int list] pop_list ~expect:l'
    in
    ()
  ;;

  let%expect_test "pop on an empty deque returns None" =
    let%quick_test[@trials 1] prop
      (l : int list)
      (m : (int[@generator Generator.small_strictly_positive_int]))
      =
      let n = List.length l in
      let m = m + n in
      let count = ref 0 in
      let owner = l |> Ws_deque.of_list in
      let count =
        for _i = 0 to m - 1 do
          match Ws_deque.pop owner with
          | This (_ : int) -> ()
          | Null -> Int.incr count
        done;
        !count
      in
      [%test_result: int] count ~expect:(m - n)
    in
    ()
  ;;
end

module%test One_producer_one_stealer = struct
  (* TEST 1 with 1 producer, 1 stealer and sequential execution. Producer domain pushes a
     list of value THEN a stealer domain steals.

     This checks :
     - order is preserved (first push = first steal)
     - Exit is raised only when the deque is empty *)

  let%expect_test "steals are in order" =
    let%quick_test[@trials 10] prop
      (l : int list)
      (n : (int[@generator Generator.small_strictly_positive_int]))
      =
      let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
      (* Main domain pushes all elements of [l] in order. *)
      let stealer = l |> Ws_deque.of_list in
      (* Then the stealer domain steals [n] times. The output list is composed of all
         stolen value. If an [Exit] is raised, it is register as a [None] value in the
         returned list. *)
      let steal_list : int option list Atomic.t = Atomic.make [] in
      Concurrent.with_scope conc () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ _ ->
          List.init n ~f:(fun _ -> steal' stealer |> Or_null.to_option)
          |> List.rev
          |> Atomic.set steal_list));
      let steal_list = Atomic.get steal_list in
      (* The stolen values should be the [n]th first elements of [l] *)
      let expected_stolen = List.take l n in
      let nfirst = List.take steal_list (List.length l) in
      List.iter2_exn
        ~f:(fun found expected ->
          match found with
          | Some found -> [%test_eq: int] found expected
          | None -> failwith "Not found")
        nfirst
        expected_stolen;
      (* The [n - (List.length l)] last values of [steal_list] should be [None] (i.e. the
         [steal] function had raised [Exit]). *)
      let exits = List.filteri ~f:(fun i _ -> i >= List.length l) steal_list in
      [%test_pred: int option list] (List.for_all ~f:Option.is_none) exits
    in
    ()
  ;;

  (* TEST 2 with 1 producer, 1 stealer and parallel execution.

     Producer domain does pushes. Simultaneously the stealer domain steals.

     This test checks :
     - order is preserved (first push = first steal)
     - Exit is raised only when the deque is empty *)

  let%expect_test "parallel pushes and steals" =
    let%quick_test[@trials 10] prop
      (l : int list)
      (n : (int[@generator Generator.small_strictly_positive_int]))
      =
      let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
      (* Initialization *)
      let #(owner, { aliased = stealer }) = create_owner_and_stealer () in
      let barrier = Barrier.create 2 in
      (* The stealer domain steals n times. If a value [v] is stolen, it is registered as
         [Some v] in the returned list whereas any [Exit] raised is registered as a
         [None]. *)
      let steal_list : int option list Atomic.t = Atomic.make [] in
      Concurrent.with_scope conc () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ conc ->
          Barrier.await (Concurrent.await conc) barrier;
          List.init n ~f:(fun _ -> steal' stealer |> Or_null.to_option)
          |> List.rev
          |> Atomic.set steal_list);
        Barrier.await (Concurrent.Spawn.await s) barrier;
        (* Main domain pushes. *)
        let owner = Capsule.Isolated.unwrap owner in
        List.iter l ~f:(fun (elt : int) ->
          Ws_deque.push owner (elt : int);
          Basement.Stdlib_shim.Domain.cpu_relax ()));
      let steal_list = Atomic.get steal_list in
      (* We don't know how the pushes and the steals are interleaved but we can check that
         if [m] values have been stolen, they are the [m] first pushed values. *)
      let stolen = List.filter_opt steal_list in
      let expected_stolen = List.take l (List.length stolen) in
      [%test_result: int list] stolen ~expect:expected_stolen;
      assert (List.length steal_list = n)
    in
    ()
  ;;

  (* TEST 3 with 1 producer, 1 stealer and parallel execution.

     Main domain does sequential pushes and then pops at the same time that a stealer
     domain steals.

     This test checks :
     - order is preserved (first push = first steal, first push = last pop)
     - no value is both popped and stolen.

     We actually have a strong property here, as all the [push] calls are done before
     [pop] and [steal] calls :

     stolen_values @ (List.rev popped_values) = pushed_values *)

  let%expect_test "parallel pops and steals" =
    let%quick_test[@trials 10] prop
      (l : int list)
      (nsteal : (int[@generator Generator.small_strictly_positive_int]))
      (npop : (int[@generator Generator.small_strictly_positive_int]))
      =
      let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
      if nsteal + npop > List.length l
      then (
        (* Initialization - sequential pushes *)
        let #(owner, { aliased = stealer }) = owner_and_stealer_of_list l in
        let barrier = Barrier.create 2 in
        Random.self_init ~allow_in_tests:true ();
        (* The stealer domain steals [nsteal] times. If a value [v] is stolen, it is
           registered as [Some v] in the returned list whereas any [Exit] raised, it is
           registered as a [None]. *)
        let steal_list : int option list Atomic.t = Atomic.make [] in
        let pop_list =
          Concurrent.with_scope conc () ~f:(fun s ->
            Concurrent.spawn s ~f:(fun _ _ conc ->
              Barrier.await (Concurrent.await conc) barrier;
              List.init nsteal ~f:(fun _ -> steal' stealer |> Or_null.to_option)
              |> List.rev
              |> Atomic.set steal_list);
            Barrier.await (Concurrent.Spawn.await s) barrier;
            (* Main domain pops and builds a list of popped values. *)
            let owner = Capsule.Isolated.unwrap owner in
            List.init npop ~f:(fun _ -> pop' owner |> Or_null.to_option) |> List.rev)
        in
        let steal_list = Atomic.get steal_list in
        (* All the pushes are done sequentially before the run so whatever how pops and
           steals are interleaved if [npop + nsteal <= npush] we should have stolen @
           (List.rev popped) = pushed . *)
        [%test_result: int] (List.length steal_list) ~expect:nsteal;
        [%test_result: int] (List.length pop_list) ~expect:npop;
        let stolen = List.filter_opt steal_list in
        let popped = List.filter_opt pop_list in
        [%test_result: int list] (stolen @ List.rev popped) ~expect:l)
    in
    ()
  ;;
end

module%test One_producer_two_stealers = struct
  (* TEST 1 with 1 producer, 2 stealers and parallel steal calls.

     Producer domain does sequential pushes. Two stealers steal simultaneously.

     This test checks :
     - order is preserved (first push = first steal)
     - no element is stolen by both stealers
     - Exit is raised only when the deque is empty *)

  let%expect_test "parallel steals" =
    let%quick_test[@trials 10] prop
      (l : int list)
      (ns1 : (int[@generator Generator.small_strictly_positive_int]))
      (ns2 : (int[@generator Generator.small_strictly_positive_int]))
      =
      let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
      (* Initialization *)
      let stealer = l |> Ws_deque.of_list in
      let barrier = Barrier.create 2 in
      let result1 = Atomic.make [] in
      let result2 = Atomic.make [] in
      (* Steal calls *)
      let multiple_steal w deque nsteal result =
        Barrier.await w barrier;
        let res = Array.create ~len:nsteal None in
        for i = 0 to nsteal - 1 do
          res.(i) <- steal' deque |> Or_null.to_option
        done;
        Atomic.set result (Array.to_list res)
      in
      Concurrent.with_scope conc () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ conc ->
          multiple_steal (Concurrent.await conc) stealer ns1 result1 [@nontail]);
        Concurrent.spawn s ~f:(fun _ _ conc ->
          multiple_steal (Concurrent.await conc) stealer ns2 result2 [@nontail]));
      let steal_list1 = Atomic.get result1 in
      let steal_list2 = Atomic.get result2 in
      let stolen1 = List.filter_opt steal_list1 in
      let stolen2 = List.filter_opt steal_list2 in
      (* We expect the stolen values to be the first ones that have been pushed. *)
      let expected_stolen = List.take l (ns1 + ns2) in
      (* [compare l l1 l2] checks that there exists an interlacing of the stolen values
         [l1] and [l2] that is equal to the beginning of the push list [l]. *)
      let rec compare (l, l1, l2) =
        match l, l1, l2 with
        | [], [], [] -> true
        | [], _, _ -> false
        | _, [], _ -> [%equal: int list] l l2
        | _, _, [] -> [%equal: int list] l l1
        | x :: l', y :: l1', z :: l2' ->
          if x = y && x = z
          then compare (l', l1, l2') || compare (l', l1', l2)
          else if x = y
          then compare (l', l1', l2)
          else if x = z
          then compare (l', l1, l2')
          else false
      in
      [%test_result: int] (List.length steal_list1) ~expect:ns1;
      [%test_result: int] (List.length steal_list2) ~expect:ns2;
      [%test_pred: int list * int list * int list]
        compare
        (expected_stolen, stolen1, stolen2)
    in
    ()
  ;;
end

let%expect_test "blit_circularly" =
  let test_case ~src ~src_pos ~dst ~dst_pos ~len =
    Portable_ws_deque.For_testing.blit_circularly ~src ~src_pos ~dst ~dst_pos ~len;
    Core.printf
      "[| %s |]"
      (String.concat ~sep:"; " (Array.to_list dst |> List.map ~f:Int.to_string))
  in
  let src = [| 1; 2; 3; 4; 5 |] in
  let dst = [| 0; 0; 0; 0; 0 |] in
  test_case ~src ~src_pos:0 ~dst ~dst_pos:1 ~len:2;
  [%expect {| [| 0; 1; 2; 0; 0 |] |}];
  let src = [| 1; 2; 3; 4; 5 |] in
  let dst = [| 0; 0; 0; 0; 0; 0; 0 |] in
  test_case ~src ~src_pos:2 ~dst ~dst_pos:2 ~len:5;
  [%expect {| [| 0; 0; 3; 4; 5; 1; 2 |] |}];
  let src = [| 1; 2; 3; 4; 5; 6; 7 |] in
  let dst = [| 0; 0; 0; 0; 0 |] in
  test_case ~src ~src_pos:0 ~dst ~dst_pos:3 ~len:5;
  [%expect {| [| 3; 4; 5; 1; 2 |] |}];
  let src = [| 1; 2; 3; 4; 5 |] in
  let dst = [| 0; 0; 0; 0; 0 |] in
  test_case ~src ~src_pos:3 ~dst ~dst_pos:2 ~len:5;
  [%expect {| [| 2; 3; 4; 5; 1 |] |}];
  let src = [| 1; 2; 3 |] in
  let dst = [| 0; 0; 0 |] in
  test_case ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:0;
  [%expect {| [| 0; 0; 0 |] |}];
  let src = [| 1; 2; 3; 4; 5 |] in
  let dst = [| 0; 0; 0; 0; 0 |] in
  test_case ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:5;
  [%expect {| [| 1; 2; 3; 4; 5 |] |}];
  let src = [| 1; 2; 3; 4; 5 |] in
  let dst = [| 0; 0; 0; 0; 0 |] in
  test_case ~src ~src_pos:3 ~dst ~dst_pos:3 ~len:5;
  [%expect {| [| 1; 2; 3; 4; 5 |] |}]
;;
