open! Base
open Expect_test_helpers_base

let force = Portable_lazy.force

let%expect_test "[lazy%portable]" =
  let p = lazy%portable (print_endline "running") in
  [%expect {| |}];
  force p;
  [%expect {| running |}]
;;

let%expect_test "[let%portable], single recursion" =
  let open struct
    [@@@expand_inline
      let () =
        let%portable rec p0 = lazy%portable (`p p0) in
        let (`p p1) = force p0 in
        let (`p p2) = force p1 in
        require (phys_equal p1 p2)
      ;;]

    let () =
      let open struct
        type nonrec 'tyvar___001_ _type___002_ = { p0 : 'tyvar___001_ }
      end in
      let { p0 } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___003_ ->
             let p0 =
               Ppx_portable_runtime.Portable_lazy.from_fun (fun () ->
                 Ppx_portable_runtime.Portable_lazy.force
                   (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___003_).p0)
             in
             { p0 = Ppx_portable_runtime.Portable_lazy.from_fun (fun () -> `p p0) }))
      in
      let () = Ppx_portable_runtime.ignore p0 in
      let (`p p1) = force p0 in
      let (`p p2) = force p1 in
      require (phys_equal p1 p2)
    ;;

    [@@@end]
  end in
  [%expect {| |}]
;;

let%expect_test "[let%portable], mutual recursion" =
  let open struct
    [@@@expand_inline
      let () =
        let%portable rec p0 = lazy%portable (`q q0)
        and q0 = lazy%portable (`p p0) in
        let (`q q1) = force p0
        and (`p p1) = force q0 in
        let (`q q2) = force p1
        and (`p p2) = force q1 in
        require (phys_equal p1 p2);
        require (phys_equal q1 q2)
      ;;]

    let () =
      let open struct
        type nonrec ('tyvar___007_, 'tyvar___008_) _type___009_ =
          { p0 : 'tyvar___007_
          ; q0 : 'tyvar___008_
          }
      end in
      let { p0; q0 } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___010_ ->
             let p0 =
               Ppx_portable_runtime.Portable_lazy.from_fun (fun () ->
                 Ppx_portable_runtime.Portable_lazy.force
                   (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___010_).p0)
             and q0 =
               Ppx_portable_runtime.Portable_lazy.from_fun (fun () ->
                 Ppx_portable_runtime.Portable_lazy.force
                   (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___010_).q0)
             in
             { p0 = Ppx_portable_runtime.Portable_lazy.from_fun (fun () -> `q q0)
             ; q0 = Ppx_portable_runtime.Portable_lazy.from_fun (fun () -> `p p0)
             }))
      in
      let () =
        Ppx_portable_runtime.ignore p0;
        Ppx_portable_runtime.ignore q0
      in
      let (`q q1) = force p0
      and (`p p1) = force q0 in
      let (`q q2) = force p1
      and (`p p2) = force q1 in
      require (phys_equal p1 p2);
      require (phys_equal q1 q2)
    ;;

    [@@@end]
  end in
  [%expect {| |}]
;;

let%expect_test "[let%portable], mutual recursion with a function" =
  let open struct
    [@@@expand_inline
      let () =
        let%portable rec p0 = lazy%portable (f ())
        and f () = `p p0 in
        let (`p p1) = force p0 in
        let (`p p2) = force p1 in
        require (phys_equal p1 p2)
      ;;]

    let () =
      let open struct
        type nonrec ('tyvar___015_, 'tyvar___016_) _type___017_ =
          { p0 : 'tyvar___015_
          ; f : 'tyvar___016_
          }
      end in
      let { p0; f } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___018_ ->
             let p0 =
               Ppx_portable_runtime.Portable_lazy.from_fun (fun () ->
                 Ppx_portable_runtime.Portable_lazy.force
                   (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___018_).p0)
             and f _x___019_ =
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___018_).f
                 _x___019_
             in
             { p0 = Ppx_portable_runtime.Portable_lazy.from_fun (fun () -> f ())
             ; f = (fun () -> `p p0)
             }))
      in
      let () =
        Ppx_portable_runtime.ignore p0;
        Ppx_portable_runtime.ignore f
      in
      let (`p p1) = force p0 in
      let (`p p2) = force p1 in
      require (phys_equal p1 p2)
    ;;

    [@@@end]
  end in
  [%expect {| |}]
;;

let%expect_test "[let%portable], mutual recursion no lazy" =
  let open struct
    [@@@expand_inline
      let () =
        let%portable rec is_even x = x = 0 || is_odd (x - 1)
        and is_odd x = x <> 0 && is_even (x - 1) in
        require (is_even 10);
        require (is_odd 11)
      ;;]

    let () =
      let open struct
        type nonrec ('tyvar___025_, 'tyvar___026_) _type___027_ =
          { is_even : 'tyvar___025_
          ; is_odd : 'tyvar___026_
          }
      end in
      let { is_even; is_odd } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___028_ ->
             let is_even _x___029_ =
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___028_).is_even
                 _x___029_
             and is_odd _x___030_ =
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___028_).is_odd
                 _x___030_
             in
             { is_even = (fun x -> x = 0 || is_odd (x - 1))
             ; is_odd = (fun x -> x <> 0 && is_even (x - 1))
             }))
      in
      let () =
        Ppx_portable_runtime.ignore is_even;
        Ppx_portable_runtime.ignore is_odd
      in
      require (is_even 10);
      require (is_odd 11)
    ;;

    [@@@end]
  end in
  [%expect {| |}]
;;

(* Quick example of a type that converts to/from [Portable_lazy.t]. *)
module Portable_sequence : sig
  type 'a t : value mod contended portable

  val create : (unit -> 'a * 'a t) @ portable -> 'a t
  val map : 'a t -> f:('a -> 'b) @ portable -> 'b t
  val of_portable_lazy : 'a t Portable_lazy.t -> 'a t
  val take : 'a t -> int -> 'a list
end = struct
  type 'a t = { force : unit -> 'a * 'a t @@ portable }

  let create force = { force }
  let of_portable_lazy p = { force = (fun () -> (Portable_lazy.force p).force ()) }

  let rec map t ~f =
    { force =
        (fun () ->
          let x, t = t.force () in
          f x, map t ~f)
    }
  ;;

  let[@tail_mod_cons] rec take t n =
    if n <= 0
    then []
    else (
      let x, t = t.force () in
      x :: take t (n - 1))
  ;;
end

let%expect_test "[let%portable], with custom type" =
  let open struct
    [@@@expand_inline
      let () =
        let%portable rec head =
          Portable_sequence.create (fun () -> 0, tail)
            [@@custom Portable_sequence.of_portable_lazy]
        and tail =
          Portable_sequence.map head ~f:Int.succ
            [@@custom Portable_sequence.of_portable_lazy]
        in
        print_s [%sexp (Portable_sequence.take head 10 : int list)]
      ;;]

    let () =
      let open struct
        type nonrec ('tyvar___037_, 'tyvar___038_) _type___039_ =
          { head : 'tyvar___037_
          ; tail : 'tyvar___038_
          }
      end in
      let { head; tail } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___040_ ->
             let head =
               Portable_sequence.of_portable_lazy
                 (Ppx_portable_runtime.Portable_lazy.from_fun (fun () ->
                    (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___040_).head))
             and tail =
               Portable_sequence.of_portable_lazy
                 (Ppx_portable_runtime.Portable_lazy.from_fun (fun () ->
                    (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___040_).tail))
             in
             { head = Portable_sequence.create (fun () -> 0, tail)
             ; tail = Portable_sequence.map head ~f:Int.succ
             }))
      in
      let () =
        Ppx_portable_runtime.ignore head;
        Ppx_portable_runtime.ignore tail
      in
      print_s
        ((fun [@merlin.hide] x__041_ -> sexp_of_list sexp_of_int x__041_)
           (Portable_sequence.take head 10))
    ;;

    [@@@end]
  end in
  [%expect {| (0 1 2 3 4 5 6 7 8 9) |}]
;;

let%expect_test "[let%portable], polymorphic" =
  let open struct
    [@@@expand_inline
      let () =
        let%portable rec f : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) -> int
          = function
          | `Leaf _ -> 1
          | `Tree list -> g list
        and g : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) list -> int =
          fun list -> 1 + List.sum (module Int) list ~f
        in
        print_s [%sexp (f (`Tree [ `Leaf 1; `Leaf 2; `Leaf 3 ]) : int)];
        [%expect {| 4 |}];
        print_s [%sexp (f (`Tree [ `Leaf "a"; `Leaf "b"; `Leaf "c" ]) : int)]
      ;;]

    let () =
      let open struct
        type nonrec _type___047_ =
          { f : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) -> int
          ; g : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) list -> int
          }
      end in
      let { f; g } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___048_ ->
             let f : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) -> int =
               fun _x___049_ ->
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___048_).f
                 _x___049_
             and g : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) list -> int =
               fun _x___050_ ->
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___048_).g
                 _x___050_
             in
             { f =
                 (function
                   | `Leaf _ -> 1
                   | `Tree list -> g list)
             ; g = (fun list -> 1 + List.sum (module Int) list ~f)
             }))
      in
      let () =
        Ppx_portable_runtime.ignore f;
        Ppx_portable_runtime.ignore g
      in
      print_s ((sexp_of_int [@merlin.hide]) (f (`Tree [ `Leaf 1; `Leaf 2; `Leaf 3 ])));
      Ppx_expect_test_block.run_test
        ~test_id:
          ((Ppx_expect_runtime.Expectation_id.of_int_exn [@alert "-ppx_expect_runtime"])
             19) [@merlin.hide];
      print_s
        ((sexp_of_int [@merlin.hide]) (f (`Tree [ `Leaf "a"; `Leaf "b"; `Leaf "c" ])))
    ;;

    [@@@end]
  end in
  [%expect {| 4 |}]
;;

let%expect_test "[let%portable], polymorphic, structure item" =
  let open struct
    [@@@expand_inline
      let%portable rec f : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) -> int
        = function
        | `Leaf _ -> 1
        | `Tree list -> g list

      and g : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) list -> int =
        fun list -> 1 + List.sum (module Int) list ~f
      ;;]

    include struct
      open struct
        type nonrec _type___055_ =
          { f : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) -> int
          ; g : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) list -> int
          }
      end

      let { f; g } =
        Ppx_portable_runtime.Portable_lazy.force
          (Ppx_portable_runtime.Portable_lazy.from_fun_fixed (fun _portable_lazy___056_ ->
             let f : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) -> int =
               fun _x___057_ ->
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___056_).f
                 _x___057_
             and g : 'a. ([ `Leaf of 'a | `Tree of 'b list ] as 'b) list -> int =
               fun _x___058_ ->
               (Ppx_portable_runtime.Portable_lazy.force _portable_lazy___056_).g
                 _x___058_
             in
             { f =
                 (function
                   | `Leaf _ -> 1
                   | `Tree list -> g list)
             ; g = (fun list -> 1 + List.sum (module Int) list ~f)
             }))
      ;;

      let () =
        Ppx_portable_runtime.ignore f;
        Ppx_portable_runtime.ignore g
      ;;
    end

    [@@@end]
  end in
  print_s [%sexp (f (`Tree [ `Leaf 1; `Leaf 2; `Leaf 3 ]) : int)];
  [%expect {| 4 |}];
  print_s [%sexp (f (`Tree [ `Leaf "a"; `Leaf "b"; `Leaf "c" ]) : int)];
  [%expect {| 4 |}]
;;
