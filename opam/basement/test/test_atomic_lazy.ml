open! Base
open Expect_test_helpers_base
open Basement

module%template
  [@mode (p, c) = ((nonportable, uncontended), (portable, contended))] Test_lazy (Lazy : sig
  @@ portable
    type (+'a : value_or_null) t

    val from_val : ('a : value_or_null mod c). 'a @ p -> 'a t @ p
    val from_fun : ('a : value_or_null). (unit -> 'a @ p) @ once p -> 'a t @ p
    val from_fun_fixed : ('a : value_or_null). ('a t @ c p -> 'a @ p) @ once p -> 'a t @ p
    val force : ('a : value_or_null). 'a t @ c local p -> 'a @ c
    val is_val : ('a : value_or_null). 'a t @ c p -> bool
    val peek : ('a : value). 'a t @ c p -> 'a Or_null_shim.t @ c
    val peek_opt : ('a : value_or_null). 'a t @ c p -> 'a option @ c

    val globalize
      : ('a : value_or_null) ('b : value_or_null).
      'b -> 'a t @ c local p -> 'a t @ c p
  end) =
struct
  let%expect_test "from_val" =
    let t = Lazy.from_val 1 in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.force t : int)];
    print_s [%message (Lazy.is_val t : bool)];
    [%expect
      {|
      ("Lazy.is_val t" true)
      ("Lazy.force t" 1)
      ("Lazy.is_val t" true)
      |}]
  ;;

  let%expect_test "from_fun" =
    let t = Lazy.from_fun (fun () -> 2 + 2) in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.force t : int)];
    print_s [%message (Lazy.is_val t : bool)];
    [%expect
      {|
      ("Lazy.is_val t" false)
      ("Lazy.force t" 4)
      ("Lazy.is_val t" true)
      |}]
  ;;

  let%expect_test "from_fun_fixed" =
    let t =
      Lazy.from_fun_fixed (fun lz ->
        print_endline "forced";
        fun n ->
          if n = 0
          then print_endline "finished"
          else (
            print_endline "called";
            (Lazy.force lz) (n - 1)))
    in
    let f = Lazy.force t in
    f 5;
    [%expect
      {|
      forced
      called
      called
      called
      called
      called
      finished
      |}];
    f 3;
    [%expect
      {|
      called
      called
      called
      finished
      |}]
  ;;

  let%expect_test "force always returns the same value" =
    let t = Lazy.from_fun (fun () -> Portable_atomic.make 0) in
    let force1 = Lazy.force t in
    let force2 = Lazy.force t in
    require (phys_equal force1 force2);
    [%expect {| |}]
  ;;

  let%expect_test "peek and peek_opt" =
    let t = Lazy.from_fun (fun () -> 1) in
    let result = Lazy.peek t in
    let result_opt = Lazy.peek_opt t in
    print_s [%message "" ~peek:(result : int or_null)];
    print_s [%message "" ~peek_opt:(result_opt : int option)];
    print_s [%message "" ~force:(Lazy.force t : int)];
    let result = Lazy.peek t in
    let result_opt = Lazy.peek_opt t in
    print_s [%message "" ~peek:(result : int or_null)];
    print_s [%message "" ~peek_opt:(result_opt : int option)];
    [%expect
      {|
      (peek ())
      (peek_opt ())
      (force 1)
      (peek (1))
      (peek_opt (1))
      |}]
  ;;

  let%expect_test "globalize" =
    let t = Lazy.from_fun (fun () -> 1) in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message "globalizing"];
    let t_copy = Lazy.globalize () t in
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.is_val t_copy : bool)];
    print_s [%message (Lazy.force t_copy : int)];
    print_s [%message (Lazy.is_val t : bool)];
    print_s [%message (Lazy.is_val t_copy : bool)];
    [%expect
      {|
      ("Lazy.is_val t" false)
      globalizing
      ("Lazy.is_val t" false)
      ("Lazy.is_val t_copy" false)
      ("Lazy.force t_copy" 1)
      ("Lazy.is_val t" true)
      ("Lazy.is_val t_copy" true)
      |}]
  ;;

  let%expect_test "relaxed value restriction applies (because it's covariant)" =
    let t : type a. a list Lazy.t = Lazy.from_fun (fun () -> []) in
    let result : int list = Lazy.force t in
    print_s [%message "" ~force:(result : _ list)];
    let result : bool list = Lazy.force t in
    print_s [%message "" ~force:(result : _ list)];
    [%expect
      {|
      (force ())
      (force ())
      |}]
  ;;
end

module _ = Test_lazy (Atomic_lazy)

module%template _ = Test_lazy [@mode portable] (struct
    open Atomic_lazy

    type nonrec ('a : value_or_null) t = 'a t

    let from_val = (from_val [@mode portable])
    let from_fun = (from_fun [@mode portable])
    let from_fun_fixed = (from_fun_fixed [@mode portable])
    let force = (force [@mode contended])
    let is_val = (is_val [@mode contended])
    let peek = (peek [@mode contended])
    let peek_opt = (peek_opt [@mode contended])
    let globalize = (globalize [@mode contended])
  end)

(* Example of [Atomic_lazy] being used for mode-polymorphism; the main novel result is
   that [of_lazy] and [fixed_point] below can be defined mode-polymorphically (rather than
   ad-hoc). *)

module%template Portable = struct
  let wrap = Fn.id
  let unwrap = Fn.id
  let[@mode portable] wrap = Modes.Portable.wrap
  let[@mode portable] unwrap = Modes.Portable.unwrap
end

module%template Generator : sig @@ portable
  type 'a t : value mod contended

  val run : 'a t -> int -> 'a
  val inspect : int t

  [@@@mode.default (p, c) = ((nonportable, uncontended), (portable, contended))]

  val return : ('a : value mod c). 'a @ p -> 'a t @ p
  val map : 'a t @ p -> f:('a -> 'b) @ p -> 'b t @ p
  val bind : 'a t @ p -> f:('a -> 'b t) @ p -> 'b t @ p
  val recursive_union : base:'a t @ p -> recur:('a t -> 'a t) @ p -> 'a t @ p
end = struct
  type 'a t = int -> 'a

  let run t n = t n
  let inspect : _ = fun n -> n

  [@@@mode.default (p, c) = ((nonportable, uncontended), (portable, contended))]

  let shrink t : _ = fun n -> run t (n / 2)
  let return x : _ = fun _ -> x
  let map t ~f : _ = fun n -> f (run t n)
  let bind t ~f : _ = fun n -> run (f (run t n)) n
  let of_lazy lz : _ = fun n -> run ((Atomic_lazy.force [@mode c]) lz) n

  let fixed_point of_generator =
    let lz =
      (Atomic_lazy.from_fun_fixed [@mode p]) (fun lz ->
        let lz' =
          (Atomic_lazy.from_fun [@mode p]) (fun () ->
            (Atomic_lazy.force [@mode c]) lz
            |> Modes.Contended.cross
            |> (Portable.unwrap [@mode p]))
        in
        (Portable.wrap [@mode p]) (of_generator ((of_lazy [@mode p]) lz')))
    in
    require (not ((Atomic_lazy.is_val [@mode c]) lz));
    let result = (Atomic_lazy.force [@mode c]) lz |> (Portable.unwrap [@mode p]) in
    require ((Atomic_lazy.is_val [@mode c]) lz);
    result
  ;;

  let recursive_union ~base ~recur =
    (fixed_point [@mode p]) (fun gen ->
      (bind [@mode p]) inspect ~f:(fun n ->
        if n = 0 then base else recur ((shrink [@mode p]) gen)))
  ;;
end

[%%template
[@@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]

let%expect_test "lazy generators work" =
  let open Generator in
  let gen @ p = (return [@mode p]) 1 in
  print_s [%message (run gen 999 : int)];
  [%expect {| ("run gen 999" 1) |}];
  let gen @ p =
    (recursive_union [@mode p])
      ~base:((return [@mode p]) ([] : int list))
      ~recur:(fun t -> bind inspect ~f:(fun n -> map t ~f:(fun l -> n :: l)))
  in
  print_s [%message (Generator.run gen 999 : int list)];
  [%expect {| ("Generator.run gen 999" (999 499 249 124 62 31 15 7 3 1)) |}]
;;]
