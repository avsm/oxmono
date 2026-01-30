open! Ppx_template_test_common

module%test [@name "no [kind] annotation"] _ = struct
  module Id : sig
    val%template id : 'a -> 'a
  end = struct
    let%template id x = x
  end

  let%test_unit "value" =
    List.iter
      Generate.(list float)
      ~f:(fun x -> [%test_result: float] (Id.id x) ~expect:x)
  ;;
end

module%test [@name "empty [kind] annotations"] _ = struct
  module Id : sig
    val%template id : 'a -> 'a [@@kind]
  end = struct
    let%template id x = x [@kind] [@@kind]
  end

  let%test_unit "value" =
    List.iter
      Generate.(list float)
      ~f:(fun x -> [%test_result: float] (Id.id x) ~expect:x)
  ;;
end

module%test [@name "single kind variable"] _ = struct
  module Id : sig
    val%template id : ('a : k). 'a -> 'a [@@kind k = (value, float64)]
  end = struct
    let%template id (type a : k) (x : a) : a = x [@@kind k = (value, float64)]
  end

  let%test_unit "value" =
    List.iter
      Generate.(list float)
      ~f:(fun x -> [%test_result: float] (Id.id x) ~expect:x)
  ;;

  let%test_unit "float64" =
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float]
          (Float_u.to_float (Id.id__float64 (Float_u.of_float x)))
          ~expect:x)
  ;;
end

module%test [@name "multiple kind variables"] _ = struct
  module Const : sig
    val%template const : ('a : a) ('b : b). 'a -> 'b -> 'a
    [@@kind a = (value, float64), b = (value, float64)]
  end = struct
    let%template const (type (a : a) (b : b)) (x : a) (_ : b) : a = x
    [@@kind a = (value, float64), b = (value, float64)]
    ;;
  end

  let%test_unit "value value" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) -> [%test_result: float] (Const.const x y) ~expect:x)
  ;;

  let%test_unit "value float64" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float]
          (Const.const__value__float64 x (Float_u.of_float y))
          ~expect:x)
  ;;

  let%test_unit "float64 value" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float]
          (Float_u.to_float (Const.const__float64__value (Float_u.of_float x) y))
          ~expect:x)
  ;;

  let%test_unit "float64 float64" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float]
          (Float_u.to_float
             (Const.const__float64__float64 (Float_u.of_float x) (Float_u.of_float y)))
          ~expect:x)
  ;;
end

module%test [@name "kinds with mod"] _ = struct
  [%%template
  [@@@kind k = (value, value mod portable, value mod contended)]

  type pos : k [@@kind k]
  type ('a : k) neg [@@kind k]
  type _t = ((pos[@kind k]) neg[@kind k]) [@@kind k]]

  [%%template
  (* [_ neg[@kind value]] accepts types at any mode crossing behavior *)
  type _t_value = ((pos[@kind k]) neg[@kind value])
  [@@kind k = (value, value mod portable, value mod contended)]

  (* ['a neg[@kind value mod portable]] guarantees ['a] mode cross portability *)
  let _f_portable
    : 'a @ nonportable -> ('a neg[@kind value mod portable]) -> 'a @ portable
    =
    fun x _y -> x
  ;;

  (* ['a neg[@kind value mod contended]] guarantees ['a] mode cross contention *)
  let _f_uncontended
    : 'a @ contended -> ('a neg[@kind value mod contended]) -> 'a @ contended
    =
    fun x _y -> x
  ;;]
end

module%test [@name "product kinds with mod"] _ = struct
  [%%template
  [@@@kind
    k
    = ( value & value
      , (value mod contended) & (value mod contended)
      , (value & value) mod portable )]

  type pos : k [@@kind k]
  type ('a : k) neg [@@kind k]
  type _t = ((pos[@kind k]) neg[@kind k]) [@@kind k]]

  [%%template
  (* [_ neg[@kind value & value]] accepts types at any mode crossing behavior *)
  type _t_value = ((pos[@kind k]) neg[@kind value & value])
  [@@kind
    k
    = ( value & value
      , (value mod contended) & (value mod contended)
      , (value & value) mod portable )]

  (* ['a neg[@kind value mod portable]] guarantees ['a] mode cross portability *)
  let _f_portable
    : 'a @ nonportable -> ('a neg[@kind (value & value) mod portable]) -> 'a @ portable
    =
    fun x _y -> x
  ;;

  (* ['a neg[@kind value mod contended]] guarantees ['a] mode cross contention *)
  let _f_uncontended
    :  'a @ contended -> ('a neg[@kind (value mod contended) & (value mod contended)])
    -> 'a @ uncontended
    =
    fun x _y -> x
  ;;]
end

module%test [@name "kinds with multiple mods mangle independent of order"] _ = struct
  module%template [@kind k = value mod contended portable] M = struct
    type t : k
  end

  module Require_same
      (A : sig
         type t : any
       end)
      (_ : sig
         type t : any = A.t [@@warning "-34"]
       end) =
  struct end

  module%template _ =
    Require_same
      (M
      [@kind value mod contended portable])
      (M [@kind value mod portable contended])
end

module%test [@name "recursive"] _ = struct
  module Apply_n_times : sig
    val%template apply_n_times : ('a : k). n:int -> ('a -> 'a) -> 'a -> 'a
    [@@kind k = (value, float64)]
  end = struct
    let%template rec apply_n_times : type (a : k). n:int -> (a -> a) -> a -> a =
      fun ~n f x -> if n <= 0 then x else (apply_n_times [@kind k]) ~n:(n - 1) f (f x)
    [@@kind k = (value, float64)]
    ;;
  end

  let%test_unit "value" =
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float]
          (Apply_n_times.apply_n_times ~n:2 Stdlib.Float.neg x)
          ~expect:x)
  ;;

  let%test_unit "float64" =
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float]
          (Float_u.to_float
             (Apply_n_times.apply_n_times__float64 ~n:2 Float_u.neg (Float_u.of_float x)))
          ~expect:x)
  ;;
end

module%test [@name "multiple bindings"] _ = struct
  module Fn : sig
    [%%template:
    val const : ('a : a) ('b : b). 'a -> 'b -> 'a
    [@@kind a = (value, float64), b = (value, float64)]

    val drop : ('a : a) ('b : b). 'a -> 'b -> 'b
    [@@kind a = (value, float64), b = (value, float64)]]
  end = struct
    let%template const (type (a : a) (b : b)) (x : a) (_ : b) : a = x
    [@@kind a = (value, float64), b = (value, float64)]

    and drop (type (a : a) (b : b)) (_ : a) (y : b) : b = y
    [@@kind a = (value, float64), b = (value, float64)]
    ;;
  end

  let%test_unit "value value" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float] (Fn.const x y) ~expect:x;
        [%test_result: float] (Fn.drop x y) ~expect:y)
  ;;

  let%test_unit "value float64" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float] (Fn.const__value__float64 x (Float_u.of_float y)) ~expect:x;
        [%test_result: float]
          (Float_u.to_float (Fn.drop__value__float64 x (Float_u.of_float y)))
          ~expect:y)
  ;;

  let%test_unit "float64 value" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float]
          (Float_u.to_float (Fn.const__float64__value (Float_u.of_float x) y))
          ~expect:x;
        [%test_result: float] (Fn.drop__float64__value (Float_u.of_float x) y) ~expect:y)
  ;;

  let%test_unit "float64 float64" =
    List.iter
      Generate.(list (tuple float float))
      ~f:(fun (x, y) ->
        [%test_result: float]
          (Float_u.to_float
             (Fn.const__float64__float64 (Float_u.of_float x) (Float_u.of_float y)))
          ~expect:x;
        [%test_result: float]
          (Float_u.to_float
             (Fn.drop__float64__float64 (Float_u.of_float x) (Float_u.of_float y)))
          ~expect:y)
  ;;
end

module%test [@name "miscellaneous mangling behavior"] _ = struct
  let%expect_test "[@kind] on expressions" =
    let open struct
      let%template x = String.split_on_char ~sep:'.' __FUNCTION__ |> List.rev |> List.hd
      [@@kind k = (value, float64)]
      ;;

      let%template f () =
        print_endline (x [@kind]);
        [%expect {| x |}];
        print_endline (x [@kind k]);
        [%expect {| x__float64 |}];
        print_endline (x [@kind value]);
        [%expect {| x |}];
        print_endline (x [@kind float64]);
        [%expect {| x__float64 |}]
      [@@kind k = float64]
      ;;
    end in
    f__float64 ()
  ;;

  (* There was a bug where the ppx tried to mangle the left-hand side of every binding,
     regardless of whether it was [@@kind] or not. But, we also allow "polymorphic"
     bindings of kind [value]. *)
  module%test [@name "doesn't complain about misc. let-bindings"] _ = struct
    module Nothing = struct
      type t = |
    end

    let%template (None : Nothing.t option) = None
    let%template[@kind value] (None : Nothing.t option) = None
  end
end

module%test [@name "expression extension"] _ = struct
  let%test_unit "polymorphic binding + monomorphize" =
    let%template id (type a : k) (x : a) = x [@@kind k = (value, float64)] in
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float] ((id [@kind value]) x) ~expect:x;
        [%test_result: float]
          (Float_u.to_float ((id [@kind float64]) (Float_u.of_float x)))
          ~expect:x)
  ;;

  let%test_unit "just monomorphize" =
    let open struct
      let%template id (type a : k) (x : a) = x [@@kind k = (value, float64)]
    end in
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        (* It is intentional that the first [%template] wraps a [Pexp_apply] while the
           second wraps a [Pexp_ident] - this demonstrates how [%template] is valid for
           all expression contexts. *)
        [%test_result: float] [%template (id [@kind value]) x] ~expect:x;
        [%test_result: float]
          (Float_u.to_float ([%template id [@kind float64]] (Float_u.of_float x)))
          ~expect:x)
  ;;
end

module%test [@name "module extensions"] _ = struct
  module Id : [%template:
  val id : ('a : k). 'a -> 'a [@@kind k = (value, float64)]
  val id_b : 'a -> 'a
  val id_u : ('a : float64). 'a -> 'a] =
    [%template
    let id (type a : k) (x : a) = x [@@kind k = (value, float64)]
    let id_b = (id [@kind value])
    let id_u = (id [@kind float64])]

  let%expect_test "monomorphize" =
    List.iter
      Generate.(list float)
      ~f:
        (fun%template x ->
          [%test_eq: float] (Id.id_b x) ((Id.id [@kind value]) x);
          [%test_eq: float]
            (Float_u.to_float (Id.id_u (Float_u.of_float x)))
            (Float_u.to_float ((Id.id [@kind float64]) (Float_u.of_float x))))
  ;;
end

module%test [@name "module bindings"] _ = struct
  module type%template S = sig
    module Id : sig
      val f : ('a : k). 'a -> 'a
    end
    [@@kind k = (value, float64)]
  end

  module%template M : S = struct
    module Id = struct
      let f (type a : k) (x : a) = x
    end
    [@@kind k = (value, float64)]
  end

  let%test_unit "id" =
    (* Unfortunately, [Ppxlib] doesn't currently support [Pexp_letmodule] expressions as
       an attribute context, and they don't have their own [module_binding] nodes, unlike
       [Pexp_let] and its [Value_bindings], so we can't easily support [@@kind] on them. *)
    let module%template Id_b = M.Id [@kind value] in
    let module%template Id_u = M.Id [@kind float64] in
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float] (Id_b.f x) ~expect:x;
        [%test_result: float] (Float_u.to_float (Id_u.f (Float_u.of_float x))) ~expect:x)
  ;;
end

module%test [@name "type extension"] _ = struct
  type%template ('a : k) id = 'a -> 'a [@@kind k = (value, float64)]

  let id_b : [%template: ('a id[@kind value])] = fun x -> x
  let id_u : [%template: ('a id[@kind float64])] = fun x -> x

  let%test_unit "id" =
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float] (id_b x) ~expect:x;
        [%test_result: float] (Float_u.to_float (id_u (Float_u.of_float x))) ~expect:x)
  ;;
end

module%test [@name "type declarations"] _ = struct
  module%template M = struct
    type ('a : k) id = 'a -> 'a [@@kind k = (value, float64)]

    let id_b : ('a id[@kind value]) = fun x -> x
    let id_u : ('a id[@kind float64]) = fun x -> x
  end

  let%test_unit "id" =
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float] (M.id_b x) ~expect:x;
        [%test_result: float] (Float_u.to_float (M.id_u (Float_u.of_float x))) ~expect:x)
  ;;
end

module%test [@name "module declarations"] _ = struct
  module type%template S = sig
    type ('a : k) id = 'a -> 'a

    val id : 'a id
  end
  [@@kind k = (value, float64)]

  module%template M : S [@kind k] = struct
    type ('a : k) id = 'a -> 'a

    let id (type a : k) (x : a) = x
  end
  [@@kind k = (value, float64)]

  let%test_unit "id" =
    let module%template M_b = M [@kind value] in
    let module%template M_u = M [@kind float64] in
    List.iter
      Generate.(list float)
      ~f:(fun x ->
        [%test_result: float] (M_b.id x) ~expect:x;
        [%test_result: float] (Float_u.to_float (M_u.id (Float_u.of_float x))) ~expect:x)
  ;;
end

module%test [@name "check that we can refer to identifiers we've previously bound"] _ =
struct
  let%template apply2 (type (a : foo) (b : bar)) f (x : a) (y : b) =
    let apply1 (type c : baz) f (z : c) = f z [@@kind baz = (foo, bar)] in
    (apply1 [@kind bar]) ((apply1 [@kind foo]) f x) y
  [@@kind foo = (value, float64), bar = (bits32, bits64)]
  ;;

  let%test_unit "value bits32" =
    List.iter
      Generate.(list (tuple float int32))
      ~f:
        (fun%template (x, y) ->
          [%test_result: float]
            ((apply2 [@kind value bits32])
               (fun x y -> x -. Int32_u.to_float y)
               x
               (Int32_u.of_int32 y))
            ~expect:(x -. Int32.to_float y))
  ;;

  let%test_unit "value bits64" =
    List.iter
      Generate.(list (tuple float int64))
      ~f:
        (fun%template (x, y) ->
          [%test_result: float]
            ((apply2 [@kind value bits64])
               (fun x y -> x -. (Int64_u.to_float y |> Float_u.to_float))
               x
               (Int64_u.of_int64 y))
            ~expect:(x -. Int64.to_float y))
  ;;

  let%test_unit "float64 bits32" =
    List.iter
      Generate.(list (tuple float int32))
      ~f:
        (fun%template (x, y) ->
          [%test_result: float]
            ((apply2 [@kind float64 bits32])
               (fun x y -> Float_u.to_float x -. Int32_u.to_float y)
               (Float_u.of_float x)
               (Int32_u.of_int32 y))
            ~expect:(x -. Int32.to_float y))
  ;;

  let%test_unit "float64 bits32" =
    List.iter
      Generate.(list (tuple float int64))
      ~f:
        (fun%template (x, y) ->
          [%test_result: float]
            ((apply2 [@kind float64 bits64])
               (fun x y -> Float_u.(x - Int64_u.to_float y |> to_float))
               (Float_u.of_float x)
               (Int64_u.of_int64 y))
            ~expect:(x -. Int64.to_float y))
  ;;
end
