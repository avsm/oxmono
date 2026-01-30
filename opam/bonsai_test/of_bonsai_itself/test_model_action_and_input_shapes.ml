open! Core
open! Import
module Bonsai = Bonsai_proc
open Bonsai.For_open
open Bonsai.Let_syntax

let line_number_regex = Re.Str.regexp ":[0-9]+:[0-9]+"
let path_regex = Re.Str.regexp ".*/bonsai/src/"

let rec censor_sexp : Sexp.t -> Sexp.t = function
  | Atom s ->
    Atom
      (Re.Str.global_replace line_number_regex "" s |> Re.Str.global_replace path_regex "")
  | List l -> List (List.map l ~f:censor_sexp)
;;

let graph_stats c =
  let driver =
    Bonsai_driver.create
      ~instrumentation:(Bonsai_driver.Instrumentation.default_for_test_handles ())
      (fun graph -> Bonsai.Private.perform graph c)
        (* we explicitly optimize the computation ourselves inside of [print], so don't do
           anything here. *)
      ~optimize:false
      ~time_source:(Ui_time_source.create ~start:Time_ns.epoch)
  in
  let lines = driver |> Bonsai_driver.For_testing.dump_dot |> String.split_lines in
  let nodes =
    List.count lines ~f:(fun line -> String.is_substring line ~substring:"shape=Mrecord")
  in
  let edges =
    List.count lines ~f:(fun line -> String.is_substring line ~substring:"->")
  in
  [%message (nodes : int) (edges : int)]
;;

let description c =
  let module Meta = Bonsai.Private.Meta in
  let module Action = Bonsai.Private.Action in
  let time_source = Ui_time_source.create ~start:Time_ns.epoch in
  let (Bonsai.Private.Computation.T { model; action; input; _ }) =
    Bonsai.Private.gather
      c
      ~recursive_scopes:Bonsai.Private.Computation.Recursive_scopes.empty
      ~time_source
  in
  let model = Meta.Model.Type_id.sexp_of_t [%sexp_of: opaque] model.type_id in
  let action = Action.Type_id.sexp_of_t action in
  let input = Meta.Input.sexp_of_t [%sexp_of: opaque] input in
  censor_sexp
    [%sexp
      { shapes = { model : Sexp.t; action : Sexp.t; input : Sexp.t }
      ; incr_graph = (graph_stats c : Sexp.t)
      }]
;;

let prepend_sexp s : Sexp.t -> Sexp.t = function
  | Atom a -> List [ Atom s; Atom a ]
  | List l -> List (Atom s :: l)
;;

let print c =
  let pre_optimization = description (Bonsai.Private.top_level_handle c)
  and post_optimization =
    Bonsai.Private.top_level_handle c |> Bonsai.Private.pre_process |> description
  in
  if Sexp.equal pre_optimization post_optimization
  then pre_optimization |> prepend_sexp "with and without optimizations" |> print_s
  else (
    pre_optimization |> prepend_sexp "without optimizations" |> print_s;
    print_endline "";
    post_optimization |> prepend_sexp "with optimizations" |> print_s)
;;

let opaque_const_value value = Bonsai.Var.create value |> Bonsai.Var.value
let constant_computation = Bonsai.const ()
let stateful_static_computation = Bonsai.state ()

let stateful_dynamic_computation =
  Bonsai.state_machine_with_input
    ~default_model:()
    ~apply_action:(fun _ _ model _ -> model)
    (opaque_const_value ())
;;

let%expect_test "constant computation" =
  print constant_computation;
  [%expect
    {|
    ("with and without optimizations"
      (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
      (incr_graph (
        (nodes 10)
        (edges 13))))
    |}]
;;

let%expect_test "stateful computation" =
  print stateful_static_computation;
  [%expect
    {|
    ("with and without optimizations"
      (shapes (
        (model
         lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
        (action (
          Leaf
          lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
        (input unit)))
      (incr_graph (
        (nodes 11)
        (edges 14))))
    |}]
;;

module%test [@name "sub"] _ = struct
  let%expect_test "two stateful computations" =
    print
      (let%sub _ = stateful_static_computation in
       stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model))
          (action (
            Sub
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (unit unit))))
        (incr_graph (
          (nodes 12)
          (edges 16))))
      |}]
  ;;

  let%expect_test "one stateful computation on left" =
    print
      (let%sub () = constant_computation in
       stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Leaf
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
          (input unit)))
        (incr_graph (
          (nodes 11)
          (edges 14))))
      |}]
  ;;

  let%expect_test "one stateful computation on right" =
    print
      (let%sub _ = stateful_static_computation in
       constant_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Leaf
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
          (input unit)))
        (incr_graph (
          (nodes 10)
          (edges 13))))
      |}]
  ;;

  let%expect_test "no stateful computations" =
    print
      (let%sub _ = constant_computation in
       constant_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
        (incr_graph (
          (nodes 10)
          (edges 13))))
      |}]
  ;;
end

module%test [@name "model_resetter"] _ = struct
  let%expect_test "model_resetter around constant computation" =
    print (Bonsai.with_model_resetter constant_computation);
    [%expect
      {|
      ("without optimizations"
        (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
        (incr_graph (
          (nodes 12)
          (edges 16))))

      ("with optimizations"
        (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
        (incr_graph (
          (nodes 11)
          (edges 14))))
      |}]
  ;;

  let%expect_test "model_resetter around non-constant computation" =
    print (Bonsai.with_model_resetter stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Model_reset (
              Leaf
              lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input unit)))
        (incr_graph (
          (nodes 13)
          (edges 18))))
      |}]
  ;;
end

module%test [@name "wrap"] _ = struct
  let wrap_around c =
    Bonsai.wrap
      ~default_model:()
      ~apply_action:(fun _context _result _model _action -> ())
      ~f:(fun _model _inject -> c)
      ()
  ;;

  let%expect_test "wrap around constant computation" =
    print (wrap_around constant_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model ("outer model for wrap-model" unit))
          (action (Wrap (Leaf Nothing.t) "action id"))
          (input (unit input))))
        (incr_graph (
          (nodes 10)
          (edges 14))))
      |}]
  ;;

  let%expect_test "wrap around non-constant computation" =
    print (wrap_around stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            "outer model for wrap-model"
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model))
          (action (
            Wrap
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)
            "action id"))
          (input (unit input))))
        (incr_graph (
          (nodes 12)
          (edges 17))))
      |}]
  ;;
end

module%test [@name "assoc"] _ = struct
  let%expect_test "constant inside assoc" =
    print
      (Bonsai.assoc (module Int) (opaque_const_value Int.Map.empty) ~f:(fun _ _ ->
         constant_computation));
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model unit)
          (action (Assoc "key id" (Leaf Nothing.t)))
          (input (optional unit))))
        (incr_graph (
          (nodes 14)
          (edges 19))))

      ("with optimizations"
        (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
        (incr_graph (
          (nodes 11)
          (edges 14))))
      |}]
  ;;

  let%expect_test "static state inside assoc" =
    print
      (Bonsai.assoc (module Int) (opaque_const_value Int.Map.empty) ~f:(fun _ _ ->
         stateful_static_computation));
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Assoc "key id" (
              Leaf
              lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (optional unit))))
        (incr_graph (
          (nodes 14)
          (edges 19))))
      |}]
  ;;

  let%expect_test "dynamic_state inside assoc" =
    print
      (Bonsai.assoc (module Int) (opaque_const_value Int.Map.empty) ~f:(fun _ _ ->
         stateful_dynamic_computation));
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Assoc "key id" (
              Leaf
              lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (optional input))))
        (incr_graph (
          (nodes 20)
          (edges 33))))
      |}]
  ;;
end

module%test [@name "assoc_on"] _ = struct
  let%expect_test "constant inside assoc_on" =
    print
      (Bonsai.Expert.assoc_on
         (module Int)
         (module Int)
         (opaque_const_value Int.Map.empty)
         ~get_model_key:(fun k _ -> k)
         ~f:(fun _ _ -> constant_computation));
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model unit)
          (action (Assoc "io key id" "model key id" (Leaf Nothing.t)))
          (input (optional unit))))
        (incr_graph (
          (nodes 12)
          (edges 16))))

      ("with optimizations"
        (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
        (incr_graph (
          (nodes 11)
          (edges 14))))
      |}]
  ;;

  let%expect_test "state inside assoc_on" =
    print
      (Bonsai.Expert.assoc_on
         (module Int)
         (module Int)
         (opaque_const_value Int.Map.empty)
         ~get_model_key:(fun k _ -> k)
         ~f:(fun _ _ -> stateful_static_computation));
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Assoc "io key id" "model key id" (
              Leaf
              lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (optional unit))))
        (incr_graph (
          (nodes 12)
          (edges 16))))
      |}]
  ;;
end

module%test [@name "switch"] _ = struct
  let%expect_test "constant inside switch" =
    print
      (match%sub opaque_const_value true with
       | false -> constant_computation
       | true -> constant_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            (0 unit)
            (1 unit)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 17)
          (edges 26))))
      |}]
  ;;

  let%expect_test "static state inside switch" =
    print
      (match%sub opaque_const_value true with
       | false -> stateful_static_computation
       | true -> stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 19)
          (edges 29))))
      |}]
  ;;

  let%expect_test "dynamic state inside switch" =
    print
      (match%sub opaque_const_value true with
       | false -> stateful_dynamic_computation
       | true -> stateful_dynamic_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 22)
          (edges 35))))
      |}]
  ;;

  let%expect_test "both static and dynamic state inside switch" =
    print
      (match%sub opaque_const_value true with
       | false -> stateful_static_computation
       | true -> stateful_dynamic_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 22)
          (edges 35))))
      |}]
  ;;
end

module%test [@name "optimizable switch"] _ = struct
  let%expect_test "constant inside switch" =
    print
      (match%sub Value.return true with
       | false -> constant_computation
       | true -> constant_computation);
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model (
            (0 unit)
            (1 unit)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 17)
          (edges 27))))

      ("with optimizations"
        (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
        (incr_graph (
          (nodes 10)
          (edges 13))))
      |}]
  ;;

  let%expect_test "static state inside switch" =
    print
      (match%sub Value.return true with
       | false -> stateful_static_computation
       | true -> stateful_static_computation);
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 19)
          (edges 30))))

      ("with optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Leaf
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
          (input unit)))
        (incr_graph (
          (nodes 11)
          (edges 14))))
      |}]
  ;;

  let%expect_test "dynamic state inside switch" =
    print
      (match%sub Value.return true with
       | false -> stateful_dynamic_computation
       | true -> stateful_dynamic_computation);
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 22)
          (edges 36))))

      ("with optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Leaf
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
          (input input)))
        (incr_graph (
          (nodes 11)
          (edges 13))))
      |}]
  ;;

  let%expect_test "both static and dynamic state inside switch" =
    print
      (match%sub Value.return true with
       | false -> stateful_static_computation
       | true -> stateful_dynamic_computation);
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 22)
          (edges 36))))

      ("with optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Leaf
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
          (input input)))
        (incr_graph (
          (nodes 11)
          (edges 13))))
      |}]
  ;;

  let%expect_test "both static and dynamic state inside switch (swapped order)" =
    print
      (match%sub Value.return true with
       | false -> stateful_dynamic_computation
       | true -> stateful_static_computation);
    [%expect
      {|
      ("without optimizations"
        (shapes (
          (model (
            (0
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
            (1
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)))
          (action Switch)
          (input (optional "enum input"))))
        (incr_graph (
          (nodes 22)
          (edges 37))))

      ("with optimizations"
        (shapes (
          (model
           lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model)
          (action (
            Leaf
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml))
          (input unit)))
        (incr_graph (
          (nodes 11)
          (edges 14))))
      |}]
  ;;
end

module%test [@name "action grid"] _ = struct
  let%expect_test "no static, both dynamic" =
    print
      (let%sub _ = stateful_dynamic_computation in
       stateful_dynamic_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model))
          (action (
            Sub
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (input input))))
        (incr_graph (
          (nodes 13)
          (edges 18))))
      |}]
  ;;

  let%expect_test "both static, no dynamic" =
    print
      (let%sub _ = stateful_static_computation in
       stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model))
          (action (
            Sub
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (unit unit))))
        (incr_graph (
          (nodes 12)
          (edges 16))))
      |}]
  ;;

  let%expect_test "one static, one dynamic" =
    print
      (let%sub _ = stateful_dynamic_computation in
       stateful_static_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model))
          (action (
            Sub
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (input unit))))
        (incr_graph (
          (nodes 13)
          (edges 17))))
      |}]
  ;;

  let%expect_test "other static, other dynamic" =
    print
      (let%sub _ = stateful_static_computation in
       stateful_dynamic_computation);
    [%expect
      {|
      ("with and without optimizations"
        (shapes (
          (model (
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model
            lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml-model))
          (action (
            Sub
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)
            (Leaf
             lib/bonsai/test/of_bonsai_itself/test_model_action_and_input_shapes.ml)))
          (input (unit input))))
        (incr_graph (
          (nodes 13)
          (edges 17))))
      |}]
  ;;
end

let%expect_test "reuse of same Value.t" =
  let constant_value = Value.return 5 in
  print
    (let%arr a = constant_value
     and b = constant_value
     and c = constant_value
     and d = constant_value
     and () = opaque_const_value () in
     a + b + c + d);
  [%expect
    {|
    ("with and without optimizations"
      (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
      (incr_graph (
        (nodes 15)
        (edges 22))))
    |}]
;;

let%expect_test "Incr.compute" =
  print (Bonsai.Incr.compute (opaque_const_value ()) ~f:(fun unit_incr -> unit_incr));
  [%expect
    {|
    ("with and without optimizations"
      (shapes ((model unit) (action (Leaf Nothing.t)) (input unit)))
      (incr_graph (
        (nodes 10)
        (edges 12))))
    |}]
;;
