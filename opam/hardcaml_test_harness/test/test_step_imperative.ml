open Core
open Hardcaml
open Hardcaml_test_harness

module I = struct
  type 'a t =
    { a : 'a [@bits 64]
    ; b : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { s : 'a [@bits 64] } [@@deriving hardcaml]
end

let create _scope { I.a; b } = { O.s = Signal.( +: ) a b }

module Bench = Step_harness.Imperative.Make_monadic (I) (O)

(* More thorough testing is done in [test_step.ml]; this test is primarily to ensure the
   imperative harness runs correctly. *)

let testbench ~inputs:(i : _ I.t) ~outputs:_ =
  let open Bench.Step.Let_syntax in
  let open Bits in
  i.a <--. 4;
  i.b <--. 5;
  let%bind () = Bench.Step.cycle () in
  return ()
;;

let%expect_test "no waves test" =
  Bench.run ~random_initial_state:`All ~create testbench;
  Bench.run
    ~random_initial_state:`All
    ~waves_config:Waves_config.no_waves
    ~create
    testbench;
  [%expect {| |}]
;;

let%expect_test "prefix test (with test name and line numbers)" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:
      (Waves_config.to_directory "/tmp/" |> Waves_config.with_always_include_line_numbers)
    ~test_name:"hello_world"
    ~create
    testbench;
  [%expect
    {| Saved waves to /tmp/test_step_imperative_ml_44_hello_world.hardcamlwaveform |}]
;;
