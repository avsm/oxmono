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

module Bench = Cyclesim_harness.Make (I) (O)

let%expect_test "no waves test" =
  Bench.run ~random_initial_state:`All ~create (fun ~inputs ~outputs:_ sim ->
    inputs.a := Bits.zero 64;
    inputs.b := Bits.zero 64;
    Cyclesim.cycle sim;
    ());
  Bench.run
    ~random_initial_state:`All
    ~waves_config:Waves_config.no_waves
    ~create
    (fun ~inputs ~outputs:_ sim ->
       inputs.a := Bits.zero 64;
       inputs.b := Bits.zero 64;
       Cyclesim.cycle sim;
       ());
  [%expect {| |}]
;;

let%expect_test "prefix test" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:(Waves_config.to_directory "/tmp/")
    ~create
    (fun ~inputs ~outputs:_ sim ->
       inputs.a := Bits.zero 64;
       inputs.b := Bits.zero 64;
       Cyclesim.cycle sim;
       ());
  [%expect {| Saved waves to /tmp/test_cyclesim_ml_prefix_test.hardcamlwaveform |}]
;;

let%expect_test "prefix test (with test name)" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:(Waves_config.to_directory "/tmp/")
    ~test_name:"hello_world"
    ~create
    (fun ~inputs ~outputs:_ sim ->
       inputs.a := Bits.zero 64;
       inputs.b := Bits.zero 64;
       Cyclesim.cycle sim;
       ());
  [%expect {| Saved waves to /tmp/test_cyclesim_ml_hello_world.hardcamlwaveform |}]
;;

let%expect_test "save here test" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:(Waves_config.to_test_directory ())
    ~create
    (fun ~inputs ~outputs:_ sim ->
       inputs.a := Bits.zero 64;
       inputs.b := Bits.zero 64;
       Cyclesim.cycle sim;
       ());
  [%expect {| Saved waves to ./test_cyclesim_ml_save_here_test.hardcamlwaveform |}]
;;

module%test [@name "testing with no expect-test name provided"] _ = struct
  let%expect_test _ =
    Bench.run
      ~random_initial_state:`All
      ~waves_config:(Waves_config.to_test_directory ())
      ~create
      (fun ~inputs ~outputs:_ sim ->
         inputs.a := Bits.zero 64;
         inputs.b := Bits.zero 64;
         Cyclesim.cycle sim;
         ());
    [%expect {| Saved waves to ./test_cyclesim_ml_81.hardcamlwaveform |}]
  ;;
end

let%test_unit "testing outside of an expect-test" =
  (* Grab [%here] once, this lets us know the line number it's going to use *)
  let test_here = [%here] in
  Bench.run
    ~here:test_here
    ~random_initial_state:`All
    ~waves_config:(Waves_config.to_test_directory ())
    ~create
    (fun ~inputs ~outputs:_ sim ->
       inputs.a := Bits.zero 64;
       inputs.b := Bits.zero 64;
       Cyclesim.cycle sim;
       ());
  let expected_filename =
    [%string "./test_cyclesim_ml_%{test_here.pos_lnum#Int}.hardcamlwaveform"]
  in
  assert (Sys_unix.file_exists_exn expected_filename)
;;

let%expect_test "multiple tests with the same name shouldn't clobber each other" =
  for _ = 1 to 5 do
    Bench.run
      ~random_initial_state:`All
      ~waves_config:(Waves_config.to_test_directory ())
      ~create
      (fun ~inputs ~outputs:_ sim ->
         inputs.a := Bits.zero 64;
         inputs.b := Bits.zero 64;
         Cyclesim.cycle sim;
         ())
  done;
  [%expect
    {|
    Saved waves to ./test_cyclesim_ml_multiple_tests_with_the_same_name_shouldn_t_clobber_each_other.hardcamlwaveform
    Saved waves to ./test_cyclesim_ml_multiple_tests_with_the_same_name_shouldn_t_clobber_each_other_1.hardcamlwaveform
    Saved waves to ./test_cyclesim_ml_multiple_tests_with_the_same_name_shouldn_t_clobber_each_other_2.hardcamlwaveform
    Saved waves to ./test_cyclesim_ml_multiple_tests_with_the_same_name_shouldn_t_clobber_each_other_3.hardcamlwaveform
    Saved waves to ./test_cyclesim_ml_multiple_tests_with_the_same_name_shouldn_t_clobber_each_other_4.hardcamlwaveform
    |}]
;;
