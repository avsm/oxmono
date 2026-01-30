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

module Bench = Step_harness.Functional.Make_monadic (I) (O)

let%expect_test "no waves test" =
  Bench.run ~random_initial_state:`All ~create (fun () ->
    let open Bench.Step.Let_syntax in
    let%bind () = Bench.Step.cycle { I.a = Bits.zero 64; b = Bits.zero 64 } >>| ignore in
    return ());
  Bench.run
    ~random_initial_state:`All
    ~waves_config:Waves_config.no_waves
    ~create
    (fun () ->
       let open Bench.Step.Let_syntax in
       let%bind () =
         Bench.Step.cycle { I.a = Bits.zero 64; b = Bits.zero 64 } >>| ignore
       in
       return ());
  [%expect {| |}]
;;

let%expect_test "prefix test" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:(Waves_config.to_directory "/tmp/")
    ~create
    (fun () ->
       let open Bench.Step.Let_syntax in
       let%bind () =
         Bench.Step.cycle { I.a = Bits.zero 64; b = Bits.zero 64 } >>| ignore
       in
       return ());
  [%expect {| Saved waves to /tmp/test_step_ml_prefix_test.hardcamlwaveform |}]
;;

let%expect_test "prefix test (with test name)" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:(Waves_config.to_directory "/tmp/")
    ~test_name:"hello_world"
    ~create
    (fun () ->
       let open Bench.Step.Let_syntax in
       let%bind () =
         Bench.Step.cycle { I.a = Bits.zero 64; b = Bits.zero 64 } >>| ignore
       in
       return ());
  [%expect {| Saved waves to /tmp/test_step_ml_hello_world.hardcamlwaveform |}]
;;

let%expect_test "prefix test (with test name and line numbers)" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:
      (Waves_config.to_directory "/tmp/" |> Waves_config.with_always_include_line_numbers)
    ~test_name:"hello_world"
    ~create
    (fun () ->
      let open Bench.Step.Let_syntax in
      let%bind () =
        Bench.Step.cycle { I.a = Bits.zero 64; b = Bits.zero 64 } >>| ignore
      in
      return ());
  [%expect {| Saved waves to /tmp/test_step_ml_69_hello_world.hardcamlwaveform |}]
;;

let%expect_test "vcd test" =
  Bench.run
    ~random_initial_state:`All
    ~waves_config:
      (Waves_config.to_directory "/tmp/"
       |> Waves_config.as_wavefile_format ~format:Waves_config.Wavefile_format.Vcd)
    ~create
    (fun () ->
      let open Bench.Step.Let_syntax in
      let%bind () =
        Bench.Step.cycle { I.a = Bits.zero 64; b = Bits.zero 64 } >>| ignore
      in
      return ());
  [%expect {| Saved waves to /tmp/test_step_ml_vcd_test.vcd |}]
;;
