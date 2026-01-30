"Hardcaml Test Harness"
=======================

`Hardcaml_test_harness` helps with common boilerplate when writing Hardcaml tests using
Cyclesim or the Step test library. The harness handles constructing a test environment
from a circuit and producing waveforms for debugging.

# Examples

## Defining a Waveforms Config

See `src/waves_config.mli` for the full up-to-date API.

```ocaml

(* Do not save any waves *)
let waves_config = Waves_config.no_waves

(* Save waves to the current working directory of the test *)
let waves_config = Waves_config.to_test_directory ()

(* Save waves to a specified directory *)
let waves_config = Waves_config.to_directory "/work/hardcaml/waves/"

(* Run the simulation for extra cycles after a failure, useful for capturing context when
   the test is terminated early due to an assertion failure *)
let waves_config =
   Waves_config.to_test_directory ()
   |> Waves_config.with_extra_cycles_after_test ~n:10
;;

(* Control the waveform file-format *)
let waves_config =
   Waves_config.to_test_directory ()
   |> Waves_config.as_wavefile_format ~format:Waves_config.Wavefile_format.Vcd
;;
```

## Example usage

See `src/harness_base.mli` for the full up-to-date API

```ocaml
open Core
open Hardcaml
open Hardcaml_test_harness

(* Your hardcaml design module to be tested, should provide:
   [module I : Interface.S],
   [module O : Interface.S],
   [val create : Scope.t -> Signal.t I.t -> Signal.t O.t] *)
module Dut = ...

module Harness = Cyclesim_harness.Make (Dut.I) (Dut.O)

let waves_config = Waves_config.to_directory "/work/hardcaml/waves/"

let%expect_test "demo" =
  Harness.run
    ~here:[%here]
    (* Whether to randomize the initial state of registers (optional) *)
    ~random_initial_state:`All
    (* Config for where to save waves (optional, defaults to no waves) *)
    ~waves_config
    (* Create function for the design *)
    ~create:Dut.create
    (* Name to apply to the waveform filename. The rules for naming the waveforms is as follows:
       - Always prefixed with the filename from [%here]
       - Uses the provided name if given, otherwise tries to use the name of the
         currently-running expect-test, otherwise falls back to just the line number *)
    ~test_name:"cyclesim_demo"

    (* Your actual cyclesim testbench *)
    (fun ~inputs ~outputs sim ->
       let open Bits in
       inputs.a <--. 1;
       inputs.b <--. 2;
       Cyclesim.cycle sim;
       assert (Bits.to_int_trunc !(outputs.s) = 3);
       ());
  [%expect {| Saved waves to /work/hardcaml/waves/test_cyclesim_ml_cyclesim_demo.hardcamlwaveform |}]
;;
```

### Use with `Hardcaml_step_testbench` (Functional version)

```ocaml
open Core
open Hardcaml
open Hardcaml_test_harness

(* Your hardcaml design module to be tested, should provide:
   [module I : Interface.S],
   [module O : Interface.S],
   [val create : Scope.t -> Signal.t I.t -> Signal.t O.t] *)
module Dut = ...

module Harness = Step_harness.Make (Dut.I) (Dut.O)

let waves_config = Waves_config.to_directory "/work/hardcaml/waves/"

let%expect_test "demo" =
  Harness.run
    ~here:[%here]
    ~random_initial_state:`All
    ~waves_config
    ~create:Dut.create
    ~test_name:"functional_step_demo"

    (* Your actual step testbench *)
    (fun () ->
       let module Step = Harness.Step in
       let open Step.Let_syntax in
       let%bind o = Step.cycle { Dut.I.a = Bits.zero 64; b = Bits.zero 64 } in
       assert (Bits.to_int_trunc o.after_edge.s = 0);
       return ());
  [%expect {| Saved waves to /work/hardcaml/waves/test_step_ml_functional_step_demo.hardcamlwaveform |}]
;;
```

### Use with `Hardcaml_step_testbench` (Imperative version)

```ocaml
open Core
open Hardcaml
open Hardcaml_test_harness

(* Your hardcaml design module to be tested, should provide:
   [module I : Interface.S],
   [module O : Interface.S],
   [val create : Scope.t -> Signal.t I.t -> Signal.t O.t] *)
module Dut = ...

module Harness = Step_harness.Make (Dut.I) (Dut.O)

let waves_config = Waves_config.to_directory "/work/hardcaml/waves/"

let%expect_test "demo" =
  Harness.run
    ~here:[%here]
    ~random_initial_state:`All
    ~waves_config
    ~create:Dut.create
    ~test_name:"imperative_step_demo"

    (* Your actual step testbench *)
    (fun ~inputs ~outputs ->
       let module Step = Harness.Step in
       let open Step.Let_syntax in
       let open Bits in
       inputs.a <--. 1;
       inputs.b <--. 2;
       let%bind () = Step.cycle () in
       assert (Bits.to_int_trunc !(outputs.s) = 3);
       return ());
  [%expect {| Saved waves to /work/hardcaml/waves/test_step_imperative_ml_imperative_step_demo.hardcamlwaveform |}]
;;
```
