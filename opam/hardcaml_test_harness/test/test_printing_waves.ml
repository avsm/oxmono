open Core
open Hardcaml
open Hardcaml_test_harness

module I = struct
  type 'a t =
    { a : 'a [@bits 1]
    ; b : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { s : 'a [@bits 1] } [@@deriving hardcaml]
end

let create _scope { I.a; b } = { O.s = Signal.( &: ) a b }

module Bench = Cyclesim_harness.Make (I) (O)

let%expect_test "printed waveforms test" =
  Bench.run
    ~create
    ~print_waves_after_test:
      (Hardcaml_waveterm.Waveform.print ~display_width:40 ~display_height:8 ~wave_width:2)
    (fun ~inputs ~outputs:_ sim ->
       inputs.a := Bits.gnd;
       inputs.b := Bits.gnd;
       Cyclesim.cycle sim;
       inputs.a := Bits.vdd;
       inputs.b := Bits.gnd;
       Cyclesim.cycle sim;
       inputs.a := Bits.gnd;
       inputs.b := Bits.vdd;
       Cyclesim.cycle sim;
       inputs.a := Bits.vdd;
       inputs.b := Bits.vdd;
       Cyclesim.cycle sim;
       Cyclesim.cycle sim;
       ());
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │a       ││      ┌─────┐     ┌─────────│
    │        ││──────┘     └─────┘         │
    │b       ││            ┌───────────────│
    │        ││────────────┘               │
    │s       ││                  ┌─────────│
    │        ││──────────────────┘         │
    └────────┘└────────────────────────────┘
    |}]
;;
