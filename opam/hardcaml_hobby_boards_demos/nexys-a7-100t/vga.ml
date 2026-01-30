open! Core
open Hardcaml
open Hardcaml_waveterm
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  module Spec = Vga.Spec
  module Scan = Vga.Scan
  include Nexys_a7_100t
end

let vga_demo scope spec (vga_spec : Spec.t) =
  let%hw enable = reg_fb spec ~width:2 ~f:(fun d -> d +:. 1) ==:. 0 in
  let hscan =
    Scan.create (Scope.sub_scope scope "hscan") spec ~enable vga_spec.horizontal_timing
  in
  let col = hscan.counter in
  let vscan =
    Scan.create
      (Scope.sub_scope scope "vscan")
      spec
      ~enable:(enable &: hscan.last &: hscan.is_front_porch)
      vga_spec.vertical_timing
  in
  let row = vscan.counter in
  let in_bounds x min max = x >=:. min &: (x <=:. max) in
  let red =
    reg
      spec
      (mux2
         (hscan.is_active
          &: vscan.is_active
          &: in_bounds col 0 (vga_spec.horizontal_timing.active - 1)
          &: in_bounds row 0 ((vga_spec.vertical_timing.active / 2) - 1))
         (of_unsigned_int ~width:4 15)
         (of_unsigned_int ~width:4 0))
  in
  let green =
    reg
      spec
      (mux2
         (hscan.is_active
          &: vscan.is_active
          &: in_bounds col 0 (vga_spec.horizontal_timing.active - 1)
          &: in_bounds
               row
               (vga_spec.vertical_timing.active / 2)
               (vga_spec.vertical_timing.active - 1))
         (of_unsigned_int ~width:4 15)
         (of_unsigned_int ~width:4 0))
  in
  let blue =
    reg
      spec
      (mux2
         (hscan.is_active
          &: vscan.is_active
          &: in_bounds col 0 (vga_spec.horizontal_timing.active - 1)
          &: in_bounds
               row
               (vga_spec.vertical_timing.active / 3)
               (2 * vga_spec.vertical_timing.active / 3))
         (of_unsigned_int ~width:4 15)
         (of_unsigned_int ~width:4 0))
  in
  { Vga.O.red
  ; green
  ; blue
  ; horizontal_sync = reg spec ~:(hscan.is_sync)
  ; vertical_sync = reg spec ~:(vscan.is_sync)
  }
;;

let create () =
  let board = Board.create () in
  let scope = Scope.create () in
  let clocking = Clock_and_reset.create board in
  let spec = Utils.sync_reg_spec clocking in
  Vga.complete board (vga_demo scope spec Spec.t640x480_60hz);
  board
;;

let test_video_timing cycles =
  let f scope (clocking : _ Clocking.t) =
    let spec = Clocking.to_spec clocking in
    vga_demo scope spec Hardcaml_hobby_boards_kernel.Vga.Spec.testing
  in
  let module Sim = Cyclesim.With_interface (Clocking) (Vga.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (f scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle ~n:cycles sim;
  waves
;;

let%expect_test "timing" =
  let waves = test_video_timing 200 in
  Waveform.expect_exact waves ~wave_width:(-2) ~start_cycle:24;
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
│                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
│clear             ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│enable            ││╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ ╥ │
│                  ││╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─│
│                  ││────────────────────────────────────────────────────────────────────│
│vga_b             ││ 0                                                                  │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────────────────────────────────────────────────────────────────────│
│vga_g             ││ 0                                                                  │
│                  ││────────────────────────────────────────────────────────────────────│
│vga_hs            ││─────╥   ╥─────────────╥   ╥─────────────╥   ╥─────────────╥   ╥────│
│                  ││     ╨───╨             ╨───╨             ╨───╨             ╨───╨    │
│                  ││───────────────────────────────────────────────╥───────╥─────────╥──│
│vga_r             ││ 0                                             ║F      ║0        ║F │
│                  ││───────────────────────────────────────────────╨───────╨─────────╨──│
│vga_vs            ││     ╥──────────────────────────────────────────────────────────────│
│                  ││─────╨                                                              │
│gnd               ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬───┬│
│hscan$counter     ││ │0│1│0│1│0  │1│2│3│0│1│0│1│0  │1│2│3│0│1│0│1│0  │1│2│3│0│1│0│1│0  ││
│                  ││─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴───┴│
│                  ││─┬───┬───┬─┬───────┬───┬───┬─┬───────┬───┬───┬─┬───────┬───┬───┬─┬──│
│hscan$sm          ││ │Fr.│Sy.│.│Active │Fr.│Sy.│.│Active │Fr.│Sy.│.│Active │Fr.│Sy.│.│A.│
│                  ││─┴───┴───┴─┴───────┴───┴───┴─┴───────┴───┴───┴─┴───────┴───┴───┴─┴──│
│vdd               ││────────────────────────────────────────────────────────────────────│
│                  ││                                                                    │
│                  ││───────────────────────┬─────────────────┬─────────────────┬────────│
│vscan$counter     ││ 0                     │1                │0                │1       │
│                  ││───────────────────────┴─────────────────┴─────────────────┴────────│
│                  ││─────┬───────────────────────────────────┬──────────────────────────│
│vscan$sm          ││ Sync│Back_porch                         │Active                    │
│                  ││─────┴───────────────────────────────────┴──────────────────────────│
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
1853ebc2ab6d3a3f7a10a2770cb5dcec
|}]
;;
