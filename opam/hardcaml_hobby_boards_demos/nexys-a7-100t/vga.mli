(** Simple VGA 640x480 demo. *)

open Hardcaml

val vga_demo
  :  Scope.t
  -> Signal.Reg_spec.t
  -> Hardcaml_hobby_boards_kernel.Vga.Spec.t
  -> Signal.t Hardcaml_hobby_boards_kernel.Nexys_a7_100t.Vga.O.t

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
