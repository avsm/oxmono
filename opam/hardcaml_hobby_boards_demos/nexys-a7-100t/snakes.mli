(** Recursive snakes!!! Genius.

    This draws a simple snake pattern across the seven segment display. The width of the
    snake pattern is programmable from 1, 2, 4 and 8 wide. The snake is repeated as
    necessary to fill all eigth seven segment displays . *)

open Hardcaml

val snake_machine : Scope.t -> Signal.Reg_spec.t -> Signal.t -> Signal.t list * Always.t

val create_snakes
  :  scope:Scope.t
  -> spec:Signal.Reg_spec.t
  -> switches:Signal.t
  -> scan_update:int
  -> snake_update:int
  -> Signal.t * Signal.t

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
