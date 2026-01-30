(** Treat the 16 input switchs as a 16 bit binary value.

    Convert to binary coded decimal using the double dabble algorithm and show it on the
    seven segment display. *)

open Hardcaml

val decimal_display : spec:Signal.Reg_spec.t -> switches:Signal.t -> Signal.t * Signal.t
val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
