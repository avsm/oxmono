(** Simple Ethernet Rx Demo. Decodes incoming ethernet traffic and increments a counter
    when a UDP packet is received. This counter is displayed on the lower 8 LEDs with the
    upper 8 LEDs used to display the rx error count *)

open! Hardcaml

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
