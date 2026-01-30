(** Simple Ethernet Tx Demo with two modes that are selected using switch 0:

    1. Loopback (OFF)
    2. Fixed UDP packet (ON)

    Loopback Mode: Any UDP packet received on the ethernet interface is loopbacked to the
    sender.

    Fixed UDP Packet Mode: A fixed data UDP packet is sent out on the ethernet interface
    on a timer.

    Each time a new packet is sent to the ethernet tx module, a counter is incremented and
    this is displayed on the lowest 8 LEDs. The upper 8 LEDs are used to indicate the
    number of rx ethernet errors received *)

open! Hardcaml

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
