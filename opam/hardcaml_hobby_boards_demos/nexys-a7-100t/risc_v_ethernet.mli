(** Risc v ethernet demo - This is a risc v design that includes UART and Ethernet
    interfaces. It can be programmed with a binary that writes UDP packets to memory and
    they will be sent out of the FPGA with the UART and Ethernet Tx modules. *)

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
