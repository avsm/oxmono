(** Risc v uart demo - This is a simple risc v design that includes UART interfaces. It
    can be programmed with a simple program binary and data written to memory will be read
    out on the uart tx interface *)

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
