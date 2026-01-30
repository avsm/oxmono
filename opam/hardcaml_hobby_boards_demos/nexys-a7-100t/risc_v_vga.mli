(** Risc v vga demo - This is a risc v design that includes UART, Ethernet and VGA
    interfaces. It will receive 64 x 48 framebuffer data via UDP packets, scale it to
    640x480 and display it on a VGA monitor *)

val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
