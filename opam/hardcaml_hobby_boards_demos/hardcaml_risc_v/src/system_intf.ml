open Hardcaml_hobby_boards_kernel
open Hardcaml

module type Config = sig
  val num_harts : int
  val io_controller : Signal.t Uart.Config.t
  val fpga_mac_address : Signal.t
end

module type Memory_config = sig
  val capacity_in_bytes : int
  val ethernet_start_address : int
  val frame_buffer_bytes : int
end
