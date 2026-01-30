(** AXI4 Interface specifications.

    See Xilinx User Guide [ug1037] for full documentation.

    Summary;

    - AXI4 (full) is for memory-mapped interfaces and allows high throughput bursts of up
      to 256 data transfer cycles with just a single address phase. Xilinx IPs generally
      use a subset of this interface, losing some of the optional signals.

    - AXI4-Lite is a light-weight, single transaction memory-mapped interface. It has a
      small logic footprint and is a simple interface to work with both in design and
      usage.

    - AXI4-Stream removes the requirement for an address phase altogether and allows
      unlimited data burst size. AXI4-Stream interfaces and transfers do not have address
      phases and are therefore not considered to be memory-mapped. *)

module type Bus_config = Master_slave_bus_config.S
module type Stream_config = Stream.Config

module Address_space_decoder = Address_space_decoder
module Axi4_xilinx = Axi4_xilinx
module C_register_interface = C_register_interface
module Internal_bus = Internal_bus
module Lite = Lite
module Lite_controller = Lite_controller
module Lite_ports = Lite_ports
module Register_bank = Register_bank
module Register_mode = Register_mode
module Safe_arbiter = Safe_arbiter
module Slave_with_data = Slave_with_data
module Stream = Stream
