open! Base
open! Hardcaml
include Internal_bus_intf

module Make (C : Master_slave_bus_config.S) = struct
  module Master_to_slave = Internal_bus_ports.Master_to_slave (C)
  module Slave_to_master = Internal_bus_ports.Slave_to_master (C)
  module Demultiplexer = Demultiplexer.Make (Master_to_slave) (Slave_to_master)
  module Register = Internal_bus_register.Make (Master_to_slave) (Slave_to_master)

  module Ram_with_byte_enables =
    Ram_with_byte_enables.Make (Master_to_slave) (Slave_to_master)

  module Register_bank = Register_bank.Make (Master_to_slave) (Slave_to_master)
end

module W32 = Make (struct
    let addr_bits = 32
    let data_bits = 32
  end)
