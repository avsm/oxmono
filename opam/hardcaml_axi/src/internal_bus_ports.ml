open Base
open! Hardcaml

module type Master_to_slave = Internal_bus_ports_intf.Master_to_slave
module type Slave_to_master = Internal_bus_ports_intf.Slave_to_master

module Master_to_slave (C : Master_slave_bus_config.S) = struct
  type 'a t =
    { write_valid : 'a
    ; write_first : 'a
    ; read_valid : 'a
    ; read_first : 'a
    ; address : 'a [@bits C.addr_bits]
    ; write_data : 'a [@bits C.data_bits]
    ; write_byte_en : 'a [@bits C.data_bits / 8]
    }
  [@@deriving hardcaml]

  include C
end

module Slave_to_master (C : Master_slave_bus_config.S) = struct
  type 'a t =
    { write_ready : 'a
    ; read_ready : 'a
    ; read_data : 'a [@bits C.data_bits]
    }
  [@@deriving hardcaml]

  include C
end
