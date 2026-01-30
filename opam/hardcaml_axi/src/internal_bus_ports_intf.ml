open! Base
open! Hardcaml

module type Master_to_slave = sig
  type 'a t =
    { write_valid : 'a
    ; write_first : 'a
    ; read_valid : 'a
    ; read_first : 'a
    ; address : 'a
    ; write_data : 'a
    ; write_byte_en : 'a
    }
  [@@deriving hardcaml]

  include Master_slave_bus_config.S
end

module type Slave_to_master = sig
  type 'a t =
    { write_ready : 'a
    ; read_ready : 'a
    ; read_data : 'a
    }
  [@@deriving hardcaml]

  include Master_slave_bus_config.S
end

module type Internal_bus_ports = sig
  module type Master_to_slave = Master_to_slave
  module type Slave_to_master = Slave_to_master

  module Master_to_slave (C : Master_slave_bus_config.S) : Master_to_slave
  module Slave_to_master (C : Master_slave_bus_config.S) : Slave_to_master
end
