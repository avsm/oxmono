open Base
open! Hardcaml

module type Master_to_slave = Lite_ports_intf.Master_to_slave
module type Slave_to_master = Lite_ports_intf.Slave_to_master

module Master_to_slave (C : Master_slave_bus_config.S) = struct
  type 'a t =
    { awaddr : 'a [@bits C.addr_bits]
    ; awvalid : 'a
    ; awprot : 'a [@bits 3]
    ; wdata : 'a [@bits C.data_bits]
    ; wstrb : 'a [@bits C.data_bits / 8]
    ; wvalid : 'a
    ; bready : 'a
    ; araddr : 'a [@bits C.addr_bits]
    ; arvalid : 'a
    ; arprot : 'a [@bits 3]
    ; rready : 'a
    }
  [@@deriving hardcaml]

  include C
end

module Slave_to_master (C : Master_slave_bus_config.S) = struct
  type 'a t =
    { awready : 'a
    ; wready : 'a
    ; bresp : 'a [@bits 2]
    ; bvalid : 'a
    ; arready : 'a
    ; rdata : 'a [@bits C.data_bits]
    ; rresp : 'a [@bits 2]
    ; rvalid : 'a
    }
  [@@deriving hardcaml]

  include C
end
