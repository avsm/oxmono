open! Base
open! Hardcaml

(** The subset of full AXI4 that is generally used by Xilinx IPs according to
    https://docs.amd.com/v/u/en-US/ug1037-vivado-axi-reference-guide

    Drives data on 3 chanels.

    - write address
    - write data
    - read address

    Receives data on 2 channels

    - write response
    - read data

    Handshakes using [valid] and [ready] are performed on each channel.

    The missing signals from the AXI4 specification are:

    - AWLOCK, AWCACHE, AWPROT, AWQOS, AWREGION, AWUSER
    - WUSER
    - ARLOCK, ARCACHE, ARPROT, ARQOS, ARREGION, ARUSER
    - BUSER
    - RUSER *)

module type Config = sig
  val addr_bits : int
  val data_bits : int
  val id_bits : int
  val burst_length_bits : int
end

module type Master_to_slave = sig
  type 'a t =
    { awaddr : 'a
    ; awburst : 'a
    ; awid : 'a
    ; awlen : 'a
    ; awsize : 'a
    ; awvalid : 'a
    ; wlast : 'a
    ; wvalid : 'a
    ; wdata : 'a
    ; wstrb : 'a
    ; araddr : 'a
    ; arburst : 'a
    ; arid : 'a
    ; arlen : 'a
    ; arsize : 'a
    ; arvalid : 'a
    ; bready : 'a
    ; rready : 'a
    }
  [@@deriving hardcaml]

  include Master_slave_bus_config.S
end

module type Slave_to_master = sig
  type 'a t =
    { awready : 'a
    ; wready : 'a
    ; arready : 'a
    ; bid : 'a
    ; bresp : 'a
    ; bvalid : 'a
    ; rid : 'a
    ; rlast : 'a
    ; rresp : 'a
    ; rvalid : 'a
    ; rdata : 'a
    }
  [@@deriving hardcaml]

  include Master_slave_bus_config.S
end

module type Axi4_xilinx = sig
  module type Config = Config
  module type Master_to_slave = Master_to_slave
  module type Slave_to_master = Slave_to_master

  module Master_to_slave (C : Config) : Master_to_slave
  module Slave_to_master (C : Config) : Slave_to_master
end
