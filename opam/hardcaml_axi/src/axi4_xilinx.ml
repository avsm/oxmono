open! Base
open! Hardcaml

include struct
  open Axi4_xilinx_intf

  module type Config = Config
  module type Master_to_slave = Master_to_slave
  module type Slave_to_master = Slave_to_master
end

module Master_to_slave (C : Config) = struct
  type 'a t =
    { awaddr : 'a [@bits C.addr_bits] [@rtlname "AWADDR"]
    ; awburst : 'a [@bits 2] [@rtlname "AWBURST"]
    ; awid : 'a [@bits C.id_bits] [@rtlname "AWID"]
    ; awlen : 'a [@bits C.burst_length_bits] [@rtlname "AWLEN"]
    ; awsize : 'a [@bits 3] [@rtlname "AWSIZE"]
    ; awvalid : 'a [@bits 1] [@rtlname "AWVALID"]
    ; wlast : 'a [@bits 1] [@rtlname "WLAST"]
    ; wvalid : 'a [@bits 1] [@rtlname "WVALID"]
    ; wdata : 'a [@bits C.data_bits] [@rtlname "WDATA"]
    ; wstrb : 'a [@bits C.data_bits / 8] [@rtlname "WSTRB"]
    ; araddr : 'a [@bits C.addr_bits] [@rtlname "ARADDR"]
    ; arburst : 'a [@bits 2] [@rtlname "ARBURST"]
    ; arid : 'a [@bits C.id_bits] [@rtlname "ARID"]
    ; arlen : 'a [@bits C.burst_length_bits] [@rtlname "ARLEN"]
    ; arsize : 'a [@bits 3] [@rtlname "ARSIZE"]
    ; arvalid : 'a [@bits 1] [@rtlname "ARVALID"]
    ; bready : 'a [@bits 1] [@rtlname "BREADY"]
    ; rready : 'a [@bits 1] [@rtlname "RREADY"]
    }
  [@@deriving hardcaml]

  include C
end

module Slave_to_master (C : Config) = struct
  type 'a t =
    { awready : 'a [@bits 1] [@rtlname "AWREADY"]
    ; wready : 'a [@bits 1] [@rtlname "WREADY"]
    ; arready : 'a [@bits 1] [@rtlname "ARREADY"]
    ; bid : 'a [@bits C.id_bits] [@rtlname "BID"]
    ; bresp : 'a [@bits 2] [@rtlname "BRESP"]
    ; bvalid : 'a [@bits 1] [@rtlname "BVALID"]
    ; rid : 'a [@bits C.id_bits] [@rtlname "RID"]
    ; rlast : 'a [@bits 1] [@rtlname "RLAST"]
    ; rresp : 'a [@bits 2] [@rtlname "RRESP"]
    ; rvalid : 'a [@bits 1] [@rtlname "RVALID"]
    ; rdata : 'a [@bits C.data_bits] [@rtlname "RDATA"]
    }
  [@@deriving hardcaml]

  include C
end
