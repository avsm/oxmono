open! Base
open! Hardcaml

(** AXI4-lite master interface.

    Drives data on 3 chanels

    - write address
    - write data
    - read address

    Receives data on 2 channels

    - write response
    - read data

    Handshakes using [valid] and [ready] are performed on each channel. *)
module type Master_to_slave = sig
  type 'a t =
    { awaddr : 'a (** Write address. *)
    ; awvalid : 'a (** Write address valid. *)
    ; awprot : 'a (** Write address protection bits. Not generally used. *)
    ; wdata : 'a (** Write data. *)
    ; wstrb : 'a (** Valid write data bytes. *)
    ; wvalid : 'a (** Write data valid. *)
    ; bready : 'a (** Write response ready *)
    ; araddr : 'a (** Read address *)
    ; arvalid : 'a (** Read address valid. *)
    ; arprot : 'a (** Read address protection bits. Not generally used. *)
    ; rready : 'a (** Read data ready. *)
    }
  [@@deriving hardcaml]

  include Master_slave_bus_config.S
end

(** AXI4-lite slave interface.

    Receives read/write addresses and write data and responds with read data and
    read/write error conditions.

    Errors are encoded as

    - 00 OKAY
    - 01 EXOKAY - not used in lite version of protocol
    - 10 SLVERR - slave indicates an error - master generally retries
    - 11 DECERR - AXI interconnect error *)
module type Slave_to_master = sig
  type 'a t =
    { awready : 'a (** Write address ready. *)
    ; wready : 'a (** Write data ready. *)
    ; bresp : 'a (** Write response. *)
    ; bvalid : 'a (** Write response valid. *)
    ; arready : 'a (** Read address ready. *)
    ; rdata : 'a (** Read data *)
    ; rresp : 'a (** Read response *)
    ; rvalid : 'a (** Read data valid. *)
    }
  [@@deriving hardcaml]

  include Master_slave_bus_config.S
end

module type Lite_ports = sig
  module type Master_to_slave = Master_to_slave
  module type Slave_to_master = Slave_to_master

  module Master_to_slave (X : Master_slave_bus_config.S) : Master_to_slave
  module Slave_to_master (X : Master_slave_bus_config.S) : Slave_to_master
end
