(** This module enables software to initiate AXI-Lite reads/writes to arbitrary memory
    addresses on the FPGA. This is useful when large memory apertures are exposed to an
    FPGA application, and the FPGA does not want to expose these apertures directly to
    software. An incrementing mode enables large memory regions to be read/written easily
    with a single read/write per transaction, instead of needing to write both an address
    and read/write a data value per transaction. A simple optional locking mechanism is
    included to prevent race conditions associated with the serial nature of setting the
    address and then initiating the data transaction. *)

open Hardcaml

module Make (X : Master_slave_bus_config.S) : sig
  module Lite : module type of Lite.Make (X)
  module Ibus : Internal_bus.S

  module Registers : sig
    type t =
      | ADDRESS (** Address to read/write from *)
      | DATA
      (** Reading triggers an AXI read from the specified ADDRESS and returns the data,
          writing triggers an AXI write of the data to the specified ADDRESS. *)
      | INCREMENTING
      (** After a read/write, if the lsb of this register is set, the ADDRESS field
          automatically increments by [X.data_bits / 8] bytes. *)
      | LOCK
      (** A very simple lock. Read from this register to acquire the lock, reading a 0
          means the lock was acquired, 1 means the lock was not acquired. Write a 0 to the
          lsb of this register to clear the lock. Using this lock is entirely optional,
          and it is up to all the users to be well behaved. *)
    [@@deriving enumerate, sexp_of, variants]

    val offset : t -> int
    val byte_addr : t -> int
  end

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; m_ibus : 'a Ibus.Master_to_slave.t
      ; s_axi : 'a Lite.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { s_ibus : 'a Ibus.Slave_to_master.t
      ; m_axi : 'a Lite.Master_to_slave.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
