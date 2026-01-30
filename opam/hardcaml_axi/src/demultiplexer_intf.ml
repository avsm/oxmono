open Base
open Hardcaml

(** A [Demultiplexer] connects one master to many slaves. {!Demultiplexer.create} takes
    the master outputs and slave outputs and creates a demultiplexer circuit and returns
    the outputs of the demultiplexer. It does not prevent switching slaves during a
    transaction initiated by the master.

    Each slave space is of size [1 << address_offset] bytes. Indiviual slaves are accessed
    using [slave_address_bits] bits where

    {[
      slave_address_bits = Int.ceil_log2 (List.length slaves)
    ]}
    .

    The bits of [master.address] which are used as the input are then the bits from
    [address_offset] to [address_offset + slave_address_bits - 1].

    For example, if [demux] was created with two slaves and [address_offset] equal to

    [10], then in order to write to address [0xcd] of the first slave, you would set the
    address as follows:

    {[
      demux.master.address := 0xcd
    ]}

    If you instead wanted to write to address [0xaf] of the second slave, you would set
    the address as follows:

    {[
      demux.master.address := 0x4af
    ]}

    which is equivalent to setting the 10th bit high, which is the first index bit of the
    demux:

    {[
      demux.master.address := (0b1 lsl 10) lor 0xaf
    ]} *)
module type S = sig
  module Master_to_slave : Internal_bus_ports.Master_to_slave
  module Slave_to_master : Internal_bus_ports.Slave_to_master

  type t = (Signal.t Slave_to_master.t, Signal.t Master_to_slave.t list) Slave_with_data.t

  val create
    :  ?reg_spec:Signal.Reg_spec.t
    -> ?check_address_out_of_range:bool
         (** For timing, skip checking the top bits of the input address indicating it's
             out of range of any slave. *)
    -> Scope.t
    -> address_offset:int
    -> master:Signal.t Master_to_slave.t
    -> slaves:Signal.t Slave_to_master.t list
    -> (Signal.t Slave_to_master.t, Signal.t Master_to_slave.t list) Slave_with_data.t

  module Builder : sig
    module Slave_instance : sig
      type t

      (** Get the [Master_to_slave.t] interface *)
      val get_master : t -> Signal.t Master_to_slave.t

      (** Get the [Slave_to_master.t] interface *)
      val get_slave : t -> Signal.t Slave_to_master.t

      (** Set the [Slave_to_master.t] interface *)
      val set_slave : t -> Signal.t Slave_to_master.t -> unit
    end

    type t

    (** Create a slave demultiplexer builder object. *)
    val create
      :  Scope.t
      -> log_size_in_bytes:int
      -> reg_spec:Signal.Reg_spec.t
      -> int_master:Signal.t Master_to_slave.t
      -> t

    (** Add a slave. Slaves spaces will be created in the order added. *)
    val add_slave : t -> Slave_instance.t

    (** Construct the slave demultiplexer hardware for the given number of interfaces and
        ensure everything is wired up appropriately. *)
    val complete : t -> Signal.t Slave_to_master.t

    (** Automatically [create] and [complete] the slave demultiplexer and pass to [f]. *)
    val with_slave_demultiplexer
      :  Scope.t
      -> log_size_in_bytes:int
      -> reg_spec:Signal.Reg_spec.t
      -> int_master:Signal.t Master_to_slave.t
      -> f:(t -> 'a)
      -> (Signal.t Slave_to_master.t, 'a) Slave_with_data.t
  end
end

module type Demultiplexer = sig
  module type S = S

  module Make
      (Master_to_slave : Internal_bus_ports.Master_to_slave)
      (Slave_to_master : Internal_bus_ports.Slave_to_master) :
    S
    with module Master_to_slave := Master_to_slave
     and module Slave_to_master := Slave_to_master
end
