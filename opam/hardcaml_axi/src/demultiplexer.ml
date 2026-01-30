open Base
open Hardcaml

module type S = Demultiplexer_intf.S

module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) =
struct
  open Signal

  module Core = struct
    type t =
      (Signal.t Slave_to_master.t, Signal.t Master_to_slave.t list) Slave_with_data.t

    let mux_and_register_slaves ?reg_spec ~slave_index ~slaves () =
      let slave =
        match slaves with
        | [ hd ] -> hd
        | _ -> Slave_to_master.Of_signal.mux (Option.value_exn slave_index) slaves
      in
      match reg_spec with
      | None -> slave
      | Some reg_spec ->
        { write_ready = reg reg_spec ~enable:vdd slave.write_ready
        ; read_ready = reg reg_spec ~enable:vdd slave.read_ready
        ; read_data = reg reg_spec ~enable:slave.read_ready slave.read_data
        }
    ;;

    let create
      ?reg_spec
      ?(check_address_out_of_range = true)
      _scope
      ~address_offset
      ~(master : Signal.t Master_to_slave.t)
      ~(slaves : Signal.t Slave_to_master.t list)
      =
      (* get address selection bits *)
      let address = master.address in
      let address_bits = Int.ceil_log2 (List.length slaves) in
      let slave_index =
        match address_bits with
        | 0 -> None
        | _ -> Some address.:[address_offset + address_bits - 1, address_offset]
      in
      let address_out_of_range =
        let top_bits = address_offset + address_bits in
        if width address <= top_bits || not check_address_out_of_range
        then gnd
        else drop_bottom address ~width:top_bits <>:. 0
      in
      (* mux slave to master interfaces *)
      let slave = mux_and_register_slaves ?reg_spec ~slave_index ~slaves () in
      let masters =
        (match slave_index with
         | None -> vdd
         | Some slave_index -> binary_to_onehot slave_index)
        &: ~:(repeat address_out_of_range ~count:(1 lsl address_bits))
        |> bits_lsb
        |> List.map ~f:(fun en ->
          { Master_to_slave.write_valid = master.write_valid &: en
          ; write_first = master.write_first &: en
          ; read_valid = master.read_valid &: en
          ; read_first = master.read_first &: en
          ; address =
              uresize
                (sel_bottom master.address ~width:address_offset)
                ~width:Master_to_slave.addr_bits
          ; write_data = master.write_data
          ; write_byte_en = master.write_byte_en
          })
      in
      (* We create [ceil_pow2 number_of_slaves] masters above by mapping over the onehot
         representation of the [slave_index]. This ensures there are equal numbers of
         slave and master interfaces. *)
      let masters = List.take masters (List.length slaves) in
      { Slave_with_data.slave; data = masters }
    ;;
  end

  module Builder = struct
    module Slave_instance = struct
      type t =
        { mutable master_is_set : bool
        ; master : Signal.t Master_to_slave.t
        ; mutable slave_is_set : bool
        ; slave : Signal.t Slave_to_master.t
        }

      let create () =
        { master_is_set = false
        ; master = Master_to_slave.Of_signal.wires ()
        ; slave_is_set = false
        ; slave = Slave_to_master.Of_signal.wires ()
        }
      ;;

      let get_master t = t.master

      let set_master t master =
        if t.master_is_set
        then raise_s [%message "Slave_demultiplexer - master is already set"]
        else (
          Master_to_slave.iter2 t.master master ~f:Signal.( <-- );
          t.master_is_set <- true)
      ;;

      let get_slave t = t.slave

      let set_slave t slave =
        if t.slave_is_set
        then raise_s [%message "Slave_demultiplexer - slave is already set"]
        else (
          Slave_to_master.iter2 t.slave slave ~f:Signal.( <-- );
          t.slave_is_set <- true)
      ;;
    end

    type t =
      { mutable complete : bool
      ; log_size_in_bytes : int
      ; reg_spec : Reg_spec.t
      ; int_master : Signal.t Master_to_slave.t
      ; mutable slave_instances : Slave_instance.t list
      ; scope : Scope.t
      }

    let create scope ~log_size_in_bytes ~reg_spec ~int_master =
      { complete = false
      ; log_size_in_bytes
      ; reg_spec
      ; int_master
      ; slave_instances = []
      ; scope
      }
    ;;

    (* return the interface for the new slave *)
    let add_slave t =
      let slave = Slave_instance.create () in
      t.slave_instances <- slave :: t.slave_instances;
      (* added backwards *)
      slave
    ;;

    let complete t =
      if t.complete
      then raise_s [%message "Slave_demultiplexer is already complete"]
      else (
        let slave_instances = List.rev t.slave_instances in
        (* put into order added *)
        List.iteri slave_instances ~f:(fun index (slave_instance : Slave_instance.t) ->
          if not slave_instance.slave_is_set
          then
            raise_s
              [%message "Slave_demultiplexer - slave has not been set" (index : int)]);
        t.complete <- true;
        let ({ slave; data = masters } : Core.t) =
          Core.create
            t.scope
            ~reg_spec:t.reg_spec
            ~address_offset:t.log_size_in_bytes
            ~master:t.int_master
            ~slaves:(List.map slave_instances ~f:Slave_instance.get_slave)
        in
        List.iter2_exn slave_instances masters ~f:Slave_instance.set_master;
        slave)
    ;;

    let with_slave_demultiplexer scope ~log_size_in_bytes ~reg_spec ~int_master ~f =
      let t = create scope ~log_size_in_bytes ~reg_spec ~int_master in
      let result = f t in
      let int_slave = complete t in
      { Slave_with_data.slave = int_slave; data = result }
    ;;
  end

  include Core
end
