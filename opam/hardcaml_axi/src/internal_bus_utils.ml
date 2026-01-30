open Base
open Hardcaml

module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) =
struct
  open Signal

  let bytes_per_word = Master_to_slave.data_bits / 8

  let word_address ~(master : _ Master_to_slave.t) ~size =
    let word_address = drop_bottom master.address ~width:(Int.ceil_log2 bytes_per_word) in
    let addr_bits = if size <= 1 then 1 else Int.ceil_log2 size in
    { With_valid.value = uresize word_address ~width:addr_bits
    ; valid = word_address <:. size
    }
  ;;

  let create_slave
    ~read_latency
    ~write_latency
    ~reg_spec
    ~(master : _ Master_to_slave.t)
    ~read_data
    =
    { Slave_to_master.write_ready =
        pipeline reg_spec ~enable:vdd ~n:write_latency master.write_first
    ; read_ready = pipeline reg_spec ~enable:vdd ~n:read_latency master.read_first
    ; read_data
    }
  ;;
end
