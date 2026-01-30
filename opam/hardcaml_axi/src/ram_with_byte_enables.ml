open Base
open Hardcaml

module type S = Ram_with_byte_enables_intf.S

module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) =
struct
  open Signal

  type t = (Signal.t Slave_to_master.t, unit) Slave_with_data.t [@@deriving sexp_of]

  include Internal_bus_utils.Make (Master_to_slave) (Slave_to_master)

  let create ~reg_spec ~(master : _ Master_to_slave.t) ~size =
    let addr = word_address ~master ~size in
    { Slave_with_data.slave =
        create_slave
          ~read_latency:1
          ~write_latency:1
          ~reg_spec
          ~master
          ~read_data:
            (List.init bytes_per_word ~f:(fun i ->
               ram_rbw
                 size
                 ~write_port:
                   { write_clock = Reg_spec.clock reg_spec
                   ; write_address = addr.value
                   ; write_enable =
                       master.write_first &: master.write_byte_en.:(i) &: addr.valid
                   ; write_data = master.write_data.:[(i * 8) + 7, i * 8]
                   }
                 ~read_port:
                   { read_clock = Reg_spec.clock reg_spec
                   ; read_address = addr.value
                   ; read_enable = master.read_first &: addr.valid
                   })
             |> concat_lsb)
    ; data = ()
    }
  ;;
end
