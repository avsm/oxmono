open! Core
open! Hardcaml
open Signal

module Make (Ibus : Internal_bus.S) = struct
  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; primary_master : 'a Ibus.Master_to_slave.t
      ; secondary_master : 'a Ibus.Master_to_slave.t
      ; slave : 'a Ibus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { master : 'a Ibus.Master_to_slave.t
      ; primary_slave : 'a Ibus.Slave_to_master.t
      ; secondary_slave : 'a Ibus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module Transaction_arbiter = Transaction_arbiter.Make (struct
      module Source = Ibus.Master_to_slave
      module Dest = Ibus.Slave_to_master

      (* Note that we don't need to check [*_first] here to detect start of a
         transaction - but we do pass it through correctly later *)
      let start_transaction (src : _ Source.t) = src.write_valid |: src.read_valid
      let finished_transaction (src : _ Source.t) _ = ~:(src.write_valid |: src.read_valid)
    end)

  let create scope { I.clocking; secondary_master; primary_master; slave } =
    let%tydi { source; primary_dest; secondary_dest; first } =
      Transaction_arbiter.hierarchical
        scope
        { clocking
        ; primary_source = primary_master
        ; secondary_source = secondary_master
        ; dest = slave
        }
    in
    { O.master =
        { source with
          write_first = source.write_valid &: first |: source.write_first
        ; read_first = source.read_valid &: first |: source.read_first
        }
    ; primary_slave = primary_dest
    ; secondary_slave = secondary_dest
    }
  ;;

  let hierarchical ?instance scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ?instance ~scope ~name:"ibus_arbiter" create i
  ;;
end
