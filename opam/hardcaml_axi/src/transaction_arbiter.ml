open Base
open Hardcaml
open Signal
include Transaction_arbiter_intf

module Make (Config : Config.S) = struct
  open Config

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; primary_source : 'a Source.t
      ; secondary_source : 'a Source.t
      ; dest : 'a Dest.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { source : 'a Source.t
      ; primary_dest : 'a Dest.t
      ; secondary_dest : 'a Dest.t
      ; first : 'a
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Primary
      | Secondary_requested
      | Secondary
    [@@deriving compare ~localize, enumerate, sexp]
  end

  let create scope { I.clocking; primary_source; secondary_source; dest } =
    let spec = Types.Clocking.to_spec clocking in
    let%hw.Always.State_machine sm =
      Always.State_machine.create (module States) ~auto_wave_format:true spec
    in
    let%hw_var primary_first = Always.Variable.reg ~width:1 spec in
    let%hw_var secondary_first = Always.Variable.reg ~width:1 spec in
    let%hw_var in_primary_transaction = Always.Variable.reg ~width:1 spec in
    let%hw finished_or_not_in_primary =
      finished_transaction primary_source dest |: ~:(in_primary_transaction.value)
    in
    Always.(
      compile
        [ secondary_first <-- gnd
        ; primary_first <-- gnd
        ; if_
            (finished_transaction primary_source dest
             &: (sm.is Primary |: sm.is Secondary_requested))
            [ in_primary_transaction <-- gnd ]
          @@ elif (start_transaction primary_source) [ in_primary_transaction <-- vdd ] []
        ; sm.switch
            [ ( Primary
              , [ when_
                    (start_transaction secondary_source)
                    [ if_
                        (finished_or_not_in_primary
                         &: ~:(start_transaction primary_source)
                            (* when racing, we have to stay with primary as dest has
                               already propagated *)
                        )
                        [ sm.set_next Secondary; secondary_first <-- vdd ]
                        [ sm.set_next Secondary_requested ]
                    ]
                ] )
            ; ( Secondary_requested
              , [ when_
                    finished_or_not_in_primary
                    [ sm.set_next Secondary; secondary_first <-- vdd ]
                ] )
            ; ( Secondary
              , [ when_
                    (finished_transaction secondary_source dest)
                    [ sm.set_next Primary; primary_first <-- vdd ]
                ] )
            ]
        ]);
    let source =
      Source.Of_signal.mux2 (sm.is Secondary) secondary_source primary_source
    in
    let primary_dest =
      Dest.Of_signal.mux2 (sm.is Secondary) (Dest.Of_signal.zero ()) dest
    in
    let secondary_dest =
      Dest.Of_signal.mux2 (sm.is Secondary) dest (Dest.Of_signal.zero ())
    in
    let first = mux2 (sm.is Secondary) secondary_first.value primary_first.value in
    { O.source; primary_dest; secondary_dest; first }
  ;;

  let hierarchical scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"secondary_ibus_arbiter" create i
  ;;
end
