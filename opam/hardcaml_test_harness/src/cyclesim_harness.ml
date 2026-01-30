open! Core
open Hardcaml

module Make (I : Interface.S) (O : Interface.S) = struct
  module Sim = Cyclesim.With_interface (I) (O)

  let sim_with_timeout ~here ~f ?timeout sim =
    match timeout with
    | Some timeout -> Cyclesim.with_timeout ~here ~timeout ~f sim
    | None -> f sim
  ;;

  let run_advanced
    ~(here : [%call_pos])
    ?(waves_config : Waves_config.t option)
    ?random_initial_state
    ?trace
    ?handle_multiple_waveforms_with_same_test_name
    ?test_name_prefix
    ?test_name
    ?print_waves_after_test
    ?clock_mode
    ?timeout
    ~create
    testbench
    =
    Harness_base.run
      ~here
      ?waves_config
      ?random_initial_state
      ?handle_multiple_waveforms_with_same_test_name
      ?trace
      ?test_name
      ?test_name_prefix
      ?print_waves_after_test
      ?clock_mode
      ~cycle_fn:Cyclesim.cycle
      ~create:(fun ~always_wrap_waveterm ~wave_mode config scope ->
        let inst = create scope in
        let simulator = Sim.create ~config inst in
        Common.cyclesim_maybe_wrap_waves ~always_wrap_waveterm ~wave_mode simulator)
      (sim_with_timeout ~here ?timeout ~f:testbench)
  ;;

  let run
    ~(here : [%call_pos])
    ?waves_config
    ?random_initial_state
    ?trace
    ?handle_multiple_waveforms_with_same_test_name
    ?test_name_prefix
    ?test_name
    ?print_waves_after_test
    ?clock_mode
    ?timeout
    ~create
    testbench
    =
    run_advanced
      ~here
      ?waves_config
      ?random_initial_state
      ?trace
      ?handle_multiple_waveforms_with_same_test_name
      ?test_name
      ?test_name_prefix
      ?print_waves_after_test
      ?clock_mode
      ?timeout
      ~create
      (fun sim ->
         let inputs = Cyclesim.inputs sim in
         let outputs = Cyclesim.outputs sim in
         testbench ~inputs ~outputs sim)
  ;;
end
