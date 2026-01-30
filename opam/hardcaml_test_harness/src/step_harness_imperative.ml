open! Core
open Hardcaml

module Make_monadic (I : Interface.S) (O : Interface.S) = struct
  open Hardcaml_step_testbench.Monadic
  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Imperative.Cyclesim

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
      ?trace
      ?handle_multiple_waveforms_with_same_test_name
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?clock_mode
      ~cycle_fn:Cyclesim.cycle
      ~create:(fun ~always_wrap_waveterm ~wave_mode config scope ->
        let inst = create scope in
        let simulator = Sim.create ~config inst in
        Common.cyclesim_maybe_wrap_waves ~always_wrap_waveterm ~wave_mode simulator)
      (fun simulator ->
        Step.run_with_timeout
          ?timeout
          ~simulator
          ~testbench:(fun _ -> testbench simulator)
          ()
        |> Option.value_exn ~here ~message:"This test harness timed out")
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
      ?handle_multiple_waveforms_with_same_test_name
      ?trace
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?clock_mode
      ?timeout
      ~create
      (fun simulator ->
         let inputs = Cyclesim.inputs simulator in
         let outputs = Cyclesim.outputs simulator in
         testbench ~inputs ~outputs)
  ;;
end

module Make_effectful (I : Interface.S) (O : Interface.S) = struct
  open Hardcaml_step_testbench.Effectful
  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Imperative.Cyclesim

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
      ?trace
      ?handle_multiple_waveforms_with_same_test_name
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?clock_mode
      ~cycle_fn:Cyclesim.cycle
      ~create:(fun ~always_wrap_waveterm ~wave_mode config scope ->
        let inst = create scope in
        let simulator = Sim.create ~config inst in
        Common.cyclesim_maybe_wrap_waves ~always_wrap_waveterm ~wave_mode simulator)
      (fun simulator ->
        Step.run_with_timeout
          ?timeout
          ~simulator
          ~testbench:(fun handler -> testbench handler simulator)
          ()
        |> Option.value_exn ~here ~message:"This test harness timed out")
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
      ?handle_multiple_waveforms_with_same_test_name
      ?trace
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?clock_mode
      ?timeout
      ~create
      (fun handler simulator ->
         let inputs = Cyclesim.inputs simulator in
         let outputs = Cyclesim.outputs simulator in
         testbench handler ~inputs ~outputs)
  ;;
end
