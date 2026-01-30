open! Core
open Hardcaml

module Make_monadic (I : Interface.S) (O : Interface.S) = struct
  open Hardcaml_step_testbench.Monadic
  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Functional.Cyclesim.Make (I) (O)

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
    ?input_default
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
          ?input_default
          ?timeout
          ~simulator
          ~testbench:(fun _ -> testbench simulator)
          ()
        |> Option.value_exn ~here ~message:"This test harness timed out")
  ;;

  let run
    ~(here : [%call_pos])
    ?(waves_config : Waves_config.t option)
    ?random_initial_state
    ?trace
    ?handle_multiple_waveforms_with_same_test_name
    ?test_name_prefix
    ?test_name
    ?print_waves_after_test
    ?clock_mode
    ?input_default
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
      ?test_name_prefix
      ?test_name
      ?clock_mode
      ?print_waves_after_test
      ?input_default
      ?timeout
      ~create
      (fun _ -> testbench ())
  ;;
end

module Make_effectful (I : Interface.S) (O : Interface.S) = struct
  open Hardcaml_step_testbench.Effectful
  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Functional.Cyclesim.Make (I) (O)

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
    ?input_default
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
          ?input_default
          ?timeout
          ~simulator
          ~testbench:(fun handler _ -> testbench handler simulator)
          ()
        |> Option.value_exn ~here ~message:"This test harness timed out")
  ;;

  let run
    ~(here : [%call_pos])
    ?(waves_config : Waves_config.t option)
    ?random_initial_state
    ?trace
    ?handle_multiple_waveforms_with_same_test_name
    ?test_name_prefix
    ?test_name
    ?print_waves_after_test
    ?clock_mode
    ?input_default
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
      ?test_name_prefix
      ?test_name
      ?print_waves_after_test
      ?clock_mode
      ?input_default
      ?timeout
      ~create
      (fun handler _simulator -> testbench handler)
  ;;
end
