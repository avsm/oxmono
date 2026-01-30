open Core
open Hardcaml
open Hardcaml_waveterm

type 'a with_test_config =
  here:[%call_pos]
  -> ?waves_config:Waves_config.t
  -> ?random_initial_state:[ `All | `Mems | `Regs | `None ]
  -> ?trace:[ `All_named | `Everything | `Ports_only ]
  -> ?handle_multiple_waveforms_with_same_test_name:[ `Save_all | `Save_last_only ]
  -> ?test_name_prefix:string
  -> ?test_name:string
  -> ?print_waves_after_test:(Waveform.t -> unit)
  -> ?clock_mode:Cyclesim.Config.Clock_mode.t
  -> 'a

(* Some tests (such as quickchecks) will invoke the harness more than once within a single
   process. If this happens, append a unique identifier to the end of each file to avoid
   overriding the waveforms. *)
let global_already_used_filenames = Hashtbl.create (module String)

let run
  (type sim)
  ~(here : [%call_pos])
  ?(waves_config : Waves_config.t = Waves_config.no_waves)
  ?(random_initial_state = `None)
  ?(trace = `Ports_only)
  ?(handle_multiple_waveforms_with_same_test_name = `Save_all)
  ?test_name_prefix
  ?test_name
  ?(print_waves_after_test : (Waveform.t -> unit) option)
  ?(clock_mode = Cyclesim.Config.Clock_mode.All_one_domain)
  ~(cycle_fn : sim -> unit)
  ~(create :
      always_wrap_waveterm:bool
      -> wave_mode:Wave_mode.t
      -> Cyclesim.Config.t
      -> Scope.t
      -> sim * Waveform.t option)
  testbench_fn
  =
  let serialize_waves_to =
    let of_prefix ~always_include_line_number ~format prefix =
      let filename =
        String.split ~on:'/' here.pos_fname |> List.last_exn |> Common.sanitize_test_name
      in
      let line_number = Int.to_string here.pos_lnum in
      let currently_running_expect_test =
        if Expect_test_helpers_base.am_running_expect_test ()
        then Expect_test_helpers_base.current_expect_test_name_exn ()
        else None
      in
      (* Prioritize the test name passed by the user if there is one, then fall back to
         trying to automatically infer it from the running expect-test. If all else fails,
         use just the filename + line number. *)
      let test_name = Option.first_some test_name currently_running_expect_test in
      let test_identifier =
        Option.value_map test_name ~default:line_number ~f:(fun name ->
          if always_include_line_number then line_number ^ "_" ^ name else name)
        |> Common.sanitize_test_name
      in
      let basename =
        prefix
        ^ filename
        ^ "_"
        ^ (match test_name_prefix with
           | Some t -> [%string "%{t#String}_"]
           | None -> "")
        ^ test_identifier
      in
      (* Handle repetitions in basename if [Save_all] is set *)
      let suffix =
        match handle_multiple_waveforms_with_same_test_name with
        | `Save_all ->
          let index =
            Hashtbl.update_and_return global_already_used_filenames basename ~f:(function
              | None -> 0
              | Some i -> i + 1)
          in
          (match index with
           | 0 -> ""
           | i -> "_" ^ Int.to_string i)
        | `Save_last_only -> ""
      in
      basename ^ suffix ^ "." ^ Waves_config.Wavefile_format.to_extension format
    in
    match waves_config with
    | Waves_config.No_waves -> None
    | Prefix { directory; config = { always_include_line_number; wavefile_format; _ } } ->
      of_prefix ~always_include_line_number ~format:wavefile_format directory |> Some
    | File { filename; _ } -> Some filename
  in
  let sim_config =
    let base =
      Cyclesim.Config.trace
        (if Option.is_some serialize_waves_to then `All_named else trace)
    in
    match random_initial_state with
    | `None -> base
    | `All ->
      Cyclesim.Config.add_random_initialization
        base
        (Cyclesim.Config.Random_initializer.create
           Cyclesim.Config.Random_initializer.randomize_all)
    | `Regs ->
      Cyclesim.Config.add_random_initialization
        base
        (Cyclesim.Config.Random_initializer.create
           Cyclesim.Config.Random_initializer.randomize_regs)
    | `Mems ->
      Cyclesim.Config.add_random_initialization
        base
        (Cyclesim.Config.Random_initializer.create
           Cyclesim.Config.Random_initializer.randomize_memories)
  in
  let sim_config = { sim_config with clock_mode } in
  let wave_mode, save_fn =
    match waves_config with
    | No_waves -> Wave_mode.None, Fn.ignore
    | Prefix { config = { wavefile_format = Hardcamlwaveform; _ }; _ }
    | File { config = { wavefile_format = Hardcamlwaveform; _ }; _ } ->
      (* This is safe because [serialize_waves_to] is only None in the [No_waves] case *)
      let serialize_waves_to = Option.value_exn serialize_waves_to in
      let save_hardcamlwaveform =
        Option.iter ~f:(fun waves ->
          printf "Saved waves to %s\n" serialize_waves_to;
          Hardcaml_waveterm.Waveform.Serialize.marshall waves serialize_waves_to)
      in
      Wave_mode.Hardcamlwaveform, save_hardcamlwaveform
    | Prefix { config = { wavefile_format = Vcd; _ }; _ }
    | File { config = { wavefile_format = Vcd; _ }; _ } ->
      (* This is safe because [serialize_waves_to] is only None in the [No_waves] case *)
      let serialize_waves_to = Option.value_exn serialize_waves_to in
      let out_channel = Out_channel.create serialize_waves_to in
      let cleanup_fn _ =
        printf "Saved waves to %s\n" serialize_waves_to;
        Out_channel.close out_channel
      in
      Wave_mode.Vcd { out_channel }, cleanup_fn
  in
  let sim, waves =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    create
      ~always_wrap_waveterm:(Option.is_some print_waves_after_test)
      ~wave_mode
      sim_config
      scope
  in
  Core.protect
    ~f:(fun () -> testbench_fn sim)
    ~finally:(fun () ->
      (* Optionally step the clock a few more cycles to flush out the waveforms of the
         cycle that caused an exception to be raised. Extra cycles defaults to zero, so
         this only happens if explicitly requested. *)
      for _ = 1 to Waves_config.Getters.extra_cycles_after_test waves_config do
        cycle_fn sim
      done;
      save_fn waves;
      Option.iter print_waves_after_test ~f:(fun fn -> fn (Option.value_exn waves)))
;;
