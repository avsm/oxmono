open! Core
open! Hardcaml

let cyclesim_maybe_wrap_waves ~always_wrap_waveterm ~(wave_mode : Wave_mode.t) simulator =
  let simulator, waves =
    match wave_mode, always_wrap_waveterm with
    | _, true | Hardcamlwaveform, false ->
      let waves, simulator = Hardcaml_waveterm.Waveform.create simulator in
      simulator, Some waves
    | None, false | Vcd _, false -> simulator, None
  in
  let simulator =
    match wave_mode with
    | Vcd { out_channel } -> Vcd.wrap out_channel simulator
    | None | Hardcamlwaveform -> simulator
  in
  simulator, waves
;;

let sanitize_test_name name =
  (* Replace non-alphanumeric characters with underscores and clamp to 128 characters *)
  name
  |> String.strip
  |> String.map ~f:(fun c -> if Char.is_alphanum c then c else '_')
  |> (Fn.flip String.prefix) 128
;;
