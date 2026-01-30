open! Core
open! Hardcaml

(** If [wave_mode] specified, wrap the provided Cyclesim backed simulation with waveforms;
    otherwise return it unchanged. *)
val cyclesim_maybe_wrap_waves
  :  always_wrap_waveterm:bool
  -> wave_mode:Wave_mode.t
  -> ('a, 'b) Cyclesim.t
  -> ('a, 'b) Cyclesim.t * Hardcaml_waveterm.Waveform.t option

(** Sanitize a test name to make it safe to use as a filename *)
val sanitize_test_name : string -> string
