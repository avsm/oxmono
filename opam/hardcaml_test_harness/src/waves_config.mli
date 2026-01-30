open! Core

module Wavefile_format : sig
  type t =
    | Hardcamlwaveform
    | Vcd
  [@@deriving sexp, string]

  val to_extension : t -> string
end

module Wave_details : sig
  type t =
    { always_include_line_number : bool
    ; extra_cycles_after_test : int
    ; wavefile_format : Wavefile_format.t
    }
  [@@deriving sexp]
end

type t = private
  | No_waves
  | Prefix of
      { directory : string
      ; config : Wave_details.t
      }
  | File of
      { filename : string
      ; config : Wave_details.t
      }
[@@deriving sexp]

(** Do not write out waveforms *)
val no_waves : t

(** Write out waveforms to the directory specified *)
val to_directory : here:[%call_pos] -> string -> t

(** Write out waveform to the file specified *)
val to_file : string -> t

(** Write out waveforms to the current working directory of the test *)
val to_test_directory : here:[%call_pos] -> unit -> t

(** Write out waveforms to the directory specified by the WAVES_PREFIX environment
    variable *)
val to_env_directory : here:[%call_pos] -> unit -> t

(** Write out waveforms to $HOME/[subdirectory] where [subdirectory] defaults to "waves" *)
val to_home_subdirectory : ?subdirectory:string -> here:[%call_pos] -> unit -> t

(** to_home_subdirectory if input true, no_waves otherwise. This helps avoid some common
    boilerplate we write in tests *)
val to_home_subdirectory_when : ?subdirectory:string -> here:[%call_pos] -> bool -> t

(** Always include line numbers in the waveform filename even if a test name is specified *)
val with_always_include_line_numbers : t -> t

(** Add some extra cycles to the waveform after a test finishes *)
val with_extra_cycles_after_test : t -> n:int -> t

(** Choose which file format to write the waveform file as *)
val as_wavefile_format : t -> format:Wavefile_format.t -> t

(** Load the settings from a sexp file *)
val load_sexp : here:[%call_pos] -> string -> t

module Getters : sig
  val extra_cycles_after_test : t -> int
end
