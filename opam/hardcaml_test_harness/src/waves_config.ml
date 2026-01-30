open Core

module Wavefile_format = struct
  type t =
    | Hardcamlwaveform
    | Vcd
  [@@deriving sexp, string ~capitalize:"snake_case"]

  let to_extension = to_string
end

module Wave_details = struct
  type t =
    { always_include_line_number : bool
    ; extra_cycles_after_test : int
    ; wavefile_format : Wavefile_format.t
    }
  [@@deriving sexp]

  let default =
    { always_include_line_number = false
    ; extra_cycles_after_test = 0
    ; wavefile_format = Hardcamlwaveform
    }
  ;;
end

type t =
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

let no_waves = No_waves

let to_directory ~(here : [%call_pos]) s =
  ();
  let s = if String.is_suffix s ~suffix:"/" then s else s ^ "/" in
  Prefix { directory = s; config = Wave_details.default }
;;

let to_file filename = File { filename; config = Wave_details.default }

let to_test_directory ~(here : [%call_pos]) () =
  ();
  to_directory ~here "./"
;;

let to_env_directory ~(here : [%call_pos]) () =
  ();
  Sys.getenv "WAVES_PREFIX"
  |> Option.value_exn ~message:"WAVES_PREFIX was not set"
  |> to_directory ~here
;;

let to_home_subdirectory ?(subdirectory = "waves/") ~(here : [%call_pos]) () =
  ();
  let home_dir =
    Sys.getenv "HOME"
    |> Option.value_exn
         ~message:"HOME environment variable is not set so we cannot serialize waveforms"
  in
  sprintf "%s/%s/" home_dir subdirectory |> to_directory ~here
;;

let to_home_subdirectory_when ?subdirectory ~(here : [%call_pos]) condition =
  if condition then to_home_subdirectory ?subdirectory ~here () else no_waves
;;

let rewrite ~f t =
  match t with
  | No_waves -> No_waves
  | Prefix { directory; config } -> Prefix { directory; config = f config }
  | File { filename; config } -> File { filename; config = f config }
;;

let with_always_include_line_numbers =
  rewrite ~f:(fun config -> { config with always_include_line_number = true })
;;

let with_extra_cycles_after_test t ~n =
  rewrite ~f:(fun config -> { config with extra_cycles_after_test = n }) t
;;

let as_wavefile_format t ~format =
  rewrite ~f:(fun config -> { config with wavefile_format = format }) t
;;

let load_sexp ~(here : [%call_pos]) filename =
  ();
  Sexp.load_sexp_conv_exn filename t_of_sexp
;;

module Getters = struct
  let extra_cycles_after_test = function
    | No_waves -> 0
    | Prefix { config = { extra_cycles_after_test; _ }; _ }
    | File { config = { extra_cycles_after_test; _ }; _ } -> extra_cycles_after_test
  ;;
end
