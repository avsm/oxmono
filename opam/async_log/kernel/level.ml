module Stable = struct
  module V1 = struct
    type t =
      [ `Debug
      | `Info
      | `Error
      ]
    [@@deriving bin_io, compare ~localize, sexp, stable_witness]

    let%expect_test "bin_digest Level.V1" =
      print_endline [%bin_digest: t];
      [%expect {| 62fa833cdabec8a41d614848cd11f858 |}]
    ;;
  end
end

module T = struct
  type t =
    [ `Debug
    | `Info
    | `Error
    ]
  [@@deriving
    bin_io, compare ~localize, enumerate, equal ~localize, globalize, sexp, sexp_grammar]

  let to_string = function
    | `Debug -> "Debug"
    | `Info -> "Info"
    | `Error -> "Error"
  ;;

  let of_string = function
    | "Debug" -> `Debug
    | "Info" -> `Info
    | "Error" -> `Error
    | s -> Core.failwithf "not a valid level %s" s ()
  ;;
end

open! Core
open! Import
include T

let arg =
  Command.Arg_type.enumerated
    ~list_values_in_help:true
    ~case_sensitive:false
    (module T : Command.Enumerable_stringable with type t = t)
;;

(* Ordering of log levels in terms of verbosity. *)
let as_or_more_verbose_than ~log_level ~msg_level =
  match log_level, msg_level with
  | `Error, Some `Error -> true
  | `Error, (None | Some (`Debug | `Info)) -> false
  | `Info, (None | Some (`Info | `Error)) -> true
  | `Info, Some `Debug -> false
  | `Debug, _ -> true
;;
