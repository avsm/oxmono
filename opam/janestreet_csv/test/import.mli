open! Core
open! Async
module Time = Time_float_unix

include module type of struct
  include Expect_test_helpers_core
  include Expect_test_helpers_async
  include Csv_tool_lib
end

(** [do_test f] runs [f] in a temporary directory with $PATH updated so "csv" means the
    csv.exe in the tree. It will restore the original $PATH, remove the temporary
    directory, and change back to the current working directory afterward. *)
val do_test : (unit -> unit Deferred.t) -> unit Deferred.t

(** [make_input_csv ?set filename rows] writes [rows] into [filename] with separator
    [sep], which by default is a comma. *)
val make_input_csv : ?sep:char -> string -> string list list -> unit Deferred.t
