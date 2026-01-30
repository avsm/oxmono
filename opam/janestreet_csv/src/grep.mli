open Core
open Async

module Target_fields : sig
  type t =
    | All
    | Field_names of String.Set.t

  val param : t Command.Param.t
end

val run
  :  ?separator:char
  -> ?skip_lines:int
  -> invert:bool
  -> always_print_header:bool
  -> grep_fields:Target_fields.t
  -> regexp:Re2.t
  -> Csv_common.Or_file.t
  -> unit Or_error.t Deferred.t
