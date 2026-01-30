open Async

(** Transfers a csv from the reader to the writer, canonicalising quoting.

    If [add_header] is passed, then it is added as a header. *)
val run
  :  ?add_header:string list
  -> ?sep:char
  -> ?suppress_header:bool
  -> Reader.t
  -> Writer.t
  -> unit Deferred.t
