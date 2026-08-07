module Sink = struct
  type t = { emit : string -> unit }

  let v emit = { emit }
  let write t s = t.emit s
end

type t =
  | Empty
  | String of string
  | Delayed of { length : int64 option; gen : unit -> string }
  | Stream of { length : int64 option; write : Sink.t -> unit }

let string s = String s

(* [declared_length t] is the length the body claims without producing it. It
   is what a HEAD or a 304 reports, so a [Delayed] generator is never run. *)
let declared_length = function
  | Empty -> Some 0L
  | String s -> Some (Int64.of_int (Stdlib.String.length s))
  | Delayed { length; _ } -> length
  | Stream { length; _ } -> length
