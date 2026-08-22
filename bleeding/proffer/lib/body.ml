module Sink = struct
  (* Two ways in, because a producer that already holds bytes should not have
     to make a string for them. A renderer writing through jsont hands over
     the encoder's own slice; one holding a finished string hands that. A
     backend that can only take strings passes no [emit_sub] and pays a copy
     per slice, which is what the mock does. *)
  type t = {
    emit : string -> unit;
    emit_sub : bytes -> int -> int -> unit;
  }

  let v ?emit_sub emit =
    let emit_sub =
      match emit_sub with
      | Some f -> f
      | None -> fun b off len -> emit (Bytes.sub_string b off len)
    in
    { emit; emit_sub }

  let write t s = t.emit s
  let write_sub t b ~off ~len = t.emit_sub b off len
end

type t =
  | Empty
  | String of string
  | Delayed of { length : int64 option; gen : unit -> string }
  | Stream of { length : int64 option; write : Sink.t -> unit }

(* [declared_length t] is the length the body claims without producing it. It
   is what a HEAD or a 304 reports, so a [Delayed] generator is never run. *)
let declared_length = function
  | Empty -> Some 0L
  | String s -> Some (Int64.of_int (Stdlib.String.length s))
  | Delayed { length; _ } -> length
  | Stream { length; _ } -> length
