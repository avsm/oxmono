module Sink = struct
  (* Two ways in, because a producer that already holds bytes should not have
     to make a string for them. A renderer writing through jsont hands over
     the encoder's own slice; one holding a finished string hands that. A
     backend that can only take strings passes no [emit_sub] and pays a copy
     per slice, which is what the mock does. *)
  (* [or_null] rather than an option, and the fallback written at the use
     site rather than built as a closure. A defaulting closure over [emit] is
     a heap block made on every streamed response, and a backend that supplies
     [emit_sub] should not pay for the one it does not use. *)
  type t = {
    emit : string -> unit;
    emit_sub : (bytes -> int -> int -> unit) or_null;
  }

  let v ?emit_sub emit =
    let emit_sub = match emit_sub with Some f -> This f | None -> Null in
    { emit; emit_sub }

  let write t s = t.emit s

  let write_sub t b ~off ~len =
    match t.emit_sub with
    | This f -> f b off len
    | Null -> t.emit (Bytes.sub_string b off len)
end

(* The payloads carry [global], not the value. What reaches a socket has to be
   readable at global; the block naming which of them it is does not, so a
   body can be built in the frame that responds. *)
type t =
  | Empty
  | String of string @@ global
  | Delayed of { length : int64 option; gen : (unit -> string) @@ global }
  | Stream of { length : int64 option; write : (Sink.t -> unit) @@ global }

(* [declared_length t] is the length the body claims without producing it. It
   is what a HEAD or a 304 reports, so a [Delayed] generator is never run.
   [exclave_], so the option is built in the caller's region. *)
let declared_length (t : t @ local) = exclave_
  match t with
  | Empty -> Some 0L
  | String s -> Some (Int64.of_int (Stdlib.String.length s))
  | Delayed { length; _ } -> length
  | Stream { length; _ } -> length
