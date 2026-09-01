module Sink = struct
  (* Two ways in, because a producer that already holds bytes should not have to make a
     string for them. A renderer writing through jsont hands over the encoder's own slice;
     one holding a finished string hands that. A backend that can only take strings passes
     no [emit_sub] and pays a copy per slice, which is what the mock does. *)
  (* [or_null] rather than an option, and the fallback written at the use site rather than
     built as a closure. A defaulting closure over [emit] is a heap block made on every
     streamed response, and a backend that supplies [emit_sub] should not pay for the one
     it does not use. *)
  type t =
    { emit : string -> unit
    ; emit_sub : (bytes -> int -> int -> unit) or_null
    }

  let v ?emit_sub emit =
    let emit_sub =
      match emit_sub with
      | Some f -> This f
      | None -> Null
    in
    { emit; emit_sub }
  ;;

  let check_range b off len =
    if off < 0 || len < 0 || off > Bytes.length b - len then
      invalid_arg "Proffer.Body.Sink.write_sub: invalid byte range"

  let write t s = t.emit s

  let write_sub t b ~off ~len =
    check_range b off len;
    match t.emit_sub with
    | This f -> if len > 0 then f b off len
    | Null -> if len > 0 then t.emit (Bytes.sub_string b off len)
  ;;
end

module Socket = struct
  type t =
    { read : (bytes -> int -> int -> int) @@ global
    ; write : (bytes -> int -> int -> unit) @@ global
    ; shutdown : (unit -> unit) @@ global
    }

  let check_range name b off len =
    if off < 0 || len < 0 || off > Bytes.length b - len then
      invalid_arg ("Proffer.Body.Socket." ^ name ^ ": invalid byte range")
  ;;

  let v ~read ~write ~shutdown = { read; write; shutdown }

  let read t b ~off ~len =
    check_range "read" b off len;
    if len = 0 then 0 else t.read b off len
  ;;

  let write_sub t b ~off ~len =
    check_range "write_sub" b off len;
    if len > 0 then t.write b off len
  ;;

  let write t s =
    let b = Bytes.unsafe_of_string s in
    if Bytes.length b > 0 then t.write b 0 (Bytes.length b)
  ;;

  let shutdown t = t.shutdown ()
end

type handoff_kind =
  | Tunnel
  | Upgrade of string @@ global

type t =
  | Empty
  | String of string @@ global
  | Delayed of
      { length : int64 option
      ; gen : unit -> string @@ global
      }
  | Stream of
      { length : int64 option
      ; write : Sink.t -> unit @@ global
      ; trailers : Headers.t
      }
  | Handoff of
      { kind : handoff_kind
      ; run : (Socket.t -> unit) @@ global
      }

(* The declared length is available without running a delayed producer, which is required
   when answering HEAD and conditional requests. [exclave_], so the option is built in the
   caller's region. *)
let declared_length (t : t @ local) = exclave_
  match t with
  | Empty -> Some 0L
  | String s -> Some (Int64.of_int (Stdlib.String.length s))
  | Delayed { length; _ } -> length
  | Stream { length; _ } -> length
  | Handoff _ -> None
;;
