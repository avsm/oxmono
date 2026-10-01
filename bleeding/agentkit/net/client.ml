(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Buf_read = Eio.Buf_read
module Buf_write = Eio.Buf_write

(* How long numptyd has to greet. It asks a curl for its version and nothing
   else, so a greeting that is a spawn and a fork away is late for a reason
   worth reporting rather than for one worth waiting out. *)
let hello_window = 30.

(* How long a killed numptyd has to go before it is killed harder. *)
let grace = 2.

(* How long a stopped numptyd has to exit of its own accord. *)
let farewell = 5.

(* How long the reason for a death waits for the last of numptyd's standard
   error. The bytes that explain a death are written in the moment before it,
   so a tail read without this wait loses exactly them. *)
let settle = 2.

(* What an operation is given before numptyd is killed for it. A fetch and a
   head are one transfer, which curl bounds below this on its own, so reaching
   this bound is a numptyd that has stopped rather than a slow server. A run is
   somebody else's program and is given what okit gives a build. *)
let short = 60.
let long = 300.

let bound : Proto.op -> float = function
  | Proto.Fetch _ | Proto.Head _ -> short
  | Proto.Run _ -> long

(* The last of numptyd's standard error, kept because a process that dies
   explains itself there and nowhere else. *)
module Tail = struct
  let size = 4096

  type t = { buf : Bytes.t; mutable len : int; mutable next : int }

  let create () = { buf = Bytes.create size; len = 0; next = 0 }

  let add t s =
    let n = String.length s in
    let off = if n > size then n - size else 0 in
    let n = n - off in
    let first = min n (size - t.next) in
    Bytes.blit_string s off t.buf t.next first;
    Bytes.blit_string s (off + first) t.buf 0 (n - first);
    t.next <- (t.next + n) mod size;
    t.len <- min size (t.len + n)

  let contents t =
    if t.len < size then Bytes.sub_string t.buf 0 t.len
    else
      Bytes.sub_string t.buf t.next (size - t.next)
      ^ Bytes.sub_string t.buf 0 t.next
end

(* A protocol fault carries the line that caused it, and a line may be a whole
   document. The same rule numptyd applies to what it reports. *)
let clip n s =
  let s =
    String.map (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c) s
  in
  if String.length s <= n then s
  else
    let dropped = Printf.sprintf "… (%d bytes)" (String.length s) in
    String.sub s 0 (max 0 (n - String.length dropped)) ^ dropped

(* A callback belongs to a caller and runs on the reader's fiber. One that
   raises must not take the session with it. *)
let guarded f s =
  try f s with Eio.Cancel.Cancelled _ as e -> raise e | _ -> ()

let duration s = if s = 1. then "1 second" else Printf.sprintf "%g seconds" s

type pending = {
  id : int;  (** The call in flight, which its traces and its result carry. *)
  on_trace : string -> unit;
  reply : (string, string) result Eio.Promise.u;
}

type conn = {
  child : [ `Generic | `Unix ] Eio.Process.ty Eio.Resource.t;
  stdin : [ Eio.Flow.sink_ty | `Close ] Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  tail : Tail.t;
  drained : unit Eio.Promise.t;
      (** Resolved when numptyd's standard error reaches end of file, which is
          when its tail is complete. *)
  trace : string -> unit;
  greeting : (Proto.hello, string) result Eio.Promise.t;
  greeter : (Proto.hello, string) result Eio.Promise.u;
  lock : Eio.Mutex.t;
  mutable ids : int;
  mutable pending : pending option;
  mutable reason : string option;
      (** Why the session ended, and the answer to every call from then on. *)
  mutable broke : bool;
      (** Whether that end was a fault rather than a stop the caller asked for.
          The daemon ends the run on a fault and not on its own stop. *)
}

type t = { conn : conn; hello : Proto.hello }

let terminate c =
  Eio.Cancel.protect @@ fun () ->
  try
    Eio.Process.signal c.child Sys.sigterm;
    match
      Eio.Time.with_timeout c.clock grace (fun () ->
          Ok (Eio.Process.await c.child))
    with
    | Ok _ -> ()
    | Error `Timeout ->
        Eio.Process.signal c.child Sys.sigkill;
        ignore
          (Eio.Time.with_timeout c.clock grace (fun () ->
               Ok (Eio.Process.await c.child)))
  with Eio.Io _ | Invalid_argument _ -> ()

let with_tail c msg =
  Eio.Cancel.protect (fun () ->
      ignore
        (Eio.Time.with_timeout c.clock settle (fun () ->
             Ok (Eio.Promise.await c.drained))));
  match String.trim (Tail.contents c.tail) with
  | "" -> msg
  | out -> Printf.sprintf "%s\nnumptyd's standard error ended with:\n%s" msg out

(* [release c reason] hands [reason] to everything that is waiting on the
   session, and is [reason]. Every path that ends a session goes through it,
   including one that finds the session already ended: the fiber that ended it
   need not have known of a call in flight, and a waiter left unresolved is the
   freeze this whole arrangement exists to prevent. *)
let release c reason =
  (match c.pending with
  | Some p ->
      c.pending <- None;
      ignore (Eio.Promise.try_resolve p.reply (Error reason))
  | None -> ());
  ignore (Eio.Promise.try_resolve c.greeter (Error reason));
  reason

(* [die c reason] ends the session for good and is the reason it gives from now
   on. It is called from the reader fiber on a fault and from a calling fiber on
   a timeout or a cancellation, so the reason is recorded before the kill, which
   suspends: a call arriving in that window is refused with the bare reason
   rather than let through to a process that is being shot. *)
let die c reason =
  match c.reason with
  | Some recorded -> release c recorded
  | None ->
      c.reason <- Some reason;
      c.broke <- true;
      terminate c;
      let reason = with_tail c reason in
      c.reason <- Some reason;
      release c reason

(* The one fiber that owns numptyd's output, from the spawn to the death.
   Nothing in numptyd waits for a reader, so a peer that is slow to start
   reading fills the pipe and blocks numptyd part way through a call. *)
let reader c source =
  let r = Buf_read.of_flow source ~max_size:Agentkit.Line.max_line in
  let fault what = ignore (die c ("numpty's network child " ^ what)) in
  let rec go () =
    match Proto.read_to_client r with
    | `Msg (Proto.Hello h) ->
        if Eio.Promise.try_resolve c.greeter (Ok h) then go ()
        else fault "greeted twice."
    | `Msg (Proto.Trace { id = None; line }) ->
        c.trace line;
        go ()
    | `Msg (Proto.Trace { id = Some id; line }) -> (
        match c.pending with
        | Some p when p.id = id ->
            p.on_trace line;
            go ()
        | _ ->
            fault
              (Printf.sprintf "traced call %d, which is not the call in flight."
                 id))
    | `Msg (Proto.Result { id; output }) -> (
        match c.pending with
        | Some p when p.id = id ->
            c.pending <- None;
            ignore (Eio.Promise.try_resolve p.reply (Ok output));
            go ()
        | _ ->
            fault
              (Printf.sprintf
                 "answered call %d, which is not the call in flight." id))
    | `Bad line -> fault ("wrote a line that is not a message: " ^ clip 200 line)
    | `Eof ->
        if Eio.Promise.is_resolved c.greeting then fault "exited."
        else fault "exited before it greeted."
  in
  try go () with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | e -> fault ("output could not be read: " ^ Printexc.to_string e)

(* Each message is written and flushed on its own. A call left in a buffer is a
   call numptyd has not been asked to make. *)
let send c msg =
  Buf_write.with_flow c.stdin (fun w -> Proto.write_to_server w msg)

let start ~sw ~proc ~clock ~trace ~argv =
  let in_r, in_w = Eio_unix.pipe sw in
  let out_r, out_w = Eio_unix.pipe sw in
  let err_r, err_w = Eio_unix.pipe sw in
  let shut f =
    try Eio.Flow.close f with Eio.Io _ | Invalid_argument _ -> ()
  in
  match
    Eio.Process.spawn ~sw proc ~stdin:in_r ~stdout:out_w ~stderr:err_w argv
  with
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e ->
      shut in_r;
      shut in_w;
      shut out_r;
      shut out_w;
      shut err_r;
      shut err_w;
      Error
        (Printf.sprintf "numpty's network child could not be started: %s: %s"
           (String.concat " " argv) (Printexc.to_string e))
  | child -> (
      (* This end of each of the child's own sides, closed at once. Left open,
         numptyd's standard input would never reach the end of file it reads as
         a shutdown, and numptyd's death would never show as one here. *)
      shut in_r;
      shut out_w;
      shut err_w;
      let greeting, greeter = Eio.Promise.create () in
      let drained, drain_done = Eio.Promise.create () in
      let c =
        {
          child;
          stdin = (in_w :> [ Eio.Flow.sink_ty | `Close ] Eio.Resource.t);
          clock :> float Eio.Time.clock_ty Eio.Resource.t;
          tail = Tail.create ();
          drained;
          trace = guarded trace;
          greeting;
          greeter;
          lock = Eio.Mutex.create ();
          ids = 0;
          pending = None;
          reason = None;
          broke = false;
        }
      in
      (* Daemons, so that a session left running does not keep the switch from
         finishing, and both are cancelled when it is released. *)
      Eio.Fiber.fork_daemon ~sw (fun () ->
          let r = Buf_read.of_flow err_r ~max_size:0x10000 in
          (try
             while not (Buf_read.at_end_of_input r) do
               Tail.add c.tail (Buf_read.take (Buf_read.buffered_bytes r) r)
             done
           with End_of_file | Eio.Io _ -> ());
          Eio.Promise.resolve drain_done ();
          `Stop_daemon);
      Eio.Fiber.fork_daemon ~sw (fun () ->
          reader c out_r;
          `Stop_daemon);
      match
        Eio.Time.with_timeout clock hello_window (fun () ->
            Ok (Eio.Promise.await greeting))
      with
      | Ok (Ok hello) -> Ok { conn = c; hello }
      | Ok (Error e) -> Error e
      | Error `Timeout ->
          Error
            (die c
               (Printf.sprintf "numpty's network child did not greet within %s."
                  (duration hello_window))))

let hello t = t.hello
let trace t line = t.conn.trace line
let alive t = t.conn.reason = None
let fault t = if t.conn.broke then t.conn.reason else None

let call t ?timeout op ~on_trace =
  let c = t.conn in
  let seconds = match timeout with None -> bound op | Some s -> s in
  (* Read before the lock is taken as well as under it. A fiber ending the
     session holds the lock while it kills numptyd, and a caller already told
     the session is dead has nothing to wait for. *)
  match c.reason with
  | Some reason -> Error reason
  | None -> (
      (* Read-only rather than read-write: an exception here, which is a
         cancellation, must unlock the mutex and not disable it. A caller who
         gave up on one call would otherwise leave every later call raising
         where this interface promises an error. *)
      Eio.Mutex.use_ro c.lock
      @@ fun () ->
      match c.reason with
      | Some reason -> Error reason
      | None -> (
          c.ids <- c.ids + 1;
          let id = c.ids in
          let reply, resolver = Eio.Promise.create () in
          c.pending <-
            Some { id; on_trace = guarded on_trace; reply = resolver };
          (* Leaving with the call still in flight is what a cancellation does,
             and a call cannot be taken back. numptyd answers a question nobody
             is waiting for, and the next call would be matched against that
             answer, so the session ends here rather than going out of step. *)
          Fun.protect ~finally:(fun () ->
              match c.pending with
              | Some p when p.id = id ->
                  c.pending <- None;
                  ignore
                    (die c
                       "numpty's network call was cancelled, and a call in \
                        flight cannot be taken back.")
              | _ -> ())
          @@ fun () ->
          (try send c (Proto.Call { id; op }) with
          | Eio.Cancel.Cancelled _ as e -> raise e
          | e ->
              ignore
                (die c
                   ("numpty's network child could not be given a call: "
                  ^ Printexc.to_string e)));
          match
            Eio.Time.with_timeout c.clock seconds (fun () ->
                Ok (Eio.Promise.await reply))
          with
          | Ok answer -> answer
          | Error `Timeout ->
              Error
                (die c
                   (Printf.sprintf
                      "numpty's network child did not answer %s within %s and \
                       was killed."
                      (Proto.name op) (duration seconds)))))

let stop t =
  let c = t.conn in
  match c.reason with
  | Some _ -> ()
  | None -> (
      let reason = "numpty's network child was stopped." in
      c.reason <- Some reason;
      (* A call in flight is answered with the stop and at once. Waiting for it
         would be waiting on the numptyd that is about to be closed. *)
      ignore (release c reason);
      (* Written under the lock, as every other message is. The caller released
         a moment ago is between its promise and its return, and it is the only
         fiber that can still be holding it. *)
      Eio.Mutex.use_ro c.lock (fun () ->
          (* A write that fails here is a numptyd that has already gone, which
             is what the write was asking for. *)
          try send c Proto.Shutdown with
          | Eio.Cancel.Cancelled _ as e -> raise e
          | _ -> ());
      (try Eio.Flow.close c.stdin with Eio.Io _ | Invalid_argument _ -> ());
      match
        Eio.Time.with_timeout c.clock farewell (fun () ->
            Ok (Eio.Process.await c.child))
      with
      | Ok _ -> ()
      | Error `Timeout -> terminate c)
