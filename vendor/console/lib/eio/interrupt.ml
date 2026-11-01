(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A signal the program left at its default disposition ends the process
   without running a finaliser. While something holds the terminal, such a
   signal is caught: what holds it gives it back, the default disposition is
   put back and the signal sent again, so the process still ends as the
   signal says. SIGTSTP is caught the same way but stops the process: what
   holds the terminal gives it back around the stop. A disposition the
   program chose is never replaced.

   A handler runs at any safe point, in the middle of a frame's write
   included: what holds the terminal gives it back at its next idle point,
   and the signal is sent again from there. *)

type holder = {
  id : int;
  idle : (unit -> unit) -> unit;
  restore : unit -> unit;
  suspend : (unit -> unit) -> unit;
}

let ends = [ Sys.sigint; Sys.sigterm ]
let holders : holder list Atomic.t = Atomic.make []
let caught = Atomic.make []
let ending = Atomic.make false
let next = Atomic.make 0

(* How long an ending signal waits for an idle point. *)
let grace = 1.

let rec update a f =
  let old = Atomic.get a in
  if not (Atomic.compare_and_set a old (f old)) then update a f

(* The process stops as the default disposition stops it, inside every
   suspension, innermost outermost; [kill] returns once it is continued. In
   a process group that no shell controls, the stop does not take effect
   and the signal stays pending: it is dropped, so that our handler, put
   back, is not called again at once and the suspensions end once. *)
let rec stop signo =
  let stop () =
    Sys.set_signal signo Sys.Signal_default;
    Unix.kill (Unix.getpid ()) signo;
    if List.mem signo (Unix.sigpending ()) then
      Sys.set_signal signo Sys.Signal_ignore;
    Sys.set_signal signo (Sys.Signal_handle stop)
  in
  let around =
    List.fold_left
      (fun k h () -> h.idle (fun () -> h.suspend k))
      stop
      (List.rev (Atomic.get holders))
  in
  try around () with Sys_error _ -> ()

(* Our handler gives way to the default disposition, and one the program
   installed since stays. *)
let rec release () =
  List.iter
    (fun s ->
      match Sys.signal s Sys.Signal_default with
      | Sys.Signal_handle h when h == handle || h == stop -> ()
      | theirs -> Sys.set_signal s theirs)
    (Atomic.exchange caught [])

(* The signal is sent again once, after the last restoration or once
   [grace] has passed, whichever comes first. An ending signal that comes
   before then is the same ending. *)
and handle signo =
  if not (Atomic.exchange ending true) then begin
    let sent = Atomic.make false in
    let send () =
      if not (Atomic.exchange sent true) then begin
        release ();
        Atomic.set ending false;
        Unix.kill (Unix.getpid ()) signo
      end
    in
    let rec close = function
      | [] -> send ()
      | h :: rest ->
          h.idle (fun () ->
              (try h.restore () with Sys_error _ -> ());
              close rest)
    in
    close (Atomic.exchange holders []);
    if not (Atomic.get sent) then
      ignore
        (Thread.create
           (fun () ->
             Thread.delay grace;
             send ())
           ()
          : Thread.t)
  end

let catch () =
  List.iter
    (fun (s, h) ->
      match Sys.signal s (Sys.Signal_handle h) with
      | Sys.Signal_default -> update caught (List.cons s)
      | chosen -> Sys.set_signal s chosen)
    ((Sys.sigtstp, stop) :: List.map (fun s -> (s, handle)) ends)

let protect ?(idle = fun k -> k ()) ?(suspend = fun stop -> stop ()) ~restore f
    =
  let id = Atomic.fetch_and_add next 1 in
  if Atomic.get holders = [] then catch ();
  update holders (List.cons { id; idle; restore; suspend });
  Fun.protect f ~finally:(fun () ->
      update holders (List.filter (fun h -> h.id <> id));
      if Atomic.get holders = [] && not (Atomic.get ending) then release ())
