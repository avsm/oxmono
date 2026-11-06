(* SPDX-License-Identifier: ISC *)

type t = {
  release : unit -> unit;
  acquire : unit -> unit;
  mutable waiting : int;
  mutable held : bool;
}

let v ~release ~acquire = { release; acquire; waiting = 0; held = true }

(* The resource is given up when the first wait begins and taken back when the
   last one ends, so waits in parallel fibers release and acquire it once.
   Only a resource held is given up, and only one given up is taken back, so
   a wait cancelled before taking it back, or one that begins while another
   fiber is taking it back, never gives it up twice. *)
let run t f =
  if t.waiting = 0 && t.held then (
    t.held <- false;
    t.release ());
  t.waiting <- t.waiting + 1;
  let take_back () =
    if t.waiting = 0 && not t.held then (
      t.acquire ();
      t.held <- true)
  in
  match f () with
  | x ->
      t.waiting <- t.waiting - 1;
      take_back ();
      x
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      t.waiting <- t.waiting - 1;
      (try take_back () with Eio.Cancel.Cancelled _ -> ());
      Printexc.raise_with_backtrace exn bt
