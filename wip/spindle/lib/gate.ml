(* SPDX-License-Identifier: ISC *)

type t = {
  release : unit -> unit;
  acquire : unit -> unit;
  mutable waiting : int;
}

let v ~release ~acquire = { release; acquire; waiting = 0 }

(* The resource is given up when the first wait begins and taken back when the
   last one ends, so waits in parallel fibers release and acquire it once. A
   wait that raises takes it back only if it is the last, and not when the
   fiber is being cancelled. *)
let run t f =
  if t.waiting = 0 then t.release ();
  t.waiting <- t.waiting + 1;
  match f () with
  | x ->
      t.waiting <- t.waiting - 1;
      if t.waiting = 0 then t.acquire ();
      x
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      t.waiting <- t.waiting - 1;
      (if t.waiting = 0 then
         try t.acquire () with Eio.Cancel.Cancelled _ -> ());
      Printexc.raise_with_backtrace exn bt
