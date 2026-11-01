(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Demonstrates Console.Progress in plain mode: a 4-step job whose tasks print
    a line when they start and when they end, the last one failing with its
    output, then a summary. A fixed clock keeps the timers deterministic so cram
    diffs are stable. *)

let () =
  let now = ref 0. in
  (* An eighth of a second is exact in binary: no step's time rounds down. *)
  let tick () = now := !now +. 0.125 in
  let root = Console.Task.root ~label:"Processing" ~now:(fun () -> !now) () in
  let display =
    Console.Progress.show ~progress:`Plain Format.std_formatter root
  in
  let step name =
    let task = Console.Task.start root name in
    tick ();
    Console.Task.succeed task
  in
  step "foo.ml";
  step "bar.ml";
  step "baz.ml";
  let task = Console.Task.start ~log_lines:2 root "qux.ml" in
  List.iter (Console.Task.log task) [ "line 1"; "line 2"; "line 3" ];
  tick ();
  Console.Task.fail task;
  Console.Task.succeed root;
  Console.Display.close display
