(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tasks shown on standard error, while the program prints its results on
    standard output without flushing it. *)

let () =
  let root = Console.Task.root ~now:(fun () -> 0.) () in
  let display = Console.Progress.show Fmt.stderr root in
  Fmt.pr "result 1\n";
  let task = Console.Task.start root "fetch" in
  Fmt.pr "result 2\n";
  Console.Task.log task "fetched";
  Console.Task.succeed task;
  Fmt.pr "result 3\n";
  Console.Task.succeed root;
  Console.Display.close display
