(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let getenv = Sys.getenv_opt
let outputs = [ (Fmt.stdout, Unix.stdout); (Fmt.stderr, Unix.stderr) ]

let set ?style_renderer ?utf_8 ?theme (ppf, fd) =
  let renderer =
    match style_renderer with
    | Some r -> r
    | None ->
        let is_tty = Unix.isatty fd || Option.is_some (Cast.file ()) in
        Colour.renderer ~getenv ~is_tty
  in
  Fmt.set_style_renderer ppf renderer;
  Fmt.set_utf_8 ppf (Option.value utf_8 ~default:(Colour.utf_8 ~getenv));
  Console.Color.set_depth ppf (Colour.depth ~getenv);
  Option.iter (Console.Theme.set ppf) theme

let setup ?style_renderer ?utf_8 ?theme () =
  List.iter (set ?style_renderer ?utf_8 ?theme) outputs;
  ignore (Recording.start ~time:(fun () -> 0.) Fmt.stdout : bool)

(* Fmt keeps what a program chose for a formatter in tag functions it
   installs on the first choice, so a formatter whose tag functions are still
   Format's own, but for its theme, has had nothing chosen for it. *)
let ensure () =
  List.iter (fun out -> if Slot.unset (fst out) then set out) outputs

let is_terminal fd = Unix.isatty fd && not (Colour.dumb ~getenv)
