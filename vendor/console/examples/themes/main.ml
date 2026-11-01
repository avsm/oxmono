(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The same title, tree, table and progress rows in the theme its argument
   names, the title graded through the theme's palette: the README's
   gallery is recordings of this program, one per theme. The table is
   drawn above the rows while they run, so a theme with motion shows its
   frame moving, and stays, still, in the view's last frame once they are
   done. *)

open Console

let accent s = View.styled Style.bold (View.role `Accent (View.text s))

let table =
  Table.view ~rules:true
    [
      Table.column (View.text "Package");
      Table.column ~align:`Right (View.text "Size");
    ]
    [
      [ accent "console"; View.text "42 KiB" ];
      [ View.text "console-eio"; View.text "9 KiB" ];
    ]

let tree =
  let leaf s = Tree.v (View.text s) [] in
  Tree.v (accent "lib")
    [
      Tree.v (View.text "display") [ leaf "row.ml"; leaf "region.ml" ];
      leaf "span.ml";
    ]

let progress ~clock table =
  (* The table above the rows and the summary. *)
  let view root =
    View.vcat [ View.sized ~width:100 table; Progress.view root ]
  in
  Console_eio.run ~clock ~ppf:Fmt.stdout ~label:"Building" ~view (fun root ->
      let ok = Task.start root "fetch" in
      Task.set_items ok ~count:3 ~total:3;
      Task.succeed ok;
      let failed = Task.start root "lint" in
      Task.fail failed;
      let compile = Task.start ~log_lines:1 root "compile" in
      Task.set_items compile ~count:5 ~total:8;
      Task.log compile "unused variable x";
      ignore (Task.start root "test");
      Eio.Time.Mono.sleep clock 3.;
      Task.cancel compile)

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "default" in
  match Theme.of_string name with
  | Error (`Msg e) ->
      prerr_endline e;
      exit 2
  | Ok theme ->
      let no_color =
        match Sys.getenv_opt "NO_COLOR" with
        | Some "" | None -> false
        | _ -> true
      in
      Console_tty.setup
        ?style_renderer:(if no_color then None else Some `Ansi_tty)
        ~theme ();
      let ocaml = String.map (function '-' -> '_' | c -> c) name in
      let title = View.gradient (View.text ("Theme." ^ ocaml)) in
      Fmt.pr "%a@.%a@.@." View.pp
        (View.styled Style.bold title)
        View.pp (Tree.view tree);
      Eio_main.run @@ fun env ->
      progress ~clock:(Console_eio.clock (Eio.Stdenv.mono_clock env)) table
