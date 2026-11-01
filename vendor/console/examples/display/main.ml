(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A build in the terminal's scrollback: steps that end are printed once and
   stay; only the steps still running are redrawn in place, under a title in
   the theme's palette and a table of the steps, whose frame moves in a theme
   with motion, and above the summary. The title, the table and the summary
   are printed once the build is done. Name a theme to see it in that one;
   pipe it through [cat] to see the lines a CI log gets. *)

open Console

(* The compiler's warnings, which stay above the rows once their step is done.
*)
let src = Logs.Src.create "ocamlopt" ~doc:"The compiler's diagnostics"

type step = { name : string; mutable status : View.t; mutable detail : string }

let step name = { name; status = View.text "waiting"; detail = "" }
let steps = [ step "fetch"; step "compile"; step "test" ]

let title =
  View.styled Style.bold
    (View.gradient (View.text "console, built step by step"))

(* The details keep their column from the start, so that the table does not
   widen as they come, but in a room too narrow for that they take what the
   steps and their status leave: the frame and the padding take ten cells,
   and the steps and their status seven each. *)
let table room =
  let detail =
    match Room.width room with Some w -> max 1 (min 19 (w - 24)) | None -> 19
  in
  Table.view ~rules:true
    [
      Table.column (View.text "Step");
      Table.column ~min_width:7 (View.text "Status");
      Table.column ~min_width:detail (View.text "Detail");
    ]
    (List.map
       (fun s -> [ View.text s.name; s.status; View.text s.detail ])
       steps)

(* What a step's row of the table says, from start to end. *)
let started name =
  let s = List.find (fun s -> String.equal s.name name) steps in
  s.status <- View.role `Accent (View.text "running");
  fun detail ->
    s.status <- View.styled Style.bold (View.role `Succeeded (View.text "done"));
    s.detail <- detail

(* What a step's commands print shows under its row while it runs, and goes
   when it succeeds. *)
let fetch clock root =
  let finished = started "fetch" in
  Task.run ~log_lines:2 root "fetch dependencies" @@ fun t ->
  let total = Int64.mul 48L 1048576L in
  for i = 0 to 12 do
    Task.set_bytes t
      ~count:(Int64.div (Int64.mul total (Int64.of_int i)) 12L)
      ~total;
    if i mod 4 = 0 then Fmt.kstr (Task.log t) "GET pkg-%d.tar.gz" i;
    Eio.Time.Mono.sleep clock 0.15
  done;
  finished "13 packages, 48 MiB"

let compile clock root =
  let finished = started "compile" in
  let files = [| "lexer.ml"; "parser.ml"; "types.ml"; "eval.ml"; "main.ml" |] in
  Task.run ~log_lines:3 root "compile" (fun t ->
      Array.iteri
        (fun i f ->
          Task.set_items t ~count:i ~total:(Array.length files);
          Fmt.kstr (Task.log t) "ocamlopt %s" f;
          Eio.Time.Mono.sleep clock 0.35)
        files;
      Task.set_items t ~count:(Array.length files) ~total:(Array.length files));
  (* Logged outside the task, the warning outlives its success. *)
  Logs.warn ~src (fun m -> m "eval.ml: unused variable x");
  finished "5 files, 1 warning"

let test clock root =
  let finished = started "test" in
  Task.run root "test" @@ fun t ->
  let unit = Task.start t "unit" in
  let fuzz = Task.start t "properties" in
  for i = 0 to 10 do
    Task.set_items unit ~count:(i * 12) ~total:120;
    Task.set_items fuzz ~count:i ~total:10;
    Eio.Time.Mono.sleep clock 0.2
  done;
  finished "130 passed"

(* The title and the table above the rows and the summary; they give way
   first when the rows need the room. The table shows the steps as they are
   at each frame. *)
let view root =
  View.vcat
    [ title; View.sized ~width:100 (View.fit table); Progress.view root ]

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "default" in
  let theme =
    match Theme.of_string name with
    | Ok theme -> theme
    | Error (`Msg e) ->
        prerr_endline e;
        exit 2
  in
  Console_tty.setup ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~label:"Building" ~view (fun root ->
      Eio.Fiber.both (fun () -> fetch clock root) (fun () -> compile clock root);
      test clock root;
      Task.succeed ~message:"build complete" root)
