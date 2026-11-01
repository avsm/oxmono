(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* [run name ~n f] runs [f] [n] times and prints the processor time and the
   words allocated per run. Processor time, not wall-clock time, so that a
   busy machine does not count against the code. *)
let run name ~n f =
  f ();
  Gc.full_major ();
  let words () =
    let minor, promoted, major = Gc.counters () in
    minor +. major -. promoted
  in
  let w0 = words () and t0 = Sys.time () in
  for _ = 1 to n do
    f ()
  done;
  let t1 = Sys.time () and w1 = words () in
  Fmt.pr "%-34s %12.0f ns/op %14.0f words/op@." name
    ((t1 -. t0) *. 1e9 /. float n)
    ((w1 -. w0) /. float n)

let ascii = String.make 80 'a'
let cjk = String.concat "" (List.init 40 (fun _ -> "界"))
let emoji = String.concat "" (List.init 20 (fun _ -> "👨‍👩‍👧"))
let prose = String.concat " " (List.init 40 (fun i -> Fmt.str "word%d" i))

(* Text of every kind of cluster: accented Latin, a combining mark, CJK, a
   Devanagari consonant with its vowel sign, an emoji, a skin tone, a flag, a
   heart with VS16 and a ZWJ family; 80 cells. *)
let mixed =
  String.concat ""
    (List.init 4 (fun _ ->
         "caf\u{00E9} e\u{0301} \u{754C}\u{754C} \u{0915}\u{093F} \u{1F389} \
          \u{1F44D}\u{1F3FD} \u{1F1EB}\u{1F1F7} \u{2764}\u{FE0F} \
          \u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}"))

let long_ascii = String.concat " " (List.init 2000 (fun _ -> "word"))
let long_mixed = String.concat " " (List.init 125 (fun _ -> mixed))
let sink = ref 0

let widths () =
  run "width ascii 80" ~n:100_000 (fun () ->
      sink := View.width (View.text ascii));
  run "width cjk 40" ~n:100_000 (fun () -> sink := View.width (View.text cjk));
  run "width emoji 20" ~n:10_000 (fun () ->
      sink := View.width (View.text emoji));
  run "width mixed 80" ~n:10_000 (fun () ->
      sink := View.width (View.text mixed));
  run "width ascii 10k" ~n:1_000 (fun () ->
      sink := View.width (View.text long_ascii));
  run "width mixed 10k" ~n:100 (fun () ->
      sink := View.width (View.text long_mixed))

let fitting () =
  let span = View.text prose in
  run "truncate prose to 40" ~n:10_000 (fun () ->
      sink := View.width (View.truncate 40 span));
  run "wrap prose to 30" ~n:10_000 (fun () ->
      sink := List.length (View.wrap 30 span));
  let span = View.text mixed in
  run "truncate mixed to 40" ~n:10_000 (fun () ->
      sink := View.width (View.truncate 40 span));
  run "wrap mixed to 30" ~n:10_000 (fun () ->
      sink := List.length (View.wrap 30 span));
  let span = View.text long_mixed in
  run "wrap mixed 10k to 80" ~n:100 (fun () ->
      sink := List.length (View.wrap 80 span))

let null = Format.make_formatter (fun _ _ _ -> ()) ignore

let utf_8 =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) ignore in
  Fmt.set_utf_8 ppf true;
  Format.pp_set_margin ppf 120;
  ppf

(* Bars and rules fill their cells with a glyph, cluster by cluster. *)
let fills () =
  run "bar 80 cells" ~n:10_000 (fun () ->
      View.pp utf_8 (Bar.horizontal ~width:80 0.5));
  let rule = Rule.view ~glyph:"\u{2550}" ~label:(View.text mixed) () in
  run "rule 120 cells, mixed label" ~n:10_000 (fun () -> View.pp utf_8 rule)

(* The line editor measures the line and steps over its clusters as it
   echoes. *)
let line_editor () =
  run "line editor mixed 80" ~n:1_000 (fun () ->
      let t = Console_input.v () in
      let echo, _ = Console_input.feed t mixed in
      let back, _ = Console_input.feed t "\x1b[D\x1b[D\x1b[D\x7f\r" in
      sink := String.length echo + String.length back)

let table () =
  let rows =
    List.init 1000 (fun i ->
        [ Fmt.kstr View.text "row %d" i; View.text prose; View.text "42" ])
  in
  let columns =
    Table.
      [
        column (View.text "Name");
        column (View.text "Text");
        column ~align:`Right (View.text "N");
      ]
  in
  Format.pp_set_margin null 120;
  run "table 1k rows" ~n:10 (fun () -> View.pp null (Table.view columns rows));
  let rows =
    List.init 1000 (fun i ->
        [ Fmt.kstr View.text "row %d" i; View.text mixed; View.text "42" ])
  in
  run "table 1k mixed rows" ~n:10 (fun () ->
      View.pp utf_8 (Table.view columns rows))

let tree () =
  let leaf i = Tree.v (Fmt.kstr View.text "leaf %d" i) [] in
  let tree =
    Tree.v (View.text "root")
      (List.init 10 (fun i ->
           Tree.v (Fmt.kstr View.text "dir %d" i) (List.init 100 leaf)))
  in
  run "tree 1k nodes" ~n:10 (fun () -> View.pp null (Tree.view tree))

let clock () =
  let t = ref 0. in
  ((fun () -> !t), fun () -> t := !t +. 0.001)

let display ~live () =
  let now, tick = clock () in
  let size = if live then Some (fun () -> (80, 24)) else None in
  let root = Task.root ~label:"bench" ~now () in
  let d = Progress.show ?size null root in
  for i = 1 to 10_000 do
    let t = Fmt.kstr (Task.start root) "task %d" i in
    for j = 1 to 10 do
      tick ();
      Task.set_items t ~count:j ~total:10;
      Task.log t "a line of output from the task"
    done;
    Task.succeed t
  done;
  Display.close d

(* Ten tasks run while the clock does not move: every redraw is the frame
   already on screen. *)
let idle () =
  let now () = 0. in
  let root = Task.root ~label:"bench" ~now () in
  let d = Progress.show ~size:(fun () -> (80, 24)) null root in
  let tasks = List.init 10 (fun i -> Fmt.kstr (Task.start root) "task %d" i) in
  run "redraw with nothing changed" ~n:10_000 (fun () -> Display.redraw d);
  List.iter Task.succeed tasks;
  Display.close d

(* A live region of 1k running tasks, each with its spinner and its time,
   drawn frame after frame as the clock moves: the cost of a frame. *)
let live_tasks () =
  let t = ref 0. in
  let root = Task.root ~label:"bench" ~now:(fun () -> !t) () in
  let d = Progress.show ~size:(fun () -> (100, 40)) null root in
  let tasks =
    List.init 1000 (fun i -> Fmt.kstr (Task.start root) "task %d" i)
  in
  run "live 1k running tasks, a frame" ~n:200 (fun () ->
      t := !t +. 0.05;
      Display.redraw d);
  List.iter Task.succeed tasks;
  Display.close d

let results n =
  Table.view
    [
      Table.column (View.text "Name");
      Table.column ~align:`Right (View.text "N");
    ]
    (List.init n (fun i ->
         [ Fmt.kstr View.text "row %d" i; Fmt.kstr View.text "%d" (i * 7) ]))

(* A view of a 200-row table and a spinner beside it, drawn frame after
   frame: the spinner turns, the table is laid out once. *)
let table_and_spinner () =
  let t = ref 0. in
  let view = View.vcat [ results 200; Spinner.view () ] in
  let d =
    Display.v ~size:(fun () -> (100, 250)) ~now:(fun () -> !t) null view
  in
  run "live 200-row table and a spinner, a frame" ~n:200 (fun () ->
      t := !t +. 0.05;
      Display.redraw d);
  Display.close d

(* A view that does not move, redrawn as the clock moves: a tick costs
   nothing. *)
let idle_view () =
  let t = ref 0. in
  let d =
    Display.v
      ~size:(fun () -> (100, 250))
      ~now:(fun () -> !t)
      null (results 200)
  in
  run "live still view, a tick" ~n:10_000 (fun () ->
      t := !t +. 0.05;
      Display.redraw d);
  Display.close d

let () =
  widths ();
  fitting ();
  table ();
  tree ();
  fills ();
  line_editor ();
  idle ();
  live_tasks ();
  table_and_spinner ();
  idle_view ();
  run "display plain 10k tasks 100k logs" ~n:1 (display ~live:false);
  run "display live 10k tasks 100k logs" ~n:1 (display ~live:true)
