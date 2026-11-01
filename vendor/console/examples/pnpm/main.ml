(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* An install that looks like pnpm's. The progress line and the box of a
   running script are redrawn in place; what is done goes to the
   scrollback as pnpm prints it: the packages added, the progress line once
   done, a script's box once it ends, the dependencies added and the time
   it all took. Pipe it through [cat] to see the lines a CI log gets, which
   are pnpm's there too. *)

open Console

let hl = Style.fg Color.bright_cyan
let rail = Style.fg Color.bright_magenta
let added = Style.fg Color.green
let grey = Style.fg Color.bright_black
let paint style s = View.styled style (View.text s)

(* A duration as pnpm writes it, with pretty-ms. *)
let duration s =
  if s < 1. then Fmt.str "%dms" (Float.to_int (s *. 1000.))
  else Fmt.str "%.1fs" s

type counts = {
  mutable resolved : int;
  mutable reused : int;
  mutable downloaded : int;
  mutable added : int;
}

let total = 312
let counts = { resolved = 0; reused = 0; downloaded = 0; added = 0 }
let fetching = ref true

(* 22 of the 312 packages are not in the store yet. *)
let in_store i = i mod 14 <> 5

let progress_line () =
  let n i = paint hl (Int.to_string i) in
  View.concat
    [
      View.text "Progress: resolved ";
      n counts.resolved;
      View.text ", reused ";
      n counts.reused;
      View.text ", downloaded ";
      n counts.downloaded;
      View.text ", added ";
      n counts.added;
      (if !fetching then View.empty else View.text ", done");
    ]

let packages n =
  View.vcat
    [
      View.(text "Packages: " ++ paint added (Fmt.str "+%d" n));
      View.fit (fun room ->
          let width = Option.value (Room.width room) ~default:80 in
          paint added (String.make (min n width) '+'));
    ]

(* Each package resolved is reused from the store or downloaded, four ticks
   later; the packages are added once all are resolved and downloaded. *)
let fetch clock =
  let downloads = Queue.create () in
  let tick = ref 0 in
  while counts.added < total do
    incr tick;
    for _ = 1 to min 6 (total - counts.resolved) do
      if in_store counts.resolved then counts.reused <- counts.reused + 1
      else Queue.push (!tick + 4) downloads;
      counts.resolved <- counts.resolved + 1;
      if counts.resolved = total then Console_eio.print (packages total)
    done;
    while (not (Queue.is_empty downloads)) && Queue.peek downloads <= !tick do
      ignore (Queue.pop downloads);
      counts.downloaded <- counts.downloaded + 1
    done;
    if counts.resolved = total && Queue.is_empty downloads then
      counts.added <- min total (counts.added + 40);
    Eio.Time.Mono.sleep clock 0.05
  done;
  fetching := false;
  Console_eio.print (progress_line ())

(* A script runs as a task, labelled as pnpm names it, and its box shows
   the task's last ten lines, also the box it leaves in the scrollback. *)
type script = { label : string; dir : string; cmd : string }

let scripts = ref []
let script label = List.find_opt (fun s -> s.label = label) !scripts
let shown = 10

let box t s =
  let lines = Task.output t and dropped = Task.dropped t in
  let n = List.length lines in
  let footer =
    match Task.state t with
    | `Running -> "Running..."
    | `Succeeded -> "Done in " ^ duration (Task.elapsed t)
    | `Failed | `Cancelled -> "Failed in " ^ duration (Task.elapsed t)
  in
  let row line = View.(paint rail "│" ++ text " " ++ text line) in
  let collapsed = dropped + max 0 (n - shown) in
  View.vcat
    ([
       View.(
         text (s.dir ^ " ") ++ paint hl "postinstall" ++ text ("$ " ^ s.cmd));
       (if collapsed > 0 then
          View.text (Fmt.str "[%d lines collapsed]" collapsed)
        else View.empty);
     ]
    @ List.filteri (fun i _ -> i >= n - shown) (List.map row lines)
    @ [ View.(paint rail "└─" ++ text " " ++ paint rail footer) ])

let box_of t = Option.map (box t) (script (Task.label t))

let postinstall clock root ~dir ~cmd output =
  let s = { label = dir ^ " postinstall"; dir; cmd } in
  scripts := !scripts @ [ s ];
  Task.run root s.label @@ fun task ->
  List.iter
    (fun (pause, line) ->
      Eio.Time.Mono.sleep clock pause;
      Task.log task line)
    output

(* Off a terminal, a script's lines as pnpm appends them. *)
let plain e =
  match (Task.Event.task e, Task.Event.kind e) with
  | Some (_, [ label ]), `Start ->
      Option.map (fun s -> View.text (label ^ "$ " ^ s.cmd)) (script label)
  | Some (_, [ label ]), `Output ->
      Option.map
        (fun text -> View.text (label ^ ": " ^ text))
        (Task.Event.text e)
  | Some (_, [ label ]), `End -> (
      match Task.Event.status e with
      | Some `Succeeded -> Some (View.text (label ^ ": Done"))
      | Some _ -> Some (View.text (label ^ ": Failed"))
      | None -> None)
  | _ -> None

let esbuild =
  [
    (0.08, "esbuild 0.21.5, platform darwin arm64");
    (0.10, "looking for @esbuild/darwin-arm64@0.21.5");
    (0.06, "  not in node_modules/@esbuild/darwin-arm64");
    (0.12, "  trying the pnpm store");
    (0.18, "  found in the store, 9.4 MB");
    (0.05, "linking bin/esbuild");
    (0.09, "checking bin/esbuild --version");
    (0.11, "  0.21.5");
    (0.04, "removing the JavaScript stub");
    (0.12, "installing the native binary");
    (0.05, "  bin/esbuild -> @esbuild/darwin-arm64/bin/esbuild");
    (0.13, "verifying the install");
    (0.07, "  esbuild 0.21.5 is ready");
  ]

let dependencies =
  [
    ("dependencies", [ ("react", "18.3.1"); ("react-dom", "18.3.1") ]);
    ( "devDependencies",
      [ ("esbuild", "0.21.5"); ("typescript", "5.5.4"); ("vite", "5.3.5") ] );
  ]

(* What was added, and the time it all took, each after a blank line. *)
let summary elapsed =
  let package (name, version) =
    View.(paint added "+" ++ text (" " ^ name ^ " ") ++ paint grey version)
  in
  let kind (kind, packages) =
    View.vcat (paint hl (kind ^ ":") :: List.map package packages)
  in
  View.(
    text "\n"
    ++ vcat ~gap:1
         (List.map kind dependencies @ [ text ("Done in " ^ duration elapsed) ]))

(* The region: the progress line while it counts, and the running scripts. *)
let view root =
  View.fit (fun _ ->
      View.vcat
        ((if !fetching then progress_line () else View.empty)
        :: List.filter_map box_of (Task.children root)))

let () =
  Console_tty.setup ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~ppf:Fmt.stdout ~view ~commit:box_of ~plain
    (fun root ->
      fetch clock;
      postinstall clock root ~dir:"node_modules/esbuild" ~cmd:"node install.js"
        esbuild;
      Console_eio.print (summary (Task.elapsed root)))
