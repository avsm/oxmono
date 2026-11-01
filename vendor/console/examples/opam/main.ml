(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [opam install dune cmdliner fmt], four jobs at once: the plan, then each
   action's line in the scrollback as it ends, while the jobs that run share
   one status line, rewritten in place, which goes once the work is done. *)

open Console

type package = {
  name : string;
  version : string;
  deps : string list;
  url : string option;
  build : (string * float) list;
  install : (string * float) list;
}

(* Each command is the text opam shows for it, and how long it runs. *)
let packages =
  [
    {
      name = "cmdliner";
      version = "1.3.0";
      deps = [];
      url = None;
      build = [ ("make all", 0.9) ];
      install = [ ("make install", 0.3) ];
    };
    {
      name = "dune";
      version = "3.16.0";
      deps = [];
      url =
        Some
          "https://github.com/ocaml/dune/releases/download/3.16.0/dune-3.16.0.tbz";
      build = [ ("ocaml 4", 2.4); ("./_boot/dune.exe build", 1.2) ];
      install = [];
    };
    {
      name = "fmt";
      version = "0.9.0";
      deps = [ "ocamlbuild"; "ocamlfind"; "topkg" ];
      url = None;
      build = [ ("ocaml build", 0.8) ];
      install = [];
    };
    {
      name = "ocamlbuild";
      version = "0.15.0";
      deps = [];
      url = None;
      build =
        [ ("make configure.make", 0.4); ("make check-if-preinstalled", 1.6) ];
      install = [];
    };
    {
      name = "ocamlfind";
      version = "1.9.6";
      deps = [];
      url = None;
      build = [ ("./configure", 0.5); ("make all", 0.9); ("make opt", 0.7) ];
      install = [ ("make install", 0.4) ];
    };
    {
      name = "topkg";
      version = "1.0.7";
      deps = [ "ocamlbuild"; "ocamlfind" ];
      url = None;
      build = [ ("ocaml build", 0.6) ];
      install = [];
    };
  ]

let requested = [ "dune"; "cmdliner"; "fmt" ]
let jobs = 4

(* Fetch, build and install each package. *)
let actions = 3 * List.length packages
let finished = ref 0
let ( ++ ) = View.( ++ )

let glyph ~utf_8 ~ascii =
  View.fit (fun room -> View.text (if Room.utf_8 room then utf_8 else ascii))

let install_mark = View.role `Succeeded (glyph ~utf_8:"∗" ~ascii:"->")
let fetch_mark = View.role `Accent (glyph ~utf_8:"⬇" ~ascii:"->")
let bold s = View.styled Style.bold (View.text s)
let nv p = bold p.name ++ View.text ("." ^ p.version)

let plan =
  let action = View.role `Succeeded (glyph ~utf_8:"∗" ~ascii:"- install") in
  let cause p =
    if List.mem p.name requested then View.empty
    else
      match List.find_opt (fun q -> List.mem p.name q.deps) packages with
      | Some q -> Fmt.kstr View.text "[required by %s]" q.name
      | None -> View.empty
  in
  let columns =
    [
      Table.column View.empty;
      Table.column ~style:Style.bold View.empty;
      Table.column View.empty;
      Table.column View.empty;
    ]
  in
  let rows =
    List.map
      (fun p -> [ action; View.text p.name; View.text p.version; cause p ])
      packages
  in
  View.vcat
    [
      View.text "The following actions will be performed:";
      View.concat ~sep:(View.text " ")
        [
          View.role `Succeeded (View.text "===");
          View.text "install";
          bold (string_of_int (List.length packages));
          View.text "packages";
        ];
      View.hcat ~gap:1
        [ View.text " "; Table.view ~border:Border.none ~gap:1 columns rows ];
    ]

let proceed =
  let n = Fmt.kstr bold "%d" (List.length packages) in
  View.fit (fun room ->
      let stats =
        if Room.utf_8 room then install_mark ++ View.text " " ++ n else n
      in
      View.concat
        [
          View.text "Proceed with "; stats; View.text " installations? [Y/n] y";
        ])

(* opam's section header: at most 80 cells, and the camel on UTF-8. *)
let header title =
  View.fit @@ fun room ->
  let width = min 80 (Option.value ~default:80 (Room.width room)) in
  let camel = Room.utf_8 room in
  let right =
    width - View.width (View.text title) - 6 - if camel then 4 else 0
  in
  let pad = String.init (max 0 right) (fun i -> "<>".[(right - i) mod 2]) in
  View.concat
    [
      View.role `Accent (View.text "<><>");
      View.text " ";
      View.styled Style.bold (View.text title);
      (if right > 0 then View.text " " ++ View.role `Accent (View.text pad)
       else View.empty);
      (if camel then View.text "  " ++ View.role `Warning (View.text "🐫")
       else View.empty);
    ]

(* A job's label is [name: command]; the name shows in green. *)
let job_text task =
  let label = Task.label task in
  match String.index_opt label ':' with
  | None -> View.text ("[" ^ label ^ "]")
  | Some i ->
      View.concat
        [
          View.text "[";
          View.role `Succeeded (View.text (String.sub label 0 i));
          View.text (String.sub label i (String.length label - i));
          View.text "]";
        ]

(* The texts that fit in [rem] cells, one space apart; when one does not,
   the line ends with how many are left out, against the right edge. *)
let rec fit_texts rem = function
  | [] -> []
  | [ t ] when View.width t < rem -> [ t ]
  | t :: ts ->
      let len = View.width t in
      if len > rem - 5 then
        [
          View.place ~align:`Right ~width:(rem - 1)
            (Fmt.kstr View.text "+%2d" (List.length ts + 1));
        ]
      else t :: fit_texts (rem - len - 1) ts

let status root =
  View.fit @@ fun room ->
  let running =
    List.filter (fun t -> Task.state t = `Running) (Task.children root)
  in
  if running = [] then View.empty
  else
    let title =
      Fmt.kstr View.text "Processing %2d/%d:"
        (!finished + List.length running)
        actions
    in
    let texts = title :: List.map job_text running in
    let texts =
      match Room.width room with Some w -> fit_texts w texts | None -> texts
    in
    View.concat ~sep:(View.text " ") texts

let job clock root slots steps =
  Eio.Semaphore.acquire slots;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release slots) @@ fun () ->
  match steps with
  | [] ->
      Eio.Time.Mono.sleep clock 0.05;
      incr finished
  | (first, _) :: _ ->
      Task.run ~transient:true root first @@ fun task ->
      List.iter
        (fun (text, duration) ->
          Task.set_label task text;
          Eio.Time.Mono.sleep clock duration)
        steps;
      (* Counted before the task ends, when its row goes. *)
      incr finished

let line v = Fmt.pr "%a@." View.pp v

let install clock root =
  let slots = Eio.Semaphore.make jobs in
  let installed =
    List.map (fun p -> (p.name, Eio.Promise.create ())) packages
  in
  let steps p = List.map (fun (cmd, d) -> (p.name ^ ": " ^ cmd, d)) in
  let package p =
    let source = Option.value ~default:"cached" p.url in
    let fetch = if Option.is_some p.url then 0.8 else 0.2 in
    job clock root slots [ (p.name ^ "." ^ p.version ^ ": dl", fetch) ];
    line
      (View.concat
         [
           fetch_mark;
           View.text " retrieved ";
           nv p;
           Fmt.kstr View.text "  (%s)" source;
         ]);
    List.iter (fun d -> Eio.Promise.await (fst (List.assoc d installed))) p.deps;
    job clock root slots (steps p p.build);
    job clock root slots (steps p p.install);
    line (View.concat [ install_mark; View.text " installed "; nv p ]);
    Eio.Promise.resolve (snd (List.assoc p.name installed)) ()
  in
  Eio.Fiber.List.iter package packages

let () =
  Console_tty.setup ();
  line plan;
  Fmt.pr "@.";
  line proceed;
  Fmt.pr "@.";
  line (header "Processing actions");
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~view:status (install clock);
  line (View.text "Done.")
