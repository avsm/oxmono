(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [cargo build] of a binary with a dozen dependencies. A crate says that it
   compiles when its first unit starts, and that line stays in the
   scrollback; the units that run are transient tasks, which leave nothing
   there, named on the one line redrawn below it. Pipe it through [cat] to
   see the lines a CI log gets. *)

open Console

(* Cargo's colours, in the terminal's named colours. *)
let header = Style.(bold + fg Color.bright_green)
let transient = Style.(bold + fg Color.bright_cyan)
let warn = Style.(bold + fg Color.bright_yellow)
let gutter = Style.(bold + fg Color.bright_blue)

let status ?(style = header) verb what =
  View.(
    hcat ~gap:1
      [ place ~align:`Right ~width:12 (styled style (text verb)); text what ])

let say verb fmt = Fmt.kstr (fun s -> Console_eio.print (status verb s)) fmt

(* What rustc says of the program's own code. *)
let unused_variable =
  let open View in
  let bar = styled gutter (text "   |") in
  vcat
    [
      styled warn (text "warning")
      ++ styled Style.bold (text ": unused variable: `retries`");
      styled gutter (text "  --> ") ++ text "src/main.rs:14:9";
      bar;
      styled gutter (text "14 |") ++ text "     let retries = 3;";
      bar ++ text "         "
      ++ styled warn
           (text
              "^^^^^^^ help: if this is intentional, prefix it with an \
               underscore: `_retries`");
      bar;
      styled gutter (text "   = ")
      ++ styled Style.bold (text "note")
      ++ text ": `#[warn(unused_variables)]` on by default";
    ]

type crate = {
  name : string;
  version : string;
  path : string option;
  kib : int;
  build_rs : bool;
  deps : string list;
  secs : float;
  warnings : View.t list;
}

let crate ?path ?(build_rs = false) ?(deps = []) ?(warnings = []) name version
    kib secs =
  { name; version; path; kib; build_rs; deps; secs; warnings }

let registry =
  [
    crate "unicode-ident" "1.0.13" 42 0.10;
    crate "proc-macro2" "1.0.86" 49 0.20 ~build_rs:true
      ~deps:[ "unicode-ident" ];
    crate "quote" "1.0.37" 28 0.12 ~deps:[ "proc-macro2" ];
    crate "syn" "2.0.79" 260 0.55
      ~deps:[ "proc-macro2"; "quote"; "unicode-ident" ];
    crate "itoa" "1.0.11" 10 0.06;
    crate "ryu" "1.0.18" 47 0.10;
    crate "memchr" "2.7.4" 96 0.25;
    crate "pin-project-lite" "0.2.14" 28 0.08;
    crate "serde_derive" "1.0.210" 55 0.45
      ~deps:[ "proc-macro2"; "quote"; "syn" ];
    crate "tokio-macros" "2.4.0" 12 0.20 ~deps:[ "proc-macro2"; "quote"; "syn" ];
    crate "serde" "1.0.210" 77 0.30 ~build_rs:true ~deps:[ "serde_derive" ];
    crate "tokio" "1.40.0" 773 0.60 ~deps:[ "pin-project-lite"; "tokio-macros" ];
    crate "serde_json" "1.0.128" 147 0.35 ~build_rs:true
      ~deps:[ "itoa"; "memchr"; "ryu"; "serde" ];
  ]

let app =
  crate "app" "0.1.0" 0 0.32 ~path:"/home/user/app"
    ~deps:[ "serde"; "serde_json"; "tokio" ]
    ~warnings:[ unused_variable ]

(* The units cargo schedules: a build script is compiled, then run, before
   rustc compiles the crate, which also waits for the crates it uses. *)
type job = {
  label : string;
  crate : crate;
  secs : float;
  rustc : bool;
  after : job list;
  ended : unit Eio.Promise.t * unit Eio.Promise.u;
}

let plan () =
  let compiled = Hashtbl.create 16 in
  let job ?(rustc = false) label crate secs after =
    { label; crate; secs; rustc; after; ended = Eio.Promise.create () }
  in
  let of_crate ~target c =
    let script, built =
      if c.build_rs then
        let compile = job (c.name ^ "(build.rs)") c 0.15 [] in
        let run = job (c.name ^ "(build)") c 0.05 [ compile ] in
        ([ compile; run ], [ run ])
      else ([], [])
    in
    let after = List.map (Hashtbl.find compiled) c.deps @ built in
    let main = job ~rustc:true (c.name ^ target) c c.secs after in
    Hashtbl.replace compiled c.name main;
    script @ [ main ]
  in
  let deps = List.concat_map (of_crate ~target:"") registry in
  deps @ of_crate ~target:"(bin)" app

let download clock root =
  say "Updating" "crates.io index";
  Eio.Time.Mono.sleep clock 0.30;
  say "Locking" "%d packages to latest compatible versions"
    (List.length registry);
  say "Downloading" "crates ...";
  let start = Task.elapsed root in
  List.iter
    (fun c ->
      Eio.Time.Mono.sleep clock 0.03;
      say "Downloaded" "%s v%s" c.name c.version)
    registry;
  let kib = List.fold_left (fun n c -> n + c.kib) 0 registry in
  say "Downloaded" "%d crates (%.1f MiB) in %.2fs" (List.length registry)
    (float kib /. 1024.)
    (Task.elapsed root -. start)

let generated c =
  let n = List.length c.warnings in
  View.(
    styled warn (text "warning")
    ++ styled Style.bold (text ":")
    ++ Fmt.kstr text
         " `%s` (bin %S) generated %d warning (run `cargo fix --bin %S` to \
          apply %d suggestion)"
         c.name c.name n c.name n)

(* A transient task counts the units that ended, as the root counts
   nothing, and each unit is a transient task under it while one of [jobs]
   slots runs it. *)
let compile ~jobs clock root =
  let plan = plan () in
  let total = List.length plan in
  Task.run ~transient:true root "build" @@ fun build ->
  let ended = ref 0 in
  let compiling = Hashtbl.create 16 in
  let slots = Eio.Semaphore.make jobs in
  Task.set_items build ~count:0 ~total;
  let run j =
    List.iter (fun a -> Eio.Promise.await (fst a.ended)) j.after;
    Eio.Semaphore.acquire slots;
    let c = j.crate in
    Task.run ~transient:true build j.label (fun _ ->
        if not (Hashtbl.mem compiling c.name) then begin
          Hashtbl.add compiling c.name ();
          let path = Option.fold ~none:"" ~some:(Fmt.str " (%s)") c.path in
          say "Compiling" "%s v%s%s" c.name c.version path
        end;
        Eio.Time.Mono.sleep clock j.secs;
        if j.rustc && c.warnings <> [] then
          Console_eio.print (View.vcat ~gap:1 (c.warnings @ [ generated c ])));
    incr ended;
    Task.set_items build ~count:!ended ~total;
    Eio.Semaphore.release slots;
    Eio.Promise.resolve (snd j.ended) ()
  in
  Eio.Fiber.List.iter run plan

(* Cargo's bar: [=] up to a [>] head, in 50 cells less the count and the
   15 cells of the status. *)
let bar count total =
  let stats = Fmt.str " %d/%d" count total in
  let width = 50 - 15 - 2 - String.length stats in
  let filled = width * count / total in
  let head =
    if filled = 0 then ""
    else String.make (filled - 1) '=' ^ if count = total then "=" else ">"
  in
  Fmt.str "[%s%s]%s" head (String.make (width - filled) ' ') stats

let running task = Task.state task = `Running

(* The one line below the scrollback, while the build runs. *)
let building root =
  View.fit (fun _ ->
      match List.filter running (Task.children root) with
      | [ build ] -> (
          match Task.progress build with
          | `Items (count, total) ->
              let units = List.filter running (Task.children build) in
              status ~style:transient "Building"
                (Fmt.str "%s: %s" (bar count total)
                   (String.concat ", " (List.map Task.label units)))
          | _ -> View.empty)
      | _ -> View.empty)

let () =
  Console_tty.setup ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~delay:0.5 ~view:building (fun root ->
      download clock root;
      compile ~jobs:4 clock root;
      say "Finished"
        "`dev` profile [unoptimized + debuginfo] target(s) in %.2fs"
        (Task.elapsed root))
