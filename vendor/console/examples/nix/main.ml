(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [nix build], imitated: the plan is printed once, then a single status line
   counts the builds, the copies and the download, and names the latest
   activity with its phase and the last line of its log. A build or a fetch
   that succeeds leaves nothing, and the status line goes when the work ends.
   Piped through [cat], each line of a build's log comes after its name and
   [>], as with [nix build -L]. *)

open Console

let cache = "https://cache.nixos.org"
let mib = 1048576.
let mebibytes n = Int64.to_float n /. mib

(* Store paths, their hashes in Nix's base-32 from a fixed seed. *)
let store =
  let rng = Random.State.make [| 0x2c1 |] in
  let base32 = "0123456789abcdfghijklmnpqrsvwxyz" in
  fun name ->
    let hash = String.init 32 (fun _ -> base32.[Random.State.int rng 32]) in
    Fmt.str "/nix/store/%s-%s" hash name

(* {1 The plan} *)

(* What the binary cache has: the bytes to download, and unpacked. *)
type substitute = {
  path : string;
  name : string;
  download : int64;
  nar : int64;
}

let bytes mebibytes = Int64.of_float (mebibytes *. mib)

let substitutes =
  List.sort
    (fun a b -> String.compare a.path b.path)
    (List.map
       (fun (name, download, nar) ->
         { path = store name; name; download = bytes download; nar = bytes nar })
       [
         ("bash-5.2p37", 1.71, 9.04);
         ("binutils-2.43.1", 5.84, 31.2);
         ("binutils-wrapper-2.43.1", 0.01, 0.06);
         ("coreutils-9.5", 1.62, 7.83);
         ("gcc-14.2.1.20250322", 37.92, 186.71);
         ("gcc-wrapper-14.2.1.20250322", 0.01, 0.08);
         ("glibc-2.40-66", 7.53, 30.32);
         ("glibc-2.40-66-bin", 0.74, 2.85);
         ("gnumake-4.4.1", 0.43, 1.79);
         ("gnused-4.9", 0.22, 0.81);
         ("perl-5.40.0", 9.12, 55.43);
         ("stdenv-linux", 0.01, 0.02);
       ])

let total f =
  List.fold_left (fun acc s -> acc +. mebibytes (f s)) 0. substitutes

let download_total = total (fun s -> s.download)
let nar_total = total (fun s -> s.nar)

(* The fetch whose first try fails. *)
let flaky = "perl-5.40.0"

let fetched name =
  (List.find (fun s -> String.equal s.name name) substitutes).path

let bash = fetched "bash-5.2p37"
let coreutils = fetched "coreutils-9.5"

type log = Phase of string | Line of string
type build = { drv : string; name : string; log : log list }

let tarball =
  let name = "hello-2.12.1.tar.gz" in
  {
    drv = store (name ^ ".drv");
    name;
    log =
      [
        Line "trying https://ftpmirror.gnu.org/hello/hello-2.12.1.tar.gz";
        Line
          "  % Total    % Received % Xferd  Average Speed   Time    Time     \
           Time  Current";
        Line
          "                                 Dload  Upload   Total   Spent    \
           Left  Speed";
        Line
          "100   146  100   146    0     0    412      0 --:--:-- --:--:-- \
           --:--:--   412";
        Line
          "100 1009k  100 1009k    0     0  1373k      0 --:--:-- --:--:-- \
           --:--:-- 4218k";
      ];
  }

let stdenv ~name ~configure ~build ~check ~bin =
  let out = store name in
  {
    drv = store (name ^ ".drv");
    name;
    log =
      [
        Phase "unpackPhase";
        Line (Fmt.str "unpacking source archive %s" (store (name ^ ".tar.gz")));
        Line (Fmt.str "source root is %s" name);
        Phase "patchPhase";
        Phase "configurePhase";
        Line (Fmt.str "configure flags: --prefix=%s" out);
      ]
      @ List.map (fun l -> Line l) configure
      @ [
          Phase "buildPhase";
          Line (Fmt.str "build flags: SHELL=%s/bin/bash" bash);
        ]
      @ List.map (fun l -> Line l) build
      @ (match check with
        | [] -> []
        | _ -> Phase "checkPhase" :: List.map (fun l -> Line l) check)
      @ [
          Phase "installPhase";
          Line (Fmt.str "install flags: SHELL=%s/bin/bash install" bash);
          Phase "fixupPhase";
          Line
            (Fmt.str "shrinking RPATHs of ELF executables and libraries in %s"
               out);
          Line
            (Fmt.str "stripping (with command strip and flags -S -p) in  %s/%s"
               out bin);
        ];
  }

let zlib =
  stdenv ~name:"zlib-1.3.1"
    ~configure:
      [
        "Checking for gcc...";
        "Checking for shared library support...";
        "Building shared library libz.so.1.3.1 with gcc.";
      ]
    ~build:
      (List.map
         (fun f ->
           Fmt.str
             "gcc -O2 -D_LARGEFILE64_SOURCE=1 -DHAVE_HIDDEN -I. -c -o %s.o %s.c"
             f f)
         [ "adler32"; "crc32"; "deflate"; "inflate"; "inftrees"; "trees" ])
    ~check:[] ~bin:"lib"

let hello =
  let cc f =
    Fmt.str
      "gcc -DHAVE_CONFIG_H -I. -I./lib -Ilib -I./src -Isrc -g -O2 -c -o %s.o \
       %s.c"
      f f
  in
  stdenv ~name:"hello-2.12.1"
    ~configure:
      [
        Fmt.str "checking for a BSD-compatible install... %s/bin/install -c"
          coreutils;
        "checking whether build environment is sane... yes";
        "checking for gcc... gcc";
        "checking whether the C compiler works... yes";
        "config.status: creating Makefile";
      ]
    ~build:
      ([
         "make  all-recursive";
         "make[1]: Entering directory '/build/hello-2.12.1'";
       ]
      @ List.map cc
          [
            "lib/c-ctype";
            "lib/exitfail";
            "lib/progname";
            "lib/xalloc-die";
            "src/hello";
            "src/system";
          ]
      @ [ "gcc -g -O2 -o hello src/hello.o src/system.o ./lib/libhello.a" ])
    ~check:
      [
        "PASS: tests/greeting-1";
        "PASS: tests/greeting-2";
        "PASS: tests/hello-1";
      ]
    ~bin:"bin"

let builds = [ tarball; zlib; hello ]
let warning = View.styled Style.(bold + fg Color.magenta) (View.text "warning:")
let error = View.styled Style.(bold + fg Color.red) (View.text "error:")

let pp_plan ppf () =
  let path p = Fmt.pf ppf "  %s@." p in
  Fmt.pf ppf "%a Git tree '/home/alice/src/hello' is dirty@." View.pp warning;
  Fmt.pf ppf "these %d derivations will be built:@." (List.length builds);
  List.iter (fun b -> path b.drv) builds;
  Fmt.pf ppf
    "these %d paths will be fetched (%.2f MiB download, %.2f MiB unpacked):@."
    (List.length substitutes) download_total nar_total;
  List.iter (fun s -> path s.path) substitutes

(* {1 The status line} *)

(* A top task per kind, each activity a child. *)
type kind = Build | Copy | Download

let kind_label = function
  | Build -> "builds"
  | Copy -> "copies"
  | Download -> "downloads"

let activities root kind =
  match
    List.find_opt
      (fun t -> String.equal (Task.label t) (kind_label kind))
      (Task.children root)
  with
  | Some t -> Task.children t
  | None -> []

let running = Style.(bold + fg Color.blue)
let finished = Style.(bold + fg Color.green)
let number pp style x = View.styled style (View.text (Fmt.str "%a" pp x))

(* [running/done/expected], each part shown as Nix shows it. *)
let numbers pp ~running:r ~done_ expected =
  let slash = View.text "/" and plain = number pp Style.none expected in
  if r > 0. then
    View.concat ~sep:slash
      [ number pp running r; number pp finished done_; plain ]
  else if Float.equal done_ expected then
    if done_ > 0. then number pp finished done_ else plain
  else View.concat [ number pp finished done_; slash; plain ]

let pp_count = Fmt.of_to_string (Fmt.str "%.0f")
let pp_mib = Fmt.of_to_string (Fmt.str "%.1f")

let counters root =
  let tasks kind state =
    float
      (List.length
         (List.filter (fun t -> Task.state t = state) (activities root kind)))
  in
  let received kind =
    List.fold_left
      (fun acc t ->
        match Task.progress t with
        | `Bytes (n, _) -> acc +. mebibytes n
        | _ -> acc)
      0. (activities root kind)
  in
  let count kind expected =
    numbers pp_count ~running:(tasks kind `Running)
      ~done_:(tasks kind `Succeeded) (float expected)
  in
  let built = View.(count Build (List.length builds) ++ text " built") in
  let copied =
    View.(
      count Copy (List.length substitutes)
      ++ text " copied ("
      ++ numbers pp_mib ~running:0. ~done_:(received Copy) nar_total
      ++ text " MiB)")
  in
  let downloaded =
    let n = received Download in
    if Float.equal n 0. then []
    else [ View.(number pp_mib finished n ++ text " MiB DL") ]
  in
  View.(
    text "["
    ++ concat ~sep:(text ", ") ([ built; copied ] @ downloaded)
    ++ text "]")

(* The latest build or fetch that has run for 10 ms: a download shows through
   the fetch it belongs to. *)
let latest root =
  let pick best (kind, t) =
    if Task.state t <> `Running || Task.elapsed t < 0.01 then best
    else
      match best with
      | Some (_, b) when Task.elapsed b < Task.elapsed t -> best
      | _ -> Some (kind, t)
  in
  let tagged kind = List.map (fun t -> (kind, t)) (activities root kind) in
  List.fold_left pick None (tagged Copy @ tagged Build)

let phase_prefix = "Running phase: "

let phase lines =
  let n = String.length phase_prefix in
  List.fold_left
    (fun acc l ->
      if String.starts_with ~prefix:phase_prefix l then
        Some (String.sub l n (String.length l - n))
      else acc)
    None lines

let activity kind t =
  let name = View.styled Style.bold (View.text (Task.label t)) in
  match kind with
  | Build -> View.(text "building " ++ name)
  | Copy | Download ->
      View.(text "fetching " ++ name ++ text (" from " ^ cache))

let status root =
  View.fit @@ fun room ->
  if Task.state root <> `Running then View.empty
  else
    let line =
      match latest root with
      | None -> counters root
      | Some (kind, t) ->
          let lines = Task.output t in
          let phase =
            match phase lines with
            | None -> View.empty
            | Some p -> View.text (Fmt.str " (%s)" p)
          in
          let last =
            match List.rev lines with
            | l :: _ -> View.text (": " ^ l)
            | [] -> View.empty
          in
          View.(counters root ++ text " " ++ activity kind t ++ phase ++ last)
    in
    match Room.width room with Some w -> View.truncate w line | None -> line

(* Off a terminal, a build's log as [nix build -L] prints it. *)
let plain e =
  match (Task.Event.kind e, Task.Event.task e, Task.Event.text e) with
  | `Output, Some (_, [ top; name ]), Some line
    when String.equal top (kind_label Build) ->
      Some View.(styled Style.faint (text (name ^ "> ")) ++ text line)
  | _ -> None

(* {1 The work} *)

let wrapped v =
  View.fit @@ fun room ->
  match Room.width room with Some w -> View.vcat (View.wrap w v) | None -> v

let fetch clock ~copies ~downloads (s : substitute) =
  Task.run copies s.name @@ fun copy ->
  let url =
    Fmt.str "%s/nar/%s.nar.xz" cache
      (String.sub (Filename.basename s.path) 0 32)
  in
  Task.run downloads url @@ fun dl ->
  if String.equal s.name flaky then begin
    Console_eio.print
      (wrapped
         View.(
           warning ++ text " " ++ error
           ++ text
                (Fmt.str
                   " unable to download '%s': HTTP error 500; retrying in 283 \
                    ms"
                   url)));
    Eio.Time.Mono.sleep clock 0.283
  end;
  let steps = 1 + Int64.to_int (Int64.div s.download (bytes 2.)) in
  for i = 0 to steps do
    let part n = Int64.(div (mul n (of_int i)) (of_int steps)) in
    Task.set_bytes dl ~count:(part s.download) ~total:s.download;
    Task.set_bytes copy ~count:(part s.nar) ~total:s.nar;
    Eio.Time.Mono.sleep clock 0.1
  done

let build clock ~builds (b : build) =
  Task.run builds b.name @@ fun t ->
  List.iter
    (function
      | Phase p ->
          Task.log t (phase_prefix ^ p);
          Eio.Time.Mono.sleep clock 0.05
      | Line l ->
          Task.log t l;
          Eio.Time.Mono.sleep clock 0.15)
    b.log

let () =
  Console_tty.setup ();
  Fmt.epr "%a" pp_plan ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~view:status ~commit:(fun _ -> None) ~plain
  @@ fun root ->
  Task.run root (kind_label Build) @@ fun builds ->
  Task.run root (kind_label Copy) @@ fun copies ->
  Task.run root (kind_label Download) @@ fun downloads ->
  Eio.Fiber.both
    (fun () ->
      Eio.Fiber.List.iter ~max_fibers:4
        (fetch clock ~copies ~downloads)
        substitutes)
    (fun () ->
      Eio.Time.Mono.sleep clock 0.8;
      build clock ~builds tarball);
  build clock ~builds zlib;
  build clock ~builds hello
