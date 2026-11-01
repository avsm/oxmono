(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [uv sync] in a fresh project: resolve, download the wheels side by side,
   install, then list what was installed. Each phase is a run of its own,
   whose rows go when it ends and leave one line in the scrollback; piped
   through [cat], those lines are printed, and a line for each large download
   as it starts and ends. *)

open Console

type package = { name : string; version : string; size : float }

let mib = 1048576.

let installed =
  [
    { name = "six"; version = "1.16.0"; size = 0.011 *. mib };
    { name = "cycler"; version = "0.12.1"; size = 0.008 *. mib };
    { name = "packaging"; version = "24.1"; size = 0.052 *. mib };
    { name = "pyparsing"; version = "3.2.0"; size = 0.104 *. mib };
    { name = "python-dateutil"; version = "2.9.0.post0"; size = 0.22 *. mib };
    { name = "contourpy"; version = "1.3.0"; size = 0.30 *. mib };
    { name = "tzdata"; version = "2024.2"; size = 0.34 *. mib };
    { name = "pytz"; version = "2024.2"; size = 0.49 *. mib };
    { name = "kiwisolver"; version = "1.4.7"; size = 1.21 *. mib };
    { name = "pillow"; version = "11.0.0"; size = 4.24 *. mib };
    { name = "fonttools"; version = "4.54.1"; size = 4.63 *. mib };
    { name = "matplotlib"; version = "3.9.2"; size = 7.83 *. mib };
    { name = "pandas"; version = "2.2.3"; size = 12.07 *. mib };
    { name = "numpy"; version = "2.1.2"; size = 15.33 *. mib };
  ]

(* The lock file is universal: it also pins the dev group and the wheels of
   other platforms. *)
let resolved =
  List.map (fun p -> p.name ^ "==" ^ p.version) installed
  @ String.split_on_char ' '
      "attrs==24.2.0 cfgv==3.4.0 colorama==0.4.6 coverage==7.6.4 \
       distlib==0.3.9 exceptiongroup==1.2.2 filelock==3.16.1 \
       hypothesis==6.115.5 identify==2.6.1 iniconfig==2.0.0 mypy==1.13.0 \
       mypy-extensions==1.0.0 nodeenv==1.9.1 platformdirs==4.3.6 pluggy==1.5.0 \
       pre-commit==4.0.1 pytest==8.3.3 pytest-cov==5.0.0 pyyaml==6.0.2 \
       ruff==0.7.1 sortedcontainers==2.4.0 tomli==2.0.2 \
       typing-extensions==4.12.2 virtualenv==20.27.0 pandas-stubs==2.2.3 \
       types-pytz==2024.2.0 matplotlib-inline==0.1.7 demo==0.1.0"

let muted s = View.role `Muted (View.text s)

(* As uv writes a duration: [126ms], [1.84s], [1m 05s]. *)
let duration s =
  if s >= 60. then
    Fmt.str "%dm %02ds" (int_of_float s / 60) (int_of_float s mod 60)
  else if s >= 1. then Fmt.str "%.2fs" s
  else Fmt.str "%dms" (int_of_float (Float.round (s *. 1000.)))

(* [Resolved 42 packages in 126ms], muted, the count in bold. *)
let phase_done verb n elapsed =
  Fmt.epr "%a@." View.pp
    View.(
      role `Muted
        (text (verb ^ " ")
        ++ styled Style.bold (Fmt.kstr text "%d packages" n)
        ++ text (" in " ^ duration elapsed)))

(* A phase's row: a spinner, what it does, and how far it is. *)
let phase =
  Progress.Layout.(
    spinner
    ++ const (View.text " ")
    ++ custom (fun t ->
        match Task.progress t with
        | `Items (n, total) ->
            View.(muted (Task.label t) ++ Fmt.kstr text " (%d/%d)" n total)
        | `Spin | `Bytes _ -> muted (Task.label t)))

(* A download's row: its name, dim, its bar, and its bytes as uv writes them,
   [3.20 MiB/15.33 MiB]. *)
let download =
  Progress.Layout.(
    label ~style:Style.faint ()
    ++ const (View.text " ")
    ++ bar ~width:30 ()
    ++ const (View.text " ")
    ++ custom (fun t ->
        match Task.progress t with
        | `Bytes (n, total) ->
            Fmt.kstr View.text "%10s/%a"
              (Fmt.str "%a" Progress.pp_bytes n)
              Progress.pp_bytes total
        | `Spin | `Items _ -> View.empty))

(* Each row as its task counts. A download that ends leaves at once, as
   indicatif's [finish_and_clear] does: it is transient. *)
let layout =
  Progress.Layout.choose (fun t ->
      match Task.progress t with `Bytes _ -> download | _ -> phase)

let large p = p.size > mib

(* Off a terminal, uv names each large download as it starts and ends. *)
let plain e =
  match (Task.Event.kind e, Task.Event.task e) with
  | `Start, Some (_, [ _; name ]) ->
      List.find_opt (fun p -> p.name = name && large p) installed
      |> Option.map (fun p ->
          View.(
            role `Accent (styled Style.bold (text "Downloading"))
            ++ Fmt.kstr text " %s " name
            ++ muted (Fmt.str "(%a)" Progress.pp_bytes (Int64.of_float p.size))))
  | `End, Some (_, [ _; name ])
    when List.exists (fun p -> p.name = name && large p) installed ->
      Some
        View.(
          text " "
          ++ role `Succeeded (styled Style.bold (text "Downloaded"))
          ++ Fmt.kstr text " %s" name)
  | _ -> None

let resolve clock root =
  Task.run ~transient:true root "Resolving dependencies..." @@ fun t ->
  List.iter
    (fun name ->
      Task.set_label t name;
      Eio.Time.Mono.sleep clock 0.003)
    resolved;
  Task.elapsed t

let fetch clock phase_task i p =
  Task.run ~transient:true phase_task p.name @@ fun t ->
  let total = Int64.of_float p.size in
  (* Three mirrors, of 6, 8 and 10 MiB/s. *)
  let speed = (6. +. float (i mod 3 * 2)) *. mib in
  let rec go done_ =
    let count = Int64.of_float (Float.min p.size done_) in
    Task.set_bytes t ~count ~total;
    if done_ < p.size then begin
      Eio.Time.Mono.sleep clock 0.05;
      go (done_ +. (speed *. 0.05))
    end
  in
  go 0.

let prepare clock root =
  Task.run ~transient:true root "Preparing packages..." @@ fun t ->
  let n = List.length installed in
  let finished = ref 0 in
  Task.set_items t ~count:0 ~total:n;
  Eio.Fiber.List.iter
    (fun (i, p) ->
      fetch clock t i p;
      incr finished;
      Task.set_items t ~count:!finished ~total:n)
    (List.mapi (fun i p -> (i, p)) installed);
  Task.elapsed t

let install clock root =
  Task.run ~transient:true root "Installing wheels..." @@ fun t ->
  let n = List.length installed in
  List.iteri
    (fun i _ ->
      Task.set_items t ~count:i ~total:n;
      Eio.Time.Mono.sleep clock 0.002)
    installed;
  Task.elapsed t

let added p =
  View.(
    role `Succeeded (text "+")
    ++ text " "
    ++ styled Style.bold (text p.name)
    ++ muted ("==" ^ p.version))

(* uv's bars and spinners are green. *)
let theme = Theme.v ~name:"uv" ~palette:[ Color.green ] ()

let () =
  Console_tty.setup ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  let run phase = Console_eio.run ~clock ~layout ~plain (phase clock) in
  let accent s = View.role `Accent (View.text s) in
  Fmt.epr "Using %a@." View.pp (accent "CPython 3.12.7");
  Fmt.epr "Creating virtual environment at: %a@." View.pp (accent ".venv");
  phase_done "Resolved" (List.length resolved) (run resolve);
  phase_done "Prepared" (List.length installed) (run prepare);
  phase_done "Installed" (List.length installed) (run install);
  List.iter
    (fun p -> Fmt.epr " %a@." View.pp (added p))
    (List.sort (fun a b -> String.compare a.name b.name) installed)
