(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [brew install wget], as Homebrew draws it: the bottle manifests and the
   bottles of wget and its dependencies download side by side, each row with
   its spinner, its phase and its sizes, and a [####] bar while it downloads.
   A download that is done leaves a check mark in the scrollback. Then each
   dependency pours, and wget last. Piped through [cat], each download prints
   one line once done, as Homebrew does off a terminal. *)

open Console

type formula = {
  name : string;
  version : string;
  bottle : string;
  manifest : float;
  size : float;
  files : int;
  installed : string;
}

let formulae =
  [
    {
      name = "ca-certificates";
      version = "2025-09-09";
      bottle = "all";
      manifest = 1_984.;
      size = 131_602.;
      files = 4;
      installed = "236.8KB";
    };
    {
      name = "openssl@3";
      version = "3.6.0";
      bottle = "arm64_sequoia";
      manifest = 11_871.;
      size = 10_934_018.;
      files = 7_564;
      installed = "35.4MB";
    };
    {
      name = "libunistring";
      version = "1.4.1";
      bottle = "arm64_sequoia";
      manifest = 7_552.;
      size = 1_903_377.;
      files = 59;
      installed = "5.6MB";
    };
    {
      name = "gettext";
      version = "0.26";
      bottle = "arm64_sequoia";
      manifest = 14_620.;
      size = 9_804_312.;
      files = 2_428;
      installed = "30.0MB";
    };
    {
      name = "libidn2";
      version = "2.3.8";
      bottle = "arm64_sequoia";
      manifest = 10_316.;
      size = 297_165.;
      files = 80;
      installed = "970.4KB";
    };
    {
      name = "wget";
      version = "1.25.0";
      bottle = "arm64_sequoia";
      manifest = 9_623.;
      size = 1_571_830.;
      files = 92;
      installed = "4.6MB";
    };
  ]

let target = List.nth formulae (List.length formulae - 1)
let dependencies = List.filter (fun f -> f != target) formulae

(* Each download has a phase, as Homebrew's [Downloadable] does. *)
type phase = Preparing | Downloading | Verifying | Verified

let phase_name = function
  | Preparing -> "Preparing"
  | Downloading -> "Downloading"
  | Verifying -> "Verifying"
  | Verified -> "Verified"

type download = { message : string; bytes : float; mutable phase : phase }

let downloads =
  List.concat_map
    (fun f ->
      [
        {
          message = Fmt.str "Bottle Manifest %s (%s)" f.name f.version;
          bytes = f.manifest;
          phase = Preparing;
        };
        {
          message = Fmt.str "Bottle %s (%s)" f.name f.version;
          bytes = f.size;
          phase = Preparing;
        };
      ])
    formulae

let longest =
  List.fold_left (fun n d -> max n (String.length d.message)) 0 downloads

let find label = List.find (fun d -> d.message = label) downloads

(* As [Formatter.disk_usage_readable_size_unit]: decimal units, to one
   decimal, the unit right-aligned in two cells. *)
let size bytes =
  let rec pick size unit = function
    | next :: rest when Float.round (size *. 10.) /. 10. >= 1000. ->
        pick (size /. 1000.) next rest
    | _ -> (size, unit)
  in
  let size, unit = pick bytes "B" [ "KB"; "MB"; "GB" ] in
  Fmt.str "%5.1f%2s" size unit

(* The line of a download as [message_with_progress] draws it, in [width]
   cells: its message, then the bar while it downloads, its phase and its
   sizes against the right edge. *)
let line ~width d count =
  let available = width - 1 in
  let fetched = size count in
  let total = size (if d.phase = Verified then count else d.bytes) in
  let progress = Fmt.str " %-11s %s/%s" (phase_name d.phase) fetched total in
  let progress =
    if d.phase = Downloading then
      let length = max 4 (available - String.length progress - longest - 1) in
      let used =
        Float.to_int (Float.round (count /. d.bytes *. float length))
      in
      " " ^ String.make used '#' ^ String.make (length - used) ' ' ^ progress
    else progress
  in
  let room = available - String.length progress in
  if room <= 0 then
    String.sub d.message 0 (min available (String.length d.message))
  else
    let message =
      if String.length d.message > room then String.sub d.message 0 room
      else d.message
    in
    Fmt.str "%-*s%s" room message progress

let count t =
  match Task.progress t with
  | `Bytes (n, _) -> Int64.to_float n
  | `Spin | `Items _ -> 0.

let row t =
  View.fit (fun room ->
      let width = Option.value (Room.width room) ~default:80 in
      View.text (line ~width (find (Task.label t)) (count t)))

let layout = Progress.Layout.(spinner ++ const (View.text " ") ++ custom row)
let check = View.role `Succeeded (View.text "✔︎")

(* A download that is done stays in the scrollback, checked. *)
let commit t =
  let d = find (Task.label t) in
  Some
    (View.fit (fun room ->
         let width = Option.value (Room.width room) ~default:80 - 2 in
         let rest = View.text (line ~width d d.bytes) in
         View.(check ++ text " " ++ rest)))

(* Off a terminal, Homebrew prints each download once done, without sizes. *)
let plain e =
  match (Task.Event.kind e, Task.Event.task e) with
  | `End, Some (_, path) when path <> [] ->
      Some View.(check ++ text (" " ^ List.nth path (List.length path - 1)))
  | _ -> None

(* Several mirrors of the bottles' CDN, at 4 to 12 MB/s. *)
let fetch clock root i d =
  Task.run ~transient:true root d.message @@ fun t ->
  let speed = 4e6 +. (float (i mod 5 * 2) *. 1e6) in
  Eio.Time.Mono.sleep clock (0.05 *. float (i mod 4));
  d.phase <- Downloading;
  let rec go fetched =
    let fetched = Float.min d.bytes fetched in
    Task.set_bytes t ~count:(Int64.of_float fetched)
      ~total:(Int64.of_float d.bytes);
    if fetched < d.bytes then begin
      Eio.Time.Mono.sleep clock 0.05;
      go (fetched +. (speed *. 0.05))
    end
  in
  go 0.;
  d.phase <- Verifying;
  Eio.Time.Mono.sleep clock (0.1 +. (d.bytes /. 1e8));
  d.phase <- Verified

(* A heading as [ohai] writes it, blue, or as [oh1], green. *)
let arrow colour v =
  Fmt.pr "%a@." View.pp
    View.(styled (Style.fg colour) (text "==>") ++ text " " ++ v)

let ohai s = arrow Color.blue View.(styled Style.bold (text s))
let identifier s = View.styled (Style.fg Color.green) (View.text s)

let rec sentence = function
  | [] -> ""
  | [ a ] -> a
  | [ a; b ] -> a ^ " and " ^ b
  | a :: rest -> a ^ ", " ^ sentence rest

let pour clock f =
  ohai (Fmt.str "Pouring %s--%s.%s.bottle.tar.gz" f.name f.version f.bottle);
  Eio.Time.Mono.sleep clock (0.2 +. (f.size /. 2e7));
  Fmt.pr "🍺  /opt/homebrew/Cellar/%s/%s: %s files, %s@." f.name f.version
    (if f.files >= 1000 then
       Fmt.str "%d,%03d" (f.files / 1000) (f.files mod 1000)
     else string_of_int f.files)
    f.installed

(* Homebrew's spinner, in its blue. *)
let theme =
  Theme.v ~name:"brew" ~palette:[ Color.blue ]
    ~spinner:[ "⠋"; "⠙"; "⠚"; "⠞"; "⠖"; "⠦"; "⠴"; "⠲"; "⠳"; "⠓" ]
    ()

let () =
  Console_tty.setup ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  arrow Color.blue
    View.(
      styled Style.bold (text "Fetching downloads for: ")
      ++ identifier target.name);
  Console_eio.run ~clock ~ppf:Fmt.stdout ~layout ~commit ~plain (fun root ->
      Eio.Fiber.List.iter
        (fun (i, d) -> fetch clock root i d)
        (List.mapi (fun i d -> (i, d)) downloads));
  arrow Color.green
    View.(
      styled Style.bold
        (Fmt.kstr text "Installing dependencies for %s:" target.name)
      ++ text " "
      ++ identifier (sentence (List.map (fun f -> f.name) dependencies)));
  List.iter
    (fun f ->
      arrow Color.green
        View.(
          styled Style.bold
            (Fmt.kstr text "Installing %s dependency: " target.name)
          ++ identifier f.name);
      pour clock f)
    dependencies;
  arrow Color.green
    View.(styled Style.bold (text "Installing ") ++ identifier target.name);
  pour clock target;
  ohai (Fmt.str "Running `brew cleanup %s`..." target.name)
