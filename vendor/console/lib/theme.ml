(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type role = [ `Accent | `Succeeded | `Warning | `Failed | `Muted | `Header ]

type mark =
  [ `Running
  | `Succeeded
  | `Failed
  | `Cancelled
  | `Info
  | `Warning
  | `Error
  | `Debug ]

type bar = [ `Line | `Block | `Ascii ]
type motion = [ `Rain | `Sweep | `Shimmer ]

type t = {
  name : string;
  accent : Color.t;
  palette : Color.t list;
  succeeded : Color.t;
  warning : Color.t;
  failed : Color.t;
  muted : Color.t option;
  header : Style.t;
  background : Color.t option;
  border : Border.t;
  motion : motion option;
  guide : Guide.t;
  spinner : string list;
  bar : bar;
  marks : mark -> string;
}

let name t = t.name
let accent t = t.accent
let palette t = t.palette
let succeeded t = t.succeeded
let warning t = t.warning
let failed t = t.failed
let muted t = t.muted
let header t = t.header
let background t = t.background
let border t = t.border
let motion t = t.motion
let guide t = t.guide
let spinner t = t.spinner
let bar t = t.bar

(* A mark is drawn on one line, as text. *)
let mark t m = Console_text.sanitize ~keep_newlines:false (t.marks m)

let unicode_marks = function
  | `Running -> "\xe2\x80\xa2"
  | `Succeeded -> "\xe2\x9c\x93"
  | `Failed | `Error -> "\xe2\x9c\x97"
  | `Cancelled -> "\xe2\x97\x8b"
  | `Info -> "\xe2\x80\xba"
  | `Warning -> "!"
  | `Debug -> "\xc2\xb7"

let ascii_marks = function
  | `Running -> "[-]"
  | `Succeeded -> "[+]"
  | `Failed -> "[x]"
  | `Cancelled -> "[~]"
  | `Info -> "#"
  | `Warning -> "!"
  | `Error -> "x"
  | `Debug -> "."

(* Entries of the xterm 256-colour palette whose contrast is 3:1 or more on
   black and on white alike, for a terminal whose background is unknown. *)
let teal = Color.ansi_256 30
let green = Color.ansi_256 28
let amber = Color.ansi_256 136
let red = Color.ansi_256 160
let blue = Color.ansi_256 63
let magenta = Color.ansi_256 164

(* The colours of the terminal's own palette, which its user chose to read
   on its background, and its text made faint. *)
let default =
  {
    name = "default";
    accent = Color.cyan;
    palette = [ Color.cyan ];
    succeeded = Color.green;
    warning = Color.yellow;
    failed = Color.red;
    muted = None;
    header = Style.bold;
    background = None;
    border = Border.rounded;
    motion = None;
    guide = Guide.single;
    spinner = Frames.braille;
    bar = `Line;
    marks = unicode_marks;
  }

let v ?(name = "custom") ?(accent = default.accent) ?palette
    ?(succeeded = default.succeeded) ?(warning = default.warning)
    ?(failed = default.failed) ?muted ?(header = default.header) ?background
    ?(border = default.border) ?motion ?(guide = default.guide)
    ?(spinner = default.spinner) ?(bar = default.bar) ?(marks = default.marks)
    () =
  let palette =
    match palette with None | Some [] -> [ accent ] | Some p -> p
  in
  let spinner = Frames.v "Theme.v" spinner in
  let muted = match muted with None -> default.muted | m -> m in
  {
    name;
    accent;
    palette;
    succeeded;
    warning;
    failed;
    muted;
    header;
    background;
    border;
    motion;
    guide;
    spinner;
    bar;
    marks;
  }

let ascii =
  v ~name:"ascii" ~border:Border.ascii ~guide:Guide.ascii ~spinner:Frames.ascii
    ~bar:`Ascii ~marks:ascii_marks ()

let dos_amber = Color.rgb 0xff 0xb0 0x00

let dos =
  v ~name:"dos" ~accent:dos_amber ~succeeded:dos_amber ~warning:dos_amber
    ~failed:(Color.rgb 0xff 0x55 0x55) ~muted:(Color.rgb 0xa1 0x6c 0x00)
    ~header:Style.(bold + fg dos_amber)
    ~background:Color.black ~border:Border.dos ~guide:Guide.single
    ~spinner:Frames.ascii ~bar:`Block ~marks:ascii_marks ()

let matrix_green = Color.rgb 0x33 0xff 0x33

let matrix =
  v ~name:"matrix" ~accent:matrix_green ~succeeded:matrix_green
    ~warning:(Color.rgb 0xcc 0xff 0x66) ~failed:(Color.rgb 0xff 0x33 0x33)
    ~muted:(Color.rgb 0x1c 0x89 0x1c)
    ~header:Style.(bold + fg matrix_green)
    ~background:Color.black ~border:Border.single ~motion:`Rain ()

let rainbow =
  v ~name:"rainbow" ~accent:magenta
    ~palette:[ red; amber; green; teal; blue; magenta ]
    ~motion:`Sweep ()

let still t = { t with motion = None; spinner = Frames.still }

(* The colours of each scheme are taken from its upstream source, named
   beside it, and degrade to 256 or 16 colours as a terminal's depth asks.
   Where a scheme's colour is below its contrast on its own background,
   4.5:1 for the colours text takes (the main colour, the roles and the muted
   colour) and 3:1 for the palette, it is moved to just above it: the muted
   colour towards the scheme's text, the others by their lightness alone,
   their hue and saturation kept. Headers are bold, in the accent unless
   [header] names another colour, and frames shimmer in the accent. *)

(* A colour written in a theme's source is valid. *)
let hex s =
  match Color.of_hex s with Ok c -> c | Error (`Msg m) -> invalid_arg m

let scheme ~name ~accent ~succeeded ~warning ~failed ~muted ?(header = accent)
    ~background () =
  v ~name ~accent:(hex accent) ~succeeded:(hex succeeded) ~warning:(hex warning)
    ~failed:(hex failed) ~muted:(hex muted)
    ~header:Style.(bold + fg (hex header))
    ~background:(hex background) ~motion:`Shimmer ()

(* Dracula, by Zeno Rocha (MIT): draculatheme.com/spec. *)
let dracula =
  scheme ~name:"dracula" ~accent:"#bd93f9" ~succeeded:"#50fa7b"
    ~warning:"#f1fa8c" ~failed:"#ff5555" ~muted:"#8692b7" ~background:"#282a36"
    ()

(* Nord, by Sven Greb (MIT): nordtheme/nord src/nord.css. *)
let nord =
  scheme ~name:"nord" ~accent:"#88c0d0" ~succeeded:"#a3be8c" ~warning:"#ebcb8b"
    ~failed:"#cf8a90" ~muted:"#969ead" ~background:"#2e3440" ()

(* Solarized, by Ethan Schoonover (MIT): altercation/solarized. *)
let solarized_dark =
  scheme ~name:"solarized-dark" ~accent:"#3094da" ~succeeded:"#859900"
    ~warning:"#b58900" ~failed:"#e56563" ~muted:"#839496" ~header:"#93a1a1"
    ~background:"#002b36" ()

(* Solarized, by Ethan Schoonover (MIT): altercation/solarized. *)
let solarized_light =
  scheme ~name:"solarized-light" ~accent:"#2075b1" ~succeeded:"#687700"
    ~warning:"#8e6b00" ~failed:"#d82824" ~muted:"#5e7379" ~header:"#586e75"
    ~background:"#fdf6e3" ()

(* gruvbox, by Pavel Pertsev (MIT/X11): morhetz/gruvbox colors/gruvbox.vim. *)
let gruvbox_dark =
  scheme ~name:"gruvbox-dark" ~accent:"#fe8019" ~succeeded:"#b8bb26"
    ~warning:"#fabd2f" ~failed:"#fb5642" ~muted:"#a89984" ~background:"#282828"
    ()

(* gruvbox, by Pavel Pertsev (MIT/X11): morhetz/gruvbox colors/gruvbox.vim. *)
let gruvbox_light =
  scheme ~name:"gruvbox-light" ~accent:"#af3a03" ~succeeded:"#74700d"
    ~warning:"#966211" ~failed:"#9d0006" ~muted:"#665c54" ~background:"#fbf1c7"
    ()

(* Catppuccin, by the Catppuccin organisation (MIT): catppuccin/palette
   palette.json 1.8.0. *)
let catppuccin_latte =
  scheme ~name:"catppuccin-latte" ~accent:"#8839ef" ~succeeded:"#327c22"
    ~warning:"#996114" ~failed:"#d20f39" ~muted:"#5c5f77" ~background:"#eff1f5"
    ()

(* Catppuccin, by the Catppuccin organisation (MIT): catppuccin/palette
   palette.json 1.8.0. *)
let catppuccin_mocha =
  scheme ~name:"catppuccin-mocha" ~accent:"#cba6f7" ~succeeded:"#a6e3a1"
    ~warning:"#f9e2af" ~failed:"#f38ba8" ~muted:"#9399b2" ~background:"#1e1e2e"
    ()

(* Tokyo Night, by enkia (MIT), night palette of folke/tokyonight.nvim
   (Apache-2.0). *)
let tokyo_night =
  scheme ~name:"tokyo-night" ~accent:"#7aa2f7" ~succeeded:"#9ece6a"
    ~warning:"#e0af68" ~failed:"#f7768e" ~muted:"#7b83ab" ~background:"#1a1b26"
    ()

(* One Dark, by GitHub (MIT): atom/atom packages/one-dark-syntax and
   packages/one-dark-ui. *)
let one_dark =
  scheme ~name:"one-dark" ~accent:"#5990ff" ~succeeded:"#98c379"
    ~warning:"#e5c07b" ~failed:"#e17179" ~muted:"#8e95a2" ~header:"#61afef"
    ~background:"#282c34" ()

(* Monokai, by Wimer Hazenberg (no licence stated): the 2007 Monokai.tmTheme. *)
let monokai =
  scheme ~name:"monokai" ~accent:"#66d9ef" ~succeeded:"#a6e22e"
    ~warning:"#e6db74" ~failed:"#fa4b8a" ~muted:"#939080" ~background:"#272822"
    ()

(* GitHub Dark, by GitHub (MIT): primer/github-vscode-theme. *)
let github_dark =
  scheme ~name:"github-dark" ~accent:"#2f81f7" ~succeeded:"#3fb950"
    ~warning:"#d29922" ~failed:"#ff7b72" ~muted:"#8b949e" ~background:"#0d1117"
    ()

(* GitHub Light, by GitHub (MIT): primer/github-vscode-theme. *)
let github_light =
  scheme ~name:"github-light" ~accent:"#0969da" ~succeeded:"#116329"
    ~warning:"#9a6700" ~failed:"#cf222e" ~muted:"#656d76" ~background:"#ffffff"
    ()

let all =
  List.map
    (fun t -> (t.name, t))
    [
      default;
      ascii;
      dos;
      matrix;
      rainbow;
      dracula;
      nord;
      solarized_dark;
      solarized_light;
      gruvbox_dark;
      gruvbox_light;
      catppuccin_latte;
      catppuccin_mocha;
      tokyo_night;
      one_dark;
      monokai;
      github_dark;
      github_light;
    ]

let of_string s =
  match List.assoc_opt s all with
  | Some t -> Ok t
  | None ->
      Fmt.error_msg "unknown theme %S, expected one of: %s" s
        (String.concat ", " (List.map fst all))

(* A theme is not a choice of colours: a formatter given one alone still has
   them decided for it. *)
let slot = Slot.v ~setting:false ~watched:true ()
let get ppf = Option.value (Slot.find slot ppf) ~default
let set ppf t = Slot.set slot ppf t

let pp_motion ppf = function
  | None -> Fmt.string ppf "none"
  | Some `Rain -> Fmt.string ppf "rain"
  | Some `Sweep -> Fmt.string ppf "sweep"
  | Some `Shimmer -> Fmt.string ppf "shimmer"

let pp ppf t =
  Fmt.pf ppf
    "@[<1>(theme %s@ accent=%a@ succeeded=%a@ warning=%a@ failed=%a@ \
     motion=%a@ marks=%a)@]"
    t.name Color.pp t.accent Color.pp t.succeeded Color.pp t.warning Color.pp
    t.failed pp_motion t.motion
    Fmt.(list ~sep:(any " ") string)
    (List.map (mark t)
       [
         `Running;
         `Succeeded;
         `Failed;
         `Cancelled;
         `Info;
         `Warning;
         `Error;
         `Debug;
       ])
