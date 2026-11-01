(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A music player in the style of Winamp 2, drawn as a live display: the
   elapsed time in LCD digits, the title scrolling past, a spectrum analyser
   whose peaks fall slowly, the seek bar and the playlist. The music is
   synthetic: each band of the spectrum is a few sines and seeded noise, so
   every run draws the same frames. It plays the five seconds of the first
   track and three of the second, then stops. *)

open Console

let rgb = Color.rgb
let green = rgb 0 232 0
let white = rgb 255 255 255
let blue = rgb 0 0 198

(* Winamp's default analyser colours (viscolor.txt), from the bottom up. *)
let viscolor =
  [
    rgb 24 132 8;
    rgb 41 206 16;
    rgb 148 222 33;
    rgb 222 165 24;
    rgb 214 90 0;
    rgb 239 49 16;
  ]

let theme =
  Theme.v ~name:"winamp" ~accent:green ~palette:viscolor
    ~muted:(rgb 150 150 150) ~background:(rgb 41 41 57) ~border:Border.single
    ~header:Style.(bold + fg (rgb 214 214 222))
    ()

type track = {
  artist : string;
  title : string;
  length : int;
  bpm : float;
  energy : float;
}

let track ~bpm ~energy artist title length =
  { artist; title; length; bpm; energy }

let playlist =
  [|
    track ~bpm:120. ~energy:0.8 "DJ Mike Llama" "Llama Whippin' Intro" 5;
    track ~bpm:128. ~energy:1.0 "Neon Arcade" "Midnight Overdrive" 222;
    track ~bpm:96. ~energy:0.7 "Kassette" "Chrome Sunset" 251;
    track ~bpm:110. ~energy:0.8 "Pixel Harbour" "Dial-Up Romance" 198;
    track ~bpm:140. ~energy:0.9 "The Modems" "56k Heartbeat" 187;
  |]

let bands = 25
let rows = 7

type player = {
  mutable current : int;
  mutable ticks : int;
  mutable rng : Random.State.t;
  level : float array;
  peak : float array;
  fall : float array;
}

let player =
  {
    current = 0;
    ticks = 0;
    rng = Random.State.make [| 0 |];
    level = Array.make bands 0.;
    peak = Array.make bands 0.;
    fall = Array.make bands 0.;
  }

(* {1 The music} *)

(* The loudness of a band at time [t]: a kick on each beat in the bass, a
   few sines across the bands, less treble than bass, and noise. *)
let loudness track rng band t =
  let x = float band /. float (bands - 1) and f = float band in
  let beat = 60. /. track.bpm in
  let kick = exp (-6. *. Float.rem t beat /. beat) *. ((1. -. x) ** 2.) in
  let tone =
    (0.25 *. sin (((1.3 +. (0.21 *. f)) *. t) +. f))
    +. (0.15 *. sin (((2.9 +. (0.07 *. f)) *. t) +. (2. *. f)))
  in
  let noise = Random.State.float rng 0.24 -. 0.12 in
  (track.energy *. (0.7 -. (0.4 *. x) +. tone +. (0.45 *. kick))) +. noise

(* The player moves on ten times a second, as the display redraws. *)
let rate = 10
let elapsed () = float player.ticks /. float rate
let seconds () = player.ticks / rate

(* Bars rise at once and drop fast; a peak stays, then falls ever faster. *)
let tick () =
  let p = player in
  p.ticks <- p.ticks + 1;
  let track = playlist.(p.current) in
  for b = 0 to bands - 1 do
    let v = Float.min 1. (Float.max 0. (loudness track p.rng b (elapsed ()))) in
    p.level.(b) <- Float.max v (p.level.(b) -. 0.15);
    if p.level.(b) >= p.peak.(b) then (
      p.peak.(b) <- p.level.(b);
      p.fall.(b) <- 0.)
    else (
      p.fall.(b) <- p.fall.(b) +. 0.006;
      p.peak.(b) <- Float.max p.level.(b) (p.peak.(b) -. p.fall.(b)))
  done

let play n =
  player.current <- n;
  player.ticks <- 0;
  player.rng <- Random.State.make [| n |]

(* {1 Drawing} *)

let mmss s = Fmt.str "%d:%02d" (s / 60) (s mod 60)
let paint c s = View.styled (Style.fg c) (View.text s)
let lit s = paint green s
let off s = View.role `Muted (View.text s)
let glyph room utf_8 ascii = if Room.utf_8 room then utf_8 else ascii

(* Each band is a bar two cells wide, its peak a line above it, and as many
   bands show as fit. A bar takes the theme's palette up its height, as
   Winamp colours its analyser. *)
let spectrum room =
  let shown =
    match Room.width room with
    | Some w -> min bands ((w + 1) / 3)
    | None -> bands
  in
  View.sized ~height:rows
    (View.hcat ~gap:1
       (List.init shown (fun b ->
            Bar.vertical ~width:2 ~peak:player.peak.(b) player.level.(b))))

let digit = function
  | '0' -> [| "█▀█"; "█ █"; "▀▀▀" |]
  | '1' -> [| " ▀█"; "  █"; "  ▀" |]
  | '2' -> [| "▀▀█"; "█▀▀"; "▀▀▀" |]
  | '3' -> [| "▀▀█"; " ▀█"; "▀▀▀" |]
  | '4' -> [| "█ █"; "▀▀█"; "  ▀" |]
  | '5' -> [| "█▀▀"; "▀▀█"; "▀▀▀" |]
  | '6' -> [| "█▀▀"; "█▀█"; "▀▀▀" |]
  | '7' -> [| "▀▀█"; "  █"; "  ▀" |]
  | '8' -> [| "█▀█"; "█▀█"; "▀▀▀" |]
  | '9' -> [| "█▀█"; "▀▀█"; "▀▀▀" |]
  | ':' -> [| "▄"; "▄"; " " |]
  | _ -> [| " "; " "; " " |]

(* The elapsed time in seven-segment digits, three lines tall. *)
let lcd room =
  let time = Fmt.str "%02d:%02d" (seconds () / 60) (seconds () mod 60) in
  if not (Room.utf_8 room) then lit time
  else
    let chars = List.init (String.length time) (String.get time) in
    View.vcat
      (List.init 3 (fun r ->
           lit (String.concat " " (List.map (fun c -> (digit c).(r)) chars))))

(* The title as Winamp scrolls it: one cell further at each step, round
   and round. *)
let marquee width title offset =
  let n = View.width title in
  let ring = View.concat (List.init ((width / n) + 2) (fun _ -> title)) in
  View.crop ~left:(offset mod n) ~width ring

let info room =
  let track = playlist.(player.current) in
  let title =
    Fmt.str "%d. %s - %s (%s)  ***  " (player.current + 1) track.artist
      track.title (mmss track.length)
  in
  let width = Option.value (Room.width room) ~default:(String.length title) in
  View.vcat
    [
      marquee width (lit title) (player.ticks / 2);
      View.concat
        [
          lit "128";
          off " kbps  ";
          lit "44";
          off " kHz  ";
          off "mono ";
          lit "stereo";
        ];
      View.hcat ~gap:1
        [
          off "VOL";
          Bar.horizontal ~width:18 0.78;
          off " BAL";
          View.concat
            [
              off (glyph room "─────" "-----");
              lit (glyph room "┃" "|");
              off (glyph room "─────" "-----");
            ];
        ];
    ]

let buttons room =
  let g = glyph room in
  View.hcat ~gap:10
    [
      View.hcat ~gap:2
        [
          off (g "|◀" "|<");
          View.styled Style.bold (lit (g "▶" ">"));
          off (g "‖" "||");
          off (g "■" "[]");
          off (g "▶|" ">|");
          off (g "▲" "^");
        ];
      View.hcat ~gap:2 [ off "SHUFFLE"; lit "REPEAT" ];
      View.hcat ~gap:2 [ off "EQ"; lit "PL" ];
    ]

(* The screens of the player, black on its grey case. *)
let screen f =
  View.fit (fun room ->
      View.styled
        (Style.bg (rgb 0 0 0))
        (View.place ?width:(Room.width room) (f room)))

let main_window =
  let top room =
    View.hcat ~gap:3
      [
        View.place ~width:2 (lit (glyph room "▶" ">"));
        View.place ~width:17 ~height:3 (lcd room);
        View.fit info;
      ]
  in
  let seek _ =
    Bar.horizontal
      ~style:(Style.fg (rgb 222 165 24))
      (elapsed () /. float playlist.(player.current).length)
  in
  Panel.view ~expand:true ~title:(View.text "WINAMP")
    (View.vcat
       [
         screen top;
         View.text " ";
         screen spectrum;
         View.fit seek;
         View.fit buttons;
       ])

(* The playing track is marked as well as highlighted, so that it shows
   without colour too. *)
let playlist_window =
  let entry room i t =
    let width = Option.value (Room.width room) ~default:60 in
    let playing = i = player.current in
    let line =
      View.hcat ~gap:1
        [
          View.text (if playing then glyph room "▶" ">" else " ");
          View.place ~width:(width - 8)
            (View.text (Fmt.str "%d. %s - %s" (i + 1) t.artist t.title));
          View.place ~align:`Right ~width:5 (View.text (mmss t.length));
        ]
    in
    if playing then View.styled Style.(fg white + bg blue) line
    else View.styled (Style.fg green) line
  in
  let sum tracks = Array.fold_left (fun n t -> n + t.length) 0 tracks in
  let total _ =
    let played = sum (Array.sub playlist 0 player.current) + seconds () in
    View.text (Fmt.str "%s/%s" (mmss played) (mmss (sum playlist)))
  in
  Panel.view ~expand:true
    ~title:(View.text "WINAMP PLAYLIST")
    ~subtitle:(View.fit total)
    (View.fit (fun room ->
         View.vcat (Array.to_list (Array.mapi (entry room) playlist))))

let view _root =
  View.sized ~width:78 (View.vcat [ main_window; playlist_window ])

let () =
  Console_tty.setup ~theme ();
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~view (fun _root ->
      List.iter
        (fun (n, seconds) ->
          play n;
          for _ = 1 to seconds * rate do
            tick ();
            Eio.Time.Mono.sleep clock (1. /. float rate)
          done)
        [ (0, 5); (1, 3) ])
