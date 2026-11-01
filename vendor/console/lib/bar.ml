(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  style : Style.t option;
  track : Style.t option;
  brackets : bool;
  width : int;
  frac : float;
}

let v ?style ?track ?(brackets = false) ~width frac =
  if width < 0 then invalid_arg "Console.Bar.horizontal: negative width";
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  { style; track; brackets; width; frac }

let framed ~glyphs ~track ~brackets body =
  let left, right =
    match glyphs with `Line -> ("\u{2595}", "\u{258f}") | _ -> ("[", "]")
  in
  if brackets then
    View_core.concat
      [ View_core.ink track left; body; View_core.ink track right ]
  else body

(* The fill and the track. A heavy line goes to the half cell on a light
   one, so that without colour the fill still shows; blocks and ASCII fill
   whole cells. *)
let cells ~glyphs ~width frac =
  let whole fill track =
    let full = int_of_float (Float.round (frac *. float_of_int width)) in
    (Widget.fill full fill, Widget.fill (width - full) track)
  in
  match glyphs with
  | `Ascii -> whole "#" "-"
  | `Block -> whole "\u{2588}" "\u{2591}"
  | `Line ->
      let e = int_of_float (Float.round (frac *. float_of_int (width * 2))) in
      let full = e / 2 and half = e mod 2 = 1 in
      let rest = width - full - Bool.to_int half in
      let fill = Widget.fill full "━" ^ if half then "╸" else "" in
      (fill, Widget.fill rest "─")

(* A palette of several colours is spread along the whole bar, each filled
   cell in the colour of its place: the fill uncovers the colours as it
   grows rather than recolouring the cells it has. A palette is never
   empty. *)
let fill ~style ~width look glyphs =
  match (style, Theme.palette look.Look.theme) with
  | Some style, _ -> View_core.ink style glyphs
  | None, [] -> View_core.ink (Widget.accent look) glyphs
  | None, [ c ] -> View_core.ink (Style.fg c) glyphs
  | None, palette ->
      let last = float_of_int (max 1 (width - 1)) in
      View_core.recell
        (fun ~cell ->
          let c = View_core.color_at palette (float_of_int cell /. last) in
          Some (fun style glyph -> (Style.(style + fg c), glyph)))
        (View_core.text glyphs)

let draw t look =
  let track = Option.value t.track ~default:(Widget.muted look) in
  let glyphs = Widget.bar look in
  let filled, rest = cells ~glyphs ~width:t.width t.frac in
  framed ~glyphs ~track ~brackets:t.brackets
    View_core.(fill ~style:t.style ~width:t.width look filled ++ ink track rest)

(* A bar given no width grows to its room, ten cells at its natural size.
   Its brackets come out of its cells, and go where the room has no cell
   for the bar between them. *)
let horizontal ?style ?track ?(brackets = false) ?width frac =
  let t =
    v ?style ?track ~brackets ~width:(Option.value width ~default:10) frac
  in
  let frame = if brackets then 2 else 0 in
  View_core.deferred
    ~grow:(if width = None then 1 else 0)
    (fun c ->
      let cells =
        match (width, Ctx.width c) with
        | None, Some room -> max 0 room
        | Some w, Some room -> min (w + frame) (max 0 room)
        | _, None -> t.width + frame
      in
      let t =
        if brackets && cells >= 3 then { t with width = cells - 2 }
        else { t with width = cells; brackets = false }
      in
      View_core.drawn (draw t))

let eighths = [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

(* Off UTF-8, a cell is a blank, a dot, a colon or a hash, as it is empty,
   filled to less than half, less than seven eighths, or more. *)
let ascii_eighths = [| " "; "."; "."; "."; ":"; ":"; ":"; "#"; "#" |]

(* The eighths of each cell, from the bottom up: whole cells, then the cell
   the rest fills, then empty ones. *)
let column ~height frac =
  let total = int_of_float (Float.round (frac *. float_of_int (height * 8))) in
  List.init height (fun i -> max 0 (min 8 (total - (i * 8))))

(* A cell's colour is that of the height its fill reaches: up the theme's
   palette if it has several colours, and otherwise from the colour of a
   success through that of a warning to that of a failure, so that the
   cells of a sparkline show their values. *)
let height_style ~style look p =
  match style with
  | Some style -> style
  | None ->
      let theme = look.Look.theme in
      let stops =
        match Theme.palette theme with
        | [] | [ _ ] ->
            [ Theme.succeeded theme; Theme.warning theme; Theme.failed theme ]
        | palette -> palette
      in
      Style.fg (View_core.color_at stops p)

let vertical_cell ~style ~track ~height ~width i e look =
  let glyphs = if Widget.bar look = `Ascii then ascii_eighths else eighths in
  let glyph = Widget.fill width glyphs.(e) in
  if e = 0 then
    View_core.ink (Option.value track ~default:(Widget.muted look)) glyph
  else
    let p = (float_of_int i +. (float_of_int e /. 8.)) /. float_of_int height in
    View_core.ink (height_style ~style look p) glyph

let clamp frac =
  if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)

(* A peak is a line low or high in the cell it reaches, if the fill leaves
   that cell empty. *)
let cap ~track ~width ~high look =
  let glyph =
    if Widget.bar look = `Ascii then "-" else if high then "▔" else "▁"
  in
  View_core.ink
    (Option.value track ~default:(Widget.muted look))
    (Widget.fill width glyph)

let draw_vertical ~style ~track ~peak ~width ~height frac =
  let cells = column ~height frac in
  let filled = List.fold_left ( + ) 0 cells in
  let peak =
    let p =
      int_of_float (Float.round (clamp peak *. float_of_int (height * 8)))
    in
    let i = (p - 1) / 8 in
    if p > 0 && i < height && filled <= i * 8 then Some (i, (p - 1) mod 8 >= 4)
    else None
  in
  View_core.concat ~sep:(View_core.text "\n")
    (List.rev
       (List.mapi
          (fun i e ->
            match peak with
            | Some (j, high) when j = i ->
                View_core.drawn (cap ~track ~width ~high)
            | _ ->
                View_core.drawn (vertical_cell ~style ~track ~height ~width i e))
          cells))

(* A vertical bar is as tall as its room, one row at its natural size, and
   as wide as it is told, or as its room in a row that shares it, and never
   wider than its room. *)
let vertical ?style ?track ?(peak = 0.) ?width frac =
  (match width with
  | Some w when w < 0 -> invalid_arg "Console.Bar.vertical: negative width"
  | _ -> ());
  let frac = clamp frac in
  View_core.deferred
    ~grow:(if width = None then 1 else 0)
    (fun c ->
      let height = match Ctx.height c with Some h -> max 1 h | None -> 1 in
      let width =
        match (width, Ctx.width c) with
        | Some w, Some room -> min w (max 0 room)
        | Some w, None -> w
        | None, Some room -> max 0 room
        | None, None -> 1
      in
      draw_vertical ~style ~track ~peak ~width ~height frac)

(* A sparkline spans its values: the lowest is the least a cell shows, an
   eighth, unless [min] says lower, and the highest fills it. It is one row
   in a room of any height, and the last values that fit in a room
   narrower than it. *)
let sparkline ?style ?min ?max values =
  let lo = List.fold_left Float.min Float.infinity values
  and hi = List.fold_left Float.max Float.neg_infinity values in
  let hi = Option.value max ~default:hi in
  let lo, floor = match min with Some m -> (m, 0.) | None -> (lo, 1. /. 8.) in
  let frac v =
    if hi <= lo then if v > lo then 1. else floor
    else floor +. ((1. -. floor) *. ((v -. lo) /. (hi -. lo)))
  in
  let cell v =
    let e = int_of_float (Float.round (clamp (frac v) *. 8.)) in
    View_core.drawn (vertical_cell ~style ~track:None ~height:1 ~width:1 0 e)
  in
  let cells = List.map cell values in
  let n = List.length cells in
  View_core.deferred ~min_width:(Int.min 1 n) (fun c ->
      match Ctx.width c with
      | Some w when w < n ->
          View_core.concat (List.drop (n - Int.max 0 w) cells)
      | _ -> View_core.concat cells)

(* [x] to three significant digits and at most four decimals, without
   trailing zeros. *)
let digits x =
  let decimals =
    if x <= 0. then 0
    else min 4 (max 0 (2 - int_of_float (Float.floor (Float.log10 x))))
  in
  let s = Fmt.str "%.*f" decimals x in
  if not (String.contains s '.') then s
  else
    let rec trim n =
      match s.[n - 1] with
      | '0' -> trim (n - 1)
      | '.' -> String.sub s 0 (n - 1)
      | _ -> String.sub s 0 n
    in
    trim (String.length s)

let units = [| "k"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y" |]

(* A value as a plain number: whole below a million, to three digits
   below ten thousand, and past those in thousands, millions and so on. *)
let value v =
  if not (Float.is_finite v) then Fmt.str "%g" v
  else
    let a = Float.abs v and sign = if v < 0. then "-" else "" in
    let plain =
      if Float.is_integer v && a < 1e6 then Fmt.str "%.0f" a
      else if a < 1e4 then digits a
      else
        let rec scaled k =
          let x = a /. (1000. ** float_of_int (k + 1)) in
          let d = digits x in
          if k + 1 < Array.length units && Float.of_string d >= 1000. then
            scaled (k + 1)
          else d ^ units.(k)
        in
        scaled 0
    in
    if plain = "0" then plain else sign ^ plain

(* An empty bar on its baseline says zero, so it has no value above it.
   The bars take the room's height but for the three lines around them;
   in a room too short for those, the values above them go first, then
   the baseline, then the labels. Lines wider than the room are cut. *)
let chart ?height ?scale ?style bars =
  let top =
    match scale with
    | Some s -> s
    | None -> List.fold_left (fun m (_, v) -> Float.max m v) 0. bars
  in
  let frac v = if top > 0. then v /. top else 0. in
  View_core.deferred (fun c ->
      let look = Ctx.look c in
      let rows =
        match (height, Ctx.height c) with
        | Some h, Some room -> max 1 (min h (room - 3))
        | Some h, None -> max 1 h
        | None, Some room -> max 1 (room - 3)
        | None, None -> 8
      in
      let spare =
        match Ctx.height c with Some room -> room - rows | None -> 3
      in
      let columns =
        List.map
          (fun (name, v) ->
            let label = Widget.one_line (View_core.text name) in
            let shown =
              if v = 0. then View_core.empty
              else View_core.ink (Widget.muted look) (value v)
            in
            let w =
              max 1 (max (View_core.width label) (View_core.width shown))
            in
            let at v = View_core.place ~align:`Center ~width:w v in
            let style = Option.map (fun f -> f name v) style in
            (at shown, vertical ?style ~width:w (frac v), at label, w))
          bars
      in
      let band f = View_core.hcat ~gap:1 (List.map f columns) in
      let total =
        List.fold_left (fun n (_, _, _, w) -> n + w + 1) (-1) columns
      in
      let base =
        View_core.ink (Widget.muted look)
          (Widget.fill (max 0 total) (Widget.line look "─"))
      in
      let bars =
        View_core.resolve
          (Ctx.with_room ~width:None ~height:(Some rows) c)
          (band (fun (_, b, _, _) -> b))
      in
      let lines =
        List.concat
          [
            (if spare >= 3 then [ band (fun (v, _, _, _) -> v) ] else []);
            [ bars ];
            (if spare >= 2 then [ base ] else []);
            (if spare >= 1 then [ band (fun (_, _, l, _) -> l) ] else []);
          ]
      in
      let chart = View_core.concat ~sep:(View_core.text "\n") lines in
      match Ctx.width c with
      | Some w -> Widget.truncate look (max 0 w) chart
      | None -> chart)
