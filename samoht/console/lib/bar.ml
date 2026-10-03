(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type style = Theme.bar
type color = Color.t

(* Eighth-of-a-cell glyphs for the smooth bar. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let rainbow_palette =
  [|
    Color.red; Color.yellow; Color.green; Color.cyan; Color.blue; Color.magenta;
  |]

(* The SGR a bar adds, dropped when the caller renders unstyled: the glyphs
   carry the shape and the colour only reinforces it, so a bar without colour
   is the same bar. *)
let code ~styled sequence = if styled then sequence else ""

(* A blocky DOS-style bar: dim [ ] brackets, full-block fill, dim shade
   track. *)
let blocky ~styled ~color ~width ~pct =
  let full = width * pct / 100 in
  let empty = max 0 (width - full) in
  let fill = String.concat "" (List.init full (fun _ -> "█")) in
  let track = String.concat "" (List.init empty (fun _ -> "░")) in
  String.concat ""
    [
      code ~styled Ansi.dim;
      "[";
      code ~styled Ansi.reset_code;
      code ~styled (Ansi.color_code color);
      fill;
      code ~styled Ansi.reset_code;
      code ~styled Ansi.dim;
      track;
      "]";
      code ~styled Ansi.reset_code;
    ]

(* A smooth bar with sub-cell eighth fills and thin brackets. *)
let smooth ~styled ~color ~width ~pct =
  let total = width * 8 in
  let filled = total * pct / 100 in
  let full = filled / 8 in
  let part = filled mod 8 in
  let empty = max 0 (width - full - if part > 0 then 1 else 0) in
  let fill =
    String.concat "" (List.init full (fun _ -> eighths.(8)))
    ^ if part > 0 then eighths.(part) else ""
  in
  String.concat ""
    [
      code ~styled Ansi.dim;
      "▕";
      code ~styled Ansi.reset_code;
      code ~styled (Ansi.color_code color);
      fill;
      code ~styled Ansi.reset_code;
      code ~styled Ansi.dim;
      String.make empty ' ';
      "▏";
      code ~styled Ansi.reset_code;
    ]

(* A rainbow bar: each filled cell steps through the spectrum. *)
let rainbow ~styled ~color:_ ~width ~pct =
  let full = width * pct / 100 in
  let empty = max 0 (width - full) in
  let buf = Buffer.create (width * 8) in
  Buffer.add_string buf
    (code ~styled Ansi.dim ^ "[" ^ code ~styled Ansi.reset_code);
  for i = 0 to full - 1 do
    let c = rainbow_palette.(i mod Array.length rainbow_palette) in
    Buffer.add_string buf
      (code ~styled (Ansi.color_code c) ^ "█" ^ code ~styled Ansi.reset_code)
  done;
  Buffer.add_string buf (code ~styled Ansi.dim);
  for _ = 1 to empty do
    Buffer.add_string buf "░"
  done;
  Buffer.add_string buf ("]" ^ code ~styled Ansi.reset_code);
  Buffer.contents buf

let draw_style :
    style -> styled:bool -> color:color -> width:int -> pct:int -> string =
  function
  | `Blocky -> blocky
  | `Smooth -> smooth
  | `Rainbow -> rainbow

(* An explicit argument wins, else the theme's, else a modern default. *)
let resolve ?theme ?style ?color () =
  let style =
    match style with
    | Some s -> s
    | None -> ( match theme with Some t -> Theme.bar t | None -> `Smooth)
  in
  let color =
    match color with
    | Some c -> c
    | None -> (
        match theme with Some t -> Theme.accent t | None -> Color.cyan)
  in
  (style, color)

let render ?theme ?style ?color ?(styled = true) ~width ~pct () =
  if width < 0 then invalid_arg "Console.Bar.render: negative width";
  let style, color = resolve ?theme ?style ?color () in
  let pct = max 0 (min 100 pct) in
  draw_style style ~styled ~color ~width ~pct

let at ?theme ?style ?color ?(styled = true) ~width ~elapsed () =
  if width < 0 then invalid_arg "Console.Bar.at: negative width";
  let style, color = resolve ?theme ?style ?color () in
  (* Sweep the fill up and back over a two-second period: a triangle wave on
     [elapsed] mapped to a percentage. *)
  let period = 2.0 in
  let phase =
    if (not (Float.is_finite elapsed)) || elapsed <= 0. then 0.
    else Float.rem (elapsed /. period) 1.0
  in
  let tri = if phase < 0.5 then phase *. 2. else (1. -. phase) *. 2. in
  draw_style style ~styled ~color ~width ~pct:(int_of_float (tri *. 100.))

let anim ?theme ?style ?color ?styled ~width () =
  Anim.v (fun ~elapsed -> at ?theme ?style ?color ?styled ~width ~elapsed ())
