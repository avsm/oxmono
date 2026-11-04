(* SPDX-License-Identifier: ISC *)
(* Terminal output as HTML. [Console_text.strip ~keep_styles:true] first
   removes every escape code but those that set colours and attributes (SGR),
   and every control character a browser should not see, so what is parsed
   here is printable text and [ESC [ params m]. Styles become classes, never
   inline styles, so pages keep a policy without 'unsafe-inline'. *)

type style = {
  fg : int option;  (** A colour of the 256-colour palette. *)
  bg : int option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  strike : bool;
  inverse : bool;
}

let plain =
  {
    fg = None;
    bg = None;
    bold = false;
    dim = false;
    italic = false;
    underline = false;
    strike = false;
    inverse = false;
  }

(* The nearest colour of the 6x6x6 cube or the grey ramp, as xterm draws
   them, for a truecolour that a class cannot name exactly. *)
let levels = [| 0; 95; 135; 175; 215; 255 |]

let nearest v =
  let best = ref 0 in
  Array.iteri
    (fun i l -> if abs (l - v) < abs (levels.(!best) - v) then best := i)
    levels;
  !best

let of_rgb r g b =
  let clamp v = max 0 (min 255 v) in
  let r = clamp r and g = clamp g and b = clamp b in
  let cube = 16 + (36 * nearest r) + (6 * nearest g) + nearest b in
  let grey_level = (r + g + b) / 3 in
  let grey = 232 + max 0 (min 23 ((grey_level - 8 + 5) / 10)) in
  let distance c =
    let cr, cg, cb =
      if c >= 232 then
        let v = 8 + ((c - 232) * 10) in
        (v, v, v)
      else
        let c = c - 16 in
        (levels.(c / 36), levels.(c / 6 mod 6), levels.(c mod 6))
    in
    ((cr - r) * (cr - r)) + ((cg - g) * (cg - g)) + ((cb - b) * (cb - b))
  in
  if distance grey < distance cube then grey else cube

(* Parameters are numbers separated by [;]. [Console_text.strip] removes the
   colon form of colours, [38:2::r:g:b], so it never reaches here. *)
let parameters s =
  List.map
    (fun p -> [ Option.value ~default:0 (int_of_string_opt p) ])
    (String.split_on_char ';' s)

let clamp n = max 0 (min 255 n)

let set which st c =
  if which = 38 then { st with fg = Some c } else { st with bg = Some c }

let single st = function
  | 0 -> plain
  | 1 -> { st with bold = true }
  | 2 -> { st with dim = true }
  | 3 -> { st with italic = true }
  | 4 -> { st with underline = true }
  | 7 -> { st with inverse = true }
  | 9 -> { st with strike = true }
  | 21 | 22 -> { st with bold = false; dim = false }
  | 23 -> { st with italic = false }
  | 24 -> { st with underline = false }
  | 27 -> { st with inverse = false }
  | 29 -> { st with strike = false }
  | n when n >= 30 && n <= 37 -> { st with fg = Some (n - 30) }
  | 39 -> { st with fg = None }
  | n when n >= 40 && n <= 47 -> { st with bg = Some (n - 40) }
  | 49 -> { st with bg = None }
  | n when n >= 90 && n <= 97 -> { st with fg = Some (n - 90 + 8) }
  | n when n >= 100 && n <= 107 -> { st with bg = Some (n - 100 + 8) }
  | _ -> st

(* A colour is [38;5;n] or [38;2;r;g;b], and likewise [48] for the
   background. *)
let rec apply st = function
  | [] -> st
  | [ ((38 | 48) as which) ] :: [ 5 ] :: [ n ] :: rest ->
      apply (set which st (clamp n)) rest
  | [ ((38 | 48) as which) ] :: [ 2 ] :: [ r ] :: [ g ] :: [ b ] :: rest ->
      apply (set which st (of_rgb r g b)) rest
  | [ (38 | 48) ] :: _ -> st
  | (n :: _) :: rest -> apply (single st n) rest
  | [] :: rest -> apply (single st 0) rest

let classes st =
  List.concat
    [
      Option.fold ~none:[] ~some:(fun c -> [ "sk-fg" ^ string_of_int c ]) st.fg;
      Option.fold ~none:[] ~some:(fun c -> [ "sk-bg" ^ string_of_int c ]) st.bg;
      (if st.bold then [ "sk-b" ] else []);
      (if st.dim then [ "sk-d" ] else []);
      (if st.italic then [ "sk-i" ] else []);
      (if st.underline then [ "sk-u" ] else []);
      (if st.strike then [ "sk-s" ] else []);
      (if st.inverse then [ "sk-inv" ] else []);
    ]

(* [runs s] is [s] as [(style, text)] pieces, with no escape left in the
   text. *)
let runs s =
  let s = Console_text.strip ~keep_styles:true s in
  let n = String.length s in
  let out = ref [] and st = ref plain and b = Buffer.create 256 in
  let flush () =
    if Buffer.length b > 0 then (
      out := (!st, Buffer.contents b) :: !out;
      Buffer.clear b)
  in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\027' && i + 1 < n && s.[i + 1] = '[' then (
      (* The parameters run to the final byte, which strip leaves only as
         [m]. Anything else is dropped whole. *)
      let j = ref (i + 2) in
      while
        !j < n && match s.[!j] with '0' .. '9' | ';' -> true | _ -> false
      do
        incr j
      done;
      if !j < n && s.[!j] = 'm' then (
        flush ();
        st := apply !st (parameters (String.sub s (i + 2) (!j - i - 2)));
        go (!j + 1))
      else go (min n (!j + 1)))
    else if s.[i] = '\027' then go (i + 1)
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  flush ();
  List.rev !out

(* The sixteen base colours follow the theme. The rest are xterm's. *)
let css =
  let b = Buffer.create 16384 in
  let light =
    [|
      "#1f2328";
      "#b3261e";
      "#1f7a3d";
      "#8a6100";
      "#1d5fb3";
      "#8e3ea6";
      "#127a8a";
      "#6e7781";
      "#57606a";
      "#d1302a";
      "#2a9d55";
      "#a87900";
      "#2f7ad6";
      "#a957c7";
      "#1f9bb0";
      "#24292f";
    |]
  and dark =
    [|
      "#8b949e";
      "#ff7b72";
      "#7ee787";
      "#e3b341";
      "#79c0ff";
      "#d2a8ff";
      "#76e3ea";
      "#c9d1d9";
      "#6e7681";
      "#ffa198";
      "#a5f0b0";
      "#f2cc60";
      "#a5d6ff";
      "#e2c5ff";
      "#b3f0ff";
      "#f0f6fc";
    |]
  in
  Printf.bprintf b ":root{";
  Array.iteri (fun i c -> Printf.bprintf b "--sk-ansi%d:%s;" i c) light;
  Printf.bprintf b
    "}\n@media (prefers-color-scheme:dark){:root:not([data-theme=light]){";
  Array.iteri (fun i c -> Printf.bprintf b "--sk-ansi%d:%s;" i c) dark;
  Printf.bprintf b "}}\n";
  for i = 0 to 255 do
    let value =
      if i < 16 then Printf.sprintf "var(--sk-ansi%d)" i
      else if i >= 232 then
        let v = 8 + ((i - 232) * 10) in
        Printf.sprintf "#%02x%02x%02x" v v v
      else
        let c = i - 16 in
        Printf.sprintf "#%02x%02x%02x"
          levels.(c / 36)
          levels.(c / 6 mod 6)
          levels.(c mod 6)
    in
    Printf.bprintf b ".sk-fg%d{color:%s}.sk-bg%d{background:%s}\n" i value i
      value
  done;
  Printf.bprintf b
    ".sk-b{font-weight:700}.sk-d{opacity:.7}.sk-i{font-style:italic}\n\
     .sk-u{text-decoration:underline}.sk-s{text-decoration:line-through}\n\
     .sk-u.sk-s{text-decoration:underline line-through}\n\
     .sk-inv{color:var(--sk-surface);background:var(--sk-ink)}\n";
  Buffer.contents b
