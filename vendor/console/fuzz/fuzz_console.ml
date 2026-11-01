(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Text whose clusters, cells and bytes do not line up, over several
   lines. *)
let alphabet = [| "x"; "\xe7\x95\x8c"; "e\xcc\x81"; " "; "\n"; "ab"; "\n\n" |]

let text s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         alphabet.(Char.code s.[i] mod Array.length alphabet)))

let lines s = String.split_on_char '\n' s
let width = Width.string_width

(* Each line of [hcat] is the spans' lines up to the last span with text
   on it, each but the last padded to its span's width and followed by
   the gap: its width is the widths and gaps before that span plus its
   line's, and no line is wider than the spans' widths and gaps. *)
let test_hcat texts gap =
  let gap = gap mod 5 in
  (* A span with no text takes no column. *)
  let kept =
    List.filter (fun s -> View.height (View.text (text s)) > 0) texts
  in
  let blocks = List.map (fun s -> lines (text s)) kept in
  let out =
    View.to_string
      (View.hcat ~gap (List.map (fun s -> View.text (text s)) texts))
  in
  let widths =
    List.map (List.fold_left (fun n l -> max n (width l)) 0) blocks
  in
  let height = List.fold_left (fun n b -> max n (List.length b)) 0 blocks in
  (* No line and one empty line print alike. *)
  let got = if out = "" && height = 0 then [] else lines out in
  if texts = [] then check_eq ~pp:Fmt.Dump.string "" out
  else begin
    check_eq ~pp:Fmt.int height (List.length got);
    let total =
      List.fold_left ( + ) 0 widths + (gap * (List.length blocks - 1))
    in
    List.iteri
      (fun i line ->
        let cell b = match List.nth_opt b i with Some l -> l | None -> "" in
        let last = ref (-1) in
        List.iteri (fun j b -> if width (cell b) > 0 then last := j) blocks;
        let before = ref 0 in
        List.iteri
          (fun j w -> if j < !last then before := !before + w + gap)
          widths;
        let want =
          if !last < 0 then 0
          else !before + width (cell (List.nth blocks !last))
        in
        check_eq ~pp:Fmt.int want (width line);
        if width line > total then failf "%S wider than %d" line total)
      got
  end

(* Widgets side by side, each at the width it is given, take on the
   terminal the cells of those widths and gaps, in one row, in any theme. *)
let test_hcat_margin n k =
  let width = 1 + (n mod 20) in
  let gap = 1 + (k mod 5) in
  let count = 2 + (k mod 4) in
  let margin = (count * width) + ((count - 1) * gap) in
  let blocks = List.init count (fun _ -> View.sized ~width (Rule.view ())) in
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf (snd (List.nth Theme.all (k mod List.length Theme.all)));
  Format.pp_set_margin ppf margin;
  Fmt.set_style_renderer ppf `Ansi_tty;
  View.pp ppf (View.hcat ~gap blocks);
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buf in
  let vte = Console_vte.v ~rows:32 ~cols:margin () in
  Console_vte.feed vte raw;
  check_eq ~pp:Fmt.int 0 (fst (Console_vte.cursor_pos vte));
  check (String.is_valid_utf_8 raw);
  check_eq ~pp:Fmt.int margin (View.width (View.text (Console_text.strip raw)))

(* With no gap, a trailing block of combining marks is still text and
   must produce the same visible glyph as ordinary span concatenation. *)
let test_hcat_combining_suffix n =
  let marks = [| 0x0300; 0x0301; 0x0308; 0x0327 |] in
  let buf = Buffer.create 16 in
  for i = 0 to n mod 4 do
    Buffer.add_utf_8_uchar buf (Uchar.of_int marks.((n + i) mod 4))
  done;
  let suffix = Buffer.contents buf in
  let base = String.make 1 (Char.chr (Char.code 'a' + (n mod 26))) in
  Buffer.clear buf;
  let got =
    View.to_string (View.hcat ~gap:0 [ View.text base; View.text suffix ])
  and want = base ^ suffix in
  let actual = Console_vte.v ~rows:2 ~cols:10 ()
  and expected = Console_vte.v ~rows:2 ~cols:10 () in
  Console_vte.feed actual got;
  Console_vte.feed expected want;
  check_eq ~pp:Fmt.Dump.string
    (Console_vte.to_string expected)
    (Console_vte.to_string actual);
  check_eq ~pp:Fmt.Dump.string want got

(* [vcat] puts the lines of each span with text under those of the span
   before it: its lines are theirs, in order, and a span with no text adds
   none. *)
let test_vcat texts =
  let spans =
    List.map (fun s -> if s = "" then View.empty else View.text (text s)) texts
  in
  let stacked = View.vcat spans in
  let want = List.filter (fun s -> not (String.equal s "")) texts in
  check_eq ~pp:Fmt.Dump.string
    (String.concat "\n" (List.map text want))
    (View.to_string stacked);
  check_eq ~pp:Fmt.int
    (List.fold_left (fun n s -> n + View.height s) 0 spans)
    (View.height stacked);
  check_eq ~pp:Fmt.int
    (List.fold_left (fun n s -> max n (View.width s)) 0 spans)
    (View.width stacked)

(* Each guarantee a widget states alone, checked on widgets put together:
   every widget as a span in every other, a table in a panel in a cell of a
   table next to a tree, in roles, styles and gradients, printed in any
   theme, at any colour depth or none, in UTF-8 or not, at any margin, and
   drawn in a display's view. *)

(* Text that is hard to lay out: wide, combining, joined and selected
   emoji, a spacing mark, controls and escapes, and ASCII. *)
let hostile =
  [|
    "x";
    "ab";
    " ";
    "\n";
    "/";
    "\u{754c}";
    "e\u{301}";
    "\u{1f468}\u{200d}\u{1f469}";
    "\u{2764}\u{fe0f}";
    "a\u{fe0f}";
    "1\u{fe0f}\u{20e3}";
    "\u{1f44d}\u{1f3fd}";
    "x\u{940}";
    "\027[31m";
    "\027[2J";
    "\t";
    "\u{202e}";
  |]

let hostile_text s =
  String.concat ""
    (List.init
       (min 24 (String.length s))
       (fun i -> hostile.(Char.code s.[i] mod Array.length hostile)))

(* A span may be a widget as a span ([Nest]), and a block a printer in a
   style ([Styled]), so that every widget nests in every other: a table in a
   panel in a cell of a table, side by side ([Hcat]) or one under the other
   ([Vcat]), in a view. *)
type span =
  | Text of int * string
  | Gradient of string
  | Link of string
  | Bar of int * int
  | Role of int * span
  | Nest of int * block
  | Cat of span list

and block =
  | View of span
  | Table of int * span list list
  | Panel of int * span * span option
  | Tree of span * span list
  | Rule of span option
  | Styled of int * block
  | Hcat of int * block list
  | Vcat of block list

let rec pp_span ppf = function
  | Text (k, s) -> Fmt.pf ppf "text(%d,%S)" k s
  | Gradient s -> Fmt.pf ppf "gradient(%S)" s
  | Link s -> Fmt.pf ppf "link(%S)" s
  | Bar (w, f) -> Fmt.pf ppf "bar(%d,%d)" w f
  | Role (k, s) -> Fmt.pf ppf "role(%d,%a)" k pp_span s
  | Nest (k, b) -> Fmt.pf ppf "nest(%d,%a)" k pp_block b
  | Cat l -> Fmt.pf ppf "cat%a" Fmt.(Dump.list pp_span) l

and pp_block ppf = function
  | View s -> pp_span ppf s
  | Table (k, rows) ->
      Fmt.pf ppf "table(%d,%a)" k Fmt.(Dump.list (Dump.list pp_span)) rows
  | Panel (k, s, t) ->
      Fmt.pf ppf "panel(%d,%a,%a)" k pp_span s Fmt.(Dump.option pp_span) t
  | Tree (s, l) -> Fmt.pf ppf "tree(%a,%a)" pp_span s Fmt.(Dump.list pp_span) l
  | Rule l -> Fmt.pf ppf "rule(%a)" Fmt.(Dump.option pp_span) l
  | Styled (k, b) -> Fmt.pf ppf "styled(%d,%a)" k pp_block b
  | Hcat (g, l) -> Fmt.pf ppf "hcat(%d,%a)" g Fmt.(Dump.list pp_block) l
  | Vcat l -> Fmt.pf ppf "vcat%a" Fmt.(Dump.list pp_block) l

let take n l = List.filteri (fun i _ -> i < n) l

let block_gen =
  fix (fun block ->
      let span =
        fix (fun span ->
            choose
              [
                map [ uint8; bytes ] (fun k s -> Text (k, hostile_text s));
                map [ bytes ] (fun s -> Gradient (hostile_text s));
                map [ bytes ] (fun s -> Link (hostile_text s));
                map [ uint8; uint8 ] (fun w f -> Bar (w mod 12, f));
                map [ uint8; span ] (fun k s -> Role (k, s));
                map [ uint8; block ] (fun k b -> Nest (k, b));
                map [ list span ] (fun l -> Cat (take 3 l));
              ])
      in
      choose
        [
          map [ span ] (fun s -> View s);
          map
            [ uint8; list1 (list1 span) ]
            (fun k rows -> Table (k, take 4 (List.map (take 3) rows)));
          map [ uint8; span; option span ] (fun k s t -> Panel (k, s, t));
          map [ span; list span ] (fun s l -> Tree (s, take 4 l));
          map [ option span ] (fun l -> Rule l);
          map [ uint8; block ] (fun k b -> Styled (k, b));
          map [ uint8; list1 block ] (fun g l -> Hcat (g mod 4, take 3 l));
          map [ list1 block ] (fun l -> Vcat (take 3 l));
        ])

let block_gen = with_printer pp_block block_gen

let styles =
  [| Style.none; Style.bold; Style.(fg Color.red); Style.(bold + underline) |]

let roles = [| `Accent; `Succeeded; `Warning; `Failed; `Muted; `Header |]

let borders =
  [|
    Border.none;
    Border.simple;
    Border.ascii;
    Border.single;
    Border.double;
    Border.dos;
    Border.rounded;
    Border.heavy;
    Border.hidden;
  |]

let pick a k = a.(k mod Array.length a)

let rec span = function
  | Text (k, s) -> View.styled (pick styles k) (View.text s)
  | Gradient s -> View.gradient (View.text s)
  | Link s -> View.link ~uri:"https://example.org/a" (View.text s)
  | Bar (w, f) -> Bar.horizontal ~width:w (Float.of_int f /. 255.)
  | Role (k, s) -> View.role (pick roles k) (span s)
  | Nest (k, b) -> nested k b
  | Cat l -> View.concat (List.map span l)

(* A block as a span: a widget by its [to_span], at its natural width or at
   one [k] gives. *)
and nested k b =
  let width = if k land 1 = 0 then None else Some (1 + (k / 2 mod 60)) in
  match b with
  | View s -> span s
  | Table (k, rows) -> View.sized ?width (table k rows)
  | Panel (k, s, t) -> View.sized ?width (panel k s t)
  | Tree (s, l) -> View.sized ?width (Tree.view (tree s l))
  | Rule l -> View.sized ~width:(Option.value width ~default:20) (rule l)
  | Styled (j, b) -> View.styled (pick styles j) (nested k b)
  | Hcat (gap, l) -> View.hcat ~gap (List.map (nested k) l)
  | Vcat l -> View.vcat (List.map (nested k) l)

and table k rows =
  let n = List.fold_left (fun n r -> max n (List.length r)) 1 rows in
  let columns =
    List.init n (fun i ->
        Table.column
          ~align:[| `Left; `Center; `Right |].((k + i) mod 3)
          ~overflow:(if (k + i) land 4 = 0 then `Wrap else `Truncate)
          (if i = 1 then View.role `Header (View.text ("c" ^ string_of_int i))
           else View.text ("c" ^ string_of_int i)))
  in
  let border = if k land 8 = 0 then None else Some (pick borders (k / 16)) in
  Table.view ?border
    ~rules:(k land 1 = 1)
    columns
    (List.map (List.map span) rows)

and panel k s t =
  let border =
    if k land 1 = 0 then None
    else Some borders.(1 + (k / 2 mod (Array.length borders - 1)))
  in
  Panel.view ?border ?title:(Option.map span t) ~padding:(k / 32 mod 3) (span s)

and tree s l = Tree.v (span s) (List.map (fun c -> Tree.v (span c) []) l)
and rule l = Rule.view ?label:(Option.map span l) ()

let rec pp = function
  | View s -> Fmt.const View.pp (span s)
  | Table (k, rows) -> Fmt.const View.pp (table k rows)
  | Panel (k, s, t) -> Fmt.const View.pp (panel k s t)
  | Tree (s, l) -> Fmt.const View.pp (Tree.view (tree s l))
  | Rule l -> Fmt.const View.pp (rule l)
  | Styled (k, b) -> Style.styled (pick styles k) (pp b)
  | (Hcat _ | Vcat _) as b -> Fmt.const View.pp (nested 0 b)

type look = {
  theme : Theme.t;
  utf_8 : bool;
  depth : Color.depth option;
  margin : int;
}

let look k m =
  {
    theme = snd (List.nth Theme.all (k mod List.length Theme.all));
    utf_8 = k land 0x100 = 0;
    depth =
      [| None; Some `Ansi_16; Some `Ansi_256; Some `True_color |].(k / 512 mod 4);
    margin = 1 + (m mod 200);
  }

let print look block =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf look.margin;
  Fmt.set_utf_8 ppf look.utf_8;
  Theme.set ppf look.theme;
  Option.iter
    (fun d ->
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf d)
    look.depth;
  match pp block ppf () with
  | () ->
      Format.pp_print_flush ppf ();
      Some (Buffer.contents buf)
  | exception Invalid_argument _ -> None

(* Every escape sequence is whole: a CSI ends at its final byte, an OSC at
   ST or BEL. *)
let escapes_whole s =
  let n = String.length s in
  let rec ground i =
    match String.index_from_opt s i '\027' with
    | None -> true
    | Some j when j + 1 >= n -> false
    | Some j -> (
        match s.[j + 1] with
        | '[' -> csi (j + 2)
        | ']' -> osc (j + 2)
        | _ -> false)
  and csi i =
    i < n && if s.[i] >= '@' && s.[i] <= '~' then ground (i + 1) else csi (i + 1)
  and osc i =
    i < n
    && ((s.[i] = '\007' && ground (i + 1))
       || (s.[i] = '\027' && i + 1 < n && s.[i + 1] = '\\' && ground (i + 2))
       || osc (i + 1))
  in
  ground 0

let is_ascii s = String.for_all (fun c -> Char.code c < 0x80) s

let rec texts_span = function
  | Text (_, s) | Gradient s | Link s -> [ s ]
  | Bar _ -> []
  | Role (_, s) -> texts_span s
  | Nest (_, b) -> texts b
  | Cat l -> List.concat_map texts_span l

and texts = function
  | View s | Rule (Some s) -> texts_span s
  | Rule None -> []
  | Table (_, rows) -> List.concat_map (List.concat_map texts_span) rows
  | Panel (_, s, t) -> texts_span s @ Option.fold ~none:[] ~some:texts_span t
  | Tree (s, l) -> List.concat_map texts_span (s :: l)
  | Styled (_, b) -> texts b
  | Hcat (_, l) | Vcat l -> List.concat_map texts l

(* A span is text, printed whole, spans side by side or one under the
   other included, and a rule's
   label wider than the margin makes the rule two cells wider. *)
let rec may_overflow = function
  | View _ | Rule (Some _) | Hcat _ | Vcat _ -> true
  | Styled (_, b) -> may_overflow b
  | _ -> false

(* The cells a line takes on a terminal wide enough for it. *)
let on_screen line =
  let term = Console_vte.v ~rows:1 ~cols:2000 () in
  Console_vte.feed term line;
  snd (Console_vte.cursor_pos term)

(* Printed anywhere, any tree of widgets: no line wider than the margin, and
   each as wide on the terminal as console counts it; the same bytes each
   time; the plain text, without colour, of the bytes in colour; valid UTF-8
   with every escape whole, none at all without colour, and ASCII alone
   when the formatter is not UTF-8 and the text is ASCII. *)
let test_printed block k m =
  let look = look k m in
  match print look block with
  | None -> ()
  | Some out ->
      Matrix_0_1.discard_stray_vs16 out;
      check_eq ~pp:Fmt.(option Dump.string) (Some out) (print look block);
      check (String.is_valid_utf_8 out);
      if not (escapes_whole out) then failf "a cut escape in %S" out;
      let plain = Console_text.strip out in
      List.iter
        (fun line ->
          let cells = Width.string_width line in
          if cells > look.margin && not (may_overflow block) then
            failf "a line of %d cells at a margin of %d: %S" cells look.margin
              line;
          let shown = on_screen line in
          if shown <> cells then
            failf "%S is %d cells, %d on the terminal" line cells shown)
        (String.split_on_char '\n' plain);
      (match look.depth with
      | None -> if String.contains out '\027' then failf "an escape: %S" out
      | Some _ ->
          check_eq
            ~pp:Fmt.(option Dump.string)
            (print { look with depth = None } block)
            (Some plain));
      if (not look.utf_8) && List.for_all is_ascii (texts block) then
        if not (is_ascii out) then failf "not ASCII: %S" out

(* The screen a display draws with [block] as its view, in [theme] at
   [time]. *)
let view_screen theme ~width ~height ~time block =
  let term = Console_vte.v ~rows:height ~cols:width () in
  let buf = Buffer.create 1024 in
  let ppf =
    Format.make_formatter (Buffer.add_substring buf) (fun () ->
        Console_vte.feed term (Buffer.contents buf);
        Buffer.clear buf)
  in
  Fmt.set_style_renderer ppf `Ansi_tty;
  Color.set_depth ppf `True_color;
  Theme.set ppf theme;
  let d =
    Display.v
      ~size:(fun () -> (width, height))
      ~now:(fun () -> time)
      ppf View.empty
  in
  (try Display.set_view d (nested 0 block) with Invalid_argument _ -> ());
  Display.redraw d;
  let g = Console_vte.grid term in
  let rows =
    List.init (height - 1) (fun y ->
        List.init width (fun x ->
            let i = Grid.idx g ~x ~y in
            (Grid.is_continuation g i, Grid.get_text g i)))
  in
  Display.close d;
  rows

let is_text c = String.length c = 1 && Char.Ascii.is_alphanum c.[0]

(* A view in a theme that moves, at any time: each frame lays its cells
   out as the still frame does, so motion changes no width, and keeps every
   letter and digit of it, so only the frame's lines move. *)
let test_view block k w t =
  let theme = snd (List.nth Theme.all (k mod List.length Theme.all)) in
  let width = 1 + (w mod 120) and height = 3 + (k / 64 mod 12) in
  let time = Float.of_int t /. 10. in
  let moving = view_screen theme ~width ~height ~time block
  and still = view_screen (Theme.still theme) ~width ~height ~time block in
  List.iteri
    (fun y (m, s) ->
      List.iteri
        (fun x ((mc, mt), (sc, st)) ->
          if mc <> sc then failf "row %d, column %d: a width moved" y x;
          if is_text st && mt <> st then
            failf "row %d, column %d: %S became %S" y x st mt)
        (List.combine m s))
    (List.combine moving still)

let nested_cells raw =
  List.concat_map
    (fun line ->
      let cols = Width.string_width (Console_text.strip line) + 2 in
      let vte = Console_vte.v ~rows:1 ~cols () in
      Console_vte.feed vte line;
      let g = Console_vte.grid vte in
      List.filter_map
        (fun i ->
          match Grid.get_text g i with "" | " " -> None | _ -> Some (g, i))
        (List.init cols Fun.id))
    (String.split_on_char '\n' raw)

(* Wrapping changes where a widget's lines break, not the glyphs, colours
   or attributes of its non-space cells. Compare those cells on matrix.vte,
   rather than accepting a correctly sized output that lost its content. *)
let test_nested_wrap s k n =
  let clusters =
    [|
      "a"; " "; "b"; "\n"; "\u{754c}"; "e\u{0301}"; "\u{1f469}\u{200d}\u{1f4bb}";
    |]
  in
  let input =
    String.concat ""
      (List.init
         (min 16 (String.length s))
         (fun i -> clusters.(Char.code s.[i] mod Array.length clusters)))
  in
  let content = View.styled Style.bold (View.gradient (View.text input)) in
  let inner =
    match k mod 5 with
    | 0 -> Panel.view content
    | 1 ->
        Table.view
          [ Table.column (View.role `Header (View.text "h")) ]
          [ [ content ] ]
    | 2 ->
        Tree.view
          (Tree.v content
             [ Tree.v (View.role `Succeeded (View.text "leaf")) [] ])
    | 3 -> View.sized ~width:20 (Rule.view ~label:content ())
    | _ -> Bar.horizontal ~width:12 0.6
  in
  let nested =
    match k / 5 mod 3 with
    | 0 -> Panel.view inner
    | 1 -> Table.view [ Table.column (View.text "h") ] [ [ inner ] ]
    | _ -> Tree.view (Tree.v (View.text "root") [ Tree.v inner [] ])
  in
  let source = View.role `Warning nested in
  let theme = snd (List.nth Theme.all (k mod List.length Theme.all)) in
  let render span =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color;
    Fmt.set_utf_8 ppf (k land 128 = 0);
    Theme.set ppf theme;
    View.pp ppf span;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let width = 2 + (n mod 19) in
  let pieces = View.wrap width source in
  List.iter (fun piece -> check (View.width piece <= width)) pieces;
  let expected = nested_cells (render source) in
  let actual =
    List.concat_map (fun piece -> nested_cells (render piece)) pieces
  in
  check_eq ~pp:Fmt.int (List.length expected) (List.length actual);
  List.iter2
    (fun (g, i) (h, j) ->
      let cell g i =
        ( Grid.get_text g i,
          Grid.get_attrs g i,
          Ansi.Color.to_rgb (Console_vte.fg g i),
          Ansi.Color.to_rgb (Console_vte.bg g i),
          Grid.hyperlink_url g (Grid.get_link g i) )
      in
      if cell g i <> cell h j then
        failf "wrapping at %d changed a cell in %s: %S became %S" width
          (Theme.name theme) (Grid.get_text g i) (Grid.get_text h j))
    expected actual

let hcat_cell g i =
  ( Grid.get_text g i,
    Grid.get_attrs g i,
    Ansi.Color.to_rgb (Console_vte.fg g i),
    Ansi.Color.to_rgb (Console_vte.bg g i),
    Grid.hyperlink_url g (Grid.get_link g i) )

(* Side-by-side widgets must keep their cells when a containing widget
   wraps them. A width check alone would miss lost text, styles or links. *)
let test_hcat_nested_wrap input k n =
  let content =
    let clusters = [| "a"; " "; "b"; "\n"; "\u{754c}"; "e\u{0301}" |] in
    String.concat ""
      (List.init
         (min 12 (String.length input))
         (fun i -> clusters.(Char.code input.[i] mod Array.length clusters)))
  in
  let left = Panel.view (View.gradient (View.text content)) in
  let right =
    Tree.view
      (Tree.v
         (View.link ~uri:"https://example.org/report" (View.text "tree"))
         [ Tree.v (View.role `Succeeded (View.text "leaf")) [] ])
  in
  let joined = View.hcat ~gap:(k mod 4) [ left; right ] in
  let source =
    View.styled Style.bold
      (match k / 4 mod 3 with
      | 0 -> Panel.view joined
      | 1 -> Table.view [ Table.column (View.text "h") ] [ [ joined ] ]
      | _ -> Tree.view (Tree.v joined []))
  in
  let theme = snd (List.nth Theme.all (k mod List.length Theme.all)) in
  let render s =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    Fmt.set_utf_8 ppf (k land 128 = 0);
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color;
    View.pp ppf s;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let width = 4 + (n mod 29) in
  let pieces = View.wrap width source in
  List.iter
    (fun s ->
      check (View.width s <= width);
      List.iter (fun line -> check (on_screen line <= width)) (lines (render s)))
    pieces;
  let expected = nested_cells (render source)
  and actual = List.concat_map (fun s -> nested_cells (render s)) pieces in
  check_eq ~pp:Fmt.int (List.length expected) (List.length actual);
  List.iter2
    (fun (g, i) (h, j) ->
      if hcat_cell g i <> hcat_cell h j then
        fail "wrapping nested hcat changed a cell")
    expected actual

let suite =
  ( "console",
    [
      test_case "wrapping hcat inside widgets preserves terminal cells"
        [ bytes; uint8; uint8 ] test_hcat_nested_wrap;
      test_case "wrapping nested widgets preserves their terminal cells"
        [ bytes; uint8; uint8 ] test_nested_wrap;
      test_case "hcat preserves trailing combining marks" [ uint8 ]
        test_hcat_combining_suffix;
      test_case "hcat widgets take the cells of their widths" [ uint8; uint8 ]
        test_hcat_margin;
      test_case "a view of widgets moves only its lines"
        [ block_gen; uint16; uint8; uint8 ]
        test_view;
      test_case "vcat lines are the views' lines in turn"
        [ list bytes ]
        test_vcat;
      test_case "hcat lines are the blocks' widths and gaps"
        [ list bytes; uint8 ]
        test_hcat;
      test_case "any tree of widgets printed anywhere"
        [ block_gen; uint16; uint8 ]
        test_printed;
    ] )
