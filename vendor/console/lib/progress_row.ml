(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Task_core
module V = View_core

let text = V.text
let pad = Widget.pad

let pad_left n v =
  let w = V.width v in
  if w >= n then v else V.(pad (n - w) ++ v)

let theme (look : Look.t) = look.theme
let cut look w s = Widget.truncate look w s
let muted look s = V.ink (Widget.muted look) s

(* Successive rows take the colours of the theme's palette in turn. *)
let color look r =
  let palette = Theme.palette (theme look) in
  List.nth palette (abs (id r) mod List.length palette)

let mark_style look (m : Theme.mark) =
  let theme = theme look in
  match m with
  | `Succeeded -> Style.(bold + fg (Theme.succeeded theme))
  | `Failed | `Error -> Style.(bold + fg (Theme.failed theme))
  | `Cancelled | `Warning -> Style.(bold + fg (Theme.warning theme))
  | `Running -> Style.(bold + fg (Theme.accent theme))
  | `Info | `Debug -> Widget.muted look

let mark look m = V.ink (mark_style look m) (Widget.mark look m)
let outcome_mark (o : outcome) = (o :> Theme.mark)
let label_of r = Option.value (message r) ~default:(label r)

let mark_of look r =
  mark look (outcome_mark (Option.value (outcome r) ~default:`Cancelled))

(* In floats: [100 * cur] overflows an int long before [cur] does. *)
let percent m =
  Option.map
    (fun f -> if f >= 1. then 100 else int_of_float (100. *. f))
    (fraction m)

(* The bar column opens with the first task of the work that counts. *)
let counted r = Work.count_cell (work r) > 0

(* No bar is drawn before work advances, since an empty track reads as an
   input field; its cells are kept blank, as are those of a task that does
   not count, so that nothing moves when it shows. Once work has advanced,
   the bar shows the least it can. A task that ended keeps its bar as far
   as it went. *)
let full = Atomic.make None

let full_bar look width =
  match Atomic.get full with
  | Some (l, w, v) when l == look && w = width -> v
  | _ ->
      let v = V.paint look (Bar.horizontal ~width 1.) in
      Atomic.set full (Some (look, width, v));
      v

let bar look r width =
  if not (counted r) then V.empty
  else
    match fraction (metric r) with
    | Some f when f >= 1. -> full_bar look width
    | Some f when f > 0. ->
        (* A half cell, the least a bar shows, in UTF-8 and in ASCII alike. *)
        let f = Float.max f (1. /. float (max 1 (width * 2))) in
        V.paint look (Bar.horizontal ~width f)
    | _ -> pad width

let count_cell look r =
  pad_left (Work.count_cell (work r)) (muted look (count_text (metric r)))

(* A count that gives way: items as they are, bytes in short units, and no
   column shared with the other rows. *)
let compact_count look r =
  match metric r with
  | Spin -> V.empty
  | Count _ -> muted look (count_text (metric r))
  | Bytes (cur, total) ->
      muted look (Task_text.bytes_short cur ^ "/" ^ Task_text.bytes_short total)

let timer look d =
  pad_left Task_text.duration_cells (muted look (Task_text.duration d))

let eta look r =
  match Task_core.eta r with
  | Some s -> muted look ("ETA " ^ Task_text.duration s)
  | None -> V.empty

let bar_max = 40
let bar_min = 10

type columns = { label : int; lead : int }

(* From the third stage of what gives way, the count and the elapsed time
   leave their columns, and so does the label. *)
let apart = 3

let lead look =
  List.fold_left
    (fun n s -> max n (Widget.cells s))
    1
    (List.map (Widget.mark look) [ `Succeeded; `Failed; `Cancelled ]
    @ List.map
        (fun f -> Widget.glyph look f ~ascii:"|")
        (Theme.spinner (theme look)))

let columns look r =
  let w = work r in
  let rec widest d n =
    if d >= Work.depths w then n
    else widest (d + 1) (max n (Work.label_cell w d))
  in
  { label = widest 0 0; lead = lead look }

(* What a piece shows at [stage]: a view, the bar that grows, the label or
   the spacer, which the row lays out. *)
type cell = Shown of V.t | Grows | Name | Rest

let cell look r ~time ~stage (p : Progress_layout.piece) =
  match p.kind with
  | Spinner ->
      Shown (V.ink (Style.fg (color look r)) (Widget.spinner look ~time))
  | Mark -> Shown (mark_of look r)
  | Label _ -> Name
  | Spacer -> Rest
  | Bar None -> if counted r then Grows else Shown V.empty
  | Bar (Some w) -> Shown (bar look r w)
  | Percent -> (
      match percent (metric r) with
      | Some p -> Shown (Fmt.kstr (muted look) "%3d%%" p)
      | None -> Shown (if counted r then pad 4 else V.empty))
  | Count ->
      Shown (if stage >= apart then compact_count look r else count_cell look r)
  | Rate ->
      Shown
        (pad_left
           (Work.rate_cell (work r))
           (muted look (rate_text (metric r) (rate r))))
  | Eta -> Shown (eta look r)
  | Elapsed ->
      Shown
        (if stage >= apart then muted look (Task_text.duration (elapsed r))
         else timer look (elapsed r))
  | Const v -> Shown (if V.is_flat v then v else V.resolve (Ctx.v look) v)
  | Custom f -> Shown (V.resolve (Ctx.v ~time look) (f r))
  | Choose _ -> Shown V.empty

(* The label is cut only once the other pieces have given way, at their
   stages: by default 1 the rate, 2 the bar and the percentage, 3 the
   count, which gets shorter and leaves the column it shares, 4 the elapsed
   time. *)

let last_stage pieces =
  List.fold_left
    (fun n (p : Progress_layout.piece) ->
      max n (Option.value p.stage ~default:0))
    apart pieces

let is_lead (p : Progress_layout.piece) =
  match p.kind with Spinner | Mark -> true | _ -> false

(* [v] in the middle of [n] cells, so that the spinners and the marks of
   the rows take as many cells and their labels start in one column. *)
let centred n v =
  let w = V.width v in
  if w >= n then v
  else
    let before = (n - w) / 2 in
    V.(pad before ++ v ++ pad (n - w - before))

(* What [pieces] show at [stage], the cells of those of a fixed width,
   whether the bar grows and whether the label ends the row. A piece that
   has given way, or that shows nothing, goes with the blank text before
   it, or after it if none is before; a row whose last piece has gone ends
   with no blank. With [lead], the spinner and the mark take that many
   cells. *)
let shown look r ~time ~stage ~lead (pieces : Progress_layout.piece array) =
  let n = Array.length pieces in
  let cells =
    Array.map
      (fun (p : Progress_layout.piece) ->
        match p.stage with
        | Some s when stage >= s -> None
        | _ -> Some (cell look r ~time ~stage p))
      pieces
  in
  let gone =
    Array.map
      (function
        | None -> true | Some (Shown v) -> V.is_empty v | Some _ -> false)
      cells
  in
  let keep = Array.map not gone in
  let blank i = keep.(i) && pieces.(i).blank in
  for i = 0 to n - 1 do
    if gone.(i) then
      if i > 0 && blank (i - 1) then keep.(i - 1) <- false
      else if i + 1 < n && blank (i + 1) then keep.(i + 1) <- false
  done;
  if n > 0 && gone.(n - 1) then begin
    let rec trim i =
      if i >= 0 && ((not keep.(i)) || pieces.(i).blank) then begin
        keep.(i) <- false;
        trim (i - 1)
      end
    in
    trim (n - 1)
  end;
  let rec collect i acc fixed grows =
    if i < 0 then (acc, fixed, grows)
    else
      match cells.(i) with
      | Some c when keep.(i) -> (
          match c with
          | Shown v ->
              let v =
                match lead with
                | Some l when is_lead pieces.(i) -> centred l v
                | _ -> v
              in
              collect (i - 1) (Shown v :: acc) (fixed + V.width v) grows
          | Grows -> collect (i - 1) (c :: acc) fixed true
          | Name | Rest -> collect (i - 1) (c :: acc) fixed grows)
      | _ -> collect (i - 1) acc fixed grows
  in
  let cells, fixed, grows = collect (n - 1) [] 0 false in
  let rec ends = function
    | [] -> false
    | [ Name ] -> true
    | _ :: rest -> ends rest
  in
  (cells, fixed, grows, ends cells)

(* The row of [r] by [layout], packed from the left, and whether its label
   is whole. With [columns], until the count leaves its column, the spinner
   or the mark takes the cells of the widest and the label is padded to the
   label column. The bar grows to fill the room up to [bar_max], and a
   spacer takes what is left. Short of room, the bar shrinks to [bar_min]
   before the pieces give way, in the order of their stages, and the label
   is cut once they all have. Without a room, nothing gives way. *)
let laid look ~time ?width ?columns ~label layout r =
  let layout = Progress_layout.for_task layout r in
  let has is = List.exists is layout in
  let named =
    has (fun (p : Progress_layout.piece) ->
        match p.kind with Label _ -> true | _ -> false)
  in
  let label_w = if named then V.width label else 0 in
  let reserve =
    if has (fun (p : Progress_layout.piece) -> p.kind = Spacer) then 1 else 0
  in
  let pieces = Array.of_list layout in
  let at stage =
    let held = stage < apart in
    let lead =
      match columns with Some c when held -> Some c.lead | _ -> None
    in
    let cells, fixed, grows, ends = shown look r ~time ~stage ~lead pieces in
    let column =
      match columns with
      | Some c when named && held -> max label_w c.label
      | _ -> label_w
    in
    (cells, fixed, grows, ends, column)
  in
  let width =
    match width with
    | Some w -> w
    | None ->
        let _, fixed, grows, _, column = at 0 in
        column + fixed + reserve + if grows then bar_max else 0
  in
  let last = last_stage layout in
  let rec fit stage =
    let ((_, fixed, grows, _, column) as x) = at stage in
    if width - fixed - reserve - column >= if grows then bar_min else 0 then
      (x, true)
    else if stage >= last then (x, false)
    else fit (stage + 1)
  in
  let (cells, fixed, grows, ends, column), fits = fit 0 in
  let room = width - fixed - reserve in
  let label, column =
    if fits || not named then (label, column)
    else
      let left = max 0 (room - if grows then 1 else 0) in
      let label = if label_w <= left then label else cut look left label in
      (label, V.width label)
  in
  let bar_w = if grows then max 1 (min bar_max (room - column)) else 0 in
  let rest = reserve + max 0 (room - column - bar_w) in
  let total = ref fixed in
  let line =
    V.concat
      (List.map
         (function
           | Name when ends ->
               total := !total + V.width label;
               label
           | Name ->
               total := !total + column;
               V.(label ++ pad (column - V.width label))
           | Rest ->
               total := !total + rest;
               pad rest
           | Grows ->
               total := !total + bar_w;
               bar look r bar_w
           | Shown v -> v)
         cells)
  in
  ( (if !total <= width then line else cut look width line),
    fits || V.width label = label_w )

let live look ~time ?width ?columns layout r =
  let style =
    List.find_map
      (fun (p : Progress_layout.piece) ->
        match p.kind with Label s -> Some s | _ -> None)
      (Progress_layout.for_task layout r)
  in
  let style = Option.value style ~default:Style.bold in
  fst (laid look ~time ?width ?columns ~label:(V.ink style (label r)) layout r)

let gap_suffix look gap =
  if gap >= 1. then muted look ("  +" ^ Task_text.duration gap) else V.empty

(* A task's output after the mark of its kind; a Logs message after the
   mark of its level, none for an application or an information message. *)
let lead_mark look = function
  | Output -> V.(mark look `Info ++ text " ")
  | Logs (`App | `Info) -> V.empty
  | Logs ((`Warning | `Error | `Debug) as l) ->
      V.(mark look (l :> Theme.mark) ++ text " ")

let body look (line : line) = V.(text line.text ++ gap_suffix look line.gap)

let tail look r =
  Queue.fold
    (fun acc line ->
      (line.seq, V.(lead_mark look line.kind ++ body look line)) :: acc)
    [] (window r)
  |> List.rev

(* [lead], then the lines of [body] broken to [width], each after the
   first under its start; without a lead, broken as the terminal wraps
   them. *)
let hanging look ~width lead body =
  let w = V.width lead in
  (* Only a room narrower than the lead cuts a line. *)
  List.map (cut look width)
    (match
       if w = 0 then V.chop (max 1 width) body
       else V.wrap (max 1 (width - w)) body
     with
    | first :: rest ->
        V.(lead ++ first) :: List.map (fun l -> V.(pad w ++ l)) rest
    | [] -> [ lead ])

(* The count and the time of a task that ended at the end of the last line
   of its label: shorter and out of their columns, then the count alone. *)
let meta look r stage =
  let count = compact_count look r in
  let time = muted look (Task_text.duration (elapsed r)) in
  match stage with
  | 0 -> if V.is_empty count then time else V.(count ++ text "  " ++ time)
  | 1 -> count
  | _ -> V.empty

(* A label is never wrapped in fewer cells than it keeps when it is cut. *)
let narrowest = 12

(* The line of a task that ended is a row of {!Progress_layout.ended}: its
   mark, then its label, bar, count, rate and time in the columns of the
   rows, giving way as they do. With [whole], a label that does not fit is
   never cut: it goes on over the lines below, at the full width left, and
   the count and the time go two cells after its last line, or give way. *)
let ended look ?width ?columns ~whole r =
  let label = text (label_of r) in
  let line, fits =
    laid look ~time:0. ?width ?columns ~label Progress_layout.ended r
  in
  match width with
  | Some width when whole && not fits ->
      let lead = V.(mark_of look r ++ text " ") in
      let lead_w = V.width lead in
      let label_w = V.width label in
      let lines =
        if width - lead_w >= min narrowest label_w then
          match V.wrap (max 1 (width - lead_w)) label with
          | first :: rest ->
              V.(lead ++ first) :: List.map (fun l -> V.(pad lead_w ++ l)) rest
          | [] -> [ lead ]
        else
          let indent = max 0 (min lead_w (width - narrowest)) in
          lead
          :: List.map
               (fun l -> V.(pad indent ++ l))
               (V.wrap (max 1 (width - indent)) label)
      in
      let rev = List.rev lines in
      let last = List.hd rev in
      let rec place stage =
        let m = meta look r stage in
        if V.is_empty m || V.width last + 2 + V.width m <= width then m
        else place (stage + 1)
      in
      let m = place 0 in
      let last = if V.is_empty m then last else V.(last ++ text "  " ++ m) in
      List.rev_map (cut look width) (last :: List.tl rev)
  | _ -> [ line ]

let dropped_line look n what =
  muted look
    (Fmt.str "%s %d %s dropped" (Widget.ellipsis look) n
       (if n = 1 then what else what ^ "s"))

let output look ~width r =
  let dropped =
    if dropped r = 0 then []
    else [ cut look width (dropped_line look (dropped r) "line") ]
  in
  dropped
  @ List.concat_map
      (fun line ->
        hanging look ~width (lead_mark look line.kind) (body look line))
      (List.rev (Queue.fold (fun acc l -> l :: acc) [] (output r)))

let finished look ~failed ~cancelled n (m : Theme.mark) =
  let some n what = if n = 0 then "" else Fmt.str ", %d %s" n what in
  V.(
    mark look m ++ text " "
    ++ muted look
         (Fmt.str "%d done%s%s" n (some failed "failed")
            (some cancelled "cancelled")))

let shed look n = dropped_line look n "task"

let more look n =
  muted look (Fmt.str "%s %d more running" (Widget.ellipsis look) n)

(* The lines of tasks that run at once interleave, so each names its task
   by its path, the labels from its top task down, rather than by an
   indentation that the line above may belong to another tree. *)

let separator look = Widget.glyph look " \u{25B8} " ~ascii:" > "

(* The labels above the task, muted, then [own]. *)
let path look r own =
  let above =
    match List.rev (path r) with _ :: up -> List.rev up | [] -> []
  in
  V.concat
    (List.map (fun l -> muted look (l ^ separator look)) above @ [ text own ])

(* A line of output is two columns in from the lines of tasks. *)
let under_task = text "  "

(* The same, compact, on one line, which says how a task that did not
   succeed ended, as no colour may. *)
let appended look r =
  let count = match count_text (metric r) with "" -> "" | c -> "  " ^ c in
  let ended =
    match Option.value (outcome r) ~default:`Cancelled with
    | `Succeeded -> ""
    | `Failed -> "  failed"
    | `Cancelled -> "  cancelled"
  in
  V.(
    mark_of look r ++ text " "
    ++ path look r (label_of r)
    ++ muted look (count ^ "  " ^ Task_text.duration (elapsed r) ^ ended))

(* In a room, a line that does not fit goes on over the lines below, under
   its start; without one it is a line of its own, as long as it is. *)
let folded look ?width lead body =
  match width with
  | None -> [ V.(lead ++ body) ]
  | Some width -> hanging look ~width lead body

(* The mark leads, so that a line that goes on over the next ones goes on
   under its text. *)
let labelled look ?width r line =
  folded look ?width
    V.(under_task ++ lead_mark look line.kind)
    V.(path look r (label r) ++ muted look ": " ++ body look line)

let under look ?width line =
  folded look ?width V.(under_task ++ lead_mark look line.kind) (body look line)

let dropped look n = V.(under_task ++ dropped_line look n "line")
let started look r = V.(mark look `Running ++ text " " ++ path look r (label r))

(* A line of the root is output of no task: a Logs message after its
   level's mark, and a line the program printed after none. *)
let message look ?width (line : line) =
  let kind = match line.kind with Output -> Logs `App | k -> k in
  folded look ?width (lead_mark look kind) (text line.text)

(* The worst of how the work and its tasks ended. *)
let state root : Theme.mark =
  let w = work root in
  match outcome root with
  | None -> `Running
  | Some o ->
      if o = `Failed || Work.failed w > 0 then `Failed
      else if o = `Cancelled || Work.cancelled w > 0 then `Cancelled
      else `Succeeded

(* In a room, the summary's name gives way last: the time and the count go
   first. *)
let summary look ?width root =
  let w = work root in
  Option.map
    (fun h ->
      let meta =
        muted look
          (String.concat ""
             [
               " ";
               Task_text.duration (elapsed root);
               " (";
               string_of_int (Work.finished w);
               "/";
               string_of_int (Work.started w);
               ")";
             ])
      in
      let lead = V.(mark look (state root) ++ text " ") in
      let h = V.ink (Theme.header (theme look)) h in
      let full = V.(lead ++ h ++ meta) in
      match width with
      | Some width when V.width full > width -> cut look width V.(lead ++ h)
      | _ -> full)
    (Work.summary w)

let verdict look ?width root =
  match (outcome root, Task_core.message root) with
  | Some o, Some msg -> (
      let lead = V.(mark look (outcome_mark o) ++ text " ") in
      let msg = V.ink Style.bold msg in
      match width with
      | None -> [ V.(lead ++ msg) ]
      | Some width -> (
          let w = V.width lead in
          match V.wrap (max 1 (width - w)) msg with
          | first :: rest ->
              V.(lead ++ first) :: List.map (fun l -> V.(pad w ++ l)) rest
          | [] -> [ lead ]))
  | _ -> []
