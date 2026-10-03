(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {1 Shared building blocks} *)

(* Colours, ANSI helpers and the themes all come from {!Theme}; the live display
   renders with them. *)
open Ansi

type color = Color.t

let src = Logs.Src.create "console.display" ~doc:"Console activity display"

module Log = (val Logs.src_log src : Logs.LOG)

let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(** {1 Human-readable quantities} *)

let bytes n =
  let units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB" |] in
  let rec pick i v =
    if v < 1024. || i = Array.length units - 1 then (v, units.(i))
    else pick (i + 1) (v /. 1024.)
  in
  let v, u = pick 0 (Int64.to_float n) in
  if u = "B" then Fmt.str "%.0f %s" v u else Fmt.str "%.2f %s" v u

(* A compact elapsed: "1.3s" below a minute, then "1m05s", then "1h02m". *)
let span secs =
  if secs < 60. then Fmt.str "%.1fs" (Float.max 0. secs)
  else
    let s = int_of_float secs in
    if s < 3600 then Fmt.str "%dm%02ds" (s / 60) (s mod 60)
    else Fmt.str "%dh%02dm" (s / 3600) (s / 60 mod 60)

(* The columns a row's timer is laid out in. {!span} is six wide at its widest
   below a hundred hours ("59m59s", "23h59m"), so a timer right-aligned in six
   does not widen as it crosses ten seconds, a minute or an hour, and nothing
   to its left moves while it ticks. A longer run takes the columns it needs
   rather than losing a digit. *)
let elapsed_cell = 6

(* Caller text is validated before it participates in width budgeting: a
   malformed sequence occupies a replacement-glyph cell in real terminals and
   controls could otherwise inject escapes or desynchronise redraw geometry. *)
let sanitize = Render.sanitize ~keep_newlines:false

(* Split on newlines, dropping only the final empty field introduced by one
   trailing newline. Internal blank lines are semantic too. *)
let lines_of_string s =
  if s = "" then []
  else
    let lines = String.split_on_char '\n' s in
    let lines =
      match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
    in
    List.map
      (fun line ->
        let n = String.length line in
        if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line)
      lines

let sanitize_multiline s =
  String.concat "\n" (List.map sanitize (lines_of_string s))

(* The live region currently painted on the terminal. [suspend] (and hence the
   Logs reporter) clears it before foreign stdout writes and redraws it
   after. *)
type region = {
  ppf : Format.formatter;
  clear : unit -> unit;
  redraw : unit -> unit;
}

let active_region : region option ref = ref None
let activity_claimed = Atomic.make false

(* Run [f] with [ppf]'s margin set to [max_int] so ANSI escapes and raw control
   characters pass through without wrapping.

   The flush belongs to the restore rather than to [f]. [Format.pp_set_margin]
   reinitialises the formatter, which throws away whatever is queued and not
   yet written, and a margin of [max_int] means nothing is written until a
   flush: the box a formatter opens has no known size until then, so every
   token behind it waits. A body that wrote and did not flush therefore had its
   whole output discarded by the restore -- which is what dropped a build's
   result block, its digest, its output directory and its mark, on a terminal,
   while the same session in an appending log kept all of it. Flushing here
   makes the loss unrepresentable rather than something each caller has to
   remember; the callers that already flush are unaffected, because a flush
   with nothing queued writes nothing. *)
let with_unlimited_margin ppf f =
  let old_margin = Format.pp_get_margin ppf () in
  Format.pp_set_margin ppf max_int;
  Fun.protect f ~finally:(fun () ->
      Format.pp_print_flush ppf ();
      Format.pp_set_margin ppf old_margin)

(* Clear [n] lines: the current one + (n-1) above; cursor ends at column 0 of
   the topmost cleared line. *)
let pp_clear ppf n =
  if n > 0 then begin
    Format.pp_print_string ppf "\r\027[K";
    for _ = 1 to n - 1 do
      Format.pp_print_string ppf "\027[1A\027[2K"
    done
  end

let pp_block ppf lines =
  let rec write = function
    | [] -> ()
    | [ last ] -> Format.pp_print_string ppf last
    | line :: rest ->
        Format.pp_print_string ppf line;
        (* A row may occupy the terminal's final column.  An explicit carriage
           return cancels VT pending-wrap before the line feed, independently
           of whether the host tty has ONLCR enabled. *)
        Format.pp_print_string ppf "\r\n";
        write rest
  in
  write lines

(* Rewrite only rows whose rendered bytes changed. The cursor starts somewhere
   on the last row of the old region and finishes on the last row again. A row
   is erased {e after} its replacement is written, so there is no blank flash;
   the erase only removes a stale suffix when the new row is shorter. *)
let pp_update ppf ~old_lines ~new_lines =
  let old_lines = Array.of_list old_lines in
  let new_lines = Array.of_list new_lines in
  let n = Array.length new_lines in
  if n > 0 then begin
    Format.pp_print_string ppf "\r";
    for _ = 1 to n - 1 do
      Format.pp_print_string ppf "\027[1A"
    done;
    for i = 0 to n - 1 do
      if not (String.equal old_lines.(i) new_lines.(i)) then begin
        Format.pp_print_string ppf new_lines.(i);
        Format.pp_print_string ppf "\027[K"
      end;
      if i < n - 1 then Format.pp_print_string ppf "\r\n"
    done
  end

(** {1 Interleaving foreign output} *)

let suspend f =
  match !active_region with
  | None -> f ()
  | Some r ->
      r.clear ();
      Fun.protect f ~finally:r.redraw

let logs_reporter (r : Logs.reporter) : Logs.reporter =
  let report src level ~over k msgf =
    r.Logs.report src level ~over k (fun construction ->
        suspend (fun () -> msgf construction))
  in
  { Logs.report }

(* The reporter Logs had before a display took over the terminal. A display
   wraps it on install and puts it back on teardown, so a program never has to
   know that printing a log line must suspend a live region, and successive
   displays do not nest one wrapper inside another. *)
let saved_reporter : Logs.reporter option ref = ref None

let install_reporter () =
  match !saved_reporter with
  | Some _ -> ()
  | None ->
      let r = Logs.reporter () in
      saved_reporter := Some r;
      Logs.set_reporter (logs_reporter r)

let uninstall_reporter () =
  match !saved_reporter with
  | None -> ()
  | Some r ->
      saved_reporter := None;
      Logs.set_reporter r

let show_cursor_on ppf =
  try
    Format.pp_print_string ppf show_cursor;
    Format.pp_print_flush ppf ()
  with Sys_error _ -> ()

(* The formatter that hid the cursor is the one that shows it. *)
let restore_terminal () =
  match !active_region with Some r -> show_cursor_on r.ppf | None -> ()

(** {1 Row layout}

    A small composable DSL for the look of a row, in the spirit of
    [Progress.Line] from the [progress] library: build a list of pieces and the
    row renders them left to right. The default is itself built from these
    primitives. *)

module Line = struct
  type piece =
    | Spinner  (** animated spinner; a green check once finished *)
    | Label  (** the row's label (or finish message), bold *)
    | Bar of int  (** an [n]-cell filled bar *)
    | Percent  (** [ 75%] *)
    | Count  (** [3/4] for counts, ["30.09 MiB / 2.63 GiB"] for bytes *)
    | Rate  (** transfer rate; byte rows only *)
    | Elapsed  (** the row's running timer *)
    | Text of string  (** a literal separator *)
    | Accent of string  (** a literal rendered with the row accent *)
    | Spacer  (** flexible gap; absorbs slack to push later pieces right *)
    | Marker of (frame:int -> Span.t)
        (** an activity marker drawn anew each frame; the outcome once finished
        *)

  type t = { pieces : piece list; color : color }

  let spinner = Spinner
  let label = Label

  let bar n =
    if n < 0 then invalid_arg "Console.Display.Line.bar: negative width";
    Bar n

  let percent = Percent
  let count = Count
  let rate = Rate
  let elapsed = Elapsed
  let text s = Text (sanitize s)
  let accent s = Accent (sanitize s)
  let spacer = Spacer
  let marker f = Marker f

  let v ?(color = Color.cyan) pieces =
    let count piece =
      List.fold_left
        (fun n candidate -> if candidate = piece then n + 1 else n)
        0 pieces
    in
    if count Label > 1 then
      invalid_arg "Console.Display.Line.v: more than one label";
    if count Spacer > 1 then
      invalid_arg "Console.Display.Line.v: more than one spacer";
    { pieces; color }

  let with_color color t = { t with color }

  let default =
    v
      [
        spinner;
        text " ";
        label;
        spacer;
        bar 10;
        text " ";
        count;
        text " ";
        rate;
        text " ";
        elapsed;
      ]
end

(** {1 Display} *)

type mode = [ `Auto | `Activity | `History ]
type metric = Spin | Count of int * int | Bytes of int64 * int64

(* The pair a row's count cell holds: items done over items announced, or the
   same in bytes. *)
let count_text = function
  | Spin -> ""
  | Count (c, tot) -> Fmt.str "%d/%d" c tot
  | Bytes (c, tot) -> bytes c ^ " / " ^ bytes tot

(* The columns [metric] needs: the pair it ends on, so the cell is sized from
   the total before the first update and a count does not walk left as it gains
   digits, and the pair it holds now, which is wider only where a unit changes
   under it ("1023.99 KiB" against a total of "1.00 MiB"). *)
let count_width metric =
  let full =
    match metric with
    | Spin -> Spin
    | Count (_, total) -> Count (total, total)
    | Bytes (_, total) -> Bytes (total, total)
  in
  max (String.length (count_text metric)) (String.length (count_text full))

module Event = struct
  type level = [ `Info | `Warning | `Error ]
  type outcome = [ `Succeeded | `Failed | `Cancelled ]
  type result = [ `Ok | `Fail ]

  type kind =
    [ `Message of level
    | `Started
    | `Log of level
    | `Finished of outcome
    | `Result of result ]

  let equal_outcome a b =
    match (a, b) with
    | `Succeeded, `Succeeded | `Failed, `Failed | `Cancelled, `Cancelled -> true
    | _ -> false

  type t = {
    elapsed : float;
    task : int option;
    key : string option;
    path : string list;
    kind : kind;
    message : string;
    detail : string option;
  }

  let elapsed t = t.elapsed
  let task t = t.task
  let key t = t.key
  let path t = t.path
  let kind t = t.kind
  let message t = t.message
  let detail t = t.detail

  let pp_kind ppf = function
    | `Message `Info -> Fmt.string ppf "info"
    | `Message `Warning -> Fmt.string ppf "warning"
    | `Message `Error -> Fmt.string ppf "error"
    | `Started -> Fmt.string ppf "started"
    | `Log `Info -> Fmt.string ppf "log"
    | `Log `Warning -> Fmt.string ppf "warning-log"
    | `Log `Error -> Fmt.string ppf "error-log"
    | `Finished `Succeeded -> Fmt.string ppf "succeeded"
    | `Finished `Failed -> Fmt.string ppf "failed"
    | `Finished `Cancelled -> Fmt.string ppf "cancelled"
    | `Result `Ok -> Fmt.string ppf "ok"
    | `Result `Fail -> Fmt.string ppf "fail"

  let pp ppf t =
    Fmt.pf ppf "@[%a%s%s: %s%a@]" pp_kind t.kind
      (if t.path = [] then "" else " ")
      (String.concat "/" t.path) t.message
      Fmt.(option (any " -- " ++ string))
      t.detail
end

(* The effects a live display needs from its environment: where to write, the
   clock that drives the animation and how to wait on it, the terminal geometry
   to fit, and whether the output is a terminal. The eio driver supplies these. *)
type view = {
  ppf : Format.formatter;
  now : unit -> float;
  wait : (float -> unit) option;
  dimensions : unit -> int * int;
  is_tty : bool;
  char_width : (Uchar.t -> int) option;
}

type ctx = Fresh of view

let view ~ppf ~now ~wait ~dimensions ~is_tty ~char_width =
  { ppf; now; wait; dimensions; is_tty; char_width }

let ctx ~ppf ~now ?wait ~dimensions ~is_tty () =
  Fresh (view ~ppf ~now ~wait ~dimensions ~is_tty ~char_width:None)

let ctx_ppf (Fresh c) = c.ppf
let ctx_now (Fresh c) = c.now ()

let with_char_width (Fresh c) char_width =
  Fresh { c with char_width = Some char_width }

let view_of_ctx (Fresh c) = c

(* A non-interactive context -- standard output, a fixed 80x24 geometry, a zero
   clock, not a tty -- for rendering to a string or a test, where no real
   terminal or clock is in play. *)
let dumb () =
  ctx ~ppf:Format.std_formatter
    ~now:(fun () -> 0.)
    ~dimensions:(fun () -> (80, 24))
    ~is_tty:false ()

type t = {
  dppf : Format.formatter;
  plain : bool;
      (* append-only lines instead of a live region: a surface with no terminal
         behind it, or a caller that asked for [`History] *)
  styling : bool;
      (* whether [dppf] takes ANSI, read once from {!Ansi.should_style}. It is a
         separate question from [plain]: a terminal whose reader turned colour
         off is still a terminal, so the region keeps redrawing in place *)
  now : unit -> float;
  wait : (float -> unit) option;
  dimensions : unit -> int * int;
      (* terminal (width, height), re-read each paint so a resize is handled *)
  char_width : (Uchar.t -> int) option;
  header : string option;
  default_style : Line.t;
  theme : Theme.t;
  palette : color list;
  dstart : float;
  mutable width : int;
  mutable height : int;
      (* current terminal geometry. [height] caps the live region so a redraw
         never scrolls a stale copy into unreachable scrollback; [width] bounds
         every emitted line so nothing wraps onto a second physical row (which
         would desync the one-line-per-row redraw maths). Re-read each paint
         unless pinned, so a resize never corrupts the display. *)
  auto_width : bool;
  auto_height : bool;
  mutable rows : row list; (* insertion order *)
  mutable count_cell : int;
      (* the columns every row lays its count pair in: the widest any row here
         has needed. One width for the whole block, so a row counting to 2 puts
         its pair in the same columns as one counting to 470, and monotone, so
         a block already on screen never narrows under a later row *)
  by_key : (string, row) Hashtbl.t;
  mutable events_rev : Event.t list;
      (* permanent semantic events, newest first *)
  mutable next_task_id : int;
  mutable pending :
    ([ `Ok of string | `Fail of string ] * string option) option;
      (* the verdict recorded by {!set_result}; rendered when the scope closes *)
  mutable finished : int;
  mutable live_height : int;
  mutable last_lines : string list; (* the lines currently on screen *)
  mutable painted_at : float; (* when the region was last painted *)
  mutable torn_down : bool;
  mutable owns_activity : bool;
  mutable suspended : int;
  mutable frame : int; (* the ticks that refreshed the projection *)
  mutable scene : (frame:int -> width:int -> Span.t list) option;
}

and row_log = {
  ltime : float;
  lgap : float;
  llevel : Event.level;
  lmessage : string;
  lkept : bool;
      (* history takes it when it leaves the tail; a window line is dropped *)
}

and row = {
  rid : int;
  rkey : string option;
  disp : t;
  parent : row option;
  style : Line.t;
  log_lines : int; (* size of the live log tail shown under the row; 0 = none *)
  mutable logs : row_log list;
      (* the last [log_lines] lines, oldest first; each paired with the gap (in
         seconds) since the previous log line, so a stall between two lines is
         visible rather than silent *)
  mutable log_prev_t : float; (* time of the previous log line *)
  mutable rlabel : string;
  mutable metric : metric;
  transient : bool; (* drawn while it runs; its ending commits no line *)
  group : bool;
      (* its rows' permanent lines wait for it, and it settles with its last
         row *)
  mutable requested : (Event.outcome * string option) option;
      (* the first ending asked of a group while rows under it were open *)
  mutable swept : bool;
  mutable waiting : bool;
      (* settled under a group that has not, so its line is not written yet *)
  mutable spilled : row_log list;
      (* kept log lines that left the row while it waited for its group *)
  rstart : float;
  mutable rfinish : float option;
  mutable rmessage : string option;
  mutable routcome : Event.outcome option;
  (* EWMA byte-rate sampling *)
  mutable last_cur : int64;
  mutable last_t : float;
  mutable rate : float;
}

type task = row

(* Nested library calls compose into the domain's current display.  The outer
   scope alone owns configuration, completion and terminal teardown.  A DLS
   binding keeps independent domains separate; the activity claim below still
   prevents two domains from repainting the same terminal concurrently. *)
let current = Domain.DLS.new_key (fun () -> None)

let pp ppf t =
  Fmt.pf ppf "<tty-display %dx%d %d/%d rows%s>" t.width t.height t.finished
    (List.length t.rows)
    (if t.torn_down then " torn-down" else "")

(* Every SGR the display adds passes through these, so one answer --
   {!Ansi.should_style} on the formatter this display writes to -- decides
   colour for the whole surface, the way it decides it for {!Span} and so for
   every other surface here. They shadow the unconditional {!Ansi} forms on
   purpose: a call site that reaches past them is a second answer. *)
let code t sequence = if t.styling then sequence else ""
let styled t sequence s = if t.styling then Ansi.styled sequence s else s
let dimmed t s = if t.styling then Ansi.dimmed s else s
let render_width t = t.width
let string_width t = Width.string_width ?char_width:t.char_width
let truncate t = Width.truncate ?char_width:t.char_width
let shorten t = Width.shorten ?char_width:t.char_width
let fold t = Width.fold ?char_width:t.char_width
let pad_left t = Width.pad_left ?char_width:t.char_width
let rec depth = function None -> 0 | Some r -> 1 + depth r.parent

(* Plain (append-only) mode keeps a flat indent: a committed line is emitted
   before later siblings are known, so it cannot draw a stable tree guide. *)
let indent row = String.make (2 * depth row.parent) ' '

(* The live (tty) view draws real tree guides, sharing {!Tree.prefix}. A row's
   guide is computed from its position among siblings each repaint. *)
let siblings t row =
  List.filter
    (fun r ->
      match (r.parent, row.parent) with
      | Some a, Some b -> a == b
      | None, None -> true
      | _ -> false)
    t.rows

let is_last_child t row =
  match List.rev (siblings t row) with last :: _ -> last == row | [] -> true

let rec lasts_path t row =
  let own = is_last_child t row in
  match row.parent with None -> [ own ] | Some p -> lasts_path t p @ [ own ]

let guide_prefix t row =
  match row.parent with
  | None -> ""
  | Some _ ->
      let nested = match lasts_path t row with _ :: rest -> rest | [] -> [] in
      "  " ^ Tree.prefix ~guide:(Theme.guide t.theme) nested

(* The vertical-bar column beneath a row, under which its log tail sits: each
   level is a pipe when more siblings follow, or blank when it was the last. *)
let continuation t row =
  let g = Theme.guide t.theme in
  let nested = match lasts_path t row with _ :: rest -> rest | [] -> [] in
  "  "
  ^ String.concat ""
      (List.map
         (fun is_last -> if is_last then Guide.space g else Guide.pipe g)
         nested)

let elapsed t row =
  match row.rfinish with
  | Some f -> f -. row.rstart
  | None -> t.now () -. row.rstart

let percent cur total =
  if total <= 0 then 0
  else if cur >= total then 100
  else int_of_float (100. *. (float cur /. float total))

let percent64 cur total =
  if total <= 0L then 0
  else if cur >= total then 100
  else int_of_float (100. *. (Int64.to_float cur /. Int64.to_float total))

let pct_of = function
  | Spin -> None
  | Count (cur, total) -> Some (percent cur total)
  | Bytes (cur, total) -> Some (percent64 cur total)

(* Render one style piece against a row's current data; [""] when the piece does
   not apply. [Label] and [Spacer] render empty here -- they are substituted
   after width budgeting. Glyphs come from the display's {!Theme.t}. *)
let render_spinner t ~finished ~color ~frame row =
  (* Match the header's marker vocabulary so a row and the summary never use two
     different glyphs for the same state: green [+] done, red [x] on error, the
     animated spinner while running. *)
  let theme = t.theme in
  if not finished then
    styled t (color_code color) (Spinner.frame (Theme.spinner theme) frame)
  else
    match row.routcome with
    | Some `Failed -> styled t (bold ^ color_code (Theme.fail theme)) "[x]"
    | Some `Cancelled -> styled t (bold ^ color_code (Theme.warn theme)) "[~]"
    | Some `Succeeded | None ->
        styled t (bold ^ color_code (Theme.ok theme)) (Theme.done_marker theme)

let render_bar t row ~finished ~color ~width =
  if finished then ""
  else
    let theme = t.theme in
    match (row.metric, pct_of row.metric) with
    | (Count (0, _) | Bytes (0L, _)), Some _ -> ""
    | (Count _ | Bytes _), Some p ->
        (* Bare delimiters look like an empty input field, not progress. Once
           work has advanced, guarantee at least the theme's smallest visible
           fill unit. *)
        let units =
          match Theme.bar theme with
          | `Smooth -> width * 8
          | `Blocky | `Rainbow -> width
        in
        let minimum = if units = 0 then 0 else (100 + units - 1) / units in
        Bar.render ~theme ~color ~styled:t.styling ~width ~pct:(max minimum p)
          ()
    | _, None -> ""
    | Spin, Some _ -> assert false

let render_percent t row ~finished =
  if finished then ""
  else
    match pct_of row.metric with
    | Some p -> Fmt.kstr (dimmed t) "%3d%%" p
    | None -> ""

(* A cell's text right-aligned in it, the padding outside the styling so an
   empty cell carries no escapes. *)
let dim_cell t width text =
  if text = "" then String.make (max 0 width) ' '
  else pad_left t width (dimmed t text)

let render_count t row = dim_cell t t.count_cell (count_text row.metric)

let render_rate t row ~finished =
  if finished then ""
  else
    match row.metric with
    | Bytes _ when row.rate > 1. ->
        dimmed t (bytes (Int64.of_float row.rate) ^ "/s")
    | _ -> ""

(* A span of the caller's, drawn in colour exactly when the surface takes it. *)
let scene_line t span =
  if t.styling then Span.to_ansi_string span else Span.to_string span

let render_piece t row ~finished ~color ~frame (piece : Line.piece) =
  match piece with
  | Line.Spinner -> render_spinner t ~finished ~color ~frame row
  | Line.Label | Line.Spacer -> ""
  | Line.Bar n -> render_bar t row ~finished ~color ~width:n
  | Line.Percent -> render_percent t row ~finished
  | Line.Count -> render_count t row
  | Line.Rate -> render_rate t row ~finished
  | Line.Elapsed -> dim_cell t elapsed_cell (span (elapsed t row))
  | Line.Text s -> s
  | Line.Accent s -> styled t (bold ^ color_code color) s
  | Line.Marker f ->
      if finished then render_spinner t ~finished ~color ~frame row
      else scene_line t (f ~frame:t.frame)

(* Render a row from its style. The label is the flexible content and a single
   [Spacer] absorbs the remaining slack so later pieces (the elapsed timer) sit
   at the right margin. A live row is one line, which the redraw counts on, so
   a label too wide for it loses words from its end, never characters; one
   whose first word does not fit is not drawn while the row runs, and the line
   the row commits carries it whole. *)
let render_row t row =
  let finished = row.rfinish <> None in
  let { Line.pieces; color } = row.style in
  let frame = int_of_float ((t.now () -. t.dstart) *. 10.) in
  let rendered =
    List.map (fun p -> (p, render_piece t row ~finished ~color ~frame p)) pieces
  in
  let indent_s = guide_prefix t row in
  let has_label = List.exists (fun (p, _) -> p = Line.Label) rendered in
  let has_spacer = List.exists (fun (p, _) -> p = Line.Spacer) rendered in
  let fixed_w =
    string_width t indent_s
    + List.fold_left
        (fun acc (p, s) ->
          match p with
          | Line.Label | Line.Spacer -> acc
          | _ -> acc + string_width t s)
        0 rendered
  in
  let label_text =
    if finished then Option.value row.rmessage ~default:row.rlabel
    else row.rlabel
  in
  (* Reserve one column for the spacer so the pieces flanking it never touch. *)
  let reserve = if has_spacer then 1 else 0 in
  let width = render_width t in
  let avail = max 0 (width - fixed_w - reserve) in
  let label_shown =
    if not has_label then ""
    else if string_width t label_text <= avail then label_text
    else shorten t avail label_text
  in
  let slack = max reserve (width - fixed_w - string_width t label_shown) in
  let buf = Buffer.create 80 in
  Buffer.add_string buf indent_s;
  List.iter
    (fun (p, s) ->
      match p with
      | Line.Label -> Buffer.add_string buf (styled t bold label_shown)
      | Line.Spacer -> Buffer.add_string buf (String.make slack ' ')
      | _ -> Buffer.add_string buf s)
    rendered;
  (* Final guard for pieces that are not the label: a row whose fixed pieces
     alone are wider than the terminal would wrap, and the redraw maths (which
     assumes one row per line) corrupts. *)
  truncate t width (Buffer.contents buf)

let committed_parts t row =
  let label = Option.value row.rmessage ~default:row.rlabel in
  let marker =
    (* A row that reaches history with no outcome is one the scrollback
       collapsed while it was still running, which is not an outcome a theme
       can name. Every outcome that exists is the theme's. *)
    match row.routcome with
    | Some outcome -> Theme.committed_marker t.theme outcome
    | None -> ".."
  in
  (indent row, marker, label, span (elapsed t row))

(* The marker's style for a committed row: the outcome's colour, or dim for a
   row that never reported one. One producer for the compact and the
   width-aligned rendering, which differ in layout alone. *)
let committed_marker_style t row =
  match row.routcome with
  | Some `Succeeded -> bold ^ color_code (Theme.ok t.theme)
  | Some `Failed -> bold ^ color_code (Theme.fail t.theme)
  | Some `Cancelled -> bold ^ color_code (Theme.warn t.theme)
  | None -> dim

(* Compact against width-aligned is the only difference between this line and
   {!styled_committed}: both read {!t.styling} through {!styled}/{!dimmed}, so
   the outcome's colour is on the marker and dim on the metadata on either
   surface. An appended line has no right edge to lay a grid against and no
   later line can move an earlier one, so its metric takes the columns it
   takes. *)
let plain_committed t row =
  let indent, marker, label, elapsed = committed_parts t row in
  let metric =
    match count_text row.metric with "" -> "" | text -> "  " ^ text
  in
  let metadata = metric ^ "  " ^ elapsed in
  Fmt.str "%s%s %s%s" indent
    (styled t (committed_marker_style t row) marker)
    label (dimmed t metadata)

let styled_committed t row =
  let marker_style = committed_marker_style t row in
  let indent, marker, label, elapsed = committed_parts t row in
  let width = render_width t in
  let marker = styled t marker_style marker in
  let fixed_width = string_width t indent + string_width t marker + 1 in
  (* Permanent TTY history keeps the same visual columns as the live row: the
     elapsed time reaches the terminal's right edge in a cell of its own, and
     the count sits in the block's cell immediately before it, so every row of
     a block ends its count where every other one does. Labels absorb all width
     variation. A terminal too narrow for both keeps the timer. *)
  let timer = pad_left t elapsed_cell elapsed in
  let metadata =
    if t.count_cell = 0 then timer
    else pad_left t t.count_cell (count_text row.metric) ^ "  " ^ timer
  in
  let metadata =
    if fixed_width + 2 + string_width t metadata <= width then metadata
    else timer
  in
  let metadata_width = string_width t metadata in
  let label_width = max 0 (width - fixed_width - metadata_width - 2) in
  (* History has no redraw to protect, so a label that does not fit folds
     onto the lines below it, like a long git subject in [git log --oneline]
     does in a terminal, rather than losing words; a word wider than the room
     stands whole and the terminal wraps it. *)
  let first, rest =
    match fold t label_width label with
    | first :: rest when string_width t first <= label_width -> (first, rest)
    | lines -> ("", lines)
  in
  let left = String.concat "" [ indent; marker; " "; first ] in
  let gap = width - string_width t left - metadata_width in
  let head =
    if gap > 0 then left ^ String.make gap ' ' ^ dimmed t metadata
    else truncate t width (left ^ dimmed t metadata)
  in
  let under = String.make fixed_width ' ' in
  String.concat "\r\n" (head :: List.map (fun line -> under ^ line) rest)

let complete t =
  let total = List.length t.rows in
  t.finished >= total

let any_error t = List.exists (fun r -> r.routcome = Some `Failed) t.rows
let any_cancelled t = List.exists (fun r -> r.routcome = Some `Cancelled) t.rows

(* The summary marker and its colour: [-] (accent) while work is in progress,
   then [+] (green) once every row is done, or [x] (red) if any finished in
   error. *)
let header_marker t =
  if not (complete t) then ("[-]", t.default_style.Line.color)
  else if
    any_error t
    ||
    match t.pending with
    | Some (`Fail _, _) -> true
    | Some (`Ok _, _) | None -> false
  then ("[x]", Theme.fail t.theme)
  else if any_cancelled t then ("[~]", Theme.warn t.theme)
  else ("[+]", Theme.ok t.theme)

(* The header: a bright marker, a bold name, a dim elapsed and done/total.
   One line under two names, [summary] and [header_styled], until the surface
   that asks for it decided the layout and the colour together; now [fit]
   alone separates the compact append-only line from the width-aligned
   terminal one, and both read {!t.styling} like every other line.

   [fit] bounds the line to the terminal width, like every row: an over-wide
   header that wrapped onto a second physical row would make the redraw
   under-clear by one line every frame and strand a stale header (the
   crawling-timestamp duplication). *)
let summary_line t ~fit =
  match t.header with
  | None -> None
  | Some h ->
      let marker, color = header_marker t in
      let meta =
        Fmt.str "%s (%d/%d)"
          (span (t.now () -. t.dstart))
          t.finished (List.length t.rows)
      in
      let h =
        if not fit then h
        else
          shorten t
            (render_width t - string_width t marker - string_width t meta - 2)
            h
      in
      let line =
        String.concat ""
          [
            code t (color_code color);
            marker;
            code t reset_code;
            " ";
            code t bold;
            h;
            code t reset_code;
            " ";
            dimmed t meta;
          ]
      in
      Some (if fit then truncate t (render_width t) line else line)

(* A live row split into its own line and its log tail (dimmed and indented
   beneath it, like buildkit's "=> => # ..." lines). Keeping the two apart lets
   the height cap drop log lines while preserving the tree of rows. *)
(* A log line's trailing time gap, shown only when at least a second elapsed
   since the previous line, so rapid output stays clean but a stall stands out
   (e.g. ["# linking  +18.0s"]). *)
let log_gap_threshold = 1.0
let gap_suffix gap = if gap >= log_gap_threshold then "  +" ^ span gap else ""

let level_marker theme = function
  | `Info -> Theme.note_marker theme
  | `Warning -> Theme.warn_marker theme
  | `Error -> Theme.error_marker theme

let level_style theme = function
  | `Info -> dim
  | `Warning -> bold ^ color_code (Theme.warn theme)
  | `Error -> bold ^ color_code (Theme.fail theme)

let styled_log t level marker message suffix =
  String.concat ""
    [
      styled t (level_style t.theme level) marker; " "; message; dimmed t suffix;
    ]

let row_block_parts t row =
  let prefix = continuation t row in
  let tail =
    List.concat_map
      (fun log ->
        let marker = level_marker t.theme log.llevel in
        let suffix = gap_suffix log.lgap in
        (* The message is what has to give when the room runs out: the marker
           says how loud the line is and the suffix how long the gap before it
           was. It gives words from its end, so what is drawn is the line's own
           start and never a line cut through a word; one whose first word does
           not fit folds under the marker, whole. *)
        let room =
          render_width t - string_width t prefix - string_width t marker - 1
          - string_width t suffix
        in
        let lines =
          match shorten t room log.lmessage with
          | "" when room > 0 -> fold t ~split:true room log.lmessage
          | "" -> []
          | shown -> [ shown ]
        in
        let under = String.make (string_width t marker) ' ' in
        List.mapi
          (fun i line ->
            let last = i = List.length lines - 1 in
            truncate t (render_width t)
              (String.concat ""
                 [
                   prefix;
                   (if i = 0 then
                      styled_log t log.llevel marker line
                        (if last then suffix else "")
                    else
                      under ^ " " ^ line ^ if last then dimmed t suffix else "");
                 ]))
          lines)
      row.logs
  in
  (* The tail is [log_lines] high on screen whatever its lines fold into. *)
  let tail =
    let excess = List.length tail - row.log_lines in
    if excess > 0 then List.filteri (fun i _ -> i >= excess) tail else tail
  in
  (render_row t row, tail)

let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

(* Keep the live region within the terminal height. A redraw clears the old
   region with cursor-relative moves ([ESC[1A]); those cannot reach a line that
   scrolled into scrollback, so a region taller than the screen would leave a
   stale copy behind on the next paint, which reads as a duplicated row.

   Trim like buildkit: the rows (the tree skeleton) always stay; only the log
   windows are shed, oldest first, until the region fits, with a marker counting
   what was hidden. If even the bare rows overflow, keep the most recent
   ones. *)
let region_marker t hidden =
  truncate t (render_width t)
    (Fmt.kstr (dimmed t) "  ... (%d more lines)" hidden)

let cap_rows t ~header ~budget ~total blocks =
  (* Even the bare rows do not fit. Keep the phase/root and the active leaf,
     collapsing the middle so both context and current work remain visible. *)
  let rows = List.map fst blocks in
  match rows with
  | [] -> header
  | first :: rest ->
      if budget = 1 then header @ [ List.hd (List.rev rows) ]
      else if budget = 2 then header @ [ first; List.hd (List.rev rest) ]
      else
        let shown = drop (List.length rest - (budget - 2)) rest in
        let hidden = total - 1 - List.length shown in
        header @ (first :: region_marker t hidden :: shown)

let cap_logs t ~header ~budget ~total blocks =
  (* Shed oldest logs while keeping every task row and one collapse marker. *)
  if List.length blocks = budget then header @ List.map fst blocks
  else
    let to_drop = total - (budget - 1) in
    let flat =
      List.concat_map
        (fun (line, logs) -> `Row line :: List.map (fun l -> `Log l) logs)
        blocks
    in
    let rec shed dropped acc = function
      | rest when dropped >= to_drop ->
          List.rev_append acc (List.map (function `Row s | `Log s -> s) rest)
      | `Log _ :: tl -> shed (dropped + 1) acc tl
      | `Row s :: tl -> shed dropped (s :: acc) tl
      | [] -> List.rev acc
    in
    header @ (region_marker t to_drop :: shed 0 [] flat)

let cap_region t ~header ~blocks =
  (* Reserve one row for the cursor / next commit. Emitting beyond this capacity
     would scroll an old frame out of reach of the relative clear. *)
  let capacity = max 0 (t.height - 1) in
  let header = if capacity = 0 then [] else header in
  let budget = max 0 (capacity - List.length header) in
  let total =
    List.fold_left (fun n (_, logs) -> n + 1 + List.length logs) 0 blocks
  in
  if budget = 0 then header
  else if total <= budget then
    header @ List.concat_map (fun (line, logs) -> line :: logs) blocks
  else if List.length blocks > budget then
    cap_rows t ~header ~budget ~total blocks
  else cap_logs t ~header ~budget ~total blocks

(* A row under a group that has not settled keeps its permanent lines until
   the group does, so the group's line and its rows' are written as one block,
   the group's first, and the rows stay drawn beneath it in the meantime. An
   appending surface has no region to keep them in and writes each line as it
   comes. *)
let rec under_open_group r =
  match r.parent with
  | None -> false
  | Some p -> (p.group && p.rfinish = None) || under_open_group p

let waits_for_group r = (not r.disp.plain) && under_open_group r

let rec row_live row =
  (row.rfinish = None
  || (waits_for_group row && not (row.swept || row.transient)))
  && match row.parent with None -> true | Some parent -> row_live parent

(* The scene goes above the header only when the region still holds every
   running row beneath it: it repeats what the rows say, so it is the first
   thing to give way. *)
let scene_lines t ~header ~blocks =
  match t.scene with
  | None -> []
  | Some scene ->
      let width = render_width t in
      let lines =
        List.map
          (fun span ->
            scene_line t (Span.truncate ?char_width:t.char_width width span))
          (scene ~frame:t.frame ~width)
      in
      let capacity = max 0 (t.height - 1) in
      if List.length lines + List.length header + List.length blocks <= capacity
      then lines
      else []

let live_lines t =
  let header =
    match summary_line t ~fit:true with Some s -> [ s ] | None -> []
  in
  let active = List.filter row_live t.rows in
  let blocks = List.map (row_block_parts t) active in
  let header = scene_lines t ~header ~blocks @ header in
  cap_region t ~header ~blocks

(* Re-read the terminal geometry (unless pinned) so a resize between frames is
   picked up before the next clear/redraw, the way buildkit reads the size every
   frame. A stale, too-large width would let a line wrap; a stale height would
   mis-size the cap -- either desyncs the redraw. *)
let refresh_size t =
  let old_width = t.width and old_height = t.height in
  if t.auto_width || t.auto_height then begin
    let width, height = t.dimensions () in
    if t.auto_width then t.width <- max 1 width;
    if t.auto_height then t.height <- max 1 height
  end;
  t.width <> old_width || t.height <> old_height

let paint t =
  if (not t.plain) && (not t.torn_down) && t.suspended = 0 then begin
    t.painted_at <- t.now ();
    let resized = refresh_size t in
    let lines = live_lines t in
    (* Skip the repaint when the frame is byte-identical to what is already on
       screen: clearing and rewriting the same glyphs on every animation tick is
       the flicker. [live_height] is the on-screen height; [commit] and [clear]
       reset it to 0, so a region that was actually cleared never matches and is
       always repainted. *)
    if lines <> t.last_lines || List.length lines <> t.live_height then begin
      with_unlimited_margin t.dppf (fun () ->
          if
            (not resized) && t.live_height > 0
            && List.length lines = t.live_height
          then pp_update t.dppf ~old_lines:t.last_lines ~new_lines:lines
          else begin
            pp_clear t.dppf t.live_height;
            pp_block t.dppf lines
          end;
          Format.pp_print_flush t.dppf ());
      (* No trailing newline: it would land on the bottom screen row and scroll
         the region up every paint, stranding the top line (the header) in
         scrollback where the cursor-relative clear can no longer reach it. The
         cursor rests at the end of the last region line; the next [pp_clear]
         returns to column 0 from there. *)
      t.live_height <- List.length lines;
      t.last_lines <- lines
    end
  end

(* Clear the activity projection, append [s] permanently, then repaint current
   activity beneath it. Permanent history is deliberately not truncated: it may
   wrap in the terminal, but it must never lose information. Since the old live
   region was cleared first and [live_height] is reset, wrapped history cannot
   corrupt the next projection. *)
let commit t s =
  ignore (refresh_size t);
  with_unlimited_margin t.dppf (fun () ->
      pp_clear t.dppf t.live_height;
      Format.pp_print_string t.dppf s;
      Format.pp_print_string t.dppf "\r\n";
      Format.pp_print_flush t.dppf ());
  t.live_height <- 0;
  paint t

(* Take over the terminal: hide the cursor, register the clear/redraw hooks that
   {!suspend} uses around foreign output, wrap the installed Logs reporter so a
   log line suspends the live region, and paint the initial frame. *)
let install_region t =
  Format.pp_print_string t.dppf hide_cursor;
  Format.pp_print_flush t.dppf ();
  install_reporter ();
  active_region :=
    Some
      {
        ppf = t.dppf;
        clear =
          (fun () ->
            with_unlimited_margin t.dppf (fun () ->
                pp_clear t.dppf t.live_height;
                Format.pp_print_flush t.dppf ());
            t.live_height <- 0);
        redraw = (fun () -> paint t);
      };
  paint t

let validate_geometry ~width ~height =
  (match width with
  | Some width when width <= 0 ->
      invalid_arg "Console.Display.run: width must be positive"
  | _ -> ());
  match height with
  | Some height when height <= 0 ->
      invalid_arg "Console.Display.run: height must be positive"
  | _ -> ()

let state ~(ctx : view) ~plain ~styling ~theme ~palette ~bar ~width ~height
    ~header =
  let auto_width = width = None and auto_height = height = None in
  let term_w, term_h = ctx.dimensions () in
  let width = match width with Some w -> w | None -> max 1 term_w in
  let height = match height with Some h -> h | None -> max 1 term_h in
  {
    dppf = ctx.ppf;
    plain;
    styling;
    now = ctx.now;
    wait = ctx.wait;
    dimensions = ctx.dimensions;
    char_width = ctx.char_width;
    header = Option.map sanitize header;
    default_style = bar;
    theme;
    palette = Option.value palette ~default:(Theme.palette theme);
    dstart = ctx.now ();
    width;
    height;
    auto_width = auto_width && not plain;
    auto_height = auto_height && not plain;
    rows = [];
    count_cell = 0;
    by_key = Hashtbl.create 16;
    events_rev = [];
    next_task_id = 0;
    pending = None;
    finished = 0;
    live_height = 0;
    last_lines = [];
    painted_at = neg_infinity;
    torn_down = false;
    owns_activity = false;
    suspended = 0;
    frame = 0;
    scene = None;
  }

let take_activity t ppf =
  if not (Atomic.compare_and_set activity_claimed false true) then
    invalid_arg "Console.Display.run: another activity display is active"
  else begin
    t.owns_activity <- true;
    match install_region t with
    | () -> t
    | exception exn ->
        t.owns_activity <- false;
        active_region := None;
        uninstall_reporter ();
        Atomic.set activity_claimed false;
        show_cursor_on ppf;
        raise exn
  end

let v ~ctx ?(mode = `Auto) ?(theme = Theme.default) ?palette
    ?(bar = Line.default) ?width ?height ?header () =
  let ctx = view_of_ctx ctx in
  validate_geometry ~width ~height;
  let plain =
    match mode with
    | `History -> true
    | `Activity -> false
    | `Auto -> not ctx.is_tty
  in
  (match !active_region with
  | Some region when plain && region.ppf == ctx.ppf ->
      invalid_arg "Console.Display.run: output formatter is already active"
  | Some _ | None -> ());
  (* Whether the surface takes ANSI is the formatter's own answer, read once
     here and threaded: a terminal whose reader turned colour off still gets
     the region, and a pipe whose caller forced colour on would get it on the
     surface it asked for rather than on the one this display happens to be
     beside. *)
  let styling = should_style ctx.ppf in
  (* A pinned [width]/[height] (tests) stays fixed; otherwise re-read the live
     terminal geometry on every paint so a resize is handled. *)
  let t =
    state ~ctx ~plain ~styling ~theme ~palette ~bar ~width ~height ~header
  in
  if plain then t else take_activity t ctx.ppf

(* A row's update paints the region when the frame on screen is older than
   {!redraw_interval}; a younger one is left to the next paint, the driver's
   tick or the next update past the interval, so a producer reporting every
   few kilobytes writes the terminal at the eye's pace and not its own. *)
let redraw_interval = 0.1

let touch r =
  let t = r.disp in
  if (not t.plain) && t.now () -. t.painted_at >= redraw_interval then paint t

(* A row's accent: an explicit [color], else the next palette colour (fixed at
   creation, so it is stable across redraws), else the base style's own. *)
let row_style t ~bar ~color =
  let base = Option.value bar ~default:t.default_style in
  match color with
  | Some c -> Line.with_color c base
  | None -> (
      match (bar, t.palette) with
      | None, (_ :: _ as pal) ->
          Line.with_color
            (List.nth pal (List.length t.rows mod List.length pal))
            base
      | _ -> base)

let check_open fn t =
  if t.torn_down then invalid_arg ("Console.Display." ^ fn ^ ": closed display")

let release_activity t =
  if t.owns_activity then begin
    t.owns_activity <- false;
    active_region := None;
    uninstall_reporter ();
    Atomic.set activity_claimed false
  end

let with_output t f =
  check_open "with_output" t;
  let call () =
    let margin = Format.pp_get_margin t.dppf () in
    Format.pp_set_margin t.dppf (render_width t);
    Fun.protect
      (fun () ->
        match f t.dppf with
        | value ->
            Format.pp_print_flush t.dppf ();
            value
        | exception exn ->
            Format.pp_print_flush t.dppf ();
            raise exn)
      ~finally:(fun () -> Format.pp_set_margin t.dppf margin)
  in
  if t.plain || t.suspended > 0 then call ()
  else
    match !active_region with
    | Some region when region.ppf == t.dppf -> (
        region.clear ();
        t.suspended <- t.suspended + 1;
        let resume ~preserve =
          t.suspended <- t.suspended - 1;
          if preserve then try paint t with Sys_error _ -> () else paint t
        in
        match call () with
        | value ->
            resume ~preserve:false;
            value
        | exception exn ->
            resume ~preserve:true;
            raise exn)
    | Some _ | None ->
        invalid_arg "Console.Display.with_output: display does not own output"

let rec task_path r =
  match r.parent with
  | None -> [ r.rlabel ]
  | Some parent -> task_path parent @ [ r.rlabel ]

let record ?task ?detail t kind message =
  let event : Event.t =
    {
      elapsed = t.now () -. t.dstart;
      task = Option.map (fun r -> r.rid) task;
      key = Option.bind task (fun r -> r.rkey);
      path = Option.fold ~none:[] ~some:task_path task;
      kind;
      message;
      detail;
    }
  in
  t.events_rev <- event :: t.events_rev

let history t = List.rev t.events_rev

module Activity = struct
  type progress = [ `Spin | `Count of int * int | `Bytes of int64 * int64 ]

  let equal_progress a b =
    match (a, b) with
    | `Spin, `Spin -> true
    | `Count (ac, at), `Count (bc, bt) -> Int.equal ac bc && Int.equal at bt
    | `Bytes (ac, at), `Bytes (bc, bt) -> Int64.equal ac bc && Int64.equal at bt
    | _ -> false

  type log = { elapsed : float; level : Event.level; message : string }

  let log_elapsed t = t.elapsed
  let log_level t = t.level
  let log_message t = t.message

  type item = {
    id : int;
    key : string option;
    parent : int option;
    label : string;
    progress : progress;
    elapsed : float;
    logs : log list;
  }

  let id t = t.id
  let key t = t.key
  let parent t = t.parent
  let label t = t.label
  let progress t = t.progress
  let elapsed t = t.elapsed
  let logs t = t.logs

  type t = item list

  let items t = t

  let pp_item ppf item =
    let pp_progress ppf = function
      | `Spin -> Fmt.string ppf "running"
      | `Count (cur, total) -> Fmt.pf ppf "%d/%d" cur total
      | `Bytes (cur, total) -> Fmt.pf ppf "%s/%s" (bytes cur) (bytes total)
    in
    Fmt.pf ppf "@[%d%s %s (%a, %s)@]" item.id
      (match item.parent with
      | None -> ""
      | Some parent -> Fmt.str "<-%d" parent)
      item.label pp_progress item.progress (span item.elapsed)

  let pp = Fmt.Dump.list pp_item
end

let activity t =
  List.filter_map
    (fun r ->
      if r.rfinish <> None then None
      else
        let progress : Activity.progress =
          match r.metric with
          | Spin -> `Spin
          | Count (cur, total) -> `Count (cur, total)
          | Bytes (cur, total) -> `Bytes (cur, total)
        in
        Some
          {
            Activity.id = r.rid;
            key = r.rkey;
            parent = Option.map (fun parent -> parent.rid) r.parent;
            label = r.rlabel;
            progress;
            elapsed = elapsed t r;
            logs =
              List.map
                (fun log ->
                  {
                    Activity.elapsed = log.ltime;
                    level = log.llevel;
                    message = log.lmessage;
                  })
                r.logs;
          })
    t.rows

let emit_history_line t ~plain ~styled =
  if t.plain then Fmt.pf t.dppf "%s@." plain else commit t styled

let validate_task ~id ~parent ~log_lines ~total t =
  check_open "task" t;
  if log_lines < 0 then invalid_arg "Console.Display.task: negative log_lines";
  (match total with
  | Some total when total < 0 ->
      invalid_arg "Console.Display.task: negative total"
  | Some _ | None -> ());
  (match id with
  | Some "" -> invalid_arg "Console.Display.task: empty id"
  | Some _ | None -> ());
  match parent with
  | Some parent when parent.disp != t ->
      invalid_arg "Console.Display.task: parent belongs to another display"
  | Some _ | None -> ()

(* The first sighting of an id fixes its parent and every later one keeps it,
   the way the first outcome of a task keeps it. A caller offering a different
   parent later is describing where the work is now, not asking for the row to
   move: an event stream carries stable ids across whatever scopes the emitter
   happens to be in, so the same id genuinely arrives under a second scope. A
   drawing surface does not get to decide whether its host survives, so a later
   parent is ignored rather than fatal. *)
let existing_task ~id t = Option.bind id (Hashtbl.find_opt t.by_key)

let add_task ~id ~parent ~bar ~color ~log_lines ~total ~transient ~group t label
    =
  let now = t.now () in
  let rid = t.next_task_id in
  t.next_task_id <- rid + 1;
  let inherited =
    match Option.bind parent (fun parent -> parent.routcome) with
    | Some `Succeeded -> Some `Succeeded
    | Some (`Failed | `Cancelled) -> Some `Cancelled
    | None -> None
  in
  let r =
    {
      rid;
      rkey = id;
      disp = t;
      parent;
      style = row_style t ~bar ~color;
      log_lines;
      logs = [];
      log_prev_t = now;
      rlabel = sanitize label;
      metric =
        (match total with Some total -> Count (0, total) | None -> Spin);
      transient;
      group;
      requested = None;
      swept = false;
      waiting = false;
      spilled = [];
      rstart = now;
      rfinish = Option.map (Fun.const now) inherited;
      rmessage = None;
      routcome = inherited;
      last_cur = 0L;
      last_t = now;
      rate = 0.;
    }
  in
  t.rows <- t.rows @ [ r ];
  Option.iter (fun id -> Hashtbl.add t.by_key id r) id;
  record ~task:r t `Started r.rlabel;
  Option.iter
    (fun outcome ->
      t.finished <- t.finished + 1;
      record ~task:r t (`Finished outcome) r.rlabel)
    inherited;
  touch r;
  r

let task ?id ?parent ?bar ?color ?(log_lines = 8) ?total ?(transient = false)
    ?(group = false) t label =
  validate_task ~id ~parent ~log_lines ~total t;
  match existing_task ~id t with
  | Some existing -> existing
  | None ->
      add_task ~id ~parent ~bar ~color ~log_lines ~total ~transient ~group t
        label

let set_label r label =
  check_open "set_label" r.disp;
  if r.rfinish = None then begin
    r.rlabel <- sanitize label;
    touch r
  end

(* One log line as a terminal's history draws it, under its row's guide. *)
let log_line t r entry =
  let marker = level_marker t.theme entry.llevel in
  let suffix = gap_suffix entry.lgap in
  continuation t r ^ styled_log t entry.llevel marker entry.lmessage suffix

(* One log line, written where history goes: above the activity region on a
   terminal, straight to the output without one, or held with its row while
   the row waits for its group. *)
let commit_log t r entry =
  if waits_for_group r then r.spilled <- r.spilled @ [ entry ]
  else
    let marker = level_marker t.theme entry.llevel in
    let plain =
      Fmt.str "%s  %s %s%s" (indent r)
        (styled t (level_style t.theme entry.llevel) marker)
        entry.lmessage
        (dimmed t (gap_suffix entry.lgap))
    in
    emit_history_line t ~plain ~styled:(log_line t r entry)

(* A line is on screen once. The bounded tail beneath a running row already
   draws it, so committing it at the same time puts the same line in two places
   at once -- the span block an apply prints under its phase, and again below
   the spinner on every repaint. The tail holds it instead, and history takes
   it when it ages out of the tail or the row finishes: still permanent, still
   in order, drawn once. A line with no tail to hold it -- plain output, a row
   that keeps none, a row already finished -- is committed where it is made. *)
let log ?(level = `Info) ?(keep = true) r line =
  check_open "log" r.disp;
  let t = r.disp in
  let lines = List.map sanitize (lines_of_string line) in
  (* Gap since the previous log line, surfacing time spent with no output. *)
  let gap_of l =
    let now = t.now () in
    let gap = now -. r.log_prev_t in
    r.log_prev_t <- now;
    (now -. t.dstart, gap, l)
  in
  List.iter
    (fun line ->
      let time, gap, line = gap_of line in
      if keep then record ~task:r t (`Log level) line;
      let entry =
        {
          ltime = time;
          lgap = gap;
          llevel = level;
          lmessage = line;
          lkept = keep;
        }
      in
      let tailed = r.rfinish = None && r.log_lines > 0 in
      let held = tailed && not t.plain in
      if (not held) && keep then commit_log t r entry;
      if tailed then begin
        r.logs <- r.logs @ [ entry ];
        if List.length r.logs > r.log_lines then
          match r.logs with
          | oldest :: rest ->
              if held && oldest.lkept then commit_log t r oldest;
              r.logs <- rest
          | [] -> ()
      end)
    lines;
  touch r

let event ?(level = `Info) t message =
  check_open "event" t;
  let lines =
    match lines_of_string message with [] -> [ "" ] | lines -> lines
  in
  List.iter
    (fun line ->
      let message = sanitize line in
      record t (`Message level) message;
      match level with
      | `Info -> emit_history_line t ~plain:message ~styled:message
      | (`Warning | `Error) as level ->
          let marker = level_marker t.theme level in
          let log =
            styled t (level_style t.theme level) marker ^ " " ^ message
          in
          emit_history_line t ~plain:log ~styled:log)
    lines

(* Every metric change goes through here, so the block's count cell widens with
   the row that needs it and one width serves them all. *)
let set_metric r metric =
  r.metric <- metric;
  r.disp.count_cell <- max r.disp.count_cell (count_width metric);
  touch r

let set_count r ~cur ~total =
  check_open "set_count" r.disp;
  if cur < 0 || total < 0 then
    invalid_arg "Console.Display.set_count: expected non-negative progress";
  if r.rfinish = None then begin
    let total = max cur total in
    let cur, total =
      match r.metric with
      | Spin -> (cur, total)
      | Count (previous, previous_total) ->
          let cur = max previous cur in
          (cur, max cur (max previous_total total))
      | Bytes _ ->
          invalid_arg "Console.Display.set_count: task already tracks bytes"
    in
    set_metric r (Count (cur, total))
  end

let advance ?(by = 1) ?total ?label r =
  check_open "advance" r.disp;
  if by < 0 then invalid_arg "Console.Display.advance: negative increment";
  (match total with
  | Some total when total < 0 ->
      invalid_arg "Console.Display.advance: negative total"
  | Some _ | None -> ());
  if r.rfinish = None then begin
    Option.iter (fun label -> r.rlabel <- sanitize label) label;
    match r.metric with
    | Spin ->
        let total = max by (Option.value total ~default:by) in
        set_count r ~cur:by ~total
    | Count (cur, known_total) ->
        let cur = if by > max_int - cur then max_int else cur + by in
        let total = max cur (max known_total (Option.value total ~default:0)) in
        set_count r ~cur ~total
    | Bytes _ ->
        invalid_arg "Console.Display.advance: task already tracks bytes"
  end

let set_bytes r ~cur ~total =
  check_open "set_bytes" r.disp;
  if cur < 0L || total < 0L then
    invalid_arg "Console.Display.set_bytes: expected non-negative progress";
  if r.rfinish = None then begin
    let total = Int64.max cur total in
    let cur, total =
      match r.metric with
      | Spin -> (cur, total)
      | Bytes (previous, previous_total) ->
          let cur = Int64.max previous cur in
          (cur, Int64.max cur (Int64.max previous_total total))
      | Count _ ->
          invalid_arg "Console.Display.set_bytes: task already tracks count"
    in
    let now = r.disp.now () in
    let dt = now -. r.last_t in
    if dt > 0. then begin
      let inst =
        Float.max 0. (Int64.to_float (Int64.sub cur r.last_cur) /. dt)
      in
      r.rate <-
        (if r.rate <= 0. then inst else (0.3 *. inst) +. (0.7 *. r.rate));
      r.last_cur <- cur;
      r.last_t <- now
    end;
    set_metric r (Bytes (cur, total))
  end

let children r =
  List.filter
    (fun child ->
      match child.parent with Some parent -> parent == r | None -> false)
    r.disp.rows

let running_children r =
  List.filter (fun child -> child.rfinish = None) (children r)

(* The lines that waited for [r], depth first in the order the rows started:
   each row's kept log lines, then the row's own line, then the rows under it.
   A row that waited for an inner group is still waiting when that group
   settles under an outer one, and is written here with the rest. *)
let rec waited_lines t r =
  List.concat_map
    (fun child ->
      let logs = List.map (log_line t child) child.spilled in
      child.spilled <- [];
      let own =
        if child.waiting then begin
          child.waiting <- false;
          [ styled_committed t child ]
        end
        else []
      in
      logs @ own @ waited_lines t child)
    (children r)

(* A group asked to end while rows under it are open keeps the first ending it
   was asked for and settles with it when its last open row settles, so its
   cost runs to that moment. Any other row settles at once, its open
   descendants first.

   [swept] is a row that never reported an ending of its own: the row it hung
   under ended while it was still running, so the display takes it out of the
   live region on that row's behalf. Such a row commits no permanent line and
   carries no message. Permanent history is what the run did, and a row that
   said nothing has nothing to say there; the only sentence the display could
   write for it would be about its own shape -- which row hung under which --
   and a reader watching a command has no rows and no children on their screen,
   only work. The event stream still carries the ending, under the row's own
   label, so a consumer that reads the run rather than the screen loses
   nothing. *)
let rec finish ?(swept = false) outcome ?message r =
  check_open "finish" r.disp;
  if r.rfinish = None then
    if (not swept) && r.group && running_children r <> [] then
      r.requested <- Some (Option.value r.requested ~default:(outcome, message))
    else settle ~swept outcome ?message r

and settle ~swept outcome ?message r =
  let child_outcome =
    match outcome with
    | `Succeeded -> `Succeeded
    | `Failed | `Cancelled -> `Cancelled
  in
  List.iter
    (fun child -> finish ~swept:true child_outcome child)
    (running_children r);
  let t = r.disp in
  (* The tail was holding these; the row is about to leave the live region,
     so history takes them, before the line that closes the row. *)
  if not t.plain then begin
    List.iter (fun log -> if log.lkept then commit_log t r log) r.logs;
    r.logs <- []
  end;
  r.rfinish <- Some (t.now ());
  r.rmessage <- Option.map sanitize message;
  r.routcome <- Some outcome;
  r.swept <- swept;
  t.finished <- t.finished + 1;
  let message = Option.value r.rmessage ~default:r.rlabel in
  record ~task:r t (`Finished outcome) message;
  let own = not (swept || r.transient) in
  (if waits_for_group r then r.waiting <- own
   else if t.plain then (if own then Fmt.pf t.dppf "%s@." (plain_committed t r))
   else
     match
       (if own then [ styled_committed t r ] else []) @ waited_lines t r
     with
     | [] -> ()
     | lines -> commit t (String.concat "\r\n" lines));
  match r.parent with
  | Some parent when not swept -> settle_requested parent
  | Some _ | None -> ()

and settle_requested group =
  match group.requested with
  | Some (outcome, message)
    when group.rfinish = None && running_children group = [] ->
      settle ~swept:false outcome ?message group
  | Some _ | None -> ()

let succeed ?message r = finish `Succeeded ?message r
let fail ?message r = finish `Failed ?message r
let cancel ?message r = finish `Cancelled ?message r

let with_task ?id ?parent ?bar ?color ?log_lines ?total ?transient ?group t
    label f =
  let task =
    task ?id ?parent ?bar ?color ?log_lines ?total ?transient ?group t label
  in
  match f task with
  | value ->
      succeed task;
      value
  | exception exn ->
      fail task;
      raise exn

let finish_running outcome ?message t =
  List.iter
    (fun task -> if task.rfinish = None then finish outcome ?message task)
    (List.rev t.rows)

let set_result t ?detail result =
  check_open "set_result" t;
  if Option.is_none t.pending then begin
    (match result with
    | `Ok _ -> finish_running `Succeeded t
    | `Fail _ -> finish_running `Cancelled ~message:"session failed" t);
    let result =
      match result with
      | `Ok message -> `Ok (sanitize message)
      | `Fail message -> `Fail (sanitize message)
    in
    let detail = Option.map sanitize_multiline detail in
    t.pending <- Some (result, detail);
    let kind, message =
      match result with
      | `Ok message -> (`Result `Ok, message)
      | `Fail message -> (`Result `Fail, message)
    in
    record ?detail t kind message
  end

let tick t =
  if not t.plain then begin
    t.frame <- t.frame + 1;
    paint t
  end

let frame t = t.frame

let set_scene t scene =
  check_open "set_scene" t;
  t.scene <- Some scene;
  if (not t.plain) && t.now () -. t.painted_at >= redraw_interval then paint t

let now t = t.now ()

(* The refusal is taken on [wait display], before any second is asked for, so
   a driver that reads the wait before forking refuses at once. *)
let wait t =
  match t.wait with
  | Some wait -> wait
  | None ->
      invalid_arg
        "Console.Display.wait: the context was built without ~wait, so its \
         clock cannot be waited on"

(* The final result banner: a faint coloured [OK]/[FAIL] marker (green or red)
   followed by the message in bold, unstyled text in history-only output. A long message
   word-wraps under a hanging indent aligned past the marker, rather than being
   cut at the width -- this is printed after the live region is released, so a
   second physical line is safe. *)
let result_line t result =
  let marker, accent, msg =
    match result with
    | `Ok msg -> (Theme.ok_marker t.theme, color_code (Theme.ok t.theme), msg)
    | `Fail msg ->
        (Theme.fail_marker t.theme, color_code (Theme.fail t.theme), msg)
  in
  if t.plain then styled t (dim ^ accent) marker ^ " " ^ styled t bold msg
  else begin
    let indent = string_width t marker + 1 in
    let body = Width.wrap ~indent t.width msg in
    (* [wrap] indents every line, including the first; drop the first line's
       indent so the marker sits there and continuation lines stay aligned. *)
    let rest =
      if String.length body >= indent then
        String.sub body indent (String.length body - indent)
      else body
    in
    styled t (dim ^ accent) marker ^ " " ^ styled t bold rest
  end

(* The optional detail paragraph shown dimmed just above the verdict banner: the
   reason, a log-path pointer, whatever context the result needs. Split on
   newlines; a line wider than the terminal word-wraps (it is printed after the
   live region is released, so extra physical lines are safe), each resulting
   line dimmed (unstyled in history-only output). *)
let detail_lines t = function
  | None -> []
  | Some d ->
      List.concat_map
        (fun l ->
          if t.plain then [ dimmed t l ]
          else
            String.split_on_char '\n' (Width.wrap t.width l)
            |> List.map (dimmed t))
        (String.split_on_char '\n' d)

let close t =
  if not t.torn_down then begin
    let result, detail =
      match t.pending with
      | Some (result, detail) -> (Some result, detail)
      | None -> (None, None)
    in
    t.torn_down <- true;
    let detail = detail_lines t detail in
    let banner = Option.map (result_line t) result in
    if t.plain then begin
      (match summary_line t ~fit:false with
      | Some s -> Fmt.pf t.dppf "%s@." s
      | None -> ());
      List.iter (fun s -> Fmt.pf t.dppf "%s@." s) detail;
      (match banner with Some s -> Fmt.pf t.dppf "%s@." s | None -> ());
      Format.pp_print_flush t.dppf ()
    end
    else
      let line s =
        Format.pp_print_string t.dppf s;
        Format.pp_print_string t.dppf "\r\n"
      in
      Fun.protect
        (fun () ->
          with_unlimited_margin t.dppf (fun () ->
              pp_clear t.dppf t.live_height;
              (match summary_line t ~fit:true with
              | Some s -> line s
              | None -> ());
              List.iter line detail;
              match banner with Some s -> line s | None -> ()))
        ~finally:(fun () ->
          t.live_height <- 0;
          show_cursor_on t.dppf;
          release_activity t)
  end

let cancel_running t message =
  List.iter
    (fun task -> if task.rfinish = None then cancel ~message task)
    (List.rev t.rows)

(* Runs while another exception unwinds: a write failure on the display's
   channel ([Sys_error], as an out_channel formatter raises) is logged so that
   the original exception is the one that propagates. *)
let cleanup_after_exception t =
  try
    cancel_running t "interrupted";
    close t
  with Sys_error _ as exn ->
    t.torn_down <- true;
    if not t.plain then begin
      show_cursor_on t.dppf;
      release_activity t
    end;
    Log.warn (fun m -> m "Console display cleanup failed: %a" Fmt.exn exn)

let run ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  match Domain.DLS.get current with
  | Some t -> f t
  | None ->
      let t = v ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header () in
      Domain.DLS.set current (Some t);
      Fun.protect
        ~finally:(fun () -> Domain.DLS.set current None)
        (fun () ->
          match f t with
          | value ->
              finish_running `Succeeded t;
              close t;
              value
          | exception exn ->
              cleanup_after_exception t;
              raise exn)
