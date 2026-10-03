(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A theme bundles every visual choice the widgets share -- the accent colour
   and palette, the box-drawing border, the tree guide, and the live display's
   spinner, bar style and markers -- so one value styles tables, trees, panels
   and the progress display consistently. It is pure data; the widgets render
   with it. *)

(* Amber-phosphor monochrome, like an old DOS satellite-workstation CRT. *)
let amber = Color.rgb 0xff 0xb0 0x00
let matrix_green = Color.rgb 0x33 0xff 0x33

type bar = [ `Blocky | `Smooth | `Rainbow ]

type t = {
  accent : Color.t;
  ok : Color.t;
  fail : Color.t;
  warn : Color.t;
  palette : Color.t list;
  border : Border.t;
  table_row_separators : bool;
  animated_border : bool;
  guide : Guide.t;
  spinner : Spinner.t;
  bar : bar;
  done_marker : string;
  ok_marker : string;
  fail_marker : string;
  note_marker : string;
  warn_marker : string;
  error_marker : string;
  committed_succeeded : string;
  committed_failed : string;
  committed_cancelled : string;
}

let equal a b =
  Color.equal a.accent b.accent
  && Color.equal a.ok b.ok && Color.equal a.fail b.fail
  && Color.equal a.warn b.warn
  && List.equal Color.equal a.palette b.palette
  && Border.equal a.border b.border
  && Bool.equal a.table_row_separators b.table_row_separators
  && Bool.equal a.animated_border b.animated_border
  && Guide.equal a.guide b.guide
  && Spinner.equal a.spinner b.spinner
  && a.bar = b.bar
  && String.equal a.done_marker b.done_marker
  && String.equal a.ok_marker b.ok_marker
  && String.equal a.fail_marker b.fail_marker
  && String.equal a.note_marker b.note_marker
  && String.equal a.warn_marker b.warn_marker
  && String.equal a.error_marker b.error_marker
  && String.equal a.committed_succeeded b.committed_succeeded
  && String.equal a.committed_failed b.committed_failed
  && String.equal a.committed_cancelled b.committed_cancelled

let pp ppf t =
  Fmt.pf ppf
    "{ done = %s; ok = %s; fail = %s; note = %s; warn = %s; error = %s; \
     succeeded = %s; failed = %s; cancelled = %s }"
    t.done_marker t.ok_marker t.fail_marker t.note_marker t.warn_marker
    t.error_marker t.committed_succeeded t.committed_failed
    t.committed_cancelled

let accent t = t.accent
let ok t = t.ok
let fail t = t.fail
let warn t = t.warn
let palette t = t.palette
let border t = t.border
let table_row_separators t = t.table_row_separators
let animated_border t = t.animated_border
let guide t = t.guide
let spinner t = t.spinner
let bar t = t.bar
let done_marker t = t.done_marker
let ok_marker t = t.ok_marker
let fail_marker t = t.fail_marker
let note_marker t = t.note_marker
let warn_marker t = t.warn_marker
let error_marker t = t.error_marker

let committed_marker t = function
  | `Succeeded -> t.committed_succeeded
  | `Failed -> t.committed_failed
  | `Cancelled -> t.committed_cancelled

let v ?(accent = Color.cyan) ?(ok = Color.green) ?(fail = Color.red)
    ?(warn = Color.yellow) ?(palette = []) ?(border = Border.rounded)
    ?(table_row_separators = false) ?(animated_border = false)
    ?(guide = Guide.unicode) ?(bar = `Smooth) ?(note_marker = "#")
    ?(warn_marker = "!") ?(error_marker = "x") ?(committed_succeeded = "=>")
    ?(committed_failed = "!!") ?(committed_cancelled = "--") ~spinner
    ~done_marker () =
  {
    accent;
    ok;
    fail;
    warn;
    palette;
    border;
    table_row_separators;
    animated_border;
    guide;
    spinner;
    bar;
    done_marker;
    ok_marker = "[OK]";
    fail_marker = "[FAIL]";
    note_marker;
    warn_marker;
    error_marker;
    committed_succeeded;
    committed_failed;
    committed_cancelled;
  }

let with_verdict ?ok ?fail ?committed_succeeded ?committed_failed
    ?committed_cancelled t =
  {
    t with
    ok_marker = Option.value ok ~default:t.ok_marker;
    fail_marker = Option.value fail ~default:t.fail_marker;
    committed_succeeded =
      Option.value committed_succeeded ~default:t.committed_succeeded;
    committed_failed = Option.value committed_failed ~default:t.committed_failed;
    committed_cancelled =
      Option.value committed_cancelled ~default:t.committed_cancelled;
  }

(* Amber DOS satellite-workstation CRT: ASCII everything, blocky bar, [+] done
   marker, [OK]/[FAIL] result banner. *)
let dos =
  {
    accent = amber;
    ok = Color.green;
    fail = Color.red;
    warn = Color.yellow;
    palette = [ amber ];
    border = Border.ascii;
    table_row_separators = false;
    animated_border = false;
    guide = Guide.ascii;
    spinner = Spinner.ascii;
    bar = `Blocky;
    done_marker = "[+]";
    ok_marker = "[OK]";
    fail_marker = "[FAIL]";
    note_marker = "#";
    warn_marker = "!";
    error_marker = "x";
    committed_succeeded = "=>";
    committed_failed = "!!";
    committed_cancelled = "--";
  }

(* Modern unicode: one restrained activity accent, rounded geometry, a braille
   spinner and semantic success/failure markers.  Colour identifies state; it
   does not decorate whole rows. *)
let unicode =
  {
    accent = Color.cyan;
    ok = Color.green;
    fail = Color.red;
    warn = Color.yellow;
    palette = [ Color.cyan ];
    border = Border.rounded;
    table_row_separators = false;
    animated_border = false;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    bar = `Smooth;
    done_marker = "✓";
    ok_marker = "✓";
    fail_marker = "✗";
    note_marker = "#";
    warn_marker = "!";
    error_marker = "x";
    committed_succeeded = "=>";
    committed_failed = "!!";
    committed_cancelled = "--";
  }

(* The Matrix: monochrome green on black, single-line border, braille
   spinner. *)
let matrix =
  {
    accent = matrix_green;
    ok = Color.green;
    fail = Color.red;
    warn = Color.yellow;
    palette = [ matrix_green ];
    border = Border.single;
    table_row_separators = false;
    animated_border = true;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    bar = `Smooth;
    done_marker = "✓";
    ok_marker = "✓";
    fail_marker = "✗";
    note_marker = "#";
    warn_marker = "!";
    error_marker = "x";
    committed_succeeded = "=>";
    committed_failed = "!!";
    committed_cancelled = "--";
  }

(* Rainbow: rows and nodes cycle the spectrum, and the bar is a gradient. *)
let rainbow =
  {
    accent = Color.magenta;
    ok = Color.green;
    fail = Color.red;
    warn = Color.yellow;
    palette =
      [
        Color.red;
        Color.yellow;
        Color.green;
        Color.cyan;
        Color.blue;
        Color.magenta;
      ];
    border = Border.rounded;
    table_row_separators = false;
    animated_border = false;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    bar = `Rainbow;
    done_marker = "✔";
    ok_marker = "✔";
    fail_marker = "✘";
    note_marker = "#";
    warn_marker = "!";
    error_marker = "x";
    committed_succeeded = "=>";
    committed_failed = "!!";
    committed_cancelled = "--";
  }

let default = unicode
