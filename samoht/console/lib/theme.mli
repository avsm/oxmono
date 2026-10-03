(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared visual theme.

    A theme bundles every visual choice the widgets share -- the accent colour
    and palette, the box-drawing border, the tree guide, and the live display's
    spinner, bar style and markers -- so one value styles tables, trees, panels
    and the progress display consistently. Every marker the display draws is one
    of these fields, so a theme is the whole of what a reader is left with once
    the colour is gone. It is pure data: the widgets render {e with} it
    ({!Bar.render}, {!Tree}, {!Table}, {!Panel}, {!Display}); it renders nothing
    itself. *)

(** {1:themes Themes} *)

type bar = [ `Blocky | `Smooth | `Rainbow ]
(** A bar's look: full blocks on a shaded track, fractional eighth-cell fills,
    or a spectrum gradient. {!Bar} renders it. *)

type t
(** A theme: every visual choice the widgets share. *)

val accent : t -> Color.t
(** [accent t] is the default row, spinner, and bar colour. *)

val ok : t -> Color.t
(** [ok t] is the colour of success: a succeeded row's marker and the [OK]
    result banner. *)

val fail : t -> Color.t
(** [fail t] is the colour of failure: a failed row's marker, an error log line
    and the [FAIL] result banner. *)

val warn : t -> Color.t
(** [warn t] is the colour of what needs attention without having failed: a
    cancelled row's marker and a warning log line. *)

val palette : t -> Color.t list
(** [palette t] is the colour cycle for successive rows and nodes. *)

val border : t -> Border.t
(** [border t] is the border for tables and panels. *)

val table_row_separators : t -> bool
(** [table_row_separators t] is [true] when a table draws a horizontal rule
    between logical data rows. A cell may itself span several physical lines;
    those lines remain inside one row. *)

val animated_border : t -> bool
(** [animated_border t] is [true] when panels and tables use rain animation. *)

val guide : t -> Guide.t
(** [guide t] is the guide for trees and nested display rows. *)

val spinner : t -> Spinner.t
(** [spinner t] is the running-row spinner. *)

val bar : t -> bar
(** [bar t] is the progress-bar style. *)

val done_marker : t -> string
(** [done_marker t] replaces the spinner on a completed row. *)

val ok_marker : t -> string
(** [ok_marker t] is the success verdict marker. *)

val fail_marker : t -> string
(** [fail_marker t] is the failure verdict marker. *)

val note_marker : t -> string
(** [note_marker t] leads an informational log line. *)

val warn_marker : t -> string
(** [warn_marker t] leads a warning, both in a row's log tail and in permanent
    history. *)

val error_marker : t -> string
(** [error_marker t] leads an error, both in a row's log tail and in permanent
    history. *)

val committed_marker : t -> [ `Succeeded | `Failed | `Cancelled ] -> string
(** [committed_marker t outcome] leads the permanent line a finished row leaves
    behind, so how a row ended survives with the colour gone. It is a separate
    vocabulary from {!ok_marker} and {!fail_marker}, which the result banner
    draws once at the end of a session. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] make the same visual choices in every
    field. *)

val pp : t Fmt.t
(** [pp] prints a short summary of a theme's markers, for diagnostics. *)

val v :
  ?accent:Color.t ->
  ?ok:Color.t ->
  ?fail:Color.t ->
  ?warn:Color.t ->
  ?palette:Color.t list ->
  ?border:Border.t ->
  ?table_row_separators:bool ->
  ?animated_border:bool ->
  ?guide:Guide.t ->
  ?bar:bar ->
  ?note_marker:string ->
  ?warn_marker:string ->
  ?error_marker:string ->
  ?committed_succeeded:string ->
  ?committed_failed:string ->
  ?committed_cancelled:string ->
  spinner:Spinner.t ->
  done_marker:string ->
  unit ->
  t
(** [v ~spinner ~done_marker ()] is a custom theme; the other fields take modern
    defaults. [ok], [fail] and [warn] default to {!Color.green}, {!Color.red}
    and {!Color.yellow}, as in every preset theme. [table_row_separators]
    defaults to [false], and the six log and committed markers to ["#"], ["!"],
    ["x"], ["=>"], ["!!"] and ["--"]. *)

val with_verdict :
  ?ok:string ->
  ?fail:string ->
  ?committed_succeeded:string ->
  ?committed_failed:string ->
  ?committed_cancelled:string ->
  t ->
  t
(** [with_verdict t] is [t] with the markers that say how something ended
    replaced: [ok] and [fail] for the result banner, and the three [committed_]
    fields for the permanent line a finished row leaves behind. An omitted field
    keeps the marker [t] already carried. *)

val dos : t
(** [dos] is amber DOS satellite-workstation CRT: ASCII everything, a blocky
    full-block bar, a [+] done marker, and an [[OK]] / [[FAIL]] result banner.
*)

val unicode : t
(** [unicode] is modern Unicode: a rounded border, a braille spinner, a smooth
    fractional bar, one restrained activity accent, and semantic success/failure
    markers. *)

val matrix : t
(** [matrix] is monochrome green on black, with a single-line border and a
    braille spinner. *)

val rainbow : t
(** [rainbow] cycles the rows and nodes through the spectrum and draws the bar
    as a colour gradient. *)

val default : t
(** [default] is {!unicode}. *)
