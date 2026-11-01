(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colours, tables, trees and progress rows for command-line tools.

    [Console] prints into the terminal's normal scrollback, as [make] or [ls]
    do. While work runs, it redraws only the rows of a {!Display}. A program
    that takes over the whole screen is better served by a full-screen library.
    Windows consoles are not supported.

    A {!View.t} is text in a {!Style.t}: colours, bold, italic and so on. The
    widgets ({!Table}, {!Tree}, {!Panel}, {!Rule} and {!Bar}) lay out views and
    print with {!Format}, and a {!Theme} gives them one look.

    Every widget is a view, by its [view], laid out only when it is printed, in
    the room it is given ({!View.fit}). So a table goes in a panel, the panel in
    a cell of another table, beside a tree ({!View.hcat}) or in a display's
    view, and each of them is drawn in the theme of the formatter that prints
    it. [Console_eio.run] runs a {!Display}.

    {b Guarantees.}
    - Widths are counted in terminal cells, as the terminal draws the text. A
      wide character, such as a Chinese one or an emoji, takes two cells, and an
      accent added to a letter takes none.
    - A widget printed by {!View.pp} is no wider than the formatter's margin,
      and in a room no wider than the room. It ends without a line end, so
      ["%a@."] prints it on its own lines.
    - A view holds only text that prints: a line end in it starts a new line and
      any other control character is a space, so caller text cannot move the
      cursor or change colours.
    - Every style a printer turns on, it turns off before the line ends.
    - Text cut to fit is cut between two characters and ends with an ellipsis.
    - On a formatter that is not {!Fmt.utf_8}, the widgets draw their frames,
      lines, bars, spinners, marks and ellipses in ASCII.
    - The guarantees of a live region are those of {!Display}.

    The repository's [INVARIANTS.md] lists each guarantee and the tests that
    check it.

    Text is in colour when its formatter is set to colour
    ({!Fmt.set_style_renderer}), in the colours its {!Color.val-depth} allows.
    [Console_tty.setup], in the [console.tty] library, sets both for standard
    output and standard error, from the terminal and the environment.

    {1:quickstart Quick start}

    {2:styled Styled text}

    {[
    open Console

    let demo_styled () =
      let style = Style.(bold + fg Color.green) in
      Fmt.pr "%a@." View.pp (View.styled style (View.text "Success!"))
    ]}

    {2:tables Tables}

    {[
    open Console

    let demo_table () =
      let table =
        Table.(
          view ~border:Border.rounded
            [
              column (View.text "Name"); column ~align:`Right (View.text "Age");
            ]
            [
              [ View.text "Alice"; View.text "30" ];
              [ View.text "Bob"; View.text "25" ];
            ])
      in
      Fmt.pr "%a@." View.pp table
    ]}

    {2:trees Trees}

    {[
    open Console

    let demo_tree () =
      let tree =
        Tree.v (View.text "src")
          [
            Tree.v (View.text "lib") [ Tree.v (View.text "parser.ml") [] ];
            Tree.v (View.text "test") [];
          ]
      in
      Fmt.pr "%a@." View.pp (Tree.view tree)
    ]}

    {2:panels Panels}

    {[
    open Console

    let demo_panel () =
      let panel =
        Panel.view ~title:(View.text "Status")
          (View.text "All systems operational")
      in
      Fmt.pr "%a@." View.pp panel
    ]}

    {2:progress Progress}

    A program reports its work as {!Task}s under a root, and calls
    [Console_eio.run] once to show them, which passes the root to the code that
    does the work:

    {[
    open Console

    let build_step root =
      Task.run root "compile" (fun step ->
          Task.set_items step ~count:1 ~total:2;
          Task.log step "ocamlopt parser.ml";
          Task.set_items step ~count:2 ~total:2)
    ]}

    [Console_eio.run ~clock ~label:"Building" build_step] runs it, with [clock]
    as [Eio.Stdenv.mono_clock env]. A library can call [Console_eio.run] itself,
    since a run inside another one gets the task current where it is called. *)

(** {1:modules Modules} *)

module Color = Color
(** Terminal colours, by name, by palette number or as RGB. *)

module Style = Style
(** Bold, italic, underline and colours, combined with [+]. *)

module Border = Border
(** Frames for tables and panels. *)

module Guide = Guide
(** The lines that connect the branches of a tree. *)

module Theme = Theme
(** One look for every widget. *)

module Room = Room
(** Where and when a view is laid out: its width, height, time and theme. *)

module View = View
(** What console prints: styled text and every widget, laid out in a room. *)

module Spinner = Spinner
(** Spinners for work of unknown length. *)

module Bar = Bar
(** Bars, for progress and for figures: horizontal, vertical, sparklines and
    charts. *)

module Panel = Panel
(** Text in a frame, with an optional title. *)

module Table = Table
(** Tables with headers and aligned columns. *)

module Tree = Tree
(** Trees, drawn like the output of [tree]. *)

module Rule = Rule
(** Horizontal lines with an optional label. *)

module Task = Task
(** The work a program reports: tasks under a root, with no drawing. *)

module Display = Display
(** A view redrawn in place: the live region. *)

module Progress = Progress
(** How tasks show: rows, lines, only failures or JSON. *)
