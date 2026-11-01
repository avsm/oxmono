(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Colour and UTF-8 on the standard outputs.

    [Console_tty] decides how {!Fmt.stdout} and {!Fmt.stderr} print, from the
    terminal and the environment, as the OCaml command-line tools do. It decides
    whether they are in colour, in how many colours, and whether they show
    UTF-8. A program calls {!setup} once, first, and then prints with [Fmt]:

    {[
    let () =
      Console_tty.setup ();
      Fmt.pr "%a@." Console.View.pp
        Console.View.(styled Console.Style.bold (text "ready"))
    ]}

    This library is [console.tty], which needs Unix but not Eio. *)

val setup :
  ?style_renderer:Fmt.style_renderer ->
  ?utf_8:bool ->
  ?theme:Console.Theme.t ->
  unit ->
  unit
(** [setup ()] sets the style renderer, the {!Fmt.utf_8}, the
    {!Console.Color.val-depth} and the {!Console.Theme} of {!Fmt.stdout} and of
    {!Fmt.stderr}:

    - The style renderer is [style_renderer] if given, as
      [Fmt_cli.style_renderer] reads a [--color] option. Otherwise it is [`None]
      if [NO_COLOR] is set and not empty, or if [TERM] is unset, empty or
      [dumb]. Else it is [`Ansi_tty] iff the output is a terminal, as both are
      when [CONSOLE_CAST] records them. [CLICOLOR_FORCE] and [FORCE_COLOR] are
      not read: [--color=always] forces colour.
    - {!Fmt.utf_8} is [utf_8] if given, else [true] iff the locale's character
      set is UTF-8. The locale is the first of [LC_ALL], [LC_CTYPE] and [LANG]
      that is set and not empty. An unset locale is not UTF-8, and the widgets
      print ASCII alone on it.
    - The colour depth is [`True_color] if [COLORTERM] is [truecolor] or
      [24bit], or if [TERM] ends in [-direct]. It is [`Ansi_256] if [TERM]
      contains [256color], and [`Ansi_16] otherwise.
    - The theme is [theme] if given, as [Cmdliner.Arg.enum Console.Theme.all]
      reads a [--theme] option. Otherwise each output keeps its own.

    [CONSOLE_CAST], set to a file name, makes [setup] record both outputs there,
    as [Console_eio.run] does, so that a program that prints without a run is
    recorded too: each write is at time zero, until a run gives the recording
    its clock. *)

val ensure : unit -> unit
(** [ensure ()] is like [setup ()] ({!setup}) but leaves alone each of
    {!Fmt.stdout} and {!Fmt.stderr} on which the program chose something
    already. Such a choice is a style renderer or {!Fmt.utf_8}, set with [Fmt],
    [Fmt_tty] or {!setup}, or a {!Console.Color.val-depth}. A {!Console.Theme}
    is not one.

    A library that prints in colour on the standard outputs calls [ensure], as
    [Console_eio.run] does. A program that did not call {!setup} still gets
    colour, and one that did keeps its choice. *)

val is_terminal : Unix.file_descr -> bool
(** [is_terminal fd] is [true] iff [fd] is a terminal and [TERM] is set, not
    empty and not [dumb]. On such an output a program can move the cursor to
    redraw what it printed. *)
