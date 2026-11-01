(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One look for every widget.

    A theme holds the choices the widgets share:
    - the main colour, and the colours of a success, a warning and a failure;
    - a muted colour, or faint text, for what frames and annotates, and the
      style of headers;
    - the marks that say how a task runs or ended and the level of a message;
    - the frame of tables and panels, and how it moves;
    - the lines of a tree and the spinner of a {!Display}.

    A formatter has one theme ({!set}), and every widget printed on it is drawn
    in that theme, with ASCII glyphs alone when the formatter is not
    {!Fmt.utf_8}. Text takes a colour of the theme, such as that of a success,
    with {!View.role}.

    The predefined themes suit different terminals:
    - {!default} is drawn in the terminal's own named colours and faint text,
      which its user chose for its background, so it reads wherever the terminal
      does and is the one to keep when nothing is known of it.
    - A theme with a {!background}, such as {!dracula} or {!solarized_light}, is
      for a terminal whose own colour scheme is that one. Its colours are chosen
      for that background, which the widgets do not paint, so on another
      background some of them may be hard to read.
    - {!ascii} draws with ASCII alone on any terminal.
    - {!still} takes the motion out of any theme, for the users who ask for no
      animation.

    A program lets its users choose with a [--theme] option ({!all}). Every
    predefined theme keeps to the ratios given under {{!contrast}Contrast}. *)

(** {1:themes Themes} *)

type role =
  [ `Accent  (** The main colour, {!accent}. *)
  | `Succeeded  (** The colour of a success, {!succeeded}. *)
  | `Warning  (** The colour of a warning, {!warning}. *)
  | `Failed  (** The colour of a failure, {!failed}. *)
  | `Muted  (** The colour of what frames and annotates, {!muted}. *)
  | `Header  (** The style of headers, {!header}. *) ]
(** The type for the parts of a theme that style text. A {!View.role} takes one
    from the theme of the formatter that prints it. *)

type mark =
  [ `Running
    (** Work still going on: a task's line as it starts on a pipe, and the
        summary until the work ends. *)
  | `Succeeded  (** A task, or the work, that succeeded. *)
  | `Failed  (** A task, or the work, that failed. *)
  | `Cancelled  (** A task, or the work, that was cancelled. *)
  | `Info  (** A line of a task's output. *)
  | `Warning  (** A message that warns. *)
  | `Error  (** A message that reports an error. *)
  | `Debug  (** A message for debugging. *) ]
(** The type for the states a mark says. Constructors may be added, so a
    function that gives marks ends with a case for the others, as
    [function `Failed -> "x" | m -> mark default m] does. *)

type bar =
  [ `Line
    (** Heavy horizontal lines (U+2501), filled to the half cell, on a light
        line (U+2500), in right and left one eighth blocks (U+2595 and U+258F).
    *)
  | `Block
    (** Whole cells of full blocks (U+2588) on a light shade (U+2591), as DOS
        programs drew progress, in square brackets with [~brackets:true]. *)
  | `Ascii
    (** Whole cells of [#] on [-], in square brackets with [~brackets:true]. *)
  ]
(** The type for the glyphs of a {!Bar}. Constructors may be added, so a match
    on a bar has a case for the others. *)

type motion =
  [ `Rain
    (** Digital rain. A bold head in the main colour runs round the frame. Two
        ASCII glyphs flicker behind it on the frame's straight lines, and the
        lines behind them fade to the muted colour. A corner or a tee keeps its
        glyph. *)
  | `Sweep
    (** The palette, graded once round the frame from its first colour at the
        top-left corner, travels round it. *)
  | `Shimmer
    (** A short highlight in the main colour travels round the frame. *) ]
(** The type for how the frame of a table or a panel moves while a live
    {!Display} draws it in a view ([Display.set_view]), clockwise and by the
    elapsed time. Only the colours of the border's cells change, and some cells'
    glyphs change to glyphs as wide, so the frame keeps its width and its cells
    their place in every frame.

    A frame stands still, in the glyphs of the theme without motion, when it is
    printed on a formatter or drawn by a display without colour or with
    [NO_COLOR] set. A sweep then keeps the colours a display starts with, while
    rain and a shimmer keep those of the theme without motion.

    Constructors may be added, so a match on a motion has a case for the others.
*)

type t
(** The type for themes. *)

val v :
  ?name:string ->
  ?accent:Color.t ->
  ?palette:Color.t list ->
  ?succeeded:Color.t ->
  ?warning:Color.t ->
  ?failed:Color.t ->
  ?muted:Color.t ->
  ?header:Style.t ->
  ?background:Color.t ->
  ?border:Border.t ->
  ?motion:motion ->
  ?guide:Guide.t ->
  ?spinner:string list ->
  ?bar:bar ->
  ?marks:(mark -> string) ->
  unit ->
  t
(** [v ()] is {!default} with the given parts replaced:

    - [name] is its {!name} (defaults to ["custom"]);
    - [accent] is the main colour (defaults to {!default}'s cyan);
    - [palette] are the colours that successive progress rows take in turn, and
      that a bar without a style spreads along its length when there are several
      (defaults to [[accent]], and [[]] is taken as the default);
    - [succeeded] colours the mark of a success, [warning] that of a warning or
      a cancellation, and [failed] that of a failure or an error (default to
      {!default}'s green, yellow and red);
    - [muted] colours frames, tree lines, rules, the track of a bar and the
      counts and timers of a display (defaults to none: they are faint, in the
      terminal's own colour);
    - [header] is the style of table headers, panel titles and the name on a
      display's summary line (defaults to {!Style.bold});
    - [background] is the light or dark background the colours are chosen for
      (defaults to none: the terminal's own), which the widgets do not paint;
    - [motion] is how frames move in a live display (defaults to none: they
      stand still);
    - [border], [guide] and [spinner] are the frame of tables and panels, the
      lines of a tree and the spinner of a running task (default to
      {!Border.rounded}, {!Guide.single} and {!Spinner.braille});
    - [bar] is the glyphs of bars and of a display's progress (defaults to
      [`Line]);
    - [marks] gives the mark of each state (defaults to those of {!default}).
      States may be added ({!type-mark}), so it ends with a case for the states
      it leaves to another theme, as
      [~marks:(function `Failed -> "x" | m -> mark default m)] does.

    @raise Invalid_argument if [spinner] is empty. *)

val name : t -> string
(** [name t] is the name of [t]: that of {!all} for a predefined theme. *)

val accent : t -> Color.t
(** [accent t] is the main colour of [t]. *)

val palette : t -> Color.t list
(** [palette t] are the colours that successive progress rows take in turn, and
    it is never empty. A bar without a style ({!Bar.horizontal}) spreads them
    along its length when there are several, and is in the one colour otherwise.
*)

val succeeded : t -> Color.t
(** [succeeded t] is the colour of a success. *)

val warning : t -> Color.t
(** [warning t] is the colour of a warning and of a cancellation. *)

val failed : t -> Color.t
(** [failed t] is the colour of a failure and of an error. *)

val muted : t -> Color.t option
(** [muted t] is the colour of frames, tree lines, rules, the track of a bar and
    the counts and timers of a display, if [t] has one. If not, they are faint
    ({!Style.faint}), in the terminal's own colour. *)

val header : t -> Style.t
(** [header t] is the style of table headers, panel titles and the name on a
    display's summary line. *)

val background : t -> Color.t option
(** [background t] is the background the colours of [t] are chosen for, if any.
*)

val border : t -> Border.t
(** [border t] is the frame of tables and panels. *)

val motion : t -> motion option
(** [motion t] is how the frames of [t] move in a live display, if they do. *)

val guide : t -> Guide.t
(** [guide t] is the lines that connect the branches of a tree. *)

val spinner : t -> string list
(** [spinner t] are the frames of a {!Spinner.view} given none, which a running
    task's row shows. It is never empty. *)

val bar : t -> bar
(** [bar t] is the glyphs of bars and of a display's progress, drawn [`Ascii] on
    a formatter that is not {!Fmt.utf_8}. *)

val mark : t -> mark -> string
(** [mark t m] is the mark of [m], on one line: a control character in it is a
    space. *)

val still : t -> t
(** [still t] is [t] with nothing that moves: no {!val-motion} and the spinner
    of one frame, which does not move, for the users who ask for no animation.
*)

(** {1:predefined Predefined themes}

    The themes after {!rainbow} take their colours from a colour scheme, as its
    authors define them. Unless said otherwise:
    - their {!background} is the scheme's;
    - their muted colour is its grey for comments or secondary text;
    - their headers are bold in the main colour;
    - their frames move with [`Shimmer].

    A colour below its contrast ({{!contrast}Contrast}) is moved towards the
    scheme's text, to just above it. *)

val default : t
(** [default] is the terminal's named colours: cyan, with green, yellow and red
    for a success, a warning and a failure, and faint text for what is muted. A
    terminal shows them in the colours of its own scheme, which its user chose
    to read on its background, and a recording in those of the recorder's. It
    has bold headers, rounded frames and a braille spinner. Its marks, for the
    states of {!type-mark} in order, are a bullet, a check mark, a cross, a
    circle, a single right-pointing angle quotation mark, [!], a cross and a
    middle dot. *)

val ascii : t
(** [ascii] is {!default} drawn with ASCII characters only: ASCII frames, tree
    lines, rules, spinner, bars and ellipses, and the marks of {!dos}. Every
    theme is drawn so on a formatter that is not {!Fmt.utf_8}, and [ascii] on
    any formatter. *)

val dos : t
(** [dos] is amber on black, with a darker amber muted and bold amber headers.
    It draws in the box-drawing characters of code page 437, as Norton Commander
    drew its panels:
    - {!Border.dos} frames, double lines divided by single ones;
    - {!Guide.single}'s single-line tree guides;
    - [`Block] bars and the ASCII spinner;
    - the marks [[-]], [[+]], [[x]], [[~]], [#], [!], [x] and [.]. *)

val matrix : t
(** [matrix] is bright green on black, a darker green muted and bold green
    headers, with single-line frames on which digital rain falls ([`Rain]). *)

val rainbow : t
(** [rainbow] gives successive progress rows the colours of the rainbow, taken
    from the xterm 256-colour palette (entries 160, 136, 28, 30, 63 and 164) for
    their contrast on dark and light backgrounds alike, and also spreads them
    along its bars and sweeps them round its frames ([`Sweep]). *)

val dracula : t
(** [dracula] is Dracula's purple, green, yellow and red, for its background
    [#282a36], its comment colour lightened to [#8692b7]. *)

val nord : t
(** [nord] is Nord's frost cyan, green, yellow and red, for its background
    [#2e3440], its comment grey lightened to [#969ead]. *)

val solarized_dark : t
(** [solarized_dark] is Solarized's blue, green, yellow and red, for its dark
    background [#002b36], its body text [#839496] muted and its emphasised text
    [#93a1a1] in bold for headers. *)

val solarized_light : t
(** [solarized_light] is like {!solarized_dark} for Solarized's light background
    [#fdf6e3], its green and yellow darkened to [#819400] and [#b18600], its
    comment grey darkened to [#5e7379] and its emphasised text [#586e75] in bold
    for headers. *)

val gruvbox_dark : t
(** [gruvbox_dark] is gruvbox's orange and its bright green, yellow and red, for
    its dark background [#282828], its [fg4] muted. *)

val gruvbox_light : t
(** [gruvbox_light] is gruvbox's faded orange, green, yellow and red, for its
    light background [#fbf1c7], its [fg3] muted. *)

val catppuccin_latte : t
(** [catppuccin_latte] is Catppuccin Latte's mauve, green, yellow and red, for
    its background [#eff1f5], its green and yellow darkened to [#3e9b2a] and
    [#be7919], its [subtext1] muted. *)

val catppuccin_mocha : t
(** [catppuccin_mocha] is Catppuccin Mocha's mauve, green, yellow and red, for
    its background [#1e1e2e], its [overlay2] muted. *)

val tokyo_night : t
(** [tokyo_night] is Tokyo Night's blue, green, yellow and red, for its
    background [#1a1b26], its comment colour lightened to [#7b83ab]. *)

val one_dark : t
(** [one_dark] is One Dark's accent blue, green, yellow and red, for its
    background [#282c34], its comment grey lightened to [#8e95a2] and its syntax
    blue [#61afef] in bold for headers. *)

val monokai : t
(** [monokai] is Monokai's cyan, green, yellow and pink, for its background
    [#272822], its comment colour lightened to [#939080]. *)

val github_dark : t
(** [github_dark] is GitHub Dark's accent blue, green, yellow and red, for its
    background [#0d1117], its muted foreground [#8b949e] muted. *)

val github_light : t
(** [github_light] is GitHub Light's accent blue, green, attention yellow
    [#9a6700] and red, for its background [#ffffff], its muted foreground
    [#656d76] muted. *)

(** {1:names Names} *)

val all : (string * t) list
(** [all] is every predefined theme under its {!name}, {!default} first, which
    are the choices of a [--theme] option as [Cmdliner.Arg.enum all] reads them.
*)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the theme of {!all} named [s]. The error message lists the
    names. A theme {!v} makes is not in {!all}: [of_string (name t)] is an
    [Error] for it. *)

(** {1:formatters Themes of formatters} *)

val set : Format.formatter -> t -> unit
(** [set ppf t] makes [t] the theme of [ppf], in which the widgets printed on
    [ppf] from now on are drawn. *)

val get : Format.formatter -> t
(** [get ppf] is the theme of [ppf]: the last one {!set} gave it, else
    {!default}. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a theme for diagnostics, in an unspecified format. *)

(** {1:contrast Contrast}

    A terminal that shows fewer colours than a theme has gets the nearest ones
    it has ({!Color.val-depth}). Contrast is measured as WCAG 2.1 measures it,
    against the theme's {!background}, and in every predefined theme:
    - the colours of text are at 4.5:1 or more, that is the {!muted} colour,
      that of the {!header}, and the colours {!View.role} gives text (the main
      colour and the colours of a success, a warning and a failure);
    - the colours of the progress rows are at 3:1 or more.

    A theme without a background that gives colours of its own is read against
    black and white alike, with its muted colour and header at 4.5:1 and its
    other colours at 3:1. A frame that moves is drawn in these colours alone, so
    each of its frames keeps the contrast of a glyph.

    {!default} gives none: its colours are the terminal's named ones and its
    muted text is faint, so their contrast is that of the terminal's own scheme,
    which console cannot know and which its user chose. At 16 colours every
    theme is drawn so, in the named colours nearest its own. *)
