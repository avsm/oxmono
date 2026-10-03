(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal styling and layout for command-line tools.

    A {!Span.t} is the unit of output: a run of text carrying a {!Style.t} -- a
    {!Color.t}, bold, italic, and so on. {!Width} measures spans the way a
    terminal does, counting wide CJK and emoji cells rather than bytes, so the
    widgets align correctly. {!Panel}, {!Table}, {!Tree} and {!Rule} compose
    spans into bordered, columned, nested and divided blocks; {!Border} and
    {!Guide} hold their drawing vocabulary, {!Layout} joins blocks, and a
    {!Theme} gives the whole set one consistent look. {!Display} is the live,
    in-place progress view for long-running work.

    {1:geometry Terminal geometry contract}

    Widths are terminal-cell widths, never byte or code-point counts. Whenever a
    widget receives a width--directly, through a formatter margin, or from a
    {!Display}--that width is a hard ceiling for every physical row. ANSI
    escapes occupy no cells; wide and combining glyphs follow terminal width;
    malformed UTF-8 is rendered as a replacement glyph; terminal controls in
    content cannot move the cursor. Fitting preferences may change which content
    is shortened, but may not push a border into the next row.

    ANSI multi-line widgets use terminal-safe physical row boundaries, so a row
    that ends exactly at the right edge cannot leave the next border or glyph
    wrapped onto an extra line.

    {1:quickstart Quick start}

    {2:styled Styled text}

    {[
    open Console

    let demo_styled () =
      let style = Style.(bold + fg Color.green) in
      Fmt.pr "%a@." (Style.styled style Fmt.string) "Success!"
    ]}

    {2:tables Tables}

    {[
    open Console

    let demo_table () =
      let table =
        Table.(
          of_rows ~border:Border.rounded
            [ column "Name"; column ~align:`Right "Age" ]
            [
              [ Span.text "Alice"; Span.text "30" ];
              [ Span.text "Bob"; Span.text "25" ];
            ])
      in
      Fmt.pr "%a@." Table.pp table
    ]}

    {2:trees Trees}

    {[
    open Console

    let demo_tree () =
      let tree =
        Tree.of_tree
          (Tree.Node
             ( Span.text "src",
               [
                 Tree.Node
                   (Span.text "lib", [ Tree.Node (Span.text "tty.ml", []) ]);
                 Tree.Node (Span.text "test", []);
               ] ))
      in
      Fmt.pr "%a@." Tree.pp tree
    ]}

    {2:panels Panels}

    {[
    open Console

    let demo_panel () =
      let panel =
        Panel.v ~title:(Span.text "Status")
          (Span.text "All systems operational")
      in
      Fmt.pr "%a@." Panel.pp panel
    ]}

    {2:progress Progress}

    A {!Display} keeps permanent event history separate from an immutable view
    of current activity. On a terminal the activity view is redrawn in one
    bounded region; finished tasks and logs remain in scrollback. Without a
    terminal, only the permanent history is emitted.

    A top-level binary calls [Console_eio.run] once and passes the resulting
    display to its libraries. Ordinary widgets are regular pretty-printers; only
    semantic task and event operations require the display. A nested
    [Console_eio.run] joins the existing session, so independently composable
    libraries cannot claim the terminal twice.

    {[
    open Console

    let build_step ~clock =
      Console_eio.run ~clock ~theme:Theme.unicode ~header:"Building"
      @@ fun display ->
      let step = Display.task display "compile" in
      Display.set_count step ~cur:1 ~total:2;
      Display.log step "compiled parser.ml";
      Display.succeed step
    ]}

    {2:animation Animation}

    An {!Anim.t} is a value sampled at an elapsed time. {!Spinner}, {!Bar} and
    {!Panel.rain} expose one.

    {[
    open Console

    let demo_anim () =
      let bar = Bar.anim ~style:`Smooth ~width:20 () in
      Fmt.pr "%s@." (Anim.frame bar ~elapsed:0.5)
    ]} *)

(** {1:text Untrusted text} *)

val sanitize : ?keep_newlines:bool -> string -> string
(** [sanitize s] is [s] made safe to write to a terminal, under the geometry
    contract above: a malformed UTF-8 sequence becomes the replacement
    character, and a C0 control, DEL or a C1 control becomes a space, so nothing
    in [s] can move the cursor, repaint the screen or change the styling of what
    follows. Newlines survive unless [keep_newlines] is [false], in which case
    they become spaces too and the result is one physical row.

    Every widget applies this to the text it lays out, and {!Span.sanitize} is
    it over a styled run. This is the same rule over a plain string, for a
    caller printing one of its own -- a subprocess's stderr, a peer's name off
    the network, a filename -- outside any widget. *)

val sanitize_styles : ?keep_newlines:bool -> string -> string
(** [sanitize_styles s] is {!sanitize} over [s] except that the styling [s]
    already carries survives, for a caller relaying the output of a tool that
    coloured it.

    What survives is a strictly-parsed SGR sequence (ECMA-48 8.3.117): CSI, then
    parameter bytes, then the final byte ['m']. The accepted parameter bytes are
    the digits and [';'] and nothing else, a strict subset of the parameter
    bytes ECMA-48 5.4 allows, so a private parameter byte and the [':']
    sub-parameter of ITU-T T.416 are refused rather than forwarded on a guess;
    the run is at most 64 bytes long, which no real SGR reaches and an
    adversary's would; and a sequence the string ends in the middle of is
    refused as well.

    Everything else is exactly what {!sanitize} makes of it -- cursor movement
    and erase, which share the grammar under a different final byte, OSC
    including the OSC 8 hyperlink, DCS, and the C1 controls, the C1 introducer
    among them, which open none of this. SGR cannot move the cursor, erase a
    region or name a resource, which is what makes it the one sequence that can
    travel and leave the geometry contract above intact.

    The result cannot change the styling of what the caller prints next: the
    last surviving sequence is followed by an SGR reset unless it is one
    already, which SGR makes a question about its last parameter, 0 (or empty,
    which reads as 0) leaving no attribute set. So text that closes its own
    styling comes back byte for byte, and a second pass changes nothing the
    first did not.

    An SGR sequence is zero columns wide ({!Width.string_width}), so what this
    returns measures and lays out as the visible text alone. *)

(** {1:setup Colour on or off} *)

val style_renderer :
  ?renderer:Fmt.style_renderer ->
  getenv:(string -> string option) ->
  is_tty:bool ->
  unit ->
  Fmt.style_renderer
(** [style_renderer ~renderer ~getenv ~is_tty ()] is whether an output takes
    ANSI styling, read from the environment [getenv] reads:
    - [renderer] when given, the reader's [--color=always|never];
    - [`None] when [NO_COLOR] is set to a non-empty value
      ({{:https://no-color.org}no-color.org});
    - [`None] when [TERM] is unset, empty or [dumb];
    - [`Ansi_tty] iff [is_tty], the output being a terminal, otherwise.

    [Console_eio.setup] sets stdout and stderr by it. *)

(** {1:modules Modules} *)

module Color = Color
(** Named and 24-bit terminal colours. *)

module Style = Style
(** Text attributes: foreground and background colour, bold, italic, underline.
*)

module Gradient = Gradient
(** Linear colour gradients laid over terminal cells. *)

module Width = Width
(** The display width of text in terminal cells, counting wide glyphs as two. *)

module Span = Span
(** A run of styled text: the unit every widget lays out. *)

module Border = Border
(** Box-drawing border presets -- ASCII, single, rounded, and friends. *)

module Guide = Guide
(** Shared guide glyphs for trees and nested progress rows. *)

module Anim = Anim
(** Time-varying values: the shape every animatable component shares. *)

module Spinner = Spinner
(** Animated spinners for indeterminate work. *)

module Theme = Theme
(** A shared visual theme -- border, tree guide, colours and live-display glyphs
    -- that styles every widget consistently. *)

module Bar = Bar
(** Horizontal progress bars, determinate or moving. *)

module Panel = Panel
(** A bordered box around content, with an optional title and subtitle. *)

module Table = Table
(** Columned tables with per-column alignment, wrapping and borders. *)

module Tree = Tree
(** Nested trees drawn with guide lines. *)

module Rule = Rule
(** Horizontal rules with optional styled labels. *)

module Layout = Layout
(** Joining rendered text blocks side by side into aligned columns. *)

module Canvas = Canvas
(** Grids of styled cells, for pixel art and scenes. *)

module Display = Display
(** A live progress display that redraws rows in place. *)

module Input = Input
(** Cooked terminal input: a line editor with history and tab completion. *)
