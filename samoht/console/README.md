# console

Terminal styling and layout widgets for OCaml CLI applications.

```
╭──────────────────────────────────────────────────────────────────╮
│                                                                  │
│    ██████╗ ██████╗ ███╗   ██╗███████╗ ██████╗ ██╗     ███████╗   │
│   ██╔════╝██╔═══██╗████╗  ██║██╔════╝██╔═══██╗██║     ██╔════╝   │
│   ██║     ██║   ██║██╔██╗ ██║███████╗██║   ██║██║     █████╗     │
│   ██║     ██║   ██║██║╚██╗██║╚════██║██║   ██║██║     ██╔══╝     │
│   ╚██████╗╚██████╔╝██║ ╚████║███████║╚██████╔╝███████╗███████╗   │
│    ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝ ╚═════╝ ╚══════╝╚══════╝   │
│                                                                  │
│   Type-safe terminal styling and layout widgets for OCaml        │
│                                                                  │
╰──────────────────────────────────────────────────────────────────╯
```

A command-line tool that prints a table, a tree or a progress bar must measure
each string in terminal cells, keep ANSI styling out of output sent to a file,
and redraw its live rows without garbling the scrollback. The terminal takes
its commands from escape sequences inside the printed text. A CSI sequence (ESC
`[` ...) moves the cursor or sets a colour; an OSC sequence (ESC `]` ...) sends
the terminal a command, and OSC 8 turns the text it wraps into a link to a URI.

`console` handles this in one place. It prints styled text, tables, trees,
panels and live progress rows *inline*, into the normal scrollback, and when
standard output is not a terminal it prints plain lines that are only ever
appended. It never switches to the alternate screen (the second buffer that
full-screen programs such as `vim` draw on, leaving the scrollback alone), and
it runs no event loop of its own on the terminal. For a full-screen program,
use [notty](https://github.com/pqwy/notty) or
[lambda-term](https://github.com/ocaml-community/lambda-term).

- **Styles** add up with `+`, as in `Style.(bold + fg Color.red + underline)`,
  over the 16 ANSI colours, the 256-colour palette and true colour (RGB or hex).
- **Spans** carry styled text and OSC 8 hyperlinks (a URI with a space or a
  control character is refused), and keep their style when wrapped or
  truncated. Printed without ANSI, a span is plain text.
- **Widgets** cover tables with headers and column alignment, directory-style
  trees, bordered panels and horizontal rules, with five border styles.
- **Themes** give every widget the same look from one `Theme.t`; the presets
  are `dos`, `unicode`, `matrix` and `rainbow`.
- **Live progress** comes from a `Display`, whose rows update in place on a
  terminal. Without one it prints a single line per step.
- **Animations** (spinners, progress bars and Matrix rain) are all `Anim.t`
  values, and `Layout` places them side by side or one above the other.
- **Widths** of CJK characters and emoji are counted in the cells they take.

## Contents

- [Installation](#installation)
- [Usage](#usage)
- [Progress](#progress)
- [Animation and layout](#animation-and-layout)
- [API](#api)

## Installation

Install with opam:

<!-- $MDX skip -->
```sh
$ opam install console
```

If opam cannot find the package, it may not yet be released in the public
`opam-repository`. Add the overlay repository, then install it:

<!-- $MDX skip -->
```sh
$ opam repo add samoht https://tangled.org/gazagnaire.org/opam-overlay.git
$ opam update
$ opam install console
```

## Usage

Styles compose with `+` and apply through a `Fmt.t` combinator:

```ocaml
open Console

let success = Style.(bold + fg Color.green)
let error = Style.(bold + fg Color.red)

let () =
  Fmt.pr "%a@." (Style.styled success Fmt.string) "✓ Build passed";
  Fmt.pr "%a@." (Style.styled error Fmt.string) "✗ Tests failed"
```

Every widget takes its text as a `Span`. Whether a span's styles and links
reach the output depends on where it is printed: `Span.pp` writes them only
when the formatter's `Fmt.style_renderer` allows ANSI, and `Span.to_string`
always returns plain text, so logs and redirected output carry no escape
sequences.

```ocaml
open Console

let docs =
  Span.(
    styled Style.bold "Read "
    ++ link ~style:Style.underline ~uri:"https://ocaml.org/docs" "the manual")

let () = Fmt.pr "%a@." Span.pp docs
```

The static widgets are plain `Fmt` printers. Each one takes the width its
content needs, unless you give it `~width` because the output has a known
limit, for instance on a remote PTY:

```ocaml
open Console

let print_files () =
  let table = Table.(
    of_rows ~border:Border.rounded
      [ column "Name"; column ~align:`Right "Size"; column "Modified" ]
      [
        [ Span.text "README.md"; Span.text "2.4K"; Span.text "2 hours ago" ];
        [ Span.text "src/"; Span.text "4.0K"; Span.text "yesterday" ];
        [ Span.text "Makefile"; Span.text "892"; Span.text "3 days ago" ];
      ]
  ) in
  Fmt.pr "%a@." Table.pp table

let main env =
  Console_eio.run ~clock:(Eio.Stdenv.clock env) @@ fun _display ->
  print_files ()
```

```
╭───────────┬──────┬─────────────╮
│ Name      │ Size │ Modified    │
├───────────┼──────┼─────────────┤
│ README.md │ 2.4K │ 2 hours ago │
│ src/      │ 4.0K │ yesterday   │
│ Makefile  │  892 │ 3 days ago  │
╰───────────┴──────┴─────────────╯
```

Trees and panels print the same way, with `Tree.pp` and `Panel.pp`. A
`Theme.t` holds the visual choices the widgets share: accent colour and
palette, border, tree guide, spinner, bar style and markers. Give each widget
the same `~theme` and the output has one look; an explicit `~border` or
`~guide` still wins over the theme. The presets are `Theme.unicode` (the
default), `Theme.dos` (an amber ASCII terminal), `Theme.matrix` and
`Theme.rainbow`, and `Theme.v` builds a new one.

For the small things between widgets there is `Rule`, a horizontal line with
an optional label, and `Layout`, which puts rendered strings next to each
other or one under the other:

```ocaml
open Console

let section =
  Rule.v ~theme:Theme.unicode ~label:(Span.text "Results") ~width:40 ()

let report =
  Layout.vcat ~gutter:1
    [ Rule.to_string section; Panel.to_string (Panel.v (Span.text "All green")) ]
```

### Progress

A `Display` records every event (a task started, a log line, a task finished)
in a history that only grows, and it also keeps a snapshot of what is running
right now. On a terminal, the snapshot is what gets redrawn, and it never
takes more lines than the screen has. Logs and finished tasks are printed once
into the scrollback. When the output is a pipe or a log file, you get the same
history and no cursor movement at all.

```ocaml
open Console

let build ~clock =
  Console_eio.run ~clock ~theme:Theme.unicode ~header:"Building" @@ fun display ->
  let step = Display.task display "compile" in
  Display.set_count step ~cur:2 ~total:2;
  Display.log step "compiled parser.ml";
  Display.succeed step
```

That is usually all you need. Wrap the work in one `Console_eio.run` and
create a `Display.task` for each step. Until a task knows its total, its row
shows a spinner; after that it shows a bar, a count, a rate and the elapsed
time. If you forget to finish a task, it is marked as succeeded when the
callback returns, or cancelled if the callback raises. Calling
`Console_eio.run` again inside the callback reuses the display that is already
there. A task can also take a stable `~id`: asking for the same id again
returns the same task, which is handy when the events for one task arrive
twice or out of order.

For another row layout, `Display.Line` builds one from pieces: spinner, label,
bar, percent, count, rate, elapsed time and spacer.

### Animation and layout

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

An `Anim.t` is a value that depends on the elapsed time; each frame reads it
at the current time. `Spinner`, `Bar`, `Panel.rain` and `Table.rain` each give
one, and a panel or table built with `~theme:Theme.matrix` rains with no more
code. `Anim.map2`, `Anim.all`, `Layout.hcat_anim` and `Layout.vcat_anim` put
several animations together and read them all at the same time. The demo in
[`examples/matrix`](examples/matrix) runs with:

<!-- $MDX skip -->
```sh
$ dune exec examples/matrix/main.exe
```

## API

| Module | Description |
|---|---|
| `Console.Color` | ANSI, RGB, hex, and 256-palette colours |
| `Console.Style` | Composable text styles |
| `Console.Span` | Styled text, hyperlinks, wrapping and truncation |
| `Console.Width` | Unicode-, CSI-, and OSC-aware cell measurement |
| `Console.Border` | Custom and predefined box-drawing borders |
| `Console.Guide` | Shared guide glyphs for nested views |
| `Console.Theme` | One visual theme for every widget |
| `Console.Table` | Styled tables with alignment and overflow policies |
| `Console.Tree` | Tree rendering with shared guides |
| `Console.Panel` | Bordered panels with edge labels |
| `Console.Rule` | Fixed-width horizontal rules with styled labels |
| `Console.Layout` | Horizontal and vertical static/animated composition |
| `Console.Spinner` | Animated spinners for indeterminate work |
| `Console.Bar` | Determinate and moving horizontal progress bars |
| `Console.Anim` | Composable time-varying values |
| `Console.Display` | Live nested progress rows with plain fallback |
| `Console.Input` | Line editing with history and completion |
| `Console_eio` | Eio driver: terminal detection, geometry, the refresh fiber |
| `Console_vte` | The screen a terminal recording put up, frame by frame |

## Related Work

- [Rich](https://github.com/Textualize/rich) - Python library for rich text and beautiful formatting. Primary inspiration.
- [lipgloss](https://github.com/charmbracelet/lipgloss) - Go library for terminal styling. Inspiration for composable style API.
- [printbox](https://github.com/c-cube/printbox) - OCaml library for printing nested boxes, tables and trees.
- [progress](https://github.com/CraigFe/progress) - Terminal progress bars for OCaml.
- [notty](https://github.com/pqwy/notty) - OCaml library for declarative terminal graphics.
- [down](https://erratique.ch/software/down) - OCaml toplevel line editor by Daniel Buenzli; model for `Console.Input`'s key bindings, history and completion.

## License

ISC. See [LICENSE.md](LICENSE.md).
