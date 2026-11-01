(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bars, for progress and for figures.

    A bar is filled in its colour to a fraction of its length, with the rest
    drawn as a track. A {!horizontal} bar shows progress; {!vertical} bars side
    by side ({!View.hcat}) are a {!sparkline} on one row and a {!chart} on
    several:

    {[
    let sparkline values =
      Console.(View.hcat ~gap:0 (List.map Bar.vertical values))

    let histogram ~height values =
      Console.(
        View.sized ~height (View.hcat ~gap:1 (List.map Bar.vertical values)))
    ]}

    The glyphs of a horizontal bar are those of the formatter's theme
    ({!Theme.get}, {!Theme.val-bar}):

    - [`Line] draws a heavy horizontal line (U+2501), in which a half filled
      cell is the left half of the line (U+2578), on a track of a light line
      (U+2500), so that the fill shows without colour too. The brackets are a
      right one eighth block (U+2595) and a left one (U+258F).
    - [`Block] fills whole cells with full blocks (U+2588) on a track of light
      shade (U+2591). Its brackets are [\[] and [\]].
    - [`Ascii] fills whole cells with [#] on a track of [-]. Its brackets are
      [\[] and [\]]. Every theme draws its bars so on a formatter that is not
      {!Fmt.utf_8}.

    A bar has brackets only with [~brackets:true] ({!horizontal}). *)

(** {1:bars Bars} *)

val horizontal :
  ?style:Style.t ->
  ?track:Style.t ->
  ?brackets:bool ->
  ?width:int ->
  float ->
  View.t
(** [horizontal ~style ~track ~brackets ~width frac] is a bar [width] cells long
    in [style], filled to [frac] of its length, with [frac] clamped to the range
    0 to 1 and NaN taken as 0. The fill is rounded to the nearest half cell in
    [`Line] glyphs, and to the nearest cell in the others.

    Without [width], the bar grows to its room ({!View.fit}): it takes what the
    other parts of a {!View.hcat} leave it, the formatter's margin under
    {!View.pp}, and ten cells at its natural size. With [width], it is [width]
    cells long, or as long as its room if that is less.

    - [track] is the style of the rest (defaults to the muted colour of the
      formatter's theme).
    - [brackets] (defaults to [false]) frames the bar in the [track] style,
      which makes it two cells wider. In a room, the brackets take two of the
      room's cells, and are left out where the room has fewer than three.

    Without [style], the bar takes the {!Theme.palette} of the formatter's
    theme. A palette of several colours is spread along the bar's whole length,
    each filled cell in the colour of its place, so the fill uncovers the
    colours as it grows. A palette of one colour gives that colour.

    The bar is drawn in the theme and {!Fmt.utf_8} of the formatter it is
    printed on. A part that {!View.truncate} or {!View.wrap} keep is drawn as
    those cells of the bar.

    @raise Invalid_argument if [width] is negative. *)

val vertical :
  ?style:Style.t ->
  ?track:Style.t ->
  ?peak:float ->
  ?width:int ->
  float ->
  View.t
(** [vertical ~style ~track ~peak ~width frac] is a bar [width] cells wide, as
    tall as its room ({!View.fit}) and one row at its natural size, filled from
    the bottom to [frac] of its height, with [frac] clamped to the range 0 to 1
    and NaN taken as 0. Without [width], it grows to the width of its room, one
    cell at its natural size, as a column does. With [width], it is no wider
    than its room.

    Each cell shows the eighths of it that are filled, rounded to the nearest
    eighth of the whole bar: a lower block from U+2581 (one eighth) to U+2587
    (seven eighths), a full block (U+2588), or a blank when empty. Off
    {!Fmt.utf_8}, and in a theme whose bars are [`Ascii], a cell is a blank, a
    dot ([.]) when filled to less than half, a colon ([:]) to less than seven
    eighths, and a hash ([#]) above.

    Without [style], each cell takes the colour of the height its fill reaches:
    up the {!Theme.palette} of the formatter's theme when it has several
    colours, and otherwise from its {!Theme.succeeded} colour through its
    {!Theme.warning} to its {!Theme.failed}, so that the cells of a sparkline
    show their values. [track] is the style of an empty cell.

    [peak] (defaults to [0.]), clamped as [frac] is, marks a height the bar
    reached: a line in the [track] style, at the bottom of the cell [peak]
    reaches if it reaches less than half of it and at its top otherwise, or a
    dash ([-]) where the bar is drawn in ASCII. It shows only above the fill, in
    a cell the fill leaves empty. A spectrum whose peaks fall slowly is a row of
    such bars:

    {[
    let spectrum ~height bands =
      Console.(
        View.sized ~height
          (View.hcat ~gap:1
             (List.map
                (fun (level, peak) -> Bar.vertical ~width:2 ~peak level)
                bands)))
    ]}

    @raise Invalid_argument if [width] is negative. *)

val sparkline :
  ?style:Style.t -> ?min:float -> ?max:float -> float list -> View.t
(** [sparkline values] is a {!vertical} bar one cell wide for each of [values],
    side by side on one row in a room of any height: each filled to its value
    between [min] and [max] (default to the lowest and the highest of [values]),
    and without [min] the lowest at an eighth, so that every value shows. In a
    room narrower than [values], it shows the last values that fit, and it may
    be laid out one cell wide ({!View.fit}'s [min_width]), so that a {!Table}
    narrows its column as it narrows the others. *)

val chart :
  ?height:int ->
  ?scale:float ->
  ?style:(string -> float -> Style.t) ->
  (string * float) list ->
  View.t
(** [chart bars] is a bar chart of [bars], a [(label, value)] each: a
    {!vertical} bar as wide as the wider of its label and its value, the value
    above it in the muted colour and the label under it, the bars one cell apart
    on a baseline in the muted colour. A value is written as a plain number: a
    whole value below a million in full, such as [12346], another value below
    ten thousand to three significant digits, or to the unit from a thousand up,
    such as [3.14], and any other value to three digits in thousands, millions
    and so on, such as [12.3k] for [12345.5]. A value of [0.] has no number
    above its bar, and a negative or NaN value has an empty bar under its
    number. The bars are [height] lines tall (defaults to the height of the room
    less the three lines of values, baseline and labels, and to 8 without one),
    filled to their value over [scale] (defaults to the largest value), each bar
    in [style label value] if [style] is given, and otherwise in the colours of
    a {!vertical} bar given no style.

    The chart keeps to its room: the bars are at most the room's height less
    three lines and at least one line, and in a room too short for all three
    lines the values go first, then the baseline, then the labels. A label is
    drawn on one line, and a line wider than the room is cut. *)
