(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables with headers, alignment, and borders.

    Tables display data in aligned columns with optional headers and
    customizable borders. *)

(** {1:types Types} *)

type align = [ `Left | `Center | `Right ]
(** Column alignment. *)

type overflow = [ `Truncate | `Wrap ]
(** How to handle content that exceeds max_width.

    - [`Truncate]: Keep the leading words that fit on one line and drop the
      rest; a value whose first word does not fit is printed whole, over several
      lines, as with [`Wrap]. No word is ever cut.
    - [`Wrap]: Wrap to multiple lines (default) *)

type column
(** A column specification. *)

type t
(** A table. *)

(** {1:column_construction Column construction} *)

val column :
  ?align:align ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  string ->
  column
(** [column ?align ?min_width ?max_width ?overflow ?style header] is a column
    specification.

    - [align] is the text alignment (default [`Left]).
    - [min_width] is the preferred minimum column width. Terminal fitting may
      reduce it as a last resort rather than overflow.
    - [max_width] is the maximum column width.
    - [overflow] is how content beyond [max_width] is handled (default [`Wrap]).
    - [shrinkable] (default [true]) sets fitting priority. A [false] column is
      protected until shrinkable columns have yielded, but may still be reduced
      rather than violate the output width.
    - [style] is applied to every cell in the column.
    - [header] is the column header text. *)

val column_span :
  ?align:align ->
  ?min_width:int ->
  ?max_width:int ->
  ?overflow:overflow ->
  ?shrinkable:bool ->
  ?style:Style.t ->
  Span.t ->
  column
(** [column_span] is like {!val-column} but takes a styled span for the header.
*)

(** {1:table_construction Table construction} *)

val v :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  t
(** [v columns] is an empty table. The border is [border] if given, else
    [theme]'s border, else {!Border.single}; the header row defaults to bold. A
    theme with {!Theme.table_row_separators} enabled draws rules between logical
    data rows. *)

val add_row : Span.t list -> t -> t
(** [add_row cells table] adds a row of styled cells. {b Raises.}
    [Invalid_argument] if the number of cells differs from the number of
    columns. *)

val add_row_strings : string list -> t -> t
(** [add_row_strings strings table] adds a row of plain strings. *)

val of_rows :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  Span.t list list ->
  t
(** [of_rows columns rows] is a table with all its data at once; styling is as
    in {!v}. A cell may contain line feeds; they produce physical lines inside
    that logical row. *)

val of_string_rows :
  ?theme:Theme.t ->
  ?border:Border.t ->
  ?header_style:Style.t ->
  column list ->
  string list list ->
  t
(** [of_string_rows] is like {!of_rows} but takes plain strings. *)

(** {1:rendering Rendering} *)

val pp : t Fmt.t
(** [pp table] follows the formatter's style renderer and fits the table to its
    margin. No physical row exceeds that margin; fitting preferences never
    override it. This makes [Fmt.pr "%a" pp table] adapt to a formatter
    configured for the terminal. Use {!to_string} for a natural-width string or
    to supply an explicit maximum width. *)

val to_string : ?width:int -> ?char_width:(Uchar.t -> int) -> t -> string
(** [to_string table] renders plain text to a string. [width], when supplied, is
    a hard maximum for every physical row. *)

val to_ansi_string : ?width:int -> ?char_width:(Uchar.t -> int) -> t -> string
(** [to_ansi_string table] renders with ANSI styling. [width], when supplied, is
    a hard maximum for every physical row. *)

(** {1:animation Animation} *)

val anim : ?width:int -> ?char_width:(Uchar.t -> int) -> t -> string Anim.t
(** [anim table] is [table] as an {!Anim.t}. It is still ({!to_ansi_string} on
    every frame) unless [table]'s theme set {!Theme.animated_border} -- as
    {!Theme.matrix} does -- in which case it is {!rain}. *)

val rain : ?width:int -> ?char_width:(Uchar.t -> int) -> t -> string Anim.t
(** [rain table] frames the rendered table with a Matrix-style ASCII
    digital-rain border (see {!Panel.rain}). Deterministic in the elapsed time.
*)
