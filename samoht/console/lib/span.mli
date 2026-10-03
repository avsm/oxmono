(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Styled text spans.

    A span is a piece of text with associated styling. Spans can be
    concatenated. *)

type t
(** A styled text span. *)

(** {1:constructors Constructors} *)

val text : string -> t
(** [text str] is a plain, unstyled span of [str]. *)

val styled : Style.t -> string -> t
(** [styled style str] is a span of [str] carrying [style]. *)

val link : ?style:Style.t -> uri:string -> string -> t
(** [link ~uri label] is a terminal hyperlink. ANSI rendering emits an OSC 8
    hyperlink while plain rendering emits only [label], so logs and redirected
    output stay clean. [style] styles the visible label. {b Raises.}
    [Invalid_argument] if [uri] is empty or contains a space or control
    character. *)

val empty : t
(** [empty] is the empty span with no content. *)

val space : t
(** [space] is a single space character span. *)

val newline : t
(** [newline] is a newline character span. *)

(** {1:concatenation Concatenation} *)

val concat : t list -> t
(** [concat spans] concatenates a list of spans. *)

val ( ++ ) : t -> t -> t
(** [a ++ b] concatenates two spans. *)

val concat_map : ?sep:t -> ('a -> t) -> 'a list -> t
(** [concat_map ~sep f xs] maps [f] over [xs] and concatenates with [sep]. *)

val sanitize : ?keep_newlines:bool -> t -> t
(** [sanitize span] replaces terminal control characters in visible text with
    spaces while preserving styles, hyperlinks, UTF-8, and line feeds.
    [keep_newlines:false] replaces line feeds too. *)

val split_lines : t -> t list
(** [split_lines span] splits [span] at line feeds without losing styles. The
    result is non-empty; an empty span is one empty line. *)

(** {1:rendering Rendering}

    Rendering follows the style renderer of the target formatter,
    [Fmt.style_renderer ppf], which [Console_eio.setup] sets from
    [--color=auto|always|never], [NO_COLOR] and [TERM]
    ({!Console.style_renderer}). There is no global colour switch. A style with
    a gradient ({!Style.fg_gradient}) colours each cell by its column, column
    [0] being the span's first cell. *)

val pp : t Fmt.t
(** [pp] pretty-prints the span with ANSI codes when the target formatter's
    style renderer permits, otherwise plain. *)

val pp_with_style : Style.t -> t Fmt.t
(** [pp_with_style base] is {!pp} with [base] composed underneath each style in
    the span. *)

val pp_plain : Format.formatter -> t -> unit
(** [pp_plain] renders the span without ANSI codes, no matter what the target
    formatter is configured for. *)

val to_string : t -> string
(** [to_string span] is the plain text of [span]. *)

val to_ansi_string : ?style:Style.t -> t -> string
(** [to_ansi_string span] renders [span] with ANSI codes. [style] is composed
    underneath each style in the span. *)

(** {1:measurements Measurements} *)

val width : ?char_width:(Uchar.t -> int) -> t -> int
(** [width span] is the display width of [span] in terminal columns, excluding
    ANSI escape sequences. *)

val truncate : ?char_width:(Uchar.t -> int) -> int -> t -> t
(** [truncate width span] is the longest styled prefix that fits in [width]
    terminal cells. Styles and hyperlinks are preserved. A non-positive width
    gives {!empty}. *)

val wrap : ?char_width:(Uchar.t -> int) -> ?hang:int -> int -> t -> t list
(** [wrap width span] word-wraps a single-line span at spaces and path
    separators, preserving styles and hyperlinks on every line. A token wider
    than [width] is continued on the next line where the room ends, so every
    character is printed and nothing is marked as cut. Every line after the
    first starts with [hang] spaces (default [0]), clamped to half of [width].
    {b Raises.} [Invalid_argument] if [width] is not positive. *)

val hanging : ?char_width:(Uchar.t -> int) -> t -> int
(** [hanging span] is the column the text after [span]'s label starts at: its
    leading spaces, its first word and the spaces after it. It is [0] when
    [span] is one word. [wrap ~hang:(hanging span)] sets a continuation under
    that text, as a label's value hangs beside it. *)

val pp_wrapped : t Fmt.t
(** [pp_wrapped] is {!pp} for a single-line span that fits its formatter's
    margin: a span wider than the margin is wrapped to it ({!wrap}), each
    continuation hanging under the text after its first word ({!hanging}). *)

(** {1:animation Animation} *)

val anim : t -> string Anim.t
(** [anim span] is [span] as a still {!Anim.t}: every frame is
    {!to_ansi_string}, so a static span drops into an animated {!Layout}
    alongside moving widgets. *)
