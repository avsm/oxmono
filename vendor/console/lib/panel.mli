(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Bordered panels with optional titles.

    A panel is lines of styled text in a {!Border}, with a title on its top edge
    and a subtitle on its bottom one. *)

(** {1:panels Panels} *)

val view :
  ?border:Border.t ->
  ?title:View.t ->
  ?subtitle:View.t ->
  ?padding:int ->
  ?expand:bool ->
  View.t ->
  View.t
(** [view content] is a panel around [content], laid out in its room
    ({!View.fit}): no wider than the room, and its content laid out in the room
    the frame and the padding leave it.

    - The border is [border] if given, else the border of the formatter's theme
      ({!Theme.get}).
    - [title] labels the top edge, in the theme's {!Theme.header}, and
      [subtitle] labels the bottom edge.
    - [padding] is the internal horizontal padding (defaults to [1]).
    - [expand] (defaults to [false]) makes the panel as wide as its room, its
      content and edge labels cut to keep the box a rectangle. Otherwise the
      panel is sized to its content.

    A line wider than the room inside the frame:
    - wraps at its words in a panel sized to its content ({!View.wrap}), hanging
      under the text after its first word;
    - is cut in a panel that expands ({!View.truncate}), and so are edge labels.

    A panel that fits its room is a rectangle. One in a room too narrow for its
    border has each line cut to the room.

    @raise Invalid_argument
      if [padding] is negative, or the padding and border are wider than a
      string can hold ({!Sys.max_string_length}). *)
