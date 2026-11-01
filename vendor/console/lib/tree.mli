(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Trees, drawn like the output of [tree].

    Each node is on its own row, behind the {!Guide} lines that connect it to
    its parent.

    {[
    let tree =
      Console.(
        Tree.v (View.text "root")
          [
            Tree.v (View.text "bin") [];
            Tree.v (View.text "etc") [ Tree.v (View.text "hosts") [] ];
          ])

    let () =
      assert (
        Console.(View.to_string (Tree.view ~guide:Guide.ascii tree))
        = "root\n+-- bin\n+-- etc\n    +-- hosts")
    ]} *)

(** {1:trees Trees} *)

type t
(** The type for trees: a label above sub-trees. *)

val v : View.t -> t list -> t
(** [v label children] is the tree of [label] above [children]. *)

val label : t -> View.t
(** [label t] is the label of [t]. *)

val children : t -> t list
(** [children t] are the sub-trees of [t], in order. *)

(** {1:views Views} *)

val view : ?guide:Guide.t -> ?indent:int -> t -> View.t
(** [view t] draws [t] laid out in its room ({!View.fit}), each node on its own
    lines, behind the lines of [guide] that connect it to its parent (defaults
    to the guide of the formatter's theme, {!Theme.get}).

    - A label is laid out in the room its guides leave it. One wider than that
      wraps, and the rest goes under its text, behind the lines that go on to
      the node's next sibling. No line is wider than the room.
    - [indent] (defaults to [0]) is the cells before the guides of every line
      but the first line of the root: a display's task tree is a
      [view ~indent:2], whose sub-tasks start under their parent's label.

    @raise Invalid_argument if [indent] is negative. *)
