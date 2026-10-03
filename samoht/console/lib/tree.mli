(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tree rendering with customizable guides.

    Renders tree structures with ASCII or Unicode guide lines. *)

(** {1:types Types} *)

(** A generic tree node. *)
type 'a node = Node of 'a * 'a node list

type t
(** A renderable tree of spans. *)

(** {1:construction Construction} *)

val of_tree : ?theme:Theme.t -> ?guide:Guide.t -> Span.t node -> t
(** [of_tree tree] is a renderable tree from a tree of spans. The branch glyphs
    come from the guide argument if given, else from {!Theme.guide}, else
    {!Guide.unicode}. *)

val v :
  ?theme:Theme.t ->
  ?guide:Guide.t ->
  (('a -> Span.t node) -> 'a -> Span.t node) ->
  'a ->
  t
(** [v f root] is a tree built by recursively applying [f]; the branch glyphs
    are resolved as in {!of_tree}.

    The function [f] receives a render function and the current node, and should
    return a tree of spans.

    Example:
    {[
    type dir = { name : string; children : dir list }

    let root_dir =
      {
        name = "root";
        children =
          [
            { name = "bin"; children = [] };
            { name = "etc"; children = [ { name = "hosts"; children = [] } ] };
          ];
      }

    let tree =
      Console.Tree.v
        (fun render (dir : dir) ->
          Console.Tree.Node
            (Console.Span.text dir.name, List.map render dir.children))
        root_dir

    let () = assert (String.length (Console.Tree.to_string tree) > 0)
    ]} *)

(** {1:guides Guides} *)

val prefix : ?guide:Guide.t -> bool list -> string
(** [prefix ?guide lasts] is the guide prefix drawn before a node, where [lasts]
    is the is-last-among-siblings flag at each level from a top-level node down
    to and including this one. Each strict ancestor adds a vertical continuation
    ({!Guide.pipe}) or blank ({!Guide.space}) depending on whether it was the
    last of its siblings, then the node itself adds the {!Guide.branch} or
    {!Guide.last} connector. The empty list is the root and has no prefix. So
    under {!Guide.unicode} the first child of a non-last parent gives
    ["│   ├── "]. A live renderer that draws its own rows (a progress tree)
    shares the guide drawing through it. *)

(** {1:rendering Rendering} *)

val pp : t Fmt.t
(** [pp] pretty-prints the tree, one node per line, with guide lines. The
    formatter's margin caps every row: a label wider than the room its guide
    leaves wraps at its words ({!Span.wrap}), each continuation hanging under
    the text after the label's first word ({!Span.hanging}) beside the guide of
    its level. *)

val to_string : t -> string
(** [to_string tree] renders as plain text at its natural width. *)

val to_ansi_string : t -> string
(** [to_ansi_string tree] renders with ANSI styling. *)

(** {1:animation Animation} *)

val anim : t -> string Anim.t
(** [anim tree] is [tree] as a still {!Anim.t}: every frame is {!to_ansi_string}
    [tree]. A tree has no border, so there is no rain variant. *)
