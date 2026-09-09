(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire <thomas@gazagnaire.org>. All rights
  reserved. SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zipper over {!Value.t} with TOML-style dotted-key pointers.

    A cursor is a focus on a sub-value of a {!Value.t} together with enough
    context to walk back up, rebuild the document, and report a structural
    {!Loc.Path.t}.

    The cursor keeps the {!Loc.Meta.t} of every parent container it descends
    into, and gives what {!set} or {!modify} puts in the place of what it
    replaced. So an edit is an edit: written back with
    [Toml.to_string ~preserve:true], the document is its own bytes everywhere
    except the leaf that changed. *)

type t
(** A cursor: a focused {!Value.t} plus the context path back to the root. *)

val pp : t Fmt.t
(** Prints a cursor's TOML dotted-key pointer. *)

val root : Value.t -> t
(** [root v] is a cursor at the root of [v]. *)

val focus : t -> Value.t
(** [focus c] is the sub-value the cursor currently points at. *)

val down_field : string -> t -> t option
(** [down_field name c] descends into the entry [name] of the focused table.
    [None] if the focus is not a {!Value.Table} or if [name] is absent. *)

val down_index : int -> t -> t option
(** [down_index n c] descends into the [n]th element (0-based) of the focused
    array. [None] if the focus is not a {!Value.Array} or if [n] is out of
    range. *)

val up : t -> t option
(** [up c] moves one step towards the root, rebuilding the parent with the
    (possibly-edited) focus in place. [None] at the root. *)

val set : Value.t -> t -> t
(** [set v c] replaces the focused sub-value with [v]. The ancestor stack stays
    unchanged; call {!top} to materialise the updated tree. Parent containers
    keep their {!Loc.Meta.t} unchanged, and [v] takes the location of what it
    replaced without its source text: it stands over those bytes and is written
    in their place rather than instead of them. A [v] that carried a location of
    its own loses it, because where a value came from says nothing about where
    it now is. *)

val modify : (Value.t -> Value.t) -> t -> t
(** [modify f c] is [set (f (focus c)) c]. *)

val top : t -> Value.t
(** [top c] rebuilds and returns the full document with any edits applied. *)

val path : t -> Loc.Path.t
(** [path c] is the structural path from the root to the focus, as a
    {!Loc.Path.t} built from {!Loc.Path.Mem} (table keys) and {!Loc.Path.Nth}
    (array indices). Suitable for rendering into error messages. *)

(** {1 Dotted-key pointers}

    TOML writes structural paths using
    {{:https://toml.io/en/v1.1.0#keys}dotted keys} ([a.b.c]), plus the
    conventional bracketed index suffix for array indexing (0-based here; arrays
    are not TOML-native path elements, but the suffix is the standard convention
    used by [jq]-like tools and is unambiguous against dotted keys).

    Supported syntax:

    - [/] (or the empty string): the root.
    - [a.b.c]: chained table lookups.
    - [a[0]]: index 0 of the array bound to [a].
    - [a.b[2].c]: mix of tables and arrays (for array-of-tables, the indexed
      step selects one of the tables, and subsequent dotted steps descend into
      it).
    - Quoted keys: a segment enclosed in double quotes names a single literal
      key (so a pointer whose body is a four-character segment made of the
      characters double-quote, [a], dot, [b], double-quote names the single
      literal key formed by [a], dot, [b], not the path from [a] to [b]). Quoted
      keys follow the same escape rules as TOML 1.1 quoted keys
      (backslash-quote, backslash-backslash, [\\n], [\\r], [\\t], [\\b], [\\f],
      [\\uXXXX], [\\UXXXXXXXX]). See
      {{:https://toml.io/en/v1.1.0#keys}TOML 1.1 Keys}.

    Rejected:

    - Empty segments ([a..b], [.a], [a.]).
    - Unclosed index suffixes.
    - Unterminated quoted keys. *)

val of_pointer : string -> t -> t option
(** [of_pointer p c] navigates from [c] according to dotted-key pointer [p].
    Returns [None] on navigation failure (missing key, wrong sort, index out of
    range) or on malformed pointer syntax. *)

val to_pointer : t -> string
(** [to_pointer c] is the dotted-key pointer string for [c]'s current position,
    suitable for feeding back into {!of_pointer}. Keys containing characters
    outside the bare-key alphabet are quoted and escaped per TOML 1.1. A root
    cursor serialises to the single-slash string [/]. *)
