(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The text okit's operations answer with.

    A build, a write and an outline are run by {!Server}, and what they answer
    with is written here rather than there. Splitting the rendering from the
    loop that dispatches keeps it testable on its own, and leaves one place to
    read for what a model is told.

    Everything here is a pure function of what dune or merlin returned. *)

val to_text : Dune_rpc.Private.Diagnostic.t -> string
(** [to_text d] is [d] as dune prints it, ending in a newline: the [File "…"]
    header when [d] has a location, the severity when the message does not
    already open with it, the message, then one line per promotion. *)

val targets : string -> string list
(** [targets s] is the list of targets in [s], which the build tool takes as one
    string separated by blanks. An empty [s] is ["."], the whole workspace. *)

val build : what:string -> Session.build -> string
(** [build ~what r] is the diagnostics of [r], bounded and followed by the count
    of those left out, and then the line [what ^ " ok"] or [what ^ " failed"].
    [what] is the word the outcome is stated in, so that a run of the tests does
    not report itself as a build. *)

val built : string -> bool
(** [built path] is whether dune compiles [path], and so whether a build after
    writing it has anything to say about it. *)

val after_write : path:string -> Session.build -> string
(** [after_write ~path r] is what a write of [path] reports about that file: its
    own diagnostics, bounded more tightly than {!val-build} bounds them, then
    the count of the ones elsewhere in the workspace, and the outcome of the
    build when [path] itself drew nothing.

    A model that has just written one file is reading about that file, so the
    rest of the workspace is counted rather than shown. The build tool shows
    them. *)

val outline : Merlin.outline_item list -> string
(** [outline items] is one line per declaration, [kind name : type], indented by
    how deeply it is nested.

    It is bounded, by a count of declarations and by the text one tool result
    carries, and one that stopped early says how many of the file's declarations
    it holds and that the file itself is where the rest are. *)

val problems : Merlin.problem list -> string
(** [problems ps] is one entry per error and warning, opening with [line:column]
    and the word [error] or [warning], and indenting the lines of a message that
    runs to several. It is bounded and counts what it left out. A file merlin
    objects to nothing in says so. *)

val completions : Merlin.completion list -> string
(** [completions cs] is one line per completion, [kind name : type] as
    {!outline} writes a declaration, with a type merlin wrapped across several
    lines put back on one. It is bounded, and says where it stopped. *)

val trim_slash : string -> string
(** [trim_slash s] is [s] without the trailing separator a directory is
    sometimes written with, so that two names for one directory compare equal.
*)

val roots : string -> string list
(** [roots dir] is the names by which [dir] may be written: the one given and,
    when it differs, the one the links in it resolve to. On macOS [/tmp] is a
    link to [/private/tmp], which dune and merlin resolve where Eio does not, so
    a file under the workspace is answered for under either name. *)

val under : roots:string list -> string -> string
(** [under ~roots file] is [file] written from whichever of [roots] contains it,
    and [file] unchanged when none does. An absolute path inside the workspace
    sends a model looking outside it, and one truly outside, such as a file of
    an installed library, is left as it is. *)

val occurrences : roots:string list -> Merlin.place list -> string
(** [occurrences ~roots places] is one [file:line:column] per use, each file
    written {!under} [roots]. It is bounded and counts what it left out. A
    position holding no name says so, rather than answering with nothing. *)

val search : roots:string list -> Merlin.hit list -> string
(** [search ~roots hits] is one line per value, [name : type] and where it is
    declared, keeping the first of the repeats merlin answers with for a value
    reachable by more than one path. A query that matched nothing says so, and
    says how a query is written. *)
