(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Ready-made {!Tool.t} values.

    Each tool reaches only what it is given. The filesystem tools work through a
    capability held in {!Caps} and named by the tool call, so a model can narrow
    its own reach with {!open_dir} but never widen it, since a new capability is
    always taken from one already held. Every capability comes from
    {!Eio.Path.open_subtree}, which rejects [".."] and symlinks that leave it.

    {!bash} is the exception. A shell runs with the whole process's authority,
    so granting it gives up the sandbox. {!tree}, {!list}, {!read},
    {!read_lines}, {!find}, {!grep} and {!stat} let an agent explore a tree
    without it.

    Each tool limits how much it returns and says so when it truncates, so a
    broad query costs a bounded amount of work and of the model's context. The
    recursive tools do not descend into directories whose name starts with a
    dot, and do not follow symlinks. *)

(** The capabilities an agent holds, each under a name.

    Names come from {!Eio.Path.pp}, so a capability is called what Eio calls it.
    That is a label rather than a path, so a name is suffixed if it would
    collide with one already held. *)
module Caps : sig
  type t

  type refusal =
    | Unknown_parent of string  (** no capability of that name is held *)
    | Denied of string  (** access to that directory was not granted *)

  val create :
    sw:Eio.Switch.t ->
    fs:_ Eio.Path.t ->
    ?approve:(string -> bool) ->
    _ Eio.Path.t ->
    t
  (** [create ~sw ~fs dir] holds [dir] as the starting capability, the only one
      available until more are asked for. Capabilities minted later live until
      [sw] finishes.

      [fs] opens directories outside [dir], and [approve] decides whether to
      allow that. [approve] is given the directory's path and defaults to
      allowing everything. It is the point at which to ask a person. *)

  val root_name : t -> string
  (** [root_name t] is the name of the starting capability. An empty name in a
      tool call means this one, so a model can call a tool before it has been
      told any name. *)

  val names : t -> string list
  (** [names t] are the capabilities held, in the order they were minted. *)

  val find : t -> string -> Eio.Fs.dir_ty Eio.Path.t option
  (** [find t name] is the capability called [name], or the starting one if
      [name] is empty. *)

  val describe : t -> string
  (** [describe t] lists each held capability with the directory it covers. *)

  val mint : t -> parent:string -> path:string -> (string, refusal) result
  (** [mint t ~parent ~path] holds a new capability and returns its name.

      A relative [path] narrows the capability named [parent], which needs no
      permission because it is already reachable. An absolute [path] asks for a
      directory outside every capability held, and is granted only if [approve]
      allows it. A leading [~] stands for the home directory. *)
end

val open_dir : caps:Caps.t -> Tool.t
(** [open_dir ~caps] returns an [open_dir] tool, which confines a new capability
    to a directory and returns its name for later calls. *)

val caps : caps:Caps.t -> Tool.t
(** [caps ~caps] returns a [caps] tool, which lists the capabilities held. *)

val tree : caps:Caps.t -> Tool.t
(** [tree ~caps] returns a [tree] tool, which summarises a directory tree to a
    given depth in one call. It replaces the repeated {!list} calls an agent
    otherwise makes to find its way around. *)

val list : caps:Caps.t -> Tool.t
(** [list ~caps] returns a [list] tool, which reports the entries of a directory
    with their kind and size. *)

val page_bytes : int
(** [page_bytes] is how much of a file one result carries. It sits under the
    agent's own limit on a tool result, so a page arrives whole rather than with
    its middle removed. *)

val read : caps:Caps.t -> Tool.t
(** [read ~caps] returns a [read] tool, which returns a file's contents.

    A file longer than {!page_bytes} comes back as the whole lines that fit,
    followed by the range shown, the number of lines the file has and the line
    to continue from with {!read_lines}. Nothing is dropped without saying which
    lines they were, so the whole of a file of any length is reachable. *)

val read_lines : caps:Caps.t -> Tool.t
(** [read_lines ~caps] returns a [read_lines] tool, which returns a numbered
    range of lines from a file. A count of zero or less reads to the end. The
    file is streamed, so reading from the middle of a large file is no more
    costly than reading its start.

    A window is bounded by {!page_bytes} as well as by the count asked for, and
    an answer that did not reach the end of the file says which lines it holds
    and how many the file has. Where it was one of those bounds that ended the
    window rather than the count asked for, it also names the line to read from
    next. *)

val find : caps:Caps.t -> Tool.t
(** [find ~caps] returns a [find] tool, which reports entries under a directory
    whose path contains a substring. *)

val grep : caps:Caps.t -> Tool.t
(** [grep ~caps] returns a [grep] tool, which searches file contents for a
    literal string and reports matches as [path:line: text]. The string is not a
    regular expression. It matches comments and unrelated names alike, so it is
    for text that is not an OCaml identifier. Binary and very large files are
    skipped. *)

val stat : caps:Caps.t -> Tool.t
(** [stat ~caps] returns a [stat] tool, which reports a path's kind, size,
    modification time and permissions. *)

val view_image : caps:Caps.t -> Tool.t
(** [view_image ~caps] returns a [view_image] tool, which adds a PNG or JPEG
    file to the model's next observation. Files are limited to 64 MiB. *)

val write : caps:Caps.t -> Tool.t
(** [write ~caps] returns a [write] tool, which creates or overwrites a file. *)

val append : caps:Caps.t -> Tool.t
(** [append ~caps] returns an [append] tool, which adds to the end of a file and
    creates it where it is absent. It answers with the bytes added and the size
    the file has reached.

    It is how a file too long to send in one reply is written, a model's reply
    being bounded by [max_tokens]. Without it a file larger than one reply
    cannot be written at all, whatever the model does, since {!write} takes the
    whole of a file as one argument. *)

val edit : caps:Caps.t -> Tool.t
(** [edit ~caps] returns an [edit] tool, which replaces one passage of a file
    and leaves the rest of it alone. It is how to change part of a file without
    sending the whole of it, which is what {!write} costs.

    The passage must occur once. See {!substitute} for what a passage that
    occurs no times or several is answered with. *)

val substitute :
  path:string -> old:string -> new_:string -> string -> (string, string) result
(** [substitute ~path ~old ~new_ source] is [source] with its one occurrence of
    [old] replaced by [new_].

    [Error why] is returned where [old] is empty, absent, or present more than
    once, and [why] says which of those it was and what to do about it. A
    passage naming several places is refused rather than resolved, because
    changing the first of them and reporting success leaves the caller believing
    something that is not so. [path] appears in [why] and is not otherwise used.

    Both toolboxes edit with this, so a model is answered alike whichever of
    them holds its [edit]. *)

val dns : net:_ Eio.Net.t -> Tool.t
(** [dns ~net] returns a [dns] tool, which resolves a hostname to one address
    per line. Holding [net] is the capability to reach the network. *)

val bash : proc:_ Eio.Process.mgr -> Tool.t
(** [bash ~proc] returns a [bash] tool, which runs a shell command and returns
    its output. Holding [proc] is the capability to run any program, so this
    tool escapes the sandbox the others are confined to. *)
