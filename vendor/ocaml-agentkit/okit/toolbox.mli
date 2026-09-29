(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Ds4.Tool.t} values that build and read a dune workspace.

    Each of these is one call to the okitd a {!Client} holds, which runs the
    [dune], the [ocamlmerlin] and the shell for this workspace. Nothing here
    spawns a program, so a process that has loaded a model forks nothing by
    using these tools.

    okitd is the caller's own child, spawned with the caller's authority, so
    granting one of these grants the right to run that program in the workspace.
    Granting a build grants more than that: a build rule runs whatever command
    the workspace's own dune files name, so a model that can build is trusted
    with whatever those files do. That is more authority than the filesystem
    tools of {!Ds4.Toolbox}, which reach only inside a capability. {!bash}
    grants the most, since a shell runs anything at all: it is
    {!Ds4.Toolbox.bash} with okitd running the shell instead of this process,
    granted or withheld exactly as that one is.

    The tools that name a file take a capability and a path under it, as the
    filesystem tools do: the capability is named as {!Ds4.Toolbox.open_dir}
    returns it, and an empty name is the starting one. okitd reads no file of
    the workspace, so a write saves and a merlin query loads through that
    capability here, and the text is sent on. What a tool returns is bounded,
    and it says where it stopped.

    Every trace a call shows is okitd's, streamed while the call runs and passed
    to the sink {!Client.start} was given, along with the session's own lines.
    Most operations name themselves and their main argument first, such as
    [write: lib/x.ml], so that a call which never returns still says which call
    it was. {!project} is the exception: it names the [dune describe] it runs
    rather than itself.

    A call is bounded: five minutes for a build, a test run and a shell command,
    which are the workspace's own work, and one minute for the rest, which are
    one exchange with a dune that is already running or one merlin. Exceeding
    the bound kills okitd and ends the session, so the bounds are set well above
    what a working machine takes and are there to catch a wedge.

    A session that has died answers every later call with what went wrong and
    the tail of okitd's standard error, and that text is the tool's result. A
    model reading it is told the server is gone, rather than left with an empty
    answer and no reason for it. *)

val build : client:Client.t -> Ds4.Tool.t
(** [build ~client] returns a [build] tool, which builds targets of the
    workspace and reports the errors and warnings the server then holds. A
    target is a path such as ["lib"] or ["src/foo.exe"], or an alias in dune's
    command-line spelling such as ["@check"]. Leaving them out builds the whole
    workspace. *)

val write : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [write ~client ~caps] returns a [write] tool, which saves a whole file
    through a capability and, when the file is one dune compiles, builds the
    workspace and answers with what the build says about that file. It replaces
    {!Ds4.Toolbox.write} for an agent working in a workspace, since it spares
    the model a build call after every change. Use {!edit} to change part of a
    file.

    The save is done here and the build is asked of okitd, so a file written
    outside any capability is refused before okitd hears of it. *)

val edit : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [edit ~client ~caps] returns an [edit] tool, which replaces one passage of a
    file through a capability and then reports what a build says about it, as
    {!write} does. It replaces {!Ds4.Toolbox.edit} for an agent working in a
    workspace.

    The substitution is {!Ds4.Toolbox.substitute}'s, so a passage that names no
    place or several is refused in the same words whichever toolbox the model
    holds. Nothing is saved and nothing is built when it is.

    This is the tool for changing code, and {!write} is for creating a file or
    replacing the whole of one. A model that writes whole files spends its
    context re-sending the lines it is not changing, and the resend is where a
    line it did not mean to touch is lost. *)

val test : client:Client.t -> Ds4.Tool.t
(** [test ~client] returns a [test] tool, which runs the workspace's tests and
    reports what they say. A test whose output differs from what is recorded
    reports the file {!promote} would accept. *)

val promote : client:Client.t -> Ds4.Tool.t
(** [promote ~client] returns a [promote] tool, which accepts the built file for
    a source path that a diagnostic offered to promote. *)

val project : client:Client.t -> Ds4.Tool.t
(** [project ~client] returns a [project] tool, which reports what the workspace
    is made of: each library and executable, the directory it is built from,
    what it requires and the modules it holds. Named a module, it reports the
    one component that holds it, which answers what that module may refer to
    without the whole map and the bound the whole map is cut at.

    okitd describes the workspace afresh on every call. The describe spawns
    dune, which okitd is small enough to fork whatever the caller has since
    loaded, so a workspace that has changed under a session is reported as it
    is. *)

val outline : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [outline ~client ~caps] returns an [outline] tool, which lists what a file
    declares, one line of [kind name : type] per declaration, indented by how
    deeply it is nested.

    An implementation with an interface beside it is answered with a line naming
    that interface first. The shape of a module is what the interface states,
    and a model that reads the implementation for it spends several times the
    context on the same answer. *)

val type_at : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [type_at ~client ~caps] returns a [type_at] tool, which reports the type of
    the smallest expression at a line and column of a file. *)

val locate : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [locate ~client ~caps] returns a [locate] tool, which reports where the name
    at a line and column is defined, as [file:line:column], and the definition
    found there. The file is written from the starting capability's directory
    when the definition lies under it, and absolute when it is elsewhere, such
    as in an installed library.

    The definition is read through the starting capability, so only one under it
    is shown. One elsewhere is answered with its path and what to do to reach
    it, since a capability is what bounds this and okitd, which holds the whole
    of this process's authority, is not asked to read around it.

    What is shown is the line the definition starts on and the lines below it
    indented further, which is one whole declaration at any level of nesting. It
    is bounded, and says so where it stops. *)

val errors : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [errors ~client ~caps] returns an [errors] tool, which reports what merlin
    makes of one file: each error and warning with the line and column it starts
    at. It type-checks against what the workspace has already built, so it is
    the state of one file rather than of the workspace, and it is the tool for a
    file changed by some means that did not build. *)

val occurrences : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [occurrences ~client ~caps] returns an [occurrences] tool, which lists every
    use of the name at a line and column, across the workspace, as
    [file:line:column]. It is the tool for finding where an identifier is used:
    it knows the identifier rather than the text, so it finds none of what a
    search for the same letters would turn up in comments and unrelated names.
    The definition itself is one of the uses reported.

    okitd builds the workspace's [@ocaml-index] alias before it asks, since
    merlin reads the uses outside the one file from that index and answers with
    the file alone where it is missing. A build that did not reach the alias is
    said so above the answer, rather than left to make a partial answer look
    whole. The call is bounded as a build is, not as a query. *)

val search : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [search ~client ~caps] returns a [search] tool, which finds values by their
    type across the workspace and every library it may refer to. The file it is
    given supplies the configuration to search in and nothing else. *)

val complete : client:Client.t -> caps:Ds4.Toolbox.Caps.t -> Ds4.Tool.t
(** [complete ~client ~caps] returns a [complete] tool, which lists what can be
    named at a position, filtered by a prefix, with the kind and type of each as
    {!outline} writes a declaration. A qualified prefix such as ["List."] is how
    to read what an installed library offers, which has no file in the workspace
    to outline. *)

val bash : client:Client.t -> Ds4.Tool.t
(** [bash ~client] returns a [bash] tool, which runs a shell command line and
    returns what it wrote to standard output. It is {!Ds4.Toolbox.bash} with the
    shell run by okitd, and it keeps that tool's name, argument and description,
    so a model cannot tell the two apart.

    A shell beside a loaded model costs what any other fork does, which is why
    it runs there. The authority is unchanged: okitd is the caller's child and
    holds the caller's authority, so granting this tool grants what granting the
    plain one granted.

    The command's standard error is left to okitd's, which the client keeps the
    tail of and shows when okitd dies. *)
