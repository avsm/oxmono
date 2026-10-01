# Maintaining ocaml-agentkit

Notes for working on this repository. See `README.md` to use it.

## Layout

    agentkit/   common agent events, line framing, stores and journal renderer
    ds4_adapter/       the DS4 event and agent adapter
    apple_adapter/     the Apple Foundation Models agent and tool adapter
    apple_tools/       Apple-native codecs for the commands' tool handlers
    bin/model_support/  shared model management registration for the commands
    cmd/        humpty's expect scripts
    okit/       okitd, its client, the agent's dune and merlin tools, and the
                workspace toolset every command assembles from
    net/        numptyd, its client, and the agent's network tools
    daemon/     numpty's store, brief, wake-up, schedule loop and socket
    bin/        humpty and its terminal interface, numpty, dumpty, and the
                agentkit journal browser
    test/       tests, some gated on DS4_LIVE
    test/expect cram transcripts of the tools humpty expect assembles

The repository builds a backend-neutral `agentkit` package, the
`agentkit-ds4` and `agentkit-apple-fm` adapters, and three programs that select
DS4 or Apple Foundation Models at run time.
`agentkit` also contains the journal browser. `humpty` is the interactive
command, its interface and its OCaml tools, and depends on the external
`dune-rpc-eio` package. `numpty` is the headless agent. `dumpty` is the
one-shot agent, and depends on `humpty`, whose `humpty.okit` library holds the
toolset it assembles.

The engines, bindings and backend-native tools belong to `ds4` and
`apple-fm`. Agentkit owns only their common event contract and adapter policy.
`ds4.cli` supplies each DS4 command's backend name, guard, seed and logging
terms, and the `list`, `download` and `chat` subcommands humpty offers.

## Building

The backend packages must be installed, or built in the same Dune workspace.
Pinning the checkouts is the simplest:

    opam pin add ds4 ../ocaml-deepseek
    opam pin add apple-fm ../ocaml-apple-fm
    dune build

To build from source together, put a `dune-workspace` in a directory holding
the three checkouts and build from there. Each DS4 command is built once per
engine backend, as `ds4` links one: `humpty-metal`,
`humpty-cuda` and `humpty-cpu`, and likewise for `numpty` and `dumpty`. CUDA
is built only under `DS4_CUDA=yes`, which `ds4`'s own `ARCH.md` explains.
The Apple driver is linked into these commands on supported Macs. The private
`bin/apple_support` selection leaves Linux builds independent of `apple-fm`.

## agentkit

`agentkit` holds what more than one agent wants: the common event vocabulary,
the framing under the line protocols, and the three stores an agent that
outlives its context keeps, being the journal, the memory and the schedule.

    agentkit/instructions.ml  what a workspace leaves for an agent, AGENTS.md
    agentkit/trace.ml         the fold from agent events to journal kinds
    agentkit/show.ml          the one rendering of a journal record, which
                              every command that prints one prints through
    agentkit/utc.ml           reading back the times the journal writes

`Agentkit.Chat` is the other way to drive a model. A bot that keeps its own
history sends the whole transcript with each request and chooses its tools,
budget and reasoning per request. `Agentkit.Turn` runs one user turn over a
`Chat.complete`: tool calls pass a guard before dispatch, results are clipped,
and a spent allowance ends in one tool-free answer request whose instruction is
also the final user message. `Agentkit.Summary` is the tool-free request
compaction needs. Crow in `avsm/crowthebot` uses all three.

In the oxmono tree only `agentkit/`, `ds4_adapter/`, `openrouter_adapter/` and
`test_core/` build. The root `dune` file lists them. The other directories need
libraries the monorepo does not provide.

`Agentkit.Agent.S` is the small operation set a backend supplies. Construction
stays in the adapter because models, generation options, and tool codecs are
backend-specific. `Agentkit.Driver` selects a qualified model and calls that
adapter's constructor. The constructor assembles native tools and returns an
agent behind the common operation set. `Agentkit.Driver.Cli` builds the common
`models` command group. Selection never fetches weights. The separate fetch
operation belongs to a driver and runs only on explicit request.
`Agentkit.Trace` is why agents over
either adapter can keep one account. An adapter reports reply fragments and a
tool call without the result that answers it, and a `Journal.kind` is a whole
statement, so the fold
that buffers text to its boundary and pairs each result with the oldest pending
call is written once. It takes an `emit` and knows nothing about where the
kinds go.
numpty and dumpty both emit into `Journal.append`, and dumpty mirrors each
appended record to standard error, so its stream and its store are one
account. `Journal.stamp` remains for a record built where there is no store
to number one.

`agentkit-ds4` losslessly maps DS4 events and wraps the DS4 send, statistics,
cancel, and close operations. `agentkit-apple-fm` owns an Apple session. Its
tool wrapper re-encodes decoded arguments through the Apple codec before it
emits a call, so the journal contains canonical JSON rather than framework
wire detail. It emits the complete handler result before Apple encodes it as
an observation. Cancellation remains Apple's: it closes the cancelled session.

The Apple adapter can rebuild a session from its transcript. Compaction asks
the model for a summary with tool calling disabled where the operating system
supports that policy, retains recent complete turns that fit, creates a new
session, and only then closes the old one. A failed compaction restores the
pre-compaction transcript. Automatic compaction is opt-in because token
counting is unavailable on early macOS 26 releases.

Every agent keeps its journal under a directory of its own: humpty's sessions
under the `ds4` state directory, numpty's runs in its store, and dumpty's
one-shot runs under the `dumpty` state directory. Each command says where its
account went when it exits, and the `agentkit` binary, which holds no model,
reads them all: `list` for what exists, `runs` for one line per run, `log`
for the records, and `show` to lay one run out as the conversation it was.

## The terminal interface

`bin/tui.ml` is a Mosaic application: an immutable model, an update function
over messages, and a view. Agent events arrive as messages, so the interface
never reaches into the agent.

Eio is the outer loop and Mosaic runs inside it, which `matrix-eio` makes
possible by giving Mosaic an Eio-backed terminal. A Mosaic command performed
during an exchange becomes an Eio fiber rather than a thread, so the agent
keeps the capabilities its tools need while the interface goes on rendering.

Nothing outside the interface may print once it starts, because it owns the
terminal. The capability approval callback queues its grants for the interface
to show for exactly this reason, and okit's `?trace` callback queues the step it
is on in a second queue. Both are drained by every message `update` takes, the
tick among them, which is what keeps the trace moving while a tool call blocks.
A trace line is ephemeral column content and never reaches the transcript.

The okit session is started before the interface exists, and starting it is
where the waiting is. A queue alone would hold those lines until the wait was
over and the answer already known, so until the interface takes the terminal
each trace line also goes to standard error as it happens. `humpty` flips that
at the call to `Tui.run` and not before.
Pushing to either queue is safe because a Mosaic perform callback becomes an Eio
fiber on the interface's own domain, so a tool runs interleaved with `update`
rather than beside it.

A resize is only handled because the application subscribes to it. Mosaic's
`handle_resize` dispatches to the resize subscription and does nothing without
one, and `Mosaic.Sub.On_resize` reports a change but never the size at startup,
which `Tui.run` seeds from `Matrix.size`. The layout reads the width from the
model and drops what will not fit, since a row wider than its terminal is cut
wherever it happens to reach.

Rendering is differential, so a run redirected to a file gives the first frame
in full and then only the cells that changed. Reading that stream by eye is
misleading, because content clipped off the screen leaves no trace in it. Check
a layout by replaying the stream into a grid of the terminal's size and printing
the result, and drive the run through a `pty` with a chosen `TIOCSWINSZ`, since
the size comes from an ioctl and a pipe supplies none. Such a replay is only as
honest as the emulator behind it. One that ignores the alternate screen
composites the startup banner into every frame, and one that ignores `ECH` and
`DCH` leaves text on screen that the interface erased, both of which read as
faults in the interface. Give the child a controlling terminal with `setsid` and
`TIOCSCTTY` and check `tcgetpgrp` before trusting anything, because a pty with
no foreground process group swallows `SIGWINCH` and every size then looks
identically broken. Attributing a frame to a size is harder than it appears,
and a run started at one fixed size settles a question that a run resized
through several will not. A short screen is what
finds the faults: at sixteen rows the columns once pushed the prompt off the
bottom, which no comfortable size revealed.

Flex items default to a content-based minimum on both axes. Anything that must
give way, which is every scrolling box and every column holding one, needs
`min_size` pinned to zero. Left unset, a box grows past its parent instead of
scrolling inside it, and the overflow is invisible until the frame is
reconstructed. There is no automated test of the layout.

## The dune RPC client

The dune RPC client over Eio began here and is now the `dune-rpc-eio` package
in a repository of its own, which okit builds on through opam. Two of its
choices still matter to code here. `Client` is exported at
`Dune_rpc.Private.Client.S` rather than `Dune_rpc.V1.Client.S`, because `V1`
keeps `Request.t` abstract and `build` is not among the requests it declares,
so `okit/session.ml` redeclares `build` and offers it through
`connect_with_menu ~private_menu`, which is how dune's own `dune rpc build`
reaches it. And `Where` reads only the socket file under the build directory
it is given, so the library's filesystem reach stays one file however much
authority the capability carries.

The wire format is written down in
`docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md`. Nothing here encodes
it any more, so that document is a record of what dune sends rather than a
specification this repository implements.

## The dune session

`okit/session.ml` spawns or attaches to a server and holds it for a workspace.
It is okit's rather than upstream's, because it is policy rather than protocol.

A session either spawns `dune build --passive-watch-mode` in the workspace and
owns that child until the switch finishes, or attaches to a server already
serving the workspace and leaves it running when it stops. Passive means the
server builds nothing until it is asked, so a build result belongs to the tool
call that asked for it.

`Session.start` takes a `?client`, the id the handshake gives the server, and
okitd passes `okit` there. It offers `build` at version 2 alone, first served
by dune 3.24, and so refuses a workspace whose dune is older. Every other
method is one `dune-rpc` declares, and the handshake negotiates its version.

Every call runs under a mutex, so two fibers cannot have requests in flight at
once and `stop` waits for a call rather than closing under it. Nothing may
leave that critical section as an exception: one that does poisons the mutex,
after which every later call raises `Eio.Mutex.Poisoned` instead of saying what
went wrong. `guard` therefore ends in a catch-all, which is what holds against
`dune-rpc` raising a Stdune `Code_error` this repository cannot name.

The connection runs on a daemon fiber. A fiber parked until `stop` would keep
the switch from finishing, and `stop` runs from that switch's release.

A build target is a path relative to the workspace root, such as `.` or
`lib/foo.ml`, or an alias dep-spec, `(alias <path>)` for the alias in one
directory and `(alias_rec <path>)` for that directory and everything below it.
A session takes dune's CLI spelling, `@name` and `@@name`, and writes the
s-expression itself. A target given as a raw `(`-form is refused with the form
to write instead, because the server answers a malformed dep-spec with a bare
`Code_error` that explains nothing. The `test` tool keeps the separate `runtest`
method, which asks for the same alias by name.

The socket lives at `_build/.rpc/dune` under the workspace, and a unix socket
address is limited to about 100 characters, so no server can be reached in a
workspace whose path is longer than that. `start` reports the address error once
its wait for the socket runs out. A test that puts its workspace under dune's
own `TMPDIR`, which is deep inside `_build`, meets this rather than the code it
meant to exercise.

## The dune tools

`okit/` gives `humpty agent` the tools an OCaml workspace wants: `build`,
`test`, `promote`, `project`, an `edit` and a `write` that answer with what the
build then says about the file they changed, and from merlin `outline`,
`type_at`, `locate`, `occurrences`, `errors`, `search` and `complete`. `edit`
replaces one passage of a file and refuses a passage that names no place or
several, since editing the first of several look-alikes and reporting success
leaves the model believing something that is not so. Both save through the
capability humpty's own tools hold and ask okitd only for the build. They are
added when the
workspace has a `dune-project` and a session starts. A session that will not
start leaves the plain tools as they were and says why once, at warning level,
since an agent that cannot build is still an agent. The system prompt gains a
paragraph naming these tools only when they are there, because a model told
about a tool it does not have asks for it and then reaches for the shell.

The merlin tools answer about the OCaml rather than about the text, which is
what makes them worth their place beside `grep` and `read`. Each one is written
to end a line of enquiry rather than to open another: `locate` answers with the
definition and not only its address, `outline` of an implementation names the
interface beside it, and `project` named a module reports the one component that
holds it rather than the whole map cut at its bound. `occurrences` needs the
index dune writes for the `@ocaml-index` alias, and merlin answers with the one
file it was given where that index is missing, so okitd builds the alias before
it asks and says above the answer when the build did not reach it. `complete`
and `search` are the two that reach past the workspace, into the interfaces of
installed libraries, which have no file here to outline.

None of this runs in humpty. `humpty okitd --dir WS` is a child of the same
binary, spawned by the assembly before the engine is created, and it holds the
dune session, the merlin and every process okit runs. The two ends speak one
compact JSON object per line over its pipes: a `hello` saying which tool
families are live and carrying the note the interface shows, then a `call` and
its `result` for each operation, with `trace` lines streaming while a call runs.
A line that does not parse is a protocol fault rather than bad input, both ends
being the same build, and each reports the line and stops. okitd reads no file
of the workspace: source text a merlin query needs is read by humpty through the
capability its own tools hold and sent in the call, so the capability discipline
stays where it is. That holds for what an answer points at as well as for what a
query asks about. The definition `locate` shows is read on humpty's side, under
the same capability, and a definition in an installed library is named with what
to do to reach it rather than read by an okitd that holds the whole of humpty's
authority. okitd treats the end of its standard input as a shutdown, so
a humpty that is force-quit leaves no dune server behind. The design is
`docs/superpowers/specs/2026-08-02-okitd-design.md`.

    okit/proto.ml     the message codecs and the line framing
    okit/session.ml   spawn or attach, request, relaunch, stop
    okit/server.ml    okitd's loop: read a call, dispatch it, stream its traces
    okit/client.ml    spawn okitd, greet, call, bound, degrade
    okit/toolbox.ml   the Tool.t values, each one call to okitd
    okit/toolset.ml   the whole list a workspace agent gets, and its prompt
    okit/status.ml    the wording of the note, written once for both ends
    okit/report.ml    the wording of a result, written once for both ends

`okit/toolset.ml` is the list itself: the capability file tools, `dns`, and
whatever okitd's greeting says it can serve, with the system prompt fragments
that describe them. It takes the argv to spawn okitd with, since each command
spawns itself, and it is what `humpty agent`, `humpty expect` and `dumpty` all
assemble through, so a transcript is evidence about the list the interface is
given. Tool assembly itself stays per command: numpty's list is memory tools
and network tools and no shell, and lives in `bin/numpty.ml`. What is shared is
the discipline around the toolsets rather than the lists.

There is no respawn. Making another okitd would fork the process that holds the
model, which is what the split exists to avoid, so the first fault ends the
session for good: every later call answers at once with what went wrong and the
tail of okitd's standard error, and that text is the tool's result. A call is
bounded, five minutes for a build, a test or a shell command and one minute for
the rest, and exceeding the bound kills okitd and degrades the same way.
`occurrences` takes the build's bound rather than the query's, since it builds
the workspace's index before it asks.

These tools run three fixed binaries. `dune` and `ocamlmerlin` are run with the
workspace root as the working directory. `bash` is run in the directory okitd
inherited from humpty, which is where the plain tool ran it, so `--dir` does not
move it. It runs in okitd for the reason everything else does, and its authority
is unchanged: okitd is humpty's child with humpty's authority, so the tool is
granted or withheld exactly as the in-process one was. The dune and merlin tools
are more authority than the filesystem tools, which reach only inside a
capability, and less than `bash`, which runs anything. A build rule runs
whatever command the workspace's own dune files name, so granting `build` trusts
those files.

Nothing okitd runs inherits okitd's own standard input, which is the pipe the
protocol arrives on. Each child is given an empty one instead. A `bash` command
that reads, `cat` or a git that prompts, would otherwise take the next call for
its input and leave that call unanswered until humpty's bound ran out.

`dune describe`, which the `project` tool reads, runs under a `DUNE_BUILD_DIR`
of its own with `INSIDE_DUNE` dropped from its environment. Without that it
waits on the build lock the passive server holds. `INSIDE_DUNE` has to go with
it, because a describe run from inside a dune test would otherwise be sent back
to the build directory it was moved out of.

It runs in okitd, on every `project` call, so the map is the workspace as it is
rather than as it was. Spawning a program forks the whole process, and `humpty
agent` creates the engine only after the tools are assembled for that reason: a
fork of a process with a model mapped into it costs minutes on macOS and blocks
the domain that asked for it, which is the one running the interface. okitd is
the only process humpty spawns, and it is spawned while humpty is still small.
okitd holds nothing large, so the forks it makes cost what a fork used to cost.
A failure after that unwinds the switch, whose release stops okitd and the
server it started, so an engine that will not load leaves no dune behind.

Each tool names itself and its main argument on the trace before it does
anything, `write: lib/x.ml` before the build it causes, so a call that never
returns still says which call it was. `project` is the exception, naming the
`dune describe` it runs rather than itself. Those lines are okitd's, streamed as
the call runs, and they reach the same sink the session's own start-up lines do.

## numpty

`numpty` is a second command over the same engine. humpty holds a conversation
with a person at a terminal. numpty runs unattended for weeks: it wakes on a
schedule a person wrote, reaches the network through a child process, writes
what it learned into a durable memory, and records every step in an append-only
journal. It has no interface, and what it did is read out of its store
afterwards. The design is
`docs/superpowers/specs/2026-08-08-numpty-design.md`.

    agentkit/journal.ml   the append-only account and its codec
    agentkit/trace.ml     the fold from agent events to journal kinds
    agentkit/memory.ml    the versioned store of what survives a context
    agentkit/schedule.ml  the file a person writes, and the firing arithmetic
    daemon/store.ml       the directory a run owns, its lock and its recovery
    daemon/brief.ml       what a wake-up starts from
    daemon/wake.ml        one wake-up, its sessions and its handovers
    daemon/daemon.ml      the loop that waits for a task to be due
    daemon/control.ml     the socket a running numpty answers on
    daemon/status.ml      the snapshot that socket reads
    daemon/memory_tools.ml the four tools the agent changes memory through
    daemon/task.ml        the schedule editor behind `numpty task`
    daemon/history.ml     what the journal says each task last did
    daemon/show.ml        what `numpty log` and `numpty memory` print

Everything a run writes is under one root, `$XDG_STATE_HOME/numpty` by default
and `--store` otherwise: `journal/` in segments named by the UTC date,
`memory/` of immutable snapshots with the version in force named in `current`,
`workspace/` where the agent's file tools are rooted, `control` and `lock`. The
schedule is not there. It is `$XDG_CONFIG_HOME/numpty/schedule.json`. Models
stay under `ds4`, where humpty already has them, since a machine holds one copy
of a model this size whichever command loads it.

There is no counter file. `Store.open_` takes the lock and then recovers: the
next `seq` and `run` come from the last line of the newest journal segment, and
the memory version from `memory/current`. A counter kept beside the journal
could disagree with it after a crash between the two writes, and a number the
journal alone supplies cannot. The lock is `Unix.lockf`, OCaml's `Unix` not
exposing `flock`, with a table of the roots this process holds beside it,
because a record lock is held per process and would not refuse a second store
opened inside one. A lock file a crashed run left behind refuses nobody, and a
live run is refused with the holder's process id.

A record is appended and fsynced before the thing it describes is allowed to
proceed. A wake-up is minutes of work and an fsync is microseconds, so there is
no reason to leave a hole in the trace to save one. `Journal.append` raises
rather than reporting a failure as a value, and every caller lets that out:
`bin/numpty.ml` attempts a `run_stop` and re-raises whatever came of it. An
account with a hole in it is worse than no account, because it reads as
complete. Reading is forward compatible in one direction only. A kind this
build does not know decodes as `Unknown` carrying its raw JSON and is shown,
and a schema version above what the reader knows stops the read, since a record
whose common members may have moved cannot be shown honestly.

Memory is a set of entries, and a version is a complete snapshot of that set,
written once and never rewritten. A mutation writes in one order: the snapshot
is written and fsynced, the journal's `memory_write` is appended, and `current`
is moved last. `Memory.write` and `Memory.forget` take that append as a
callback rather than returning between the two steps, so a caller cannot get
the order wrong. The two crash windows are what `Memory.recover` settles at the
next startup. A snapshot no record names was never in force and is removed, one
a record does name and `current` is behind of is a version and is adopted, and
each is journalled as an `error`. Nothing prunes.

A wake-up creates a fresh `Agent.t` on the resident engine, so a new context
costs a prefill and not a model load. Its first and only work message is the
brief, assembled from the memory in force, the unfinished work, the task a
person scheduled, and a digest of the journal since the last handover. A
scheduled task and an open item are kept in separate sections of it. A
scheduled task is an instruction from a person, in a file numpty cannot write,
and an open item is numpty's own note, and running the two together would let
the agent edit its own orders. The wake-up then sends the handover prompt,
which asks what should survive, and the agent answers by writing memory through
its tools.

`Agent.send` runs a whole turn including its tool calls, so the turn boundary is
the only place the loop can act, and it is where the fault, the stop and the
context are all read. A session that has filled three quarters of
`max_ctx_size`, which `--handover-at` sets, is closed after its handover and a
fresh one starts on the same task from the new memory, with a `continued`
record linking the two, so a task spanning five sessions reads as one task in
the journal. Five is the bound, since a task that fills its context every time
would otherwise hold the one engine for ever while the other tasks wait.
`Squeezed` is the failure this avoids and is journalled if it is ever reached.

The file instructs and the socket reports. `numpty task` parses the schedule,
applies the change, validates the result and renames a complete file over the
old one, running as the person in a process that holds no model. The daemon
stats that file on every tick and rereads it when it changes, and never writes
it. The agent reaches neither: its tools reach the journal and the memory store
alone, there is no `bash` since a program is run through numptyd, and the
capability approval refuses every directory outside the workspace because an
unattended agent has nobody to ask. Nothing in the schedule file is state.
Whether a task has fired, whether a `once` task is done and whether a `run_now`
serial has been honoured are read out of the journal at startup, so a firing is
idempotent under a reread and survives a restart with nothing to transfer.

The control socket answers only what the store cannot, being which job is
running, how far into its context it is, how far a prefill has got, which tool
call is in flight, when each task fires next and whether numptyd is alive.
Everything else is on disk before the daemon proceeds past it, so `numpty log`
and `numpty memory` read the files and need no daemon. The constraint that
shapes it is that `Agent.send` blocks for minutes and `Agent.stats` waits for
the engine, so the control fiber must never ask the agent anything. The wake-up
publishes a whole immutable `Status.job` at every state change and the control
fiber reads that record. `Agent.prefill_progress` is the one live call it may
make, because it reads two atomics the engine's progress hook writes and does
not wait for the engine. So a status is as fresh as the last turn boundary,
apart from the prefill counter.

A numptyd fault ends the run. The call that met it answers with what went
wrong, the turn finishes on that answer, the handover is taken, `run_stop`
names the fault, and numpty exits nonzero. There is no respawn for the reason
okitd has none, and a fresh numptyd has to be made before an engine is loaded,
which is a supervisor's restart. A run without a supervisor stays down until a
person returns, which is what the nonzero exit is for.

Two known limits. The control socket is bound after the engine has loaded, so a
`numpty status` asked during a model load reports nothing listening. A store
path that pushes `control` past the hundred characters a unix address allows is
reported at warning level once and the daemon runs without a socket, since an
agent that cannot be queried is still an agent.

## numptyd

`numpty netd` is a subcommand of the same binary, documented as internal,
spawned by the daemon before `V4.create` and held for the life of the run. It
holds every program numpty runs, so a numpty that has loaded a model forks
nothing. The split is okitd's and so is most of what follows.

    net/proto.ml   the message codecs, over the framing in `Agentkit.Line`
    net/server.ml  numptyd's loop: read a call, run it, stream its traces
    net/client.ml  spawn numptyd, greet, call, bound, fault
    net/tools.ml   the `Tool.t` values, each one call to numptyd
    net/report.ml  the wording of a result, written once for both ends

Three ops. `fetch` runs curl with `--silent --show-error --location`, a
`--max-time` of 45 seconds and a `--max-filesize`, and reports the status code,
the final URL after redirects, the content type, the byte count and then the
body. A non-2xx is reported as a non-2xx with the body after it, since a 404
page returned as though it were the article is the failure an agent is least
able to detect for itself. `head` is `fetch` without a body. `run` runs a named
program with arguments, which is the authority `bash` has in okitd. `dns` stays
in `Toolbox`, since it resolves through an Eio net capability and forks
nothing.

The URL is passed as `--url`, so one beginning with a dash is a URL and not an
option, and the metadata comes from a `--write-out` report curl writes after the
body behind a delimiter, split at that delimiter's last occurrence, since a body
may contain anything the delimiter included. A body is bounded at one mebibyte
by default and two at most, which is an eighth of `Agentkit.Line.max_line`,
because a body goes into one line of JSON and escaping grows it. A fetch over
the bound is refused with the size it would have been rather than cut down into
something that reads as the whole document.

There is no respawn, for the reason okitd has none. A call is bounded, a minute
for a fetch or a head and five for a run, and exceeding the bound kills numptyd.
From the first fault onwards every call answers at once with what went wrong and
the tail of numptyd's standard error, and `Client.fault` is how the daemon sees
that the run is over. A stop the daemon asked for is not a fault.

Nothing numptyd spawns inherits its standard input, which is the pipe the
protocol arrives on. Each child is given an empty one, so a curl that would
prompt for a password does not take the next call for its input. EOF on that
input is a shutdown, so a force-quit numpty leaves nothing behind. numptyd
catches no signal, unlike okitd, which catches `SIGTERM` to stop the dune server
it started. numptyd starts nothing that outlives it and writes no file of
numpty's: a fetched body goes back over the pipe and numpty writes it, if it
writes it at all, through the capability its own tools hold.

## dumpty

`dumpty` is a third command over the same engine, and the whole of it is
`bin/dumpty.ml`. humpty holds a conversation, numpty runs for weeks, and dumpty
does one job and exits, which is what a script or a build step wants. The
design is `docs/superpowers/specs/2026-08-09-dumpty-design.md`.

The stream contract is the interface. Standard output carries exactly one line,
written when the exchange ends and nothing before it: the reply of the final
turn, the turn and tool-call counts, the context used and its size, whether
the reply was squeezed, and the directory the run's account was appended to. A
run that failed prints nothing there and exits nonzero, so an empty standard
output and a nonzero status is the whole failure contract and a consumer never
has to decide whether a partial object is a whole one. Standard error carries
the journal, the same records numpty appends to its store and under the same
codec, each line flushed as it is written. The records are also appended to a
journal of dumpty's own under the `dumpty` state directory, with `run` and
`seq` recovered from it, and one append stamps both, so the stream and the
store are byte for byte the same account and a run whose standard error nobody
kept can still be read back. There is no `wake`, `brief` or `memory_write`,
there being no schedule and no memory. Diagnostics
reach standard error too, standard output being spoken for, so okit's progress
is logged at info level and the note that okit's tools are absent is journalled
as an `error` rather than logged: at the default verbosity the stream is
records and nothing else.

Every failure after the `run_start` appends a `run_stop` naming the reason,
through the same wrapper `bin/numpty.ml` uses, since a stream whose last record
is the start reads as a run still going. The model is checked before that first
record rather than left to the engine, which reports a model it cannot open and
then calls `exit`, where a run is already under way and no `run_stop` could be
appended.

dumpty grants a capability outside the workspace where numpty refuses one. A
person invoked this run deliberately and the grant is on the record, the
`open_dir` call and its result being journal records like every other call,
whereas an unattended numpty has nobody to ask. The engine runs on a domain of
its own so that the fiber bounding the exchange keeps running while a turn is
blocked in generation, and expiry leaves the process rather than unwinding,
which would wait on the work that is already too slow. okitd is spawned before
the engine exists and is `dumpty okitd`, an internal subcommand identical to
humpty's, since `Okit.Toolset.assemble` takes the argv and every command spawns
itself.

## Tests

    dune runtest                          # unit tests
    DS4_LIVE=1 dune runtest --force -j1   # adds tests that load a real model

The live tests need a downloaded model and several minutes. `live_okit_fork`
is the end-to-end pin of the okitd split: it starts okitd, creates the engine,
and then times a fork and exec made directly against a build through okitd. It
prints both durations and fails only above 120 seconds. `live_expect_prompts`
drives `humpty expect` prompt lines against a real model, and is registered for
the Metal build alone, because the spawned humpty is what loads the model.

`humpty expect` drives the tools `humpty agent` assembles from a script, and a
script's prompt lines drive the model itself through the same agent loop. It
prints a transcript that is stable enough to record in a test. Both commands
build their tool list in the one function in `bin/humpty.ml` that does it, so a
transcript is evidence about the list the interface is given.
The script reader and the scrubber that makes a transcript repeatable live in
`cmd/`, apart from the subcommand, so that `expect_unit` can test them without a
workspace or a process.

`test/expect` holds those transcripts as cram tests, which `dune runtest` runs
and which load no model. Each scaffolds its fixture workspace under `/tmp`
rather than in the cram sandbox, whose path is longer than a unix socket address
may be, and runs `humpty-cpu expect` against it. Every transcript that reaches
an active okit says whether an `ocamlmerlin` answered for the workspace, so
those tests carry `(enabled_if %{bin-available:ocamlmerlin})` and are skipped
where none is installed. Only a change to a command in a `.t` file re-runs it,
since dune's cram rule depends on the commands it extracts rather than on the
whole file.

`okit_dune`, `okit_server`, `okit_client` and `okit_tools` spawn a real dune
server on a workspace they make under `/tmp`, so they need no model and a few
seconds. The merlin queries are skipped when no `ocamlmerlin` answers on PATH.
The last three drive the `humpty-cpu` this build produced, passed to them on the
command line by their dune rule: `okit_server` runs `okitd` over pipes of its
own, `okit_client` runs it and also runs shell stand-ins for the faults a real
okitd is written not to have, and `okit_tools` drives the `Tool.t` values
through their JSON schemas against it. What a stand-in is for is that a death, a
protocol fault and a timeout must each be answered promptly, finally and with
the tail of okitd's standard error, which cannot be staged with a working peer.

`agentkit_journal`, `agentkit_memory` and `agentkit_schedule` cover the three
durable stores on a real temporary directory and start no process. The
journal's properties are that an unknown kind survives a read and a write with
its JSON intact, that a schema version above what this build reads stops the
read, that an append which cannot be written raises, and that sequence numbers
are gapless over a midnight segment roll and over a restart. The memory's are
that a version is never rewritten, that a forgotten entry is still there at
every version it was in, and that both crash windows of a mutation are settled
by recovery, which the test stages by making the journal callback raise. The
schedule's are that a file which does not parse comes back as an error rather
than as an empty schedule, that a wall clock time a daylight saving change skips
still fires exactly once that day, checked against a zone the test builds rather
than the machine's, and that missed fires do not queue.

`numpty_proto` pins numptyd's wire, round trips and protocol faults alike, over
a buffer sink and a string reader. `numpty_netd` and `numpty_client` drive the
`numpty-cpu` this build produced, passed to them on the command line as
`okit_server`'s is. The first runs a real `netd` over pipes against a server it
stands up on the loopback, so that a 404 reads as a 404, a body over the bound
is refused with the size it would have been, and a program numptyd runs is not
handed the pipe the protocol arrives on. The second adds shell stand-ins for the
death, the protocol fault and the timeout a real numptyd is written not to have.
Neither reaches the internet.

`agentkit_browse` drives the `agentkit` binary this build produced against a
journal the test writes with the same writer the agents use. Its properties
are that every run is listed, that a filter naming no known kind is refused
rather than matching nothing, that a shown run carries its prompt, its tool
traffic and how it stopped, and that a run the journal does not hold is an
error naming where the runs are listed.

`numpty_store` covers the lock, what opening recovers from a crash, and that
each memory tool mints exactly one version and appends exactly one record naming
both versions, the entry and the reason. `numpty_brief` covers the brief, which
is pure, and the digest it draws from the journal. `numpty_task` covers the
schedule editor on a real temporary file, and `numpty_show` what `numpty log`
and `numpty memory` print against a store written with the writers a run uses.
`numpty_control` is the one to read first: it binds a real unix socket in a
temporary store and checks that a status is answered while a turn is blocked,
the turn stood in for by a fiber on a promise nobody resolves, which is what a
four minute prefill looks like from outside. It also covers the socket file a
crashed run left, a store path too long for a unix address, and a client that
goes away mid-answer.

`live_numpty_wake` is numpty's end-to-end pin under `DS4_LIVE`. It drives
`numpty once` against a page it serves on the loopback and checks that the
journal holds every step, from the `run_start` through the wake, the brief, the
prompt, the fetch and its result, to the handover and the `run_stop`, and that
the memory version advanced by exactly the number of `memory_write` records. A
version that moved without a record, or a record with no version behind it, is
the two stores disagreeing, which is what the write order exists to prevent.

`dumpty_cli` drives the `dumpty-cpu` this build produced against the failure
contract, which is the property that matters: a model that names nothing exits
nonzero, writes nothing at all on standard output, and says on standard error
which model was not there. It checks the two arguments that are refused before
any of that as well, a negative `--timeout` and a standard input larger than
the prompt limit. `live_dumpty` is the other half under `DS4_LIVE`,
registered for the Metal build alone as `live_expect_prompts` is, because the
spawned dumpty is what loads the model. It runs one exchange in a scratch
workspace holding a file the model has not seen and checks the whole contract:
exit zero, one line on standard output that is one JSON object with a reply
naming the token, and a standard error whose every line `Journal.of_string`
accepts, numbered from 1 without a gap, starting at `run_start` and ending at
`run_stop` with the prompt and the turn statistics between. It then runs the
same exchange under a bound of a second, where the contract is the other one:
nothing on standard output, a nonzero status, and a last record naming the
bound that was reached.

Run one model at a time. A model of this size will not load twice at once.

## Known limits

An okitd that has died is not replaced. A respawn would fork the process that
holds the model, which is the cost the split exists to avoid, so the session's
tools answer with the death from then on and the agent works without them for
the rest of the run. Restarting humpty is the way back. Inside okitd a relaunch
is nothing to avoid, since okitd holds nothing large, which is why a dune server
that dies under it is still relaunched from the call that met it.

okitd catches `SIGTERM`, which is what a client sends when it gives up on one,
and stops its dune session before it goes. The `SIGKILL` two seconds later is
not catchable, so an okitd that does not stop within that grace still leaves the
dune server it started holding the workspace's build lock. Nothing collects such
a server but the person who finds it.

A humpty whose okitd never started keeps the plain file tools and no shell. The
model-facing toolset cannot bypass its capability boundary. The low-level Okit
shell RPC remains available to explicit library clients.
