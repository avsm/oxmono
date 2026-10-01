# numpty: a headless agent that keeps working

numpty is a second command over the same engine. Where humpty holds a
conversation with a person at a terminal, numpty runs unattended for weeks: it
wakes on a schedule, reaches the network, writes what it learned into a
persistent memory, and records every step in an append-only journal. It has no
interface. What it did is read out of its store afterwards.

The parts humpty would also want, being the memory, the journal and the
schedule, move into a library both link. numpty itself is the daemon, its
network child, and the tools that child holds.

## Why a second command

Three things humpty does not need, and would be worse for carrying.

An agent that runs for weeks cannot keep one conversation. The context does not
compact, it grows to a ceiling and then squeezes, and a squeezed turn cuts a
reply short. So numpty does not keep a conversation at all. Each wake-up builds
a fresh context from a durable memory, works, writes back what should survive,
and discards the context. Memory is the thing that lasts, and the context is
scratch.

An agent nobody is watching must leave evidence. Every prompt, every reply,
every tool call and every memory version is a record in a journal that is
written before the next thing happens. A person arriving a week later
reconstructs what happened from that file alone.

An agent that reaches the network spawns programs. Forking a process that holds
a hundred-gigabyte engine costs minutes on macOS, which is why okitd exists.
numpty takes the same split for the same reason.

## Packages and layout

    agentkit/     deepseek.agentkit: the line framing, the journal, the
                  memory, the schedule and the model catalogue
    net/          numpty.net: numptyd's protocol, the child that serves it,
                  the client that holds one, and the network tools
    daemon/       numpty.daemon: the store, the memory tools, the brief, the
                  wake-up, the schedule loop and the control socket
    bin/numpty.ml the subcommands

    packages: deepseek  dune-rpc-eio  humpty  numpty

`deepseek.agentkit` is a sublibrary of the `deepseek` package rather than part
of the `deepseek` library itself, so the engine bindings stay what they are and
nothing in them learns about a filesystem store. It depends on `deepseek`, which
is virtual, so the backend is still chosen by whichever executable links it.

`numpty` is a fourth opam package, built once per backend as `numpty-metal`,
`numpty-cuda` and `numpty-cpu`, mirroring `bin/cpu` and `bin/cuda`. It depends
on `deepseek` and not on `humpty`.

That last point forces one move. The model catalogue in `cmd/humpty_cmd.ml`,
which finds a downloaded GGUF and supplies `--model`, is wanted by both
commands, so `Model` moves to `agentkit/model.ml` and `humpty.cmd` keeps only
the expect script reader and its scrubber, which are humpty's alone. The
`deepseek` package already depends on `cmdliner` and `xdge`, so this adds no
dependency.

## The store

Everything numpty owns lives under one directory, `$XDG_STATE_HOME/numpty` by
default and `--store` otherwise.

    journal/2026-08-08.jsonl    one record per line, append only
    memory/000017.json          an immutable snapshot of the whole memory
    memory/current              the version in force
    workspace/                  the directory the agent's filesystem tools hold
    control                     the socket a running numpty answers on
    lock                        held for the life of a run

There is no counter file. The next `seq` and `run` are recovered at startup
from the last line of the newest journal segment, and the memory version from
`memory/current`. A counter kept beside the journal could disagree with it
after a crash between the two writes, and a number the journal alone supplies
cannot.

The schedule is not here. It is `$XDG_CONFIG_HOME/numpty/schedule.json`, written
by a person and only read by numpty, so nothing numpty does can rewrite the
instructions it was given.

Two runs sharing a store would interleave journal lines and race the version
counter, so a run takes `lock` and a second run refuses with the first one's
process id. The engine already admits one model per process, so this costs
nothing that was available.

`workspace/` is where the agent's filesystem tools are rooted. The agent holds
the standard toolbox, `tree` through `edit`, over a capability minted on that
directory alone, so what it accumulates sits beside the journal that explains
it. There is no `--dir`. An unattended agent does not share a tree a person is
editing.

## The journal

One JSON object per line, encoded and decoded by one jsont codec in the style
`okit/proto.ml` uses. The codec is the schema. Nothing writes a record by hand.

    {"v":1,"seq":412,"t":"2026-08-08T09:14:07Z","run":9,
     "tool_call":{"call":3,"name":"fetch","arguments":"{\"url\":\"...\"}"}}

Four members are on every record. `v` is the schema version, `seq` is a gapless
counter over the whole journal, `t` is RFC 3339 in UTC, and `run` names the
process run. The fifth member names the kind and carries its fields, which is
the shape `okit/proto.ml` already uses on the wire.

`v` is per record rather than per file. A journal outlives the binary that wrote
it, unlike the netd protocol where both ends are the same build, so each line
has to describe itself. Eight bytes a line is the price of that.

The kinds:

    run_start     pid, numpty version, backend, model path, ctx_size
    run_stop      why it stopped
    wake          which task fired, when it was due, and why it fired now
    brief         the memory version, the open items and the byte count
    prompt        what was sent to the model
    reasoning     the model's reasoning, when thinking is on
    content       the model's reply
    tool_call     call id, tool name, arguments as written
    tool_result   call id, name, output, seconds taken, whether truncated
    stats         the Agent.stats record after a turn
    expanded      the context grew to this size
    squeezed      the turn had only this budget
    continued     this session succeeded that one, on the same task
    memory_write  from version, to version, entry id, and why
    handover      the memory version the wake-up finished at
    error         where it happened and what it said
    schedule_load the tasks read, and what changed since the last read

A record is appended and fsynced before the thing it describes is allowed to
proceed. A wake-up is minutes of work and an fsync is microseconds, so there is
no reason to leave a hole in the trace to save one.

**A record that cannot be written stops the run.** An audit trace with a hole in
it is worse than no trace, because it reads as complete. This is the property
worth testing, and it is a negative one.

Reading is forward compatible in one direction only. An unknown kind is returned
as an unknown kind carrying its raw JSON, never dropped, since a later numpty
reading an earlier journal must show everything in it. A `v` above what the
reader knows stops the read and says so, since a record whose common fields may
have moved cannot be shown honestly.

Segments are per day, named by UTC date. A run that spans midnight rolls to a
new file and the `seq` counter carries across it.

## Memory

Memory is a set of entries. A version is a complete snapshot of that set,
written once and never rewritten. Versions form a chain, each naming its parent.

    { "version": 17, "parent": 16, "t": "2026-08-08T09:14:31Z", "seq": 419,
      "cause": "feeds: recorded that the upstream tag moved to v0.9",
      "entries": [
        { "id": "ds4-upstream", "kind": "fact", "title": "...",
          "body": "...", "tags": ["ds4"],
          "created": 4, "updated": 17 } ] }

A snapshot rather than a diff, because memory is kilobytes and a version has to
be inspectable on its own. Replaying a chain to see what numpty believed last
Tuesday is work a person doing an audit should not have to do.

`seq` points at the journal record that caused the version, and the journal's
`memory_write` names both versions and the entry. The two stores each answer
what the other cannot: the journal gives the order of everything, and the memory
store gives the full text at any point. The redundancy is deliberate.

An entry has one of four kinds, which is what a brief sorts on.

`fact` is something durable that was learned. `open_item` is work in progress,
which is the kind that makes a context restart survivable. `reference` is a
pointer outward, a URL or an identifier. `procedure` is how to do something
numpty has had to work out once already.

The agent changes memory through tools, `memory_list`, `memory_read`,
`memory_write` and `memory_forget`. Each mutation mints one version, so a
version corresponds to one tool call and the audit trace lines up with the
model's actions one to one. `memory_forget` writes a version with the entry
absent and does not touch the version that held it.

A mutation writes in one order: the snapshot is written and fsynced, the
journal's `memory_write` is appended, and `current` is rewritten last. A crash
between the snapshot and the record leaves a snapshot the journal never named.
Such a file is the leaving of a crash and not a version, since it was never in
force, so startup removes it and journals an `error` saying so. A crash between
the record and the move leaves the other case, a snapshot the journal did name
and a `current` behind it. That one is a version, so startup moves `current`
forward to it and journals that too. The refusal to overwrite applies to
journalled versions, which is what keeps the recovery from being blocked by
its own debris.

Nothing prunes. A version is never removed and never rewritten. A store that
grows without bound over years is a problem for a person with a broom, and a
store that quietly loses history is a problem nobody can fix.

## The brief and the handover

This is how numpty runs for a month in a window that holds an afternoon.

A wake-up creates a fresh `Agent.t` on the resident engine. The engine is loaded
once for the life of the daemon, so a new context costs a prefill and not a
model load. The agent is given a system prompt and one first message, the brief,
assembled from four things:

  1. the memory at its current version, `fact` and `procedure` entries in full
  2. the `open_item` entries, which are what was left unfinished
  3. the scheduled task that fired, with its prompt
  4. a digest of the journal since the last handover, being what was done and
     what it returned, not the full text of it

Scheduled tasks and open items are different things and the brief keeps them
apart. A scheduled task is an instruction from a person, in a file numpty cannot
write. An open item is numpty's own note that something is unfinished. Confusing
the two would let the agent edit its own orders.

The brief goes in as the system prompt plus the first user message, because
`Agent.create` and `Agent.send` admit nothing else. There is no way to seed a
prior assistant turn, and none is needed.

The wake-up ends in a handover. numpty sends a final prompt asking what should
survive, the agent writes memory through its tools, and the agent is closed. A
`handover` record names the version it finished at.

The handover also fires early. numpty watches `ctx_used` on the `Stats` event
after every turn, and once it passes a fraction of `max_ctx_size`, three
quarters by default, it stops feeding the agent new work.
It sends the handover prompt at that turn boundary, closes the session, and
starts a fresh one on the same task from the new memory. A `continued` record
links the two, so a task spanning five sessions reads as one task in the
journal. `Squeezed` is the failure this avoids and is journalled if it is ever
reached.

A turn boundary is the only place this can happen, since `Agent.send` runs a
whole turn including its tool calls. That is the granularity numpty's loop
works at.

The system prompt says all of this to the model, in a paragraph: that its
context will not survive the wake-up, that memory will, and that anything it
wants to know next time has to be written down. A model that is not told this
writes nothing and loses a day's work.

## The schedule

    { "tasks": [
      { "id": "feeds", "every": "15m", "prompt": "Check the feeds in ..." },
      { "id": "digest", "at": "07:00", "days": ["mon","tue","wed","thu","fri"],
        "prompt": "Summarise yesterday's journal into memory." },
      { "id": "probe", "once": true, "prompt": "Fetch ... and report." } ] }

Three forms, and no crontab expression. `every D` takes a duration such as
`15m`, `6h` or `1d`. `at HH:MM` fires daily in local time, narrowed by an
optional `days`. The journal is UTC, so the due time is converted at each
firing, and a wall-clock time that a DST change skips or repeats still fires
exactly once that day. `once` fires at the next opportunity and then never
again. The
five-field crontab spelling is familiar but it is a parser and a set of edge
cases in exchange for expressiveness nothing here has asked for. Adding it later
as a fourth form breaks nothing.

### Changing the schedule

The file is the only place work is asked for, and the CLI writes it. The daemon
reads it and never writes it.

    numpty task add feeds --every 15m "Check the feeds in memory ..."
    numpty task add probe --once "Fetch ... and report."
    numpty task rm feeds
    numpty task disable digest
    numpty task run feeds

`numpty task` is an ordinary file editor. It parses the schedule, applies the
change, validates the result, writes a sibling temporary file and renames it
over the original, so the daemon never stats a half-written file. It needs no
daemon, and a change made while numpty is stopped is picked up at startup, so
the same commands work either way and neither has to know about the other.

The distinction that matters is not that the file is edited by hand. It is that
the *agent* cannot edit it. `numpty task` runs as the person, on the person's
authority, in a process that holds no model. The daemon and the tools it gives
the model reach the journal and the memory store and never the schedule. So the
orders stay a thing a person can read, diff and keep in git, and nothing numpty
concludes can change what it was told to do.

The daemon stats the file on every tick and rereads it when it changes, and
`SIGHUP` rereads it at once. A `schedule_load` record says what was read and
what changed, so an edit is in the audit trace like everything else. A file that
does not parse is reported at error level and the previous schedule stays in
force, since a person mid-edit should not lose a daemon.

### Running a task now

`numpty task run feeds` sets `"run_now": n` on that task, an integer it
increments. The daemon fires the task when the serial is above the last one it
journalled for that id, and the `wake` record carries the serial and a trigger
reason.

A serial rather than a boolean, because a boolean would have to be cleared and
the daemon does not write the file. This way the journal remains the only state:
the firing is idempotent under a reread, it survives a restart with nothing to
transfer, and a person reading the trace sees exactly which request caused which
wake.

`once` tasks are marked done the same way, in the journal and not in the file. A
task whose id already has a `wake` record for a `once` firing does not fire
again.

Missed fires do not queue. A daemon that was down over four due times of a
fifteen minute task fires once at startup and not four times. `on_missed` is
`run_once` by default and may be `skip`. Next fire times are not persisted as
such, they are computed at startup from the last `wake` record for each task,
so the journal remains the only state that matters.

`SIGTERM` finishes the turn in flight, takes the handover, and stops. The
journal's `run_stop` says which it was.

## numptyd, the network child

`numpty netd` is a subcommand of the same binary, documented as internal,
spawned by the daemon before `V4.create` and held for the life of the run. It
holds every program numpty runs. The daemon never forks once the engine exists.

The protocol is one compact JSON object per line over its pipes, with `hello`,
`call`, `trace`, `result` and `shutdown`, exactly as okitd's is. The framing is
the same framing, so it moves: `Agentkit.Line` gets `max_line`, the reader and
writer, the rule that a bad line is a terminal protocol fault rather than
something to skip, and the rule that an invalid UTF-8 sequence becomes `U+FFFD`
rather than stopping both ends over the user's own bytes. `okit/proto.ml` and
`net/proto.ml` then supply only their own codecs.

Everything okitd learned applies unchanged. A call is bounded, sixty seconds for
a fetch and five minutes for a run. Exceeding the bound kills numptyd. There is
no respawn, since respawning would fork the process holding the model. A
numptyd fault instead ends the run: the call that met it answers with what
went wrong and the tail of numptyd's stderr, the turn finishes on that answer,
the handover is taken, `run_stop` names the fault, and the daemon exits
nonzero. A supervisor that restarts numpty gets a fresh numptyd before the
engine loads, so the network heals without a person and the journal shows both
the fault and the restart. Nothing
numptyd spawns inherits its stdin, which is the protocol, so a curl that would
prompt for a password gets an empty one instead of the next call. EOF on stdin
is a shutdown, so a force-quit numpty leaves nothing behind.

numptyd writes no file of numpty's. A fetch returns bytes over the pipe and
numpty writes them through the capability its own tools hold, which is the same
discipline that keeps okitd out of the workspace.

## The network tools

Three ops in the first version.

`fetch` runs curl with `--silent --show-error --location`, a `--max-time` and a
`--max-filesize`, and no stdin. It reports the status code, the final URL after
redirects, the content type, the byte count, and then the body.

A non-2xx is reported as a non-2xx with the body after it. Returning a 404 page
as though it were the article is the failure the repository's rule about
silencing is written against, and it is the one an agent is least able to
detect for itself.

`fetch` takes a `render` of `text` or `raw`. `text` reduces HTML by dropping
script and style content, removing tags and collapsing whitespace, which is what
keeps a wiki page from costing forty thousand tokens of markup. It is a
reduction and not a renderer, and the tool description says so, so `raw` is
there for when the reduction misleads.

A body is bounded at one mebibyte by default. A larger fetch is refused with the
size it would have been and the bound that refused it, rather than truncated
into something that reads as the whole document. The hard ceiling is two
mebibytes, an eighth of the protocol's `max_line`, because a body goes into one
line of JSON and escaping grows it. A call asking for more is served the ceiling
instead, since the alternative is a line the peer reads as a protocol fault.

`head` is `fetch` without a body, for checking whether something moved.

`run` runs a named program with arguments, which is how a service that ships a
command line is reached before it has an op of its own. Egress is not restricted
and this is consistent with that, and it is the same authority `bash` already
has in okitd.

`dns` stays in `Toolbox`, in process. It resolves through an Eio net capability
and forks nothing, so there is no reason to send it across a pipe.

A service that earns it gets its own op rather than a `run` invocation, so that
its trace line and the wording of its result are written once, in the way
`okit/report.ml` does for the dune tools. `run` is the door, not the corridor.

Every call is journalled as `tool_call` and `tool_result` with the arguments as
the model wrote them and the output as the tool returned it, before the result
reaches the model. Egress being unrestricted, the journal is the whole of the
control, so it has to be written first.

## The control socket

A running numpty answers questions on a unix socket at `control` under the
store. It is how a person sees what the daemon is doing now, which is the one
thing the files cannot say.

**The socket answers only what the store cannot.** The journal, the memory
versions and the schedule are on disk, durable before the daemon proceeds past
them, so a client reads those from the files and needs no daemon to do it. What
is left is live state: which job is running, how far into its context it is, how
far a prefill has got, which tool call is in flight, when each task fires next,
and whether numptyd is still alive. That is the whole of the protocol, and
keeping it that small is what makes it cheap to put behind capnp-rpc later.

### Not entering the engine

The constraint that shapes this. `Agent.send` blocks for minutes at a time, and
the control fiber has to answer while it does. `Agent.stats` waits for the
engine and so must never be called from the control fiber. Two things make that
work.

The agent loop publishes a `Status.t` snapshot, an ordinary immutable record, at
every turn boundary and at every state change it makes. The control fiber reads
the snapshot and never the agent.

`Agent.prefill_progress` is the exception it may call, because it exists for
this. It reads the two atomics the engine's progress hook writes and does not
wait for the engine, which is what lets the interface in humpty show a prefill
that is minutes long. numpty uses it for the same reason and from the same kind
of fiber.

So a status answered while a turn is blocked is honest about the turn without
touching it. A client polling every second costs the daemon a record copy.

### The protocol

One compact JSON object per line, request and response, over `Agentkit.Line`.
That is the framing's third user after okit and numptyd, and it carries the same
rules: a line that does not parse is a terminal fault for that connection, and
an invalid UTF-8 sequence becomes `U+FFFD` rather than stopping the exchange
over somebody's bytes.

    -> {"status":{}}
    <- {"status":{"run":9,"since":"2026-08-08T06:00:11Z","model":"...",
                  "backend":"metal","netd":"alive","job":"feeds"}}
    -> {"jobs":{}}
    -> {"memory":{"at":null}}
    -> {"follow":{"kinds":["tool_call","content"]}}

Five methods, all read-only.

`status` is the daemon itself: which run, since when, which model and backend,
whether numptyd is alive or which fault killed it, the memory version in force,
and the id of the running job if there is one.

`jobs` is the work. One job is one wake-up, and there is at most one running,
since a process holds one engine and the loop is sequential. The running job
reports its task id, when it started, which session it is on if it has handed
over and continued, its `ctx_used` against `ctx_size`, its live prefill
progress, its turn and tool-call counts, and the name of the tool call in
flight. Beside it come the tasks that are due and waiting, and every task's next
fire time.

`memory` and `log` are conveniences, served from the store rather than from
memory, so that one client reaches everything without knowing which side of the
line a thing falls on.

`follow` streams journal records as they are appended, filtered by kind. It is
the one method whose shape differs between the two transports, being a stream of
lines here and a callback interface under capnp-rpc.

Nothing mutates, and adding a job is not a gap in that. Jobs are added, removed
and triggered by `numpty task`, which writes the schedule file directly and
needs no daemon. Routing those through the socket would buy nothing and cost
two things: the commands would stop working while numpty was down, and the
daemon would become the writer of the file it is meant only to obey.

So the split is that the socket reports and the file instructs. A client asks
what is running, and a person changes what will run. Adding a `nudge` method
later would be a convenience over `numpty task run` rather than a new power,
and the `wake` record already carries the trigger reason it would use.

### Shape for capnp-rpc

The control surface is an OCaml signature over plain records, and the line
protocol is one adapter over it. capnp-rpc becomes a second adapter against the
same signature, and the daemon holds one implementation for both. Every response
is a record with a jsont codec, which maps onto a capnp struct without
rearrangement. No response carries a callback, apart from `follow`, which is why
that one is called out above.

Authority is the file system's. The socket is created mode 0600 in a store
directory the owner already controls, and there is no authentication, because
anyone who can open it can already read the journal that says everything the
socket would. That stops being true the moment it is exposed over a network,
which is where capnp-rpc's own authentication belongs and why this version does
not invent one.

### Faults

A unix socket address is limited to about a hundred characters, the same bound
that keeps a dune server out of a deep workspace. A store path that pushes
`control` past it is reported at warning level once, and the daemon runs without
a socket. An agent that cannot be queried is still an agent, which is how humpty
already treats an okit that will not start.

The daemon takes `lock` before it binds, so a socket file left by a crashed run
belongs to nobody and is unlinked and replaced. A live daemon still holds the
lock, so a second run refuses before it reaches the socket at all.

A client that disconnects mid-response is not an event. Its fiber ends and the
daemon does not notice, which is the point of answering from a snapshot.

## Commands

    numpty run                     the daemon
    numpty once "prompt"           one wake-up, no daemon, same store
    numpty netd                    internal, the network child
    numpty status                  the running daemon, over the socket
    numpty jobs                    the running job and what fires next
    numpty follow [--kind K]       stream the journal as it is written
    numpty log [--since T] [--kind K] [--task T] [--run N]
    numpty memory [show|history|diff V W] [--at V]
    numpty task [list|check|add|rm|enable|disable|run]

`log` and `memory` open the store, decode with the same jsont codecs that wrote
it, and print. They work while numpty runs, while it is stopped, and on a store
copied off the machine.

`numpty task` reads and writes the schedule file, and likewise needs no daemon.
A running one notices the change on its next tick.

`status`, `jobs` and `follow` need the socket. With no daemon listening they say
so and, where the journal can answer at all, print what the last run was doing
when it stopped rather than nothing.

`numpty once` is how a change is tested without a daemon, and it is what the
live test drives. It binds no socket, since it is not there to be asked.

## What humpty gains

`Journal`, `Memory` and `Schedule` are in `deepseek.agentkit` and humpty may
link them without linking numpty.

The journal is the immediate one. `humpty agent` writing a `Journal.t` gives a
transcript that survives the session, which is something the terminal interface
cannot provide, since a trace line there is ephemeral column content by design.

Memory would let a humpty session start knowing what the last one found. That is
a larger change, because it needs the brief and the handover as well, and it is
not proposed here. The library admits it.

## What has to change in what exists

Four things, each small and each in its own commit.

`Agent.close` does not exist. numpty creates and discards agents through the
life of a run and must release each session promptly, not at the collector's
convenience, since a session holds a KV cache sized to its context. It closes
the session and marks the agent unusable.

`Humpty_cmd.Model` moves to `Agentkit.Model`, unchanged, so numpty can find a
model without depending on the humpty package.

The line framing in `okit/proto.ml` moves to `Agentkit.Line`, unchanged, and
`okit/proto.ml` keeps its codecs. numptyd and the control socket are its second
and third users. This is a pure move and belongs in a commit with no behaviour
in it.

`ARCH.md` gains a section for numpty and one for numptyd, and `CHANGES.md` gains
its entries.

## Tests

The properties worth pinning are mostly negative ones.

The journal stops the run when it cannot be written. Give the store an
unwritable journal directory part way through and check that numpty stops and
says why, rather than continuing unlogged.

Sequence numbers are gapless and monotonic across a restart, and across a
midnight segment roll.

A memory version is never rewritten. A write against a version number that
exists is refused. `memory --at V` gives what was there after later versions
have been written.

A daemon down over three due times of one task fires once and not three.

`numpty task run` fires the task once and not on every reread, which is what the
serial is for. Bump it twice while the daemon is stopped and it fires once at
startup. A schedule file that does not parse leaves the running schedule in
force rather than emptying it, which is the failure a person mid-edit will meet
and the one that would otherwise stop every task at once.

numptyd answers a death, a protocol fault and a timeout promptly, finally, and
with the tail of its stderr, and the run then stops with a `run_stop` naming
the fault. `test/okit_client.ml` stages exactly these with shell stand-ins and
the technique carries over.

A 404 is reported as a 404. A fetch over the body bound is refused with its size
and not truncated.

A session driven past the context fraction hands over and continues, and the
journal links the two sessions.

The control socket answers while a turn is blocked. This is the one worth
writing first, because it is the property the whole design of the status
snapshot exists to hold. Drive a wake-up whose tool call blocks, query `status`
and `jobs` from a client, and check both answer promptly and that the turn is
unaffected. A regression here shows up as a daemon that stops answering
precisely when a person most wants to ask it something.

A crashed run's socket file is unlinked and replaced, and a live run's is not,
since the lock is taken first. A store path too long for a unix address gives a
daemon that runs and warns rather than one that will not start.

Under `DS4_LIVE=1`, one end-to-end wake-up: fire a task, fetch a page from a
local server, write memory, hand over, and check the journal holds every step
and the memory version advanced by exactly the number of `memory_write` records.

## Limits

There is no compaction and this design does not add one. It replaces the
conversation instead, and a task that genuinely needs more working context than
the ceiling holds will lose detail at each handover. What survives is what the
model chose to write down, and a model that writes down the wrong thing loses
the right one. The journal is the recourse, since the digest that goes into the
next brief is drawn from it.

The control socket answers and does not act. Work is asked for by writing the
schedule file, which `numpty task` does without a daemon, so nothing is out of
reach. What it does mean is that a client holding only the socket, which is the
position a future capnp-rpc peer is in, can watch numpty and not direct it.
Directing it remotely is a second decision about authority and it is not made
here.

A status is as fresh as the last turn boundary, apart from the prefill counter,
which is live. A turn that runs for four minutes reports the tool call it is in
and the context it had when the turn began, since reading anything more current
would mean entering the engine the turn is inside.

numptyd is not respawned, for the reason okitd is not, so a fault ends the run
rather than leaving a daemon that works for weeks without a network. The
daemon hands over, records the fault, and exits nonzero, and healing is the
supervisor's restart. A numpty run without a supervisor stays down until a
person returns, which is what the nonzero exit is for.

Egress is unrestricted by choice. The journal records every call before it is
made, which makes the trace an account and not a control. A machine where that
distinction matters wants an allowlist in front of `fetch`, and the place for it
is numptyd, before the curl.

## Departures taken in implementation

What was built and this document did not say.

A wake-up takes at most five sessions, which the wake-up's `max_sessions`
bounds. A task that fills its context every time would otherwise hold the daemon
for ever, and the other tasks are waiting on the one engine.

The brief has a fifth section, `Pointers`, holding the `reference` entries one
line each rather than in full. A reference is an address and its first sentence,
and a wake-up that wants the rest reads it with `memory_read`.

The lock is `Unix.lockf`, since OCaml's `Unix` does not expose `flock`, and it
is paired with a table of the roots this process already holds. A record lock is
per process rather than per descriptor, so the table is what refuses a second
store opened inside one process, and the lock is what refuses a second process.

`Schedule.poll` takes a `since`, being when the caller began watching. Only the
`skip` policy reads it, and it is what tells a due time nobody was there for
from one the caller watched come round.

The control socket is bound after the engine has loaded, so a `numpty status`
asked during a model load reports nothing listening. That is a known limit
rather than a decision, and the socket answering before the run it describes
exists would be its own kind of lie.

`memory_write` and `memory_forget` refuse an empty `why`. The version and the
journal record both carry it, and a version whose cause is blank tells a person
doing an audit nothing.

`Memory.write` and `Memory.forget` take the journal append as a callback rather
than returning between the two steps, so a caller cannot get the order wrong.

numptyd passes a URL to curl as `--url`, so one beginning with a dash is a URL
and not an option, and takes a fetch's metadata from a `--write-out` report
written after the body behind a delimiter, split at that delimiter's last
occurrence, since a body may contain anything including the delimiter.

numptyd catches no signal. okitd catches `SIGTERM` to stop the dune server it
started, and numptyd starts nothing that outlives it and writes no file, so the
end of its standard input is the whole of its shutdown.

The capability approval refuses every directory outside the workspace. An
unattended agent has nobody to ask, and the refusal reaches the model, which can
say so and write it down. There is no `bash` tool for the same reason a fork is
avoided, and a program is run through numptyd's `run`.

An absent schedule file is the empty schedule, so a numpty started before anyone
has asked for work runs and waits. A file that does not parse is an error, which
the daemon keeps its old schedule over and `numpty task` refuses to overwrite.

`numpty once` journals its wake-up under the task id `once`, so a store holds
the ad hoc runs and the scheduled ones in the one account.

The store is `$XDG_STATE_HOME/numpty` and the schedule `$XDG_CONFIG_HOME/numpty`,
while the models stay under `ds4` where humpty already has them. A machine holds
one copy of a hundred-gigabyte model whichever command loads it.
