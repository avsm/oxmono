# dumpty: a one-shot agent, and the agentkit consolidation behind it

dumpty is the third command over the same engine. humpty holds a conversation
with a person at a terminal. numpty runs unattended for weeks. dumpty does one
job and exits: `dumpty 'rename module Foo to Bar'` assembles the same workspace
tools humpty has, runs one exchange to completion, streams its account to
standard error, and prints one JSON object on standard output.

The account on standard error is the same thing numpty writes to disk: agentkit
journal records, one compact JSON object per line, under the same codec. The
three commands then differ only in where the account goes. humpty shows events
in its interface, numpty appends them to a store, and dumpty streams them to
standard error. The fold from agent events to journal records is written once,
in agentkit, and all three sit on the parts of it they use.

## The stream contract

Standard output carries exactly one line, written when the exchange ends, and
nothing before it:

    {"reply":"Renamed Foo to Bar...","turns":6,"tool_calls":11,
     "ctx_used":18342,"ctx_size":32768,"squeezed":false}

`reply` is the text of the final turn, which is the reply the exchange ended
on. `turns`, `tool_calls`, `ctx_used` and `ctx_size` come from the last stats
the agent reported. `squeezed` says whether the final reply ran in a context
that could no longer grow, in which case it may be cut off. A run that fails
prints nothing on standard output and exits nonzero, so an empty standard
output and a nonzero status is the whole failure contract, and a consumer
never has to guess whether a partial object is a whole one.

Standard error carries the journal: `run_start` first, then `prompt`,
`tool_call`, `tool_result`, `reasoning`, `content`, `stats`, `expanded`,
`squeezed` and `error` records as the exchange runs, and a `run_stop` last,
each flushed as it is written so a watcher sees the run as it happens. The
records are `Agentkit.Journal` records with `run` set to the process id and
`seq` counting from 1, so `Journal.of_string` reads every line and the stream
reads like a store journal read forward. There is no `wake`, `brief` or
`memory_write`, since there is no schedule and no store.

Diagnostic logging also reaches standard error, since standard output is
spoken for. At the default level that is at most the odd warning, and a
consumer that wants only records keeps the lines that parse as one. The note
about okit tools being absent is journalled as an `error` record rather than
logged, so the account says it and the stream stays clean in the common case.

The exit status is zero when the exchange finished, squeezed or not. It is
nonzero when there was nothing to print: the model was not there, the engine
would not load, the prompt did not fit a context that could no longer grow, or
the exchange outran `--timeout`. Each of those appends a `run_stop` naming the
reason where the journal exists to append to.

## The command

    dumpty [OPTIONS] PROMPT

`PROMPT` is what to do, and `-` reads it from standard input, for a prompt
assembled by another program. The options are humpty's where they mean the
same thing: `--model`, `-d/--dir` for the workspace the file tools are
confined to, `-s/--system`, `--think`, `--seed`, `--ctx` and `--max-ctx`.
`--timeout SECS` bounds the whole exchange, 600 by default and 0 for none,
since a one-shot command left in a script must not hang the script. Expiry
appends a `run_stop` and exits nonzero, leaving okitd to stop itself when its
current call is answered, as `humpty expect` does.

dumpty assembles the toolset humpty's agent gets, okit included, because the
work it is for is workspace work: the capability file tools, `dns`, `bash`
through okitd, and the dune and merlin tools when the workspace has them. It
spawns itself as `dumpty okitd`, an internal subcommand identical to humpty's.
AGENTS.md is read and appended to the system prompt as humpty does. A
capability outside the workspace is granted, as humpty grants it, because a
person invoked this run deliberately and the grant is on the record: the
`open_dir` call and its result are journal records like every other call.

The engine runs on its own domain, as everywhere else, so the timeout fiber
can act while a turn is blocked in the engine.

## The agentkit rearrangement

This is unreleased software, so the modules move to where they belong rather
than gaining shims.

    agentkit/trace.ml         the fold from agent events to journal kinds
    agentkit/cli.ml           what every command front-end repeats
    agentkit/instructions.ml  what a workspace leaves for an agent (AGENTS.md)
    okit/toolset.ml           the workspace toolset and its system prompt

`Agentkit.Trace` is the fold that today lives inside `daemon/wake.ml`'s
`on_event`: it buffers reasoning and content and flushes each as one record at
the boundary it is complete at, counts tool calls, pairs each result with the
oldest pending call, times it, and marks a result the model saw less of as
truncated. It takes an `emit : Journal.kind -> unit` and a clock, and knows
nothing about where the kinds go. `wake.ml` emits into `Journal.append` and
derives its status updates from the kinds it sees pass. dumpty emits into a
stamper that writes one line to standard error. The stamper needs a record
built outside a store, so `Journal` gains a `stamp` (name at the
implementer's discretion) that takes a clock, a run and a sequence number and
returns the record `to_string` already prints.

`Agentkit.Cli` takes what `bin/humpty.ml` and `bin/numpty.ml` hold two copies
of: `backend_name`, `guard`, `self`, `resolve_seed`, the `resolve_model`
failwith wrapper, and the logs cmdliner term, with a flag for the threaded
reporter humpty needs. The shared cmdliner arguments whose wording is genuinely
shared move too, `--seed` and `--think` among them; an argument whose doc
string differs per command stays where it is, or takes the doc as a parameter.
The sublibraries this adds to `agentkit/dune`, `logs.cli`, `logs.fmt`,
`logs.threaded`, `fmt.cli` and `fmt.tty`, ship with opam packages `deepseek`
already depends on, so no package gains a dependency.

`Agentkit.Instructions` is `workspace_instructions` from `bin/humpty.ml`: load
AGENTS.md if it is there, cap it, and say in the text when it was cut. humpty
and dumpty both call it.

`Okit.Toolset` is humpty's `assemble` moved into the okit library, with the
three system prompt fragments beside it: the capability discipline prompt that
was `default_agent_system`, `dune_system` and `merlin_system`. It takes the
argv to spawn okitd with, since humpty spawns `humpty okitd` and dumpty spawns
`dumpty okitd`, and returns the tools, the prompt addition and the okit status
as `assemble` does today. It registers stopping the client on the switch, as
`assemble` does. The wording helpers that flatten a status for a transcript
stay in `humpty.cmd`, which dumpty does not need, since dumpty journals the
status rather than wording it.

Tool assembly itself stays per command. numpty's toolset is its own, memory
tools and network tools and no shell, and stays in `bin/numpty.ml`. What is
shared is the discipline around the toolsets, not the lists.

## Packages

`dumpty` is a fifth opam package, built once per backend as `dumpty-metal`,
`dumpty-cuda` and `dumpty-cpu`, mirroring `bin/cpu` and `bin/cuda`. It depends
on `deepseek` and on `humpty`, whose `humpty.okit` library holds the toolset;
it does not depend on `numpty`. `bin/dumpty.ml` holds the one-shot command and
the internal `okitd` subcommand and nothing else. Downloading and listing
models stays humpty's job, and dumpty's error for a missing model says so.

## Tests

`agentkit_trace` covers the fold without an engine: text is flushed as one
record at a tool call and at the end of a turn, reasoning before content;
results pair with calls first in first out and an orphan result gets call 0;
the truncated flag follows the limit; stats flush the text before they are
emitted. The existing numpty tests hold the wake-up's behaviour across the
refactor, and `live_numpty_wake` pins the journal it writes end to end.

`dumpty_cli` drives the built `dumpty-cpu` with a model path that names
nothing and checks the failure contract: nonzero exit, empty standard output,
a reason on standard error. `live_dumpty`, under `DS4_LIVE` and registered for
the Metal build as `live_expect_prompts` is, runs one real exchange in a
scratch workspace and checks the whole contract: exit zero, one JSON object on
standard output whose reply is not empty, and a standard error whose every
line `Journal.of_string` accepts, starting at `run_start` and ending at
`run_stop`.

## Commits

One branch, one commit per self-contained change, refactors apart from
behaviour:

1. the pending removal of the vendored `dune-rpc-eio`, which now comes from
   opam, with the ARCH.md and README lines that named it brought up to date
2. `Agentkit.Cli` and `Agentkit.Instructions`, with humpty and numpty moved
   onto them
3. `Agentkit.Trace` and the `Journal` stamper, with `daemon/wake.ml` moved
   onto them
4. `Okit.Toolset`, with humpty moved onto it
5. dumpty: the package, the binary, its tests and its documentation

CHANGES.md gains entries for what a user notices, which is dumpty itself and
the dune-rpc-eio move; the interior refactors change nothing a user sees.
