# ocaml-agentkit

Agents over local models. The `agentkit` library defines the event stream,
journals, memory, and schedules shared by a persistent agent. `agentkit-ds4`
connects it to [DS4](https://tangled.org/anil.recoil.org/ocaml-deepseek).
`agentkit-apple-fm` connects it to
[Apple Foundation Models](https://tangled.org/anil.recoil.org/ocaml-apple-fm).

`humpty` is an interactive agent in a terminal interface, `numpty` runs an
agent unattended, and `dumpty` runs one job and exits. Each can select DS4 or
Apple Foundation Models at run time on a supported Mac.

## Summary trees

`Agentkit.Memo` is a pure OCaml summary tree inspired by
[OptMem by Victor Taelin](https://github.com/VictorTaelin/OptMem). This is an
independent implementation of its hierarchical, age-weighted memory idea.
No upstream Python source or storage format is used.

Supply original records oldest first, with stable IDs and revisions. The tree
returns a complete overview with a node budget, retaining finer detail at the
recent end. Each node has an opaque key. `expand` returns its children or its
original source. Summaries are lossy, untrusted data. Read originals before
relying on exact facts.

`maintain` fills a bounded number of missing summaries. Small merges use raw
records and larger merges use child summaries. Connect its `summarize` callback
to `Agentkit.Summary.run ~instructions:Agentkit.Memo.instructions`. Cache keys
include source IDs, revisions and contents. Store adapters must recheck keys
and authorization when saving after inference. Missing summaries are explicit
range pointers and do not prevent reading memory. `render` bounds UTF-8 bytes,
including truncation notices. A byte limit is not a tokenizer limit.

Numpty adds an `episode` memory kind for observations and completed activity.
Its wake-up brief includes at most eight episode ranges within 3500 bytes.
Current facts, procedures and open items remain in their own sections.
Each enduring kind shows at most eight entries, most recently updated first,
within a fixed section budget. Shortened bodies and omitted entries point to
`memory_read` and `memory_list`. The complete brief fits 32768 UTF-8 bytes,
including an 8000-byte journal digest and a task prompt of at most 8192 bytes. The
agent can use `memory_overview`, `memory_expand` and `memory_summarize` to
inspect episodes and fill the derived cache after reading sources. Summaries
are limited to 512 bytes and stored separately from immutable memory versions.
Editing or forgetting an episode makes dependent summaries unreachable.
Historical memory versions retain their original contents.

In oxmono the daemon remains source-only. Its brief and memory tools compile
in `test_core`, which exercises persistence, corrections and pinned entries.
The core tree, journal and memory modules are built as part of `agentkit`.

## Building

Install the backend used by the packages being built. DS4 supports Linux and
macOS:

    opam pin add ds4 ../ocaml-deepseek
    dune build

Apple Foundation Models requires Apple silicon, macOS 26 or later, and an SDK
containing `FoundationModels.framework`:

    opam pin add apple-fm ../ocaml-apple-fm
    dune build

`ds4`'s README lists what each backend requires. humpty also needs `mosaic`,
`matrix` and `matrix-eio`, and the external `dune-rpc-eio` package.

## Backend adapters

Both adapters implement `Agentkit.Agent.S`. Construction remains specific to
the backend because model selection and tool schemas differ. DS4 programs
create a `Ds4.Agent.t`, then call `Agentkit_ds4.Agent.send`. An Apple program
creates instrumented tools and an agent through `Agentkit_apple_fm`:

```ocaml
let echo =
  let open Apple_fm.Codec in
  let arguments =
    Invoke.map "echo" Fun.id
    |> Invoke.param ~enc:Fun.id "text" string
    |> Invoke.seal
  in
  Agentkit_apple_fm.Tool.v ~description:"Echo text." arguments Fun.id

let run sw on_event =
  let agent = Agentkit_apple_fm.Agent.create ~sw [ echo ] in
  Agentkit_apple_fm.Agent.send agent ~on_event "Call echo with hello."
```

The Apple adapter streams content, instruments tool calls and complete text
results, reports the common statistics, supports cancellation and transcript
replacement, and can compact a transcript explicitly. Pass `~compact_at:75`
to enable automatic compaction. Measurements unavailable from the operating
system are zero in the common statistics.

`Agentkit.Driver` merges model drivers for a program that links both adapters.
Use `--model ds4/q4` or `--model apple/default` with its `model_term` term.
Each driver receives the model name and constructs an agent with its own tool
codecs. The common registry does not encode or translate tool schemas.

```ocaml
let drivers =
  Agentkit.Driver.merge
    [ Agentkit.Driver.v ~name:"ds4" ~models:ds4_models
        ~create:create_ds4_session;
      Agentkit.Driver.v ~name:"apple" ~models:apple_models
        ~create:create_apple_session ]

let run choice prompt on_event =
  match Agentkit.Driver.create drivers choice with
  | Error message -> failwith message
  | Ok session ->
      Fun.protect
        ~finally:(fun () -> Agentkit.Driver.close session)
        (fun () -> Agentkit.Driver.send session ~on_event prompt)
```

`Agentkit.Driver.models drivers` supplies fully qualified names for a `list`
command. `select` resolves a choice without loading a model. A DS4 driver may
also accept a file path after `ds4/`. `agentkit-apple-tools` supplies native
Apple codecs for the workspace, network, and memory handlers used here.

## Commands

Each agent is built once per DS4 compute backend. Model selection remains a
run-time choice within that build:

| Command        | Backend | Availability                     |
| -------------- | ------- | -------------------------------- |
| `humpty-metal` | Metal   | macOS on Apple Silicon           |
| `humpty-cuda`  | CUDA    | Linux, with `DS4_CUDA=yes`       |
| `humpty-cpu`   | CPU     | everywhere                       |

The examples below use `humpty-metal`. Substitute `humpty-cpu` to run without a
GPU. `numpty` and `dumpty` ship the same three ways, as `numpty-metal`,
`numpty-cuda` and `numpty-cpu`, and `dumpty-metal`, `dumpty-cuda` and
`dumpty-cpu`.

    humpty-metal models list           # models and their availability
    humpty-metal models show apple/default
    humpty-metal models fetch ds4/q4   # explicit download
    humpty-metal chat "Explain monads in one sentence."
    humpty-metal agent -d ./workspace --model ds4/q4
    humpty-metal agent -d ./workspace --model apple/default

`models list`, `models show DRIVER/MODEL`, and `models fetch DRIVER/MODEL` are
also available in `numpty` and `dumpty`. Selecting `--model` never downloads
weights. Fetching an installed DS4 model is a no-op. Apple models are managed
by macOS and cannot be fetched through Agentkit. `humpty list` and
`humpty download` remain as compatibility commands. `chat` remains DS4-only.
Model-management output uses color on terminals and stays plain in pipes or
when `NO_COLOR` is set. Pass `--color=always` or `--color=never` to override it.
DS4 model files are kept under `$XDG_DATA_HOME/ds4` whichever command fetched
them. See the `ocaml-deepseek` README for which model to choose. Run
`humpty-metal <command> --help` for the options of each.

## The agent

`agent` puts the same model in a loop with tools, so it can work across several
turns.

    humpty-metal agent -d ./workspace

It opens a terminal interface. The transcript fills the screen, a line beneath
it shows how full the context is and how fast the model is running, and the
prompt is at the foot.

Type a request and press Enter. The prompt clears at once, and anything typed
while the model is working waits its turn instead of being refused, so a second
thought can be written down as it arrives. The arrow keys walk back through
earlier prompts.

Tab unfolds the tool output of the turns, which is folded to one line each by
default, since it is bulky and rarely what you want to read afterwards. Ctrl-C
leaves, as does ctrl-D at an empty prompt. The keys are listed above the prompt.

The layout follows the width of the window. Below about 76 columns the tool
column goes, leaving the conversation the whole screen, and the line of vitals
keeps whichever figures still fit.

If the workspace holds an `AGENTS.md`, it is added to the agent's instructions,
so a project states its conventions once rather than in every request. The
startup banner says when one is in use.

### Tools

`list`, `tree`, `read`, `read_lines`, `find`, `grep` and `stat` read the
filesystem, `edit` changes part of a file, `write` creates one or replaces the
whole of one and `append` adds to the end of one. `dns` resolves hostnames.

The filesystem tools reach only what they are granted. The agent starts with a
capability for the workspace given by `-d`, and any directory outside it must be
requested with `open_dir`, which prints the grant. A capability rejects `..` and
symlinks that lead out of it, so no path the model supplies can escape.

The model-facing toolset has no shell, so every filesystem operation goes
through this capability boundary.

In a workspace with a `dune-project`, the agent also gets `build`, `test`,
`promote` and `project`, and an `edit` and a `write` that answer with what the
build then says about the file they changed. Where an `ocamlmerlin` answers for
the workspace it gets `outline`, `type_at`, `locate`, `occurrences`, `errors`,
`search` and `complete` too. These run in `okitd`, a child humpty starts before
it loads the model.

A reply is bounded too, at 2048 tokens, and a tool call the model does not
finish inside that is discarded rather than half made. The agent says so, tells
the model, and gives it another turn, which is what `append` is for: a file
longer than one reply is written in parts. Three such turns running end the
exchange with an error, since a model that cannot make the call smaller will
not get there by trying again.

### Context

The agent starts with a context of 32768 tokens, which `--ctx` sets. Tool
results are shortened to 4000 characters before being added to the
conversation, because tool output is what usually fills a context. A result
shortened that way loses its middle, so the tools that read a file stop short
of the limit instead and name the line to read on from. A file of any length
can be read whole, a page at a time.

A conversation that outgrows its context moves to a larger one, up to 262144
tokens, without reloading the model. Growing costs one pass over the
conversation, so the context doubles rather than creeping. Beyond that ceiling
the agent reports that it is full and keeps the conversation, rather than
exiting.

## Running unattended

`numpty` puts the same model to work with nobody watching. It wakes on a
schedule you write, works, writes what should survive into a memory that
outlives the context it was learned in, and records every step in a journal you
read afterwards. There is no interface.

    numpty-metal task add feeds --every 6h "Check example.org for news."
    numpty-metal task list
    numpty-metal run

`run` loads the model once and waits. Each time a task comes due it builds a
brief from memory, works through it, asks the agent what should survive, and
closes the session, so a run of weeks never rests on one conversation. One task
runs at a time, since a process holds one model. `SIGTERM` lets the turn in
flight finish before it stops. `numpty-metal once "..."` does a single wake-up
and exits, which is the way to try something without a daemon.

Everything numpty owns is under `$XDG_STATE_HOME/numpty`: the journal, the
memory versions, and the `workspace/` its file tools are rooted at. The schedule
is not there. It is `$XDG_CONFIG_HOME/numpty/schedule.json`, which `numpty task`
writes and the daemon only reads, so nothing numpty concludes can change what
you asked for. Keep it in git if you like.

    numpty-metal log --since 2h       # what it did
    numpty-metal memory show          # what it knows
    numpty-metal status               # what it is doing now

`log` and `memory` read the store directly and need no daemon, so they work
while numpty runs, while it is stopped, and on a store copied off the machine.
`status`, `jobs` and `follow` ask the running daemon over a unix socket, since
what is happening now is the one thing the files cannot say.

The agent holds the filesystem tools, rooted at the workspace and unable to
reach outside it, `dns`, a `fetch`, `head` and `run` served by a child process
that holds the network, and four tools for reading and writing its memory. There
is no shell.

## One-shot: dumpty

`dumpty` gives the same model the same tools `humpty agent` has, runs one
prompt to completion, and exits. It is the one to reach for from a script or a
build step, where nobody is at the terminal.

    dumpty-metal -d ./workspace "rename module Foo to Bar"

Standard output carries one line, written when the exchange ends and nothing
before it:

    {"reply":"Renamed Foo to Bar...","turns":6,"tool_calls":11,
     "ctx_used":18342,"ctx_size":32768,"squeezed":false,
     "journal":"/home/you/.local/state/dumpty/journal"}

`reply` is the text of the final turn. `squeezed` says whether that reply ran
in a context that could no longer grow, in which case it may be cut off.
`journal` names the directory the run's account was appended to.

Standard error carries the account as it happens, one journal record per line
in the same format `numpty log` reads: a `run_start` first, then the prompt,
each tool call and its result, the model's text and what each turn cost, and a
`run_stop` last. Watch it, pipe it through `jq`, or ignore it. The same
records are appended to the journal the result names, so a run whose standard
error nobody kept is still on the record.

A run that could not do the work prints nothing at all on standard output and
exits nonzero, so an empty standard output and a nonzero status is the whole
failure contract. `--timeout` bounds the exchange at ten minutes by default,
since a command left in a script must not hang the script. A prompt of `-` is
read from standard input.

## Browsing the journals

Every agent records what it did into an append-only journal: `humpty` each
interactive session, `numpty` each unattended run, and `dumpty` each one-shot
exchange. Each says where its account went when it exits, and `agentkit`
reads them all without loading a model:

    agentkit list                # which journals exist and what they hold
    agentkit runs                # one line per run, across every agent
    agentkit log dumpty --since 2h
    agentkit show humpty 12      # one run, laid out as the conversation it was

`log` takes `--since`, `--kind` and `--run`, and `show` takes `--full` to see
reasoning and tool output whole. A source may also be the path of a journal
directory, which is how a store copied off another machine is read. `humpty
log` and `numpty log` print the same records the same way, nearer to hand.

## Oxmono integration

The `agentkit` core and the DS4 and Apple adapters are vendored here. The
`openrouter_adapter` directory adds the same `Agent.S` interface for the
OpenRouter client in `bleeding/openrouter`. Programs can merge the three
runtime drivers with `Agentkit.Driver.merge`; use the prefixes `ds4/`,
`apple/`, and `openrouter/` when selecting a model. The native DS4 and Apple
Foundation Models libraries remain platform-specific; OpenRouter is the
portable network backend.

A program that already has the three native constructors can combine them as:

```ocaml
let providers =
  Agentkit_backends.registry
    ~ds4:(Agentkit_ds4.driver ~models:ds4_models ~create:ds4_create)
    ~apple:(Agentkit_apple_fm.driver ~create:apple_create ())
    ~openrouter:(Agentkit_openrouter.driver ~models:remote_models
                   ~create:remote_create ())
let choice = Agentkit.Driver.create providers "openrouter/openai/gpt-4o"
```

The OpenRouter adapter keeps conversation history, streams text/reasoning and
tool-call fragments, and reports the common Agentkit events. It deliberately
receives an already configured `Openrouter.t`, so API keys and Fetch policy
stay with the application.
