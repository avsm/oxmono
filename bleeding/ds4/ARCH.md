# Maintaining ds4

Notes for working on this repository. See `README.md` to use it.

The package was named `deepseek` until the engine grew GLM beside DeepSeek, and
is now `ds4` after the engine it binds. The agents `humpty`, `numpty` and
`dumpty` and the `agentkit` journal browser have moved to the `ocaml-agentkit`
repository, which depends on this package through opam. Notes below that name
`deepseek` are history.

## Layout

    lib/        the ds4 library: engine bindings, agent loop, tools
    lib_metal/  Metal implementation of the virtual backend
    lib_cuda/   CUDA implementation of the same
    lib_cpu/    CPU implementation of the same
    cli/        the model catalogue and what every command front-end repeats,
                including the list, download and chat subcommands
    dsml/       prompt encoding and reply parsing, in the DeepSeek V4, V4.1,
                GLM and Qwen markups, chosen by the dialect the agent reads
                from the loaded model
    csrc/       the vendored DS4 engine and the FFI stub
    bin/        ds4-agent
    test/       tests, some gated on DS4_LIVE
    test/cli    cram transcripts of what ds4-agent refuses

The repository builds one package, `ds4`. A change to the engine, the FFI, the
agent loop or the plain tools belongs here, and a change to how a command
presents or records an agent belongs in `ocaml-agentkit`.

`ds4.cli` is a sublibrary of the `ds4` package rather than part of the `ds4`
library, so the engine bindings depend on no command line. It holds the
catalogue that finds and downloads a model and what every front-end over the
engine repeats: the backend name, the guard, the seed, the logging terms, the
refusal of a model that is not there, and the subcommands that list, fetch and
query a model. The commands in `ocaml-agentkit` link it, so a model is found,
fetched and refused the same way whichever command a person runs.

`ds4-agent` links nothing outside the `ds4` package. `bin/ds4_agent.ml` only
assembles subcommands, and the `agent` one is `Ds4_cli.Coder`: its system
prompt, its tool list, the printer that shows an exchange, the loop that reads
prompts and the subcommand itself, each reusable apart from the rest. It is the
engine and the agent loop on a plain terminal, and is kept that small so that a
fault seen through it is a fault in this repository.

Ctrl-C is a `Sys.sigint` handler that calls `Agent.cancel` on the prompt in
flight, and exits when there is none. The handler only sets an atomic, which is
safe from a signal handler, and the agent reads it between tokens, so an
interrupt lands within one step of generation.
Each refusal of a model names `ds4-agent-<backend> download`, since that is the
command sure to be installed wherever the library is.

`ds4` is a virtual library. The backend is chosen at link time by
depending on `ds4.metal`, `ds4.cuda` or `ds4.cpu`, and each
supplies the same `Backend` module. The engine archive and link flags come from
whichever is linked, so the reported backend always matches the code.

## Building the CUDA backend

    DS4_CUDA=yes dune build

Dune's `enabled_if` can test the platform and the environment but cannot read
what a probe found, so nothing can detect a CUDA toolkit and switch the backend
on by itself. `DS4_CUDA=yes` is that switch, and it guards the library, the
executable and every compile rule, so a host without CUDA never runs `nvcc`.
`config/discover.ml` refuses any other value, because dune can only test for
equality and a typo would otherwise skip the backend without saying so.

`config/discover.ml` finds the toolkit and writes the flags. `DS4_CUDA_HOME`,
`DS4_NVCC` and `DS4_CUDA_ARCH` override what it finds, and `config/dune` lists
all four variables as `env_var` dependencies, since dune re-runs a rule only for
the variables that rule declares.

One flag is not upstream's. `-Xcompiler -fPIC` is needed because
`ds4_cuda.cu` has thread-local statics, which otherwise compile to the
local-exec model and emit relocations the linker refuses in a shared object.
The link names `-lstdc++` for the same reason nvcc would have: the file uses
`std::unordered_map` and C++ exceptions, and the final link is driven by
`ocamlopt`.

`DS4_CUDA_ARCH` defaults to `native`, which needs a GPU present at build time.
Set it (`sm_89` for an L4, `sm_90` for an H100) when building anywhere else.

## Vendored engine

`csrc/` holds a copy of the [DS4](https://github.com/antirez/ds4) engine, at the
revision recorded in `csrc/DS4_VERSION`. Everything there is verbatim upstream
apart from `ds4_stubs.c`, which is ours, and the local patches in
`csrc/logging-patch.pl`.

The patch routes the engine's diagnostics through an installable sink rather
than writing them to stderr, because a library inside another program should
not own its console. To move to a newer upstream:

    DS4_REF=main ./csrc/vendor.sh
    dune build

`vendor.sh` re-copies the sources and reapplies the patch with
`csrc/logging-patch.pl`. Every substitution is anchored on unique upstream text
and checked, so a moved anchor fails the vendor run rather than silently
dropping part of the patch.

Two consequences of the patch are worth knowing. Upstream printed most of its
diagnostics unconditionally and without a type, so the patch leaves the stub to
give each a level. One whose opening words report a failure, such as `failed`,
`cannot` or `requires`, is a warning, since a model that will not open is
explained only there and `V4.create` raises no more than that it could not
open the model. One ending in a carriage return is a progress line redrawn in
place and is debug, and the rest, the device, the mappings and the buffer
sizes, are informational and appear under `-v`. The list of words is in
`ds4_log_reports_failure`, and a failure worded without any of them is shown
only at info level. An error always reaches stderr, whatever the verbosity,
because the engine calls `exit` straight after reporting one.

`ds4_engram.c` is compiled with `engram_cflags`, which is `core_cflags` without
`-ffast-math`, as upstream's Makefile compiles it. It reproduces the Engram
tables' rounded values exactly, and fast-math would let the compiler change
them.

`ds4_cuda.cu` takes the same rewrite, and one thing besides: it is C++ and
includes no header that declares `ds4_diag`, so the patch adds a declaration of
its own. That declaration must be `extern "C"`, since the definition is in
`ds4.c` and a mangled reference would not resolve.

## FFI

`csrc/ds4_stubs.c` is the whole boundary. Five things about it constrain any
change:

The heavy calls release the runtime lock, so anything they need must be copied
out of the OCaml heap first. The garbage collector may move a string once the
lock is dropped. Grep for `DS4_ENTER_BLOCKING` and check that no `String_val`,
`Double_val` or `Field` survives inside one.

The lock and thread flags are thread-local, not process-wide. An engine runs on
its own domain while the program calls in from another, so a process-wide flag
would let one thread's state describe the other's. A stale reading of it would
call into OCaml without the runtime lock, which corrupts the heap.

A session holds a root on its engine. Freeing a session reads through the
engine, and the collector gives no ordering between their finalizers.

Handles report their real size with `caml_alloc_custom_mem`, so that dropping a
model or a KV cache creates collection pressure proportional to what it holds.

The session callbacks must touch no OCaml value. Progress fires on the engine's
worker domain inside a released section, so it stores two atomics and returns.
Cancellation reads another atomic. Reaching into OCaml from either callback
would mean taking the runtime lock in the middle of a prefill and handing the
result across a domain.

## Speculative decoding

`V4.create ~mtp` sets upstream's `glm_mtp`, which arms the multi-token
prediction head a GLM 5.3 or Qwen3.8 GGUF carries. DeepSeek's DSpark drafter
is a separate file and is not wired up. `V4.Session.eval_speculative` commits
the token the caller sampled and whatever drafted tokens the model verifies
after it, and the agent feeds them through the markup decoder one at a time,
stopping where sampling token by token would have stopped.

That leaves the cache holding tokens the transcript does not: a drafted stop
token, text after a finished tool call, or a tool call the ceiling cut and the
transcript dropped. `ds4_session_sync` refills from scratch whenever the cache
is not a prefix of the prompt, so the agent compares the two when a turn ends
and rewinds the cache to what they share. The engine keeps a snapshot of a
verify block's recurrent state, so rewinding the token or two a block
over-commits costs nothing. `live_speculative` checks both that and that
greedy text is the same with the head as without it.

## Compaction

`Agent.send` compacts the conversation, at most once a turn, when it no
longer fits at `max_ctx_size` and growth cannot help: either the context is
already at that ceiling, or a single tool result would not fit even in a
fresh one. The model writes a summary of its own conversation, greedily and
without reasoning, and the conversation is rebuilt as the system prompt, that
summary as a user turn, and a verbatim tail.

`Agent.cut_point` chooses where the tail begins. It never reaches into the
turn now in progress, which stays whole, and never asks the summary step for
more than a session of `ctx_size` can hold, which the point that triggered
compaction may already exceed on its own; within those two limits it keeps as
much recent history as a tenth of the context allows. A summary costs a fixed
wrapper on top of whatever the model writes, so compacting past a tool result
too big for an otherwise short conversation would only make it longer:
`Agent.compact` refuses when there is less to summarise than a bare minimum,
leaving the caller's own fallback, eliding the middle of the oversized result,
to run instead.

The rebuilt transcript shares no prefix with the old one from the summary
onward, so a compaction costs a fresh prefill of the tail, the same as
growing the context does. `live_compaction` drives a conversation into
compaction with a `ctx_size` fixed at `max_ctx_size`, so growth never rescues
it, and checks that the reported context never exceeds that ceiling, that
every compaction shrinks the conversation, and that the exchange goes on
working afterwards.

## Tests

    dune runtest                          # unit tests
    DS4_LIVE=1 dune runtest               # adds tests that load a real model

The suite depends only on this repository and its declared opam packages.
Install them with `opam install . --deps-only --with-test`. No checkout or
installation of `ocaml-agentkit` is needed.

Dune tracks the live-test environment and serialises model loads with a shared
lock. macOS runs the Metal live tests, and other systems run the CPU live tests.
Set `DS4_MODEL` to select a model. An enabled live test fails if its model is
missing. Use `--force` to repeat a run with unchanged settings.

The live tests need a downloaded model and several minutes. `live_lifetime`
covers engine and session finalization, and `live_agent_mock` drives the agent
loop with mocked tool I/O. `live_speculative` runs on the Metal build against
a GLM 5.3 or Qwen3.8 model, or the one `DS4_SPEC_MODEL` names. `test_scan_tools` checks that the filesystem tools
cannot escape the capability they are given, which is the property most worth
testing on a real filesystem rather than a mock. `test_agent_ctx` checks the
window arithmetic that decides when a turn has room to reply and how far the
context grows.

`test/cli` holds cram transcripts of what `ds4-agent-cpu` refuses before it
loads anything: a model path that names nothing, a directory given as a model,
a target that is not downloaded, and no model at all. Each refusal must say
what to do instead, and none may write to standard output.

Run one model at a time. A model of this size will not load twice at once.

## Converting a new DeepSeek build

DeepSeek ships FP8 safetensors, and DS4 runs its own GGUF quants in which only
the routed experts are quantised. A new build can land before anyone publishes a
matching GGUF, and in that window you can quantise it yourself, provided you
already have a DS4 GGUF of the same shape to use as a template. The template
supplies the metadata, tensor order and per-tensor types, so only the weights
change.

    # The original weights, about 167 GB.
    uvx hf download deepseek-ai/DeepSeek-V4-Flash-0731 --repo-type model \
      --local-dir ~/.local/share/ds4/hf/DeepSeek-V4-Flash-0731

    # Upstream's quantizer.
    git clone https://github.com/antirez/ds4 /tmp/ds4
    make -C /tmp/ds4/gguf-tools

    # Check the plan. "type_changes: 0" means the new weights map onto the
    # template's layout exactly.
    DS4=~/.local/share/ds4
    /tmp/ds4/gguf-tools/deepseek4-quantize \
      --hf $DS4/hf/DeepSeek-V4-Flash-0731 \
      --template $DS4/<an existing DS4 GGUF>.gguf \
      --out $DS4/<output>.gguf \
      --dry-run

    # The real run: drop --dry-run and add --threads. About 13 minutes on an
    # M3 Ultra with --threads 24.

Prefer a published quant when one exists. A local conversion has no imatrix,
which every published Flash and PRO quant uses, and an imatrix weights the
quantisation by measured activation statistics. The published imatrix is
calibrated against particular weights and does not transfer to a retrained
build. Generating a fresh one means running `ds4 --imatrix-dataset` against a
no-imatrix quant, then quantising again with `--imatrix`.

## Known limits

Artifacts are built with `-mcpu=native` and are not portable across Apple
Silicon generations.

Upstream's ROCm backend is not vendored.

A GPU serves the model from its own memory. A card that cannot hold the whole
model still runs it, because the engine registers the mapped GGUF for device
access and reads what does not fit across the bus, but it then pays that cost
for every token: a 24 GB card on the 81 GB `q2` measured 5.5 tokens per second
of prefill and 1.1 of generation, against 3.5 and 3.3 for the CPU backend on the
same machine. Size the card to the model.

One engine may be opened successfully per process. The backend's kernels are
located through the process environment, which a second engine would overwrite,
so `V4.create` refuses one. A failed open releases the reservation so a caller
can correct it and retry. Sessions and agents are not limited, and any number
may share the engine.

`--think max` is reduced to `high` below the engine's minimum context of 384K
tokens, as the C command line does.

An agent's context grows once a turn no longer leaves room for a `max_tokens`
reply, up to `max_ctx_size`. Growth turns on the room a whole reply needs
rather than on the prompt fitting, so a reply is not cut off while the window
could still have grown. Once it cannot grow further, the conversation is
compacted instead; see "Compaction" above. Only where neither helps does the
turn run in the room that is left and report a `Squeezed` event carrying its
budget, and only a prompt that no longer fits at all raises
`Context_exhausted`.

A reply is bounded by `max_tokens`, which is 2048 and which no command changes.
Generation stopping there and the model stopping there are different events, and
the loop reports which it was: a `Cut_off` event carries the ceiling and whether
a tool call was being written when it was reached. A call cut in half is
discarded, since there is nothing to run, and `Dsml.Stream.in_tool_call` is what
tells that half from a reply. What the model wrote of it is reported so the
account holds it, but kept out of the conversation, which carries a note saying
the call was discarded and that the next one must be smaller. The model then
takes another turn. Three turns running that end that way and call nothing raise
`Tool_call_cut_off`, because a model that will not make the call smaller does not
get there by repeating.

That note is the whole reason `append` exists. `write` takes a file as one
argument, so a file longer than a reply cannot be written at all with it, however
the model is prompted. Written in parts it can.

Growing throws the KV cache away. The engine cannot resize a session, so the
conversation moves to a new one, which shares no prefix with the old and must
prefill every token again. That is tens of thousands of tokens and minutes of
work in which nothing else happens, which is why the session reports how far it
has got. `V4.Session.prefill_progress` and `Agent.prefill_progress` are the
counters the engine's progress hook writes, read without entering the engine so
that a caller can poll them while the fiber doing the prefill is blocked. An
interface that polls them on its tick shows a long prefill as a long prefill
rather than as a hung process.
