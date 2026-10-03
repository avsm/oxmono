# ds4 -- local agents with the DS4 inference engine

OCaml bindings for the
[DS4 inference engine](https://github.com/antirez/ds4), which runs the
DeepSeek V4, DeepSeek V4.1 Flash, GLM 5.3 Flash and Qwen3.8 Flash Next language
models locally, plus `ds4-agent`, a plain command over them.

Requires OCaml 5. Metal needs macOS on Apple Silicon and the Xcode Metal
toolchain; CUDA needs Linux and the CUDA toolkit. Downloading a model needs
[`uv`](https://github.com/astral-sh/uv) or `hf` on your `PATH`.

The `ds4.metal` library also links on non-macOS platforms. Its engine calls
raise `Failure` with a message that Metal requires macOS. Pure helpers remain
usable. Link `ds4.cpu` to run inference without Metal.

## Commands

`ds4-agent` is built once per backend:

| Command           | Backend | Availability                     |
| ----------------- | ------- | -------------------------------- |
| `ds4-agent-metal` | Metal   | macOS on Apple Silicon           |
| `ds4-agent-cuda`  | CUDA    | Linux, with `DS4_CUDA=yes`       |
| `ds4-agent-cpu`   | CPU     | everywhere                       |

The examples below use `ds4-agent-metal`. Substitute `ds4-agent-cpu` to run
without a GPU.

`list` shows which models exist, `download` fetches one, `chat` sends one prompt
and prints the reply, and `agent` runs the model in a tool-using loop. A
`--model` naming no file is refused before anything is loaded.

`dune build` builds Metal on macOS and CPU everywhere. CUDA is off unless you
ask for it:

    DS4_CUDA=yes dune build

That compiles for the GPU doing the build. Set `DS4_CUDA_ARCH` (`sm_89` for an
L4, `sm_90` for an H100) to build for another one, and `DS4_CUDA_HOME` if the
toolkit is not on the `PATH`. A GPU must hold the whole model in memory to run
at full speed, so size the card to the model or use `ds4-agent-cpu`.

    dune build
    dune exec -- ds4-agent-metal list                  # what models exist
    dune exec -- ds4-agent-metal download q4           # fetch one
    dune exec -- ds4-agent-metal chat "Explain monads in one sentence."
    dune exec -- ds4-agent-metal agent -d ./workspace "Summarise README.md"

Run `ds4-agent-metal <command> --help` for the options of each.

## Choosing a model

For DeepSeek, use the 0731 build of Flash, DeepSeek's 2026-07-31 retrain. Pick
the largest quantisation your machine has memory for.

| Memory     | Alias  | Target               | Size    |
| ---------- | ------ | -------------------- | ------- |
| 256 GB+    | `q4`   | `q4-imatrix-0731`    | 153 GB  |
| 128 GB     | `q2q4` | `q2-q4-imatrix-0731` | 98 GB   |
| 96 GB      | `q2`   | `q2-imatrix-0731`    | 81 GB   |

DeepSeek V4.1 Flash is newer and larger. Its Engram tables stay on disk and are
read as needed, so it uses less memory than its size on disk. It runs on Metal
only.

| Memory     | Alias    | Target    | On disk | Resident |
| ---------- | -------- | --------- | ------- | -------- |
| 512 GB     | `v41-q4` | `ds41-q4` | 483 GB  | 294 GB   |
| 256 GB     | `v41-q2` | `ds41-q2` | 341 GB  | 152 GB   |

The Q4 is published in two parts, which `download` fetches and joins into one
file. Joining needs another 37 GB free.

GLM 5.3 Flash and Qwen3.8 Flash Next run on the same engine. Qwen's n-gram
tables stay on disk as V4.1's Engram tables do, and it runs on Metal only.

| Memory     | Alias     | Target      | On disk | Resident |
| ---------- | --------- | ----------- | ------- | -------- |
| 256 GB+    | `glm-q4`  | `glm53-q4`  | 178 GB  | 178 GB   |
| 96 GB+     | `glm-q2`  | `glm53-q2`  | 90 GB   | 90 GB    |
| 128 GB+    | `qwen-q4` | `qwen38-q4` | 165 GB  | 70 GB    |
| 64 GB+     | `qwen-q2` | `qwen38-q2` | 137 GB  | 42 GB    |

Each of GLM 5.3, DeepSeek V4.1 Flash and Qwen3.8 Flash Next also has a small
vision sidecar (`glm-vision`, `v41-vision`, `qwen-vision`, all under 1.2 GB),
which `agent --vision FILE` loads beside the language model.

DeepSeek V4 Flash Vision Experimental is a checkpoint of its own, frozen
before the 0731 retrain, rather than the regular Flash weights with a sidecar
added. All three of its quants share one sidecar, `ds4f-vision-encoder`
(alias `vision-exp-encoder`).

| Memory    | Alias          | Target              | Size   |
| --------- | -------------- | ------------------- | ------ |
| 96 to 128 | `vision-q2`    | `ds4f-vision-q2`    | 81 GB  |
| 128 GB    | `vision-q2q4`  | `ds4f-vision-q2-q4` | 91 GB  |
| 256 GB+   | `vision-mxfp4` | `ds4f-vision-mxfp4` | 145 GB |

Models are stored under `$XDG_DATA_HOME/ds4`. With no `--model`, the best
DeepSeek build you have downloaded is used, V4.1 ahead of V4 at the same
quantisation, so `chat` needs no arguments after `download`. A GLM or Qwen
model is chosen by asking for it, as in `chat -m qwen-q4`.

A target is named for the build it carries, and the short aliases follow the
newest one. Asking for `q4` gets whichever build is current, while
`q4-imatrix-preview` still names the first Flash release. `list` dims the
builds a newer one has superseded.

Two things affect the choice. The quantisations above are built with an
imatrix, which weights the quantisation by measured activation statistics.
`mxfp4-0731` is not, so despite being larger it is not necessarily better.
Memory use is also more than the model's size, because the KV cache grows with
the context, and on Apple Silicon the GPU can address only about 75% of memory
by default. `sysctl iogpu.wired_limit_mb` reports that limit.

The much larger PRO models are also listed, and need 512 GB.

## Chatting

`chat` sends one prompt and prints the reply.

    ds4-agent-metal chat "Hello?"
    ds4-agent-metal chat -m q2 "Hello?"                # a particular model
    ds4-agent-metal chat -m /path/to/model.gguf "Hello?"
    ds4-agent-metal chat --think high "Prove it."      # reason before answering
    ds4-agent-metal chat --ctx 16384 "..."             # a larger context

The model keeps nothing between runs. A `--model` that names no file is refused
before anything is loaded.

GLM 5.3 and Qwen3.8 Flash Next carry a draft head, which `chat` and `agent`
arm on a GPU backend so that each step can commit tokens it drafted as well as
the one it sampled. A Qwen agent runs about a quarter faster for it. Greedy
decoding gives the same text either way, and `--no-mtp` turns it off.

`--think` opens the conversation with the model's own reasoning-effort
instruction, so a reasoning model reasons in the form it was trained for.

## The agent

`agent` puts the same model in a loop with tools, so it can work across several
turns.

    ds4-agent-metal agent -d ./workspace "Rename Foo to Bar in lib/"
    ds4-agent-metal agent -d ./workspace < prompts.txt

If the workspace holds an `AGENTS.md`, it joins the system prompt, so a project
states its conventions once. Ctrl-C interrupts a reply and withdraws its
prompt, and a second Ctrl-C at the prompt leaves.

With a prompt argument it runs that one exchange and exits. Without one it reads
prompts from standard input, one per line, into one conversation, until the end
of input. At a terminal it shows a `?` prompt once the model has loaded, and
Ctrl-D ends the session. The reply goes to standard output. The prompt, each
tool call, the first line of its result, and any change to the context go to
standard error, so a reply can be captured on its own.

`list`, `tree`, `read`, `read_lines`, `find`, `grep` and `stat` read the
filesystem, `edit` changes part of a file, `write` creates one or replaces the
whole of one and `append` adds to the end of one. There is no shell.

`--vision FILE` loads the sidecar that matches the model and adds `view_image`,
confined to the same capability as the other file tools. GLM 5.3, DeepSeek
V4.1 Flash and Qwen3.8 Flash Next each publish one, as the `glm53-vision`,
`ds41-vision` and `qwen38-vision` download targets.

`grep` quotes each matching line as it is in the file, so it can be quoted back
to `edit`. `write` and `edit` replace a file by renaming a new one over it, so
an interrupted change leaves the old file whole, and `edit` answers with the
changed lines numbered and how far the lines after them moved.

The filesystem tools reach only what they are granted. The agent starts with a
capability for the workspace given by `-d`, and any directory outside it must be
requested with `open_dir`. The grant is reported on standard error. A capability
rejects `..` and symlinks that lead out of it, so no path the model supplies can
escape.

A reply is bounded at 16384 tokens, which `--max-tokens` sets, and a tool call the model does not finish
inside that is discarded rather than half made. The agent tells the model and
gives it another turn, which is what `append` is for. A call that is malformed,
rather than unfinished, is reported and retried. Work can be cancelled
cooperatively.

The agent starts with a context of 32768 tokens, which `--ctx` sets. A
conversation that outgrows it moves to a larger one, up to `--max-ctx`, without
reloading the model. Once it cannot grow further, the model is asked to
summarise its own conversation, which is then replaced by that summary and a
verbatim tail of the most recent exchanges, so a long-running task keeps
going rather than stopping.

## Library

The `ds4` library is the same functionality without the command line.
`V4.generate` sends one prompt, `V4.Session` keeps a conversation, `Agent` adds
the tool loop, and `Toolbox` provides the tools. The backend is chosen by
depending on `ds4.metal`, `ds4.cuda` or `ds4.cpu`. `ds4.cli` holds the model
catalogue, the `list`, `download` and `chat` subcommands, and in `Coder` the
parts of the `agent` subcommand, and `ds4.dsml` the
prompt encoding and reply parsing. See the interface files in `lib/`, `cli/` and
`dsml/`.

## Maintenance

See [ARCH.md](ARCH.md) for the repository layout, the vendored engine and how to
update it, the FFI, and how to quantise a DeepSeek release yourself.

## Local patches in oxmono

- `lib_metal_unsupported` supplies `ds4.metal` on non-macOS systems. It keeps
  the primitive signatures from `csrc/ds4_stubs.c` but raises on engine calls.
  When updating the FFI, update both sets of signatures and run Agentkit's
  `test_core` alias, which checks that repeated failed opens remain safe.

- `Tool.raw` in `lib/tool.ml` and `lib/tool.mli` wraps a tool that already has
  a JSON schema and a string handler. `Agentkit_ds4` uses it to run Agentkit's
  backend-neutral tools. It is not upstream.
- `spawn_worker` in `lib/v4.ml` runs engine jobs on the calling domain rather
  than through `Eio.Domain_manager`, because a stream captured by the worker
  domain is not portable under OxCaml. It is not upstream.
