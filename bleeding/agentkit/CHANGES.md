# unreleased

- Bound pinned memory and journal text in numpty briefs. Share call budgets
  across native and returned tools and check request text before inference.

- Add OptMem-inspired OCaml summary trees and bounded episodic numpty briefs,
  with persistent derived summaries and expandable source records.

- Build the Apple adapters on non-macOS platforms. Model operations report
  that Foundation Models requires macOS.

- Add `Chat.User_images` and `Chat.image_of_string`, which the OpenRouter
  adapter sends as image parts and the DS4 adapter notes as omitted.
- Add `Agentkit.Chat`, one model request over a supplied transcript with
  per-request tools, token budget and reasoning effort, and `complete`
  functions in the OpenRouter and DS4 adapters.
- Add `Agentkit.Turn`, a tool loop with a call allowance, a fail-closed guard,
  bounded results, a tool-free answer request and a fallback that is never
  blank.
- Add `Agentkit.Summary`, a bounded summary request that disables reasoning,
  asks for words and accepts fenced JSON.
- Fix the streaming OpenRouter agent, which split a tool call whose later
  fragments omit its id. It now runs tools and reports cut-off replies.

- Color model statuses on terminals and add `--color=auto|always|never` to the
  shared `models` subcommands. Piped output and `NO_COLOR` remain plain.

- Add shared `models list`, `models show`, and explicit `models fetch` commands
  to humpty, numpty, and dumpty. Model selection does not download weights.

- Let humpty, numpty, and dumpty select `ds4/MODEL` or `apple/default` at run
  time. `humpty list` shows both drivers where Apple Foundation Models is available.

- Add a model driver registry with `driver/model` selection and native tool
  construction for DS4 and Apple Foundation Models.

- Add backend-neutral agent events and common operations, with `agentkit-ds4`
  and `agentkit-apple-fm` adapters. Existing version 1 journals are unchanged.
- Add Apple streaming, instrumented tools, accounting, cancellation,
  transcripts, and compaction without linking either runtime into `agentkit`.

- Follow ds4's agent changes: speculative decoding adds a `drafted` field to
  `Agent.stats`, read as 0 from a journal written before it existed, and a
  full conversation is now compacted rather than stopping, journalled as a
  new `Journal.Compacted` kind and shown by every command that prints one.

- Split from ocaml-deepseek, whose `ds4` package is now a dependency. The
  `ds4.agentkit` library is renamed `agentkit`, and the `agentkit` command
  moves to a package of that name.
- `list`, `download` and `chat` in humpty are `ds4.cli`'s, shared with
  `ds4-agent`. A `--model` naming no file is refused with the same message in
  every command.
