# Model backend boundary

Agentkit owns the durable agent vocabulary. `ds4` and `apple-fm` remain
independent model libraries. A program may link either adapter or both without
making one model library depend on the other.

## Packages

`agentkit` contains common events and operations, journals, memory, schedules,
and the journal browser. It links no model runtime.

`agentkit-ds4` converts every DS4 event and exposes a `Ds4.Agent.t` through
`Agentkit.Agent.S`. The existing commands use this adapter without changing
their command line or journal representation.

`agentkit-apple-fm` owns an Apple session and exposes the same common
operations. Its tool constructor wraps Apple codecs and handlers so calls and
complete results enter the event stream. It also provides transcript snapshots,
replacement, and model-written compaction. The package is available only where
`apple-fm` is available.

`agentkit-apple-tools` specializes the commands' existing workspace, network,
and memory handlers with Apple-native tool codecs. An unsupported tool name is
refused. It does not define a common tool schema.

Adapters are ordinary libraries rather than implementations of a Dune virtual
library. One executable may therefore link both and choose at run time.
`Agentkit.Driver` merges the adapters' model lists and dispatches a qualified
choice such as `ds4/q4` or `apple/default`. Each adapter's constructor receives
the selected model and builds tools with that backend's native codec. The
registry only sees an agent through `Agentkit.Agent.S` after construction.
The command-line term accepts `--model DRIVER/MODEL`. Unqualified DS4 names
remain accepted by the commands for compatibility. `humpty list` merges the
available model lists. `download` and `chat` remain DS4 commands.
Every agent command also exposes `models list`, `models show`, and `models
fetch`. The first two inspect the registered drivers. Fetch dispatches to the
selected driver only when requested. The Apple system model is managed by
macOS and has no fetch operation.

## Common surface

Agentkit owns `stats`, `cut`, `compaction`, `tool_call`, `event`, and the
`Agent.S` operations. Their shape preserves the version 1 journal JSON. An
adapter uses zero for a measurement its backend cannot provide.

Construction is backend-specific. DS4 selects an engine, model file, context
growth policy, and DSML tools. Apple selects a system model, generation and
context options, and Foundation Models tools. The driver constructor keeps
these controls and codecs in the adapter selected for the model.

Apple tools must be created through `Agentkit_apple_fm.Tool` when their traffic
must be journalled. The Foundation Models API does not expose raw calls made to
an already constructed `Apple_fm.Tool.t`, so the adapter cannot instrument one
after the fact.

## Event guarantees

Text is streamed as `Content`. DS4 also reports reasoning. A successfully
decoded tool invocation reports `Tool_call` before its handler runs and one
`Tool_result` after every ordinary return or exception. Cancellation and fatal
runtime exceptions escape the handler and may leave the final call without a
result because the exchange has ended.

Apple re-encodes the decoded value through the tool codec for the call event.
This makes its arguments canonical JSON and includes defaulted members. A call
whose arguments Apple cannot decode never reaches the handler and cannot be
observed through the public Foundation Models API.

`Stats` followed by `Done` completes a successful exchange. Apple supplies
cumulative usage only on macOS 27. The adapter uses transcript token counting
where available and zero where the operating system provides no measurement.
Apple does not report generation or prefill time, so those fields are zero.
It does not expose model-turn boundaries, so `turns` is also zero.

## Compatibility checks

The standalone `agentkit` package builds without DS4 or Apple Foundation
Models. Version 1 journal fixtures decode unchanged. Each adapter type-checks
against `Agentkit.Agent.S`. Adapter unit tests do not load a model. DS4 live
tests remain gated by `DS4_LIVE=1`, and Apple live tests belong behind a
separate `APPLE_FM_LIVE=1` gate so the two large model runtimes are never
loaded together.
