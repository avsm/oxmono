# Binding design

## Framework surface

An OCaml agent uses the following parts of Foundation Models:

1. `SystemLanguageModel.availability` to distinguish unsupported hardware,
   disabled Apple Intelligence, and a model that is still downloading.
2. A persistent `LanguageModelSession` so prompts, responses, tool calls, and
   tool outputs remain in one transcript across turns.
3. `streamResponse` because an interactive agent must display text before a
   complete response is available.
4. `GenerationOptions` for sampling, temperature, response limits, and tool
   calling policy.
5. `Tool` plus `DynamicGenerationSchema` so OCaml can define tools at runtime
   and receive guided JSON arguments.
6. Codable transcripts and token accounting for persistence and context
   management.
7. `GeneratedContent`, context options, capabilities, and image attachments for
   structured, reasoning, and multimodal turns.

The coding agent uses a small, direct instruction and four focused tools. This
follows Apple's guidance for the smaller on-device model: keep prompts concise,
reduce conditional reasoning, split complicated work into simpler steps, and
remember that tool definitions consume context.

The agent measures the transcript before and after turns, reserving enough room
to ask for a durable summary before the context is full. Compaction reconstructs
the transcript through Apple's public `Transcript.Entry` types: it retains the
original instructions and tool definitions, replaces any older summary, and
keeps a bounded number of recent complete turns. A fresh session restores the
result, which works on macOS 26 without the macOS 27 mutable-transcript API.
Tool observations are capped because a single unbounded command result can
otherwise cross the compaction reserve inside an active response.

## ABI boundary

Foundation Models has no C or Objective-C API. Its sessions, async sequences,
generic `Tool` protocol, and generated-content types are Swift-only. A direct
`ctypes` or Core Foundation binding therefore cannot reach the framework.

`AppleFMBridge.swift` owns every framework value. It exports a narrow C ABI of
opaque session pointers, UTF-8 strings, integers, and doubles. Swift compiles to
a static archive which Dune links with the C stub into native consumers. Apple
system libraries satisfy the Swift and Foundation Models references. There is
no private dynamic library to locate at runtime.

`cf` is useful when an Apple framework exposes `CFTypeRef` objects. It does not
help at this boundary because none of the Foundation Models objects are Core
Foundation types.

## Concurrency and tools

Model generation runs in a Swift `Task`. A condition-backed queue carries four
event types to the OCaml caller: text delta, tool request, completion, and
error. Eio waits on that queue with `Eio_unix.run_in_systhread`, leaving the Eio
domain free to run other fibers. The C stub also releases the OCaml runtime lock
while the system thread waits.

Each Swift `DynamicTool` converts its `GeneratedContent` arguments to JSON,
places a request on the queue, and suspends on a checked continuation. The
responding Eio fiber finds the named tool, parses the JSON, invokes the handler,
and resolves the continuation with generated JSON content for the model. Tool
handlers can suspend using Eio normally. Swift never calls an OCaml closure
from an unmanaged thread.

Sessions take an `Eio.Switch.t` and close automatically when the switch is
released. Response deltas are written to an `Eio.Flow.sink`. Eio cancellation
propagates to the Swift generation task and closes that session. A fresh session
is required after cancellation because immediate reuse of Apple's cancelled
`LanguageModelSession` can trap inside the framework. An `Eio.Mutex` serializes
turns on each session without blocking the domain.

Typed tool codecs use `jsont` to decode calls and map to Apple's runtime
schemas:

| OCaml schema | Apple schema |
| --- | --- |
| `string` | `String` |
| `integer` | `Int` |
| `number` | `Double` |
| `boolean` | `Bool` |
| `array` | `DynamicGenerationSchema(arrayOf:)` |
| `object_` | Named properties with optionality |
| `one_of` | A guided string choice |
| `null` | Explicit generated null |
| `any_of` | A choice among schemas |
| `reference` | A named schema dependency |

Scalar schemas also carry string and numeric generation guides. Schema
documents keep the root separate from named dependencies, matching
`GenerationSchema(root:dependencies:)`.

Fixed bridge payloads are described by typed, encoding-only `jsont` codecs and
are written directly to the strings accepted by the C ABI with
`jsont.bytesrw`. Typed union values use `Jsont.json` internally to try their
alternative codecs, and transcript validation uses it because the public API
intentionally treats a transcript as opaque JSON.

The binding has no dependency on an agent framework. Direct users and adapters
use the typed `Codec` constructors and member-by-member `Codec.Object` or
`Codec.Invoke` builders, which derive the Apple schema and jsont decoder
together. The public API does not attach arbitrary schemas to arbitrary
decoders because jsont codecs are abstract and the correspondence could not be
checked. Scalar and array codecs also enforce their Apple generation
constraints while decoding locally. Recursive codecs carry their named Apple
dependencies alongside a recursive jsont decoder, so nesting them in arrays,
objects, or unions does not lose the dependency document. `Tool.invoke_json`
exposes the generated-content JSON which crosses the Swift boundary, so an
adapter can observe invocations without either package depending on the other.

## Version boundaries

The baseline remains the on-device system model on macOS 26. Transcript
persistence and guided responses work there. Token counting and explicit null
schemas require macOS 26.4. Model capabilities and variants, usage statistics,
reasoning options, image attachments, and in-place transcript replacement
require macOS 27. Calls that request unavailable functionality raise a typed
``Error.E (`Unsupported_version _)`` Eio exception.

Private Cloud Compute, feedback attachments, dynamic profiles and hooks, and
custom model executors remain outside the package. They are independent of the
on-device agent interface and can be added as separate modules without changing
the event protocol.
