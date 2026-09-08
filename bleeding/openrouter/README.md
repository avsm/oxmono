# Openrouter

An Eio client for OpenRouter and compatible inference servers, using a
caller-supplied Fetch capability. The public interface is
[`Openrouter`](lib/openrouter.mli). Generated protocol bindings are private.

## Chat

```ocaml
let ask fetch ~api_key ~model prompt =
  let client = Openrouter.of_fetch ~api_key fetch in
  let request =
    Openrouter.Chat.request ~model ~max_tokens:256
      ~messages:[ Openrouter.Message.user prompt ] ()
  in
  Openrouter.Chat.complete client request
```

The result contains native choice, tool-call, finish-reason and usage records.
`Models.list client` returns model identifiers and available context limits.
For a compatible server, pass its complete API prefix, for example
`~base_url:"http://sequoia.cl.cam.ac.uk:8000/v1"` without an API key.

## Streaming and Eio lifetimes

```ocaml
let stream_text client request stdout =
  Openrouter.Chat.stream client request ~on_event:(function
    | Openrouter.Chat.Text { choice = 0; text } ->
        Eio.Flow.copy_string text stdout;
        `Continue
    | _ -> `Continue)
```

`Chat.stream` selects streaming automatically. Events include `Started`,
`Text`, `Reasoning`, `Refusal`, `Tool_call`, `Finished` and `Usage`. The
callback runs in the calling fiber and provides backpressure. It may perform
Eio operations. Returning `Stop` closes the response and returns `Stopped`.
Normal termination requires the protocol's completion sentinel and returns
`Complete`. Premature EOF is an error. A choice's `Finished` event can precede
the final usage event.

Requests create no background fibers or transport backend. Cancellation and
callback exceptions propagate after response cleanup. Use
`Eio.Time.with_timeout_exn clock seconds (fun () -> ...)` for a deadline.
The caller's Fetch stack owns connections, retries and concurrency policy.
The library adds no retries or stream reconnection.

HTTP, stream and protocol failures use `Eio.Io (Openrouter.E error, context)`.
Fetch transport, policy and decoding failures keep their Fetch error identity.
Response bodies default to a 16 MiB bound and SSE blocks to 1 MiB. Configure
these with `of_fetch ~max_response_bytes` and `Chat.stream ~max_event`.

## Vision and images

```ocaml
let describe client ~model image_flow =
  let image = Openrouter.Image.of_flow ~format:Png image_flow in
  let message =
    Openrouter.Message.user_parts
      [ Openrouter.Content.text "Describe this picture.";
        Openrouter.Content.image image ]
  in
  let request =
    Openrouter.Chat.request ~model ~max_tokens:256 ~messages:[ message ] ()
  in
  Openrouter.Chat.complete client request
```

`Message.user_parts` preserves text/image order and supports multiple images.
The same request works with `Chat.stream`. `Image.of_string` embeds bytes
already in memory. `Image.of_flow` reads an Eio source with a default 20 MiB
limit before base64 encoding and leaves it open. Both accept `~max_bytes`.
Oversized input raises `Openrouter.E (Image_too_large { limit })` inside
`Eio.Io`. Empty input is rejected. The caller identifies PNG, JPEG, WebP or
GIF content. The library does not decode pixels or verify the claimed format.

`Image.of_url "https://..."` sends an image URL for the model provider to
fetch. It performs no local fetch. Local Fetch restrictions and credentials
do not govern the provider's request. Use an explicit Eio file or Fetch
response capability with `Image.of_flow` to send locally obtained bytes.
No filesystem paths are opened by the library.

Image detail is optional: `Auto`, `Low`, `High` or `Original`. The last is an
OpenRouter extension. Model/provider support determines accepted formats,
image counts, sizes and detail levels. A model identifier alone does not
establish vision support.

## Function tools

Declare tools with `Tool.v ~name ~parameters:json_schema ()`. Pass them through
`Chat.request ~tools` and optionally select a native `tool_choice`.
Non-streaming choices carry completed `Tool.call` records. Decode arguments
with `Tool.arguments codec call`, validate them and authorize execution in
the application. The library never executes tools.

Streaming tool events carry fragments. Accumulate arguments by choice and
tool index, retaining the initial identifier/name, before decoding them.
Continue the conversation with `Message.assistant ~tool_calls ...` followed
by `Message.tool_result ~tool_call_id ...`.

## Capabilities and compatibility

All API requests are restricted to the supplied base URL prefix. Optional
bearer credentials and application headers share that scope. Bearer keys
require HTTPS. Writes do not follow redirects. Existing restrictions on the
injected Fetch client remain in force.

Requests use schema-validated generated codecs. Public response codecs
validate the fields exposed by the native interface and ignore unrelated
metadata. Optional null values and vLLM's `reasoning_content` alias are
accepted. Unknown finish reasons are preserved as `Other string`.

The current public API covers model discovery, text/vision chat, function
tools, reasoning text, usage and streaming. Native Responses, embeddings,
audio/video/document input, image generation/output, provider-routing
controls, structured-output options and account/admin APIs remain unfinished.
Reasoning-detail blocks and provider-specific metadata are not retained.
Tool argument aggregation and automatic tool loops are also left to callers.

## Schema and regeneration

[`openapi.json`](openapi.json) is a sanitized snapshot downloaded from
<https://openrouter.ai/openapi.json> on 2026-09-08. It declares OpenAPI 3.1.0,
API version 1.0.0 and the MIT license, with 797 component schemas.
Six full key-shaped examples and twelve abbreviated key labels are replaced
with `OPENROUTER_API_KEY_EXAMPLE`. All other schema content is unchanged.
The checked-in snapshot's SHA-256 is
`545bc333c15fd04ebd623bd081f7a6d50132c6d4dcf848c87ff1c9aff86fe609`.
The original download's SHA-256 was
`99c7d7b960ec3a68ba6678af1a3bf536b999ae9ae36377e337a3c43986d2225e`.

Dune generates private bindings from the snapshot without network access.
`normalize.py` corrects one upstream dialect error in a build-only copy:
`VideoGenerationRequest.upscale_factor` uses the 3.0 spelling
`minimum: 0, exclusiveMinimum: true` inside a 3.1 document. It is translated
to `exclusiveMinimum: 0`, preserving the strict bound. The generic generator
continues to reject invalid dialect constraints.

To update, download outside the repository and sanitize before importing:

```sh
curl --fail --location https://openrouter.ai/openapi.json -o /tmp/openrouter-openapi.json
python3 bleeding/openrouter/sanitize.py /tmp/openrouter-openapi.json > bleeding/openrouter/openapi.json
python3 bleeding/openrouter/sanitize.py --check bleeding/openrouter/openapi.json
```

Record the download date and original/sanitized hashes here and run the scoped
tests. The test alias rejects key-shaped examples in the checked-in snapshot.
Review or remove the normalization when upstream changes that field.
The generator's `--fetch-only` option omits backend construction and curl
dependencies. Declared SSE responses produce callback-based stream operations.

## Build and test

From the monorepo root:

```sh
opam exec --switch=5.2.0+ox -- dune build @bleeding/openrouter/all
opam exec --switch=5.2.0+ox -- dune runtest \
  bleeding/openrouter bleeding/openapi --force
```

The offline tests cover native codecs, image/tool requests and responses,
stream ordering, cancellation, closure, limits, errors and credential scope.
Live probes are opt-in and read no API key from the environment:

```sh
opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/openrouter/test/live/probe.exe -- \
  http://sequoia.cl.cam.ac.uk:8000/v1 Qwen/Qwen3.8-27B-FP8
opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/openrouter/test/live/vision.exe -- \
  http://sequoia.cl.cam.ac.uk:8000/v1 Qwen/Qwen3.8-27B-FP8 \
  bleeding/openrouter/test/red.png
```

The vision fixture is a synthetic 224-by-224 red PNG. The probe checks that
ordinary and streamed image responses identify its color.
Both live probes passed on Sequoia on 2026-09-08, including ordinary/streamed
usage, early stop, cancellation and subsequent client reuse.

The CLI example accepts `BASE_URL MODEL PROMPT [--stream]`:

```sh
opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/openrouter/examples/chat.exe -- \
  http://sequoia.cl.cam.ac.uk:8000/v1 Qwen/Qwen3.8-27B-FP8 'Say hello' --stream
```

The example reads `OPENROUTER_API_KEY` when set. Unset it for this HTTP test
server. Applications choose their own Fetch backend. Only examples and live
probes depend on `fetch-curl` and `eio_main`.
