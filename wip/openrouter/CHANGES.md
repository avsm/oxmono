# Changes

- Clarify opam descriptions of package functionality and interfaces.

## Unreleased

- Support strict JSON Schema output in chat requests.

- Add native Eio chat, vision, tools and streaming over Fetch, with a checked-in
  OpenRouter schema and offline and Sequoia integration tests.
- Add `?reasoning_effort` to `Chat.request`, so callers can disable reasoning
  on OpenRouter and vLLM.
