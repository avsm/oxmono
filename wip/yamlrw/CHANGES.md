v1.0.0 (dev)
------

- Require OxCaml based on OCaml 5.2 or later.

- Recognize inline comments across reader slice boundaries and reject invalid
  lookahead offsets before reading input.
- Draining the remaining input consumes buffered bytes and advances source
  positions consistently for strings and reusable reader buffers.
- Avoid per-byte input cursor and scanner lookahead allocations with scalar
  cursor fields, nullable byte results and local scanner lookahead results.
- Read buffered lookahead directly and preserve borrowed reader slices when
  looking beyond them, including readers that reuse their backing buffer.

