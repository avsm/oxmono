# Changes

- Share named encrypted credentials through `sekrit:NAME` and profile
  `secret_ref` fields. Resolve lazily at connection time and preserve references
  in the CLI, profile example and Mosaic forms. Legacy literal profiles still load.

- Read private credentials through Sekrit with one-line, ownership and size checks.
  Refuse symlinks, preserve password spaces and redact all secret characters.

- Add an account- and origin-confined mail capability with bounded batch reads,
  state anchors and incremental changes. Crow now shares this implementation.
- Share resource-free HTML mail text extraction with Termanil and Siftbot.

- Add typed calendar codecs and Chain reads, with source-preserving archival
  reads through the ordinary response decoder.

- Add a storage-independent mirror state machine with validated cursor codecs.
