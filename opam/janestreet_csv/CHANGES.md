## Release v0.17.0

- Exit gracefully from `csv grep`, `csv grid`, `csv validate`, `csv header` when stdout is
  closed unexpectedly, as happens when output is piped to, e.g., head
- Add `csv pp` as an alias for `csv pretty`
- Add multi-column sorting in `csv sort`
