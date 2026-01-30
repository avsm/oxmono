## Release v0.17.0
- Allow logging of structured message payloads from `ppx_log`, and encourage users to use
  `ppx_log` instead of direct functions in this library.
- Export a `Async_log.Ppx_log_syntax.Ppx_log_syntax`
- Create `async_log_kernel` for logging outside of unix.
- Remove the background processing loop that causes all log statements to be asynchronous.

## Release v0.16.0

- Initial release that splits out the `Log` module from `Async_unix`.
