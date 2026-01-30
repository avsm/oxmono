## Release v0.17.0
* Migrate from `utf8_text` to `Core.String.Utf8`. Users of this library should follow
  suit. `utf8_text` is deprecated.
* Fix stack overflow in `Text_block.render` on large inputs

## Release v0.16.0

* Added `Text_block.span_banner` to indicate boundaries of text content
