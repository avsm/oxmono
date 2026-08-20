## Unreleased

- `Srcsetter.MS` is no longer `Map.Make (String)`. It keeps `empty`, `of_list`,
  `bindings`, `cardinal` and `fold`, and its type now declares a kind, so a
  `Srcsetter.t` can be read by a function marked `portable`. The JSON written
  and read is unchanged.

## v0.1.0 (2026-01-16)

Initial release.

- Core `srcsetter` library for managing responsive image metadata with JSON serialization
- `srcsetter-cmd` CLI tool for batch processing images into responsive WebP variants
- Support for multiple input formats: PNG, WebP, JPEG, JPG, BMP, HEIC, GIF, PDF
- Parallel image processing using Eio fibers
- Generates responsive images at standard breakpoints (320-3840px)
- Outputs `index.json` manifest with image dimensions and variants
