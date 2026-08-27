## Unreleased

- The unused GIF to animated WebP conversion is gone. A GIF is copied to the
  destination whole and given no variants, which is what the pipeline has done
  for some time, because animated WebP is not rendered reliably enough to
  convert to.
- `Srcsetter.MS` is no longer `Map.Make (String)`. It keeps `empty`, `of_list`,
  `bindings`, `cardinal` and `fold`, and its type now declares a kind, so a
  `Srcsetter.t` can be read by a function marked `portable`. The JSON written
  and read is unchanged.
- Every value in `Srcsetter` is now `portable` except the four JSON codecs,
  which are built from `Jsont` combinators that carry no modality. A function
  marked `portable` can read an image entry and walk its variants.

## v0.1.0 (2026-01-16)

Initial release.

- Core `srcsetter` library for managing responsive image metadata with JSON serialization
- `srcsetter-cmd` CLI tool for batch processing images into responsive WebP variants
- Support for multiple input formats: PNG, WebP, JPEG, JPG, BMP, HEIC, GIF, PDF
- Parallel image processing using Eio fibers
- Generates responsive images at standard breakpoints (320-3840px)
- Outputs `index.json` manifest with image dimensions and variants
