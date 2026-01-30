## v0.1.0 (2026-01-16)

Initial release.

- Core `srcsetter` library for managing responsive image metadata with JSON serialization
- `srcsetter-cmd` CLI tool for batch processing images into responsive WebP variants
- Support for multiple input formats: PNG, WebP, JPEG, JPG, BMP, HEIC, GIF, PDF
- Parallel image processing using Eio fibers
- Generates responsive images at standard breakpoints (320-3840px)
- Outputs `index.json` manifest with image dimensions and variants
