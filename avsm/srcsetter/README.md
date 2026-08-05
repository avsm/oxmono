# srcsetter

Responsive image generation for HTML srcset attributes.

## Overview

Srcsetter processes a directory of images and outputs responsive variants
suitable for embedding as `<img srcset>` tags in websites. It uses ImageMagick
for image processing and outputs WebP format.

## Packages

- **srcsetter** - Core library for image entry management
- **srcsetter-cmd** - CLI tool for batch processing

## Installation

```
opam install srcsetter     # Library only
opam install srcsetter-cmd # CLI tool (includes library)
```

## Usage

### Library

```ocaml
(* Load entries from JSON *)
match Srcsetter.list_of_json json_string with
| Ok entries ->
    List.iter (fun entry ->
      let name = Srcsetter.name entry in
      let (w, h) = Srcsetter.dims entry in
      Printf.printf "%s: %dx%d\n" name w h;
      (* Access variants *)
      Srcsetter.MS.iter (fun variant_name (vw, vh) ->
        Printf.printf "  %s: %dx%d\n" variant_name vw vh
      ) (Srcsetter.variants entry)
    ) entries
| Error msg -> Printf.printf "Error: %s\n" msg
```

### CLI

```bash
srcsetter process input_dir/ output_dir/
```

## Image Entry Structure

Each entry tracks:
- **name** - Output filename (e.g., `photo.webp`)
- **slug** - URL-safe identifier
- **origin** - Original source file path
- **dims** - Base image dimensions (width, height)
- **variants** - Map of variant filenames to dimensions

## JSON Format

```json
[
  {
    "name": "photo.webp",
    "slug": "photo",
    "origin": "photos/DSC_1234.jpg",
    "dims": [1920, 1080],
    "variants": {
      "photo-640.webp": [640, 360],
      "photo-1280.webp": [1280, 720]
    }
  }
]
```

## Requirements

- ImageMagick CLI tools

## License

ISC
