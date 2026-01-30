# Bushel

Personal knowledge base and research entry management for OCaml.

Bushel is a library for managing structured research entries including notes,
papers, projects, ideas, videos, and contacts. It provides typed access to
markdown files with YAML frontmatter and supports link graphs, markdown
processing with custom extensions, and search integration.

## Features

- **Entry Types**: Papers, notes, projects, ideas, videos, and contacts
- **Frontmatter Parsing**: YAML metadata extraction using `frontmatter`
- **Markdown Extensions**: Custom `:slug`, `@handle`, and `##tag` link syntax
- **Link Graph**: Bidirectional link tracking between entries
- **Typesense Integration**: Full-text search indexing
- **Eio-based I/O**: Async directory loading with Eio

## Subpackages

- `bushel`: Core library with entry types and utilities
- `bushel.eio`: Eio-based directory loading
- `bushel.config`: XDG-compliant TOML configuration
- `bushel.sync`: Sync pipeline for images and thumbnails
- `bushel.typesense`: Typesense search schema definitions

## Installation

```bash
opam install bushel
```

## Usage

```ocaml
(* Load entries using Eio *)
Eio_main.run @@ fun env ->
let fs = Eio.Stdenv.fs env in
let entries = Bushel_loader.load fs "/path/to/data" in

(* Look up entries by slug *)
match Bushel.Entry.lookup entries "my-note" with
| Some (`Note n) -> Printf.printf "Title: %s\n" (Bushel.Note.title n)
| _ -> ()

(* Get backlinks *)
let backlinks = Bushel.Link_graph.get_backlinks_for_slug "my-note" in
List.iter print_endline backlinks
```

## CLI

The `bushel` binary provides commands for:

- `bushel list` - List all entries
- `bushel show <slug>` - Show entry details
- `bushel stats` - Show knowledge base statistics
- `bushel sync` - Sync images and thumbnails
- `bushel paper <doi>` - Add paper from DOI
- `bushel config` - Show configuration
- `bushel init` - Initialize configuration

## License

ISC License. See [LICENSE.md](LICENSE.md).
