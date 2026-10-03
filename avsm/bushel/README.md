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
- **Eio-based I/O**: Async directory loading with Eio

## Subpackages

- `bushel`: Core library with entry types and utilities
- `bushel.eio`: Eio-based directory loading
- `bushel.config`: XDG-compliant TOML configuration
- `bushel.sync`: Sync pipeline for images and thumbnails

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

## Releases

`bushel release` records code releases made on GitHub or tangled, with a
one-line summary, in `releases.yml` beside `links.yml`. arod shows each as a
line in the notes view. The date comes from the forge. ecosyste.ms says which
package registries, such as PyPI and opam, carry the version.

- `bushel release discover [--repo REPO] [--since DATE]` lists releases you
  published that are not registered.
- `bushel release add REPO TAG [--summary TEXT] [--project SLUG] [--force]`
  registers one. `TAG` is the git tag on GitHub and the version on tangled.
- `bushel release refresh [--days N]` attaches registries that have gained a
  registered version since.
- `bushel release list` shows what is registered.

```
$ bushel release add ucam-eo/geotessera v0.10.2
Registered ucam-eo/geotessera 0.10.2 (2026-09-04): Python library interface to the Tessera geofoundation model embeddings
  pypi.org: https://pypi.org/project/geotessera/0.10.2
```

Configure it in `config.toml`. `github_user` is the login whose GitHub releases
count as yours. Set `GITHUB_TOKEN` to raise the GitHub rate limit.

```toml
[releases]
github_user = "avsm"
github = ["mirage/ocaml-cohttp"]
tangled = ["anil.recoil.org/dune-rpc-eio"]
registries = ["pypi.org", "opam.ocaml.org"]

[releases.projects]
"ucam-eo/geotessera" = "tessera"
```

See `RELEASE-SPEC.md` for the design.

## License

ISC License. See [LICENSE.md](LICENSE.md).
