# zotero-translation

OCaml client for the [Zotero Translation Server](https://github.com/zotero/translation-server),
which provides DOI/URL resolution and bibliographic format export.

## Features

- Resolve DOIs and URLs to bibliographic metadata
- Export to multiple formats: BibTeX, BibLaTeX, CSL-JSON, RIS, and more
- Built on Eio for efficient async I/O
- Includes a BibTeX parser/encoder

## Installation

```
opam install zotero-translation
```

## Usage

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let client = Zotero_translation.create ~sw env ~base_url:"http://localhost:1969" in

(* Resolve a DOI *)
let metadata = Zotero_translation.resolve_doi client "10.1145/3341301.3359630" in

(* Export to BibTeX *)
let bibtex = Zotero_translation.export client Bibtex metadata in
print_endline bibtex
```

## Requirements

This library requires a running [Zotero Translation Server](https://github.com/zotero/translation-server) instance.

## License

ISC - see [LICENSE.md](LICENSE.md)
