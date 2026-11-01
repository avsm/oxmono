# frontmatter - Parse YAML Frontmatter from Markdown

An OCaml library for parsing YAML frontmatter (Jekyll-format) from Markdown files. Supports extracting structured metadata and body content from files with YAML headers delimited by `---` markers.

## Key Features

- Parse Jekyll-style YAML frontmatter from Markdown documents
- Type-safe extraction of metadata fields using jsont codecs
- Support for common frontmatter fields (title, date, tags, etc.)
- Eio-based file I/O support via `frontmatter-eio` package

## Usage

```ocaml
(* Parse frontmatter from a string *)
let content = {|---
title: My Post
date: 2025-01-15
tags:
  - ocaml
  - tutorial
---

# Hello World

This is the body content.
|}

let () =
  match Frontmatter.of_string content with
  | Ok doc ->
    let title = Frontmatter.get_string "title" doc in
    let body = Frontmatter.body doc in
    Printf.printf "Title: %s\nBody: %s\n"
      (Option.value ~default:"untitled" title)
      body
  | Error e ->
    Printf.eprintf "Parse error: %s\n" e
```

With Eio file I/O:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let doc = Frontmatter_eio.read_file env#fs "posts/my-post.md" in
  (* ... process document ... *)
```

## Installation

```
opam install frontmatter frontmatter-eio
```

## Documentation

API documentation is available via:

```
opam install frontmatter
odig doc frontmatter
```

## License

ISC
