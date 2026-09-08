# zotero-translation

OCaml client for the [Zotero Translation Server](https://github.com/zotero/translation-server),
which provides DOI/URL resolution and bibliographic format export.

## Features

- Resolve DOIs and URLs to bibliographic metadata
- Export to multiple formats: BibTeX, BibLaTeX, CSL-JSON, RIS, and more
- Eio I/O through a caller-supplied Fetch capability
- Bounded JSON decoding and bibliographic exports
- BibTeX parsing and formatting with string source labels

## Installation

```
opam install zotero-translation
```

## Usage

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let fetch = Fetch_curl.std ~sw env in
let client =
  Zotero_translation.of_fetch ~base_url:"http://localhost:1969" fetch
in

(* Resolve a DOI *)
let metadata = Zotero_translation.resolve_doi client "10.1145/3341301.3359630" in

(* Export to BibTeX *)
let bibtex = Zotero_translation.export client Zotero_translation.Bibtex metadata in
print_endline bibtex
```

The application selects and owns the Fetch backend. This example links
`eio_main`, `fetch-curl` and `zotero-translation`. Any Fetch backend or narrowed
capability works. The library itself does not depend on curl, access the
filesystem or create background fibers.

`of_fetch ~base_url fetch` replaces `create ?session ~sw env ~base_url`.
Construct the Fetch client once in the application and pass it in. The client
narrows its authority to POST requests beneath the configured server URL.
An existing restriction on the supplied capability remains in force. Base
URLs may include a path prefix but must not contain credentials, a query or
a fragment. Redirects are disabled to avoid replaying submitted data elsewhere.

`max_response_bytes` defaults to 16 MiB. JSON responses use Fetch's JSON codec
and must carry a supported JSON content type. Exports preserve their bytes,
except for the existing whitespace trimming of BibTeX. Error bodies are capped
at the smaller of the response limit and 64 KiB. An oversized error body is
replaced by a diagnostic while retaining the status in `Api_error`.

HTTP errors raise `Zotero_translation.Api_error (status, body)`. Transport,
redirect, policy and decoding errors propagate as `Eio.Io (Fetch.E _, _)`.
Cancellation propagates unchanged. To set a deadline for an entire operation:

```ocaml
Eio.Time.Mono.with_timeout env#mono_clock 30. (fun () ->
    Zotero_translation.json_of_doi client ~slug:"paper" "10.1145/3341301.3359630")
```

## BibTeX and files

`Zotero_translation.Bibtex.of_string` and `of_string'` take an optional
`~file:string` used only in error messages, defaulting to `"-"`. There is no
Fpath dependency. Read files through an Eio path capability in the caller:

```ocaml
let source = Eio.Path.load Eio.Path.(env#cwd / "papers.bib") in
Zotero_translation.Bibtex.of_string' ~file:"papers.bib" source
```

The bundled parser supports entries and nested braces. It does not implement
`@string`, `@preamble`, `@comment` or general TeX escape processing.

## Tests

From the monorepo root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/zotero-translation/all @bleeding/zotero-translation/runtest --force
```

Tests use Fetch's mock backend without sockets or a running Zotero server.
They cover endpoints, all export formats, DOI fallback, URL validation,
capability restrictions, disabled redirects, body limits, decoding failures,
cancellation cleanup and BibTeX source labels.

## Requirements

This library requires a running [Zotero Translation Server](https://github.com/zotero/translation-server) instance.

## License

ISC - see [LICENSE.md](LICENSE.md)
