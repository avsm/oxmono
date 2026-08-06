# OCaml Karakeep API Client

An OCaml client library for the [Karakeep](https://karakeep.app) bookmark service API.
Built on [Eio](https://github.com/ocaml-multicore/eio) for structured concurrency with
a type-safe interface using [jsont](https://erratique.ch/software/jsont) for JSON encoding/decoding.

## Features

- Full API coverage for bookmarks, tags, lists, highlights, and user operations
- Automatic pagination support
- Type-safe JSON encoding/decoding with jsont
- Built on Eio for structured concurrency
- Cmdliner integration for CLI tools

## Installation

```bash
opam install karakeep
```

Or pin the development version:

```bash
opam pin add karakeep.dev git+https://tangled.org/@anil.recoil.org/ocaml-karakeep.git
```

## Configuration

The library requires an API key for authentication. You can obtain an API key from your Karakeep instance settings.

## Usage Example

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  (* Create the client *)
  let client =
    Karakeep.create ~sw ~env
      ~base_url:"https://hoard.recoil.org"
      ~api_key:"your_api_key"
  in

  (* Fetch recent bookmarks *)
  let result = Karakeep.fetch_bookmarks client ~limit:10 () in
  List.iter (fun bookmark ->
    let title = Karakeep.bookmark_title bookmark in
    Printf.printf "- %s\n" title
  ) result.bookmarks;

  (* Fetch all bookmarks (handles pagination automatically) *)
  let all_bookmarks = Karakeep.fetch_all_bookmarks client () in
  Printf.printf "Total bookmarks: %d\n" (List.length all_bookmarks);

  (* Create a new bookmark *)
  let _new_bookmark =
    Karakeep.create_bookmark client
      ~url:"https://ocaml.org"
      ~title:"OCaml Programming Language"
      ~tags:["programming"; "ocaml"]
      ()
  in

  (* Search bookmarks *)
  let search_results = Karakeep.search_bookmarks client ~query:"ocaml" () in
  Printf.printf "Found %d results\n" (List.length search_results.bookmarks)
```

## Error Handling

All operations may raise `Eio.Io` exceptions with a `Karakeep.E` error payload:

```ocaml
try
  let bookmarks = Karakeep.fetch_bookmarks client () in
  (* ... *)
with
| Eio.Io (Karakeep.E err, _) ->
    Printf.eprintf "Karakeep error: %s\n" (Karakeep.error_to_string err)
```

## Documentation

- [API Documentation](doc/index.md)
- Online documentation is generated with `dune build @doc`

## License

ISC License. See [LICENSE.md](LICENSE.md) for details.
