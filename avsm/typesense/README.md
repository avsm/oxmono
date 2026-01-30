# typesense

OCaml bindings for the [Typesense](https://typesense.org/) search API.

Typesense is a fast, typo-tolerant search engine. This library provides
high-quality OCaml bindings using [Eio](https://github.com/ocaml-multicore/eio)
for async operations.

## Features

- Collection management (create, list, get, delete)
- Document operations (import, get, update, delete, export)
- Search with filtering, faceting, and highlighting
- Multi-search across collections
- Analytics rules and event tracking

## Installation

```
opam install typesense
```

## Usage

```ocaml
open Typesense

let () =
  Eio_main.run @@ fun env ->
  let auth = Auth.create ~endpoint:"http://localhost:8108" ~api_key:"xyz" in
  Client.with_client env auth @@ fun client ->

  (* Create a collection *)
  let schema =
    Collection.schema ~name:"books"
      ~fields:
        [
          Collection.field ~name:"title" ~type_:"string" ();
          Collection.field ~name:"author" ~type_:"string" ~facet:true ();
          Collection.field ~name:"year" ~type_:"int32" ();
        ]
      ~default_sorting_field:"year" ()
  in
  let _ = Collection.create client schema in

  (* Search *)
  let params = Search.params ~q:"harry" ~query_by:["title"; "author"] () in
  let result = Search.search client ~collection:"books" params in
  Printf.printf "Found %d books\n" result.found
```

## Error Handling

All API operations raise `Eio.Io` exceptions with `Error.E` error codes:

```ocaml
try Collection.get client ~name:"nonexistent" with
| Eio.Io (Error.E { code = Not_found; message; _ }, _) ->
    Printf.eprintf "Collection not found: %s\n" message
```

## License

ISC License. See [LICENSE.md](LICENSE.md) for details.
