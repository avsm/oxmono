# `4-json-patch`

Pointers are the addressing mechanism used by
[RFC 6902 JSON Patch](https://www.rfc-editor.org/rfc/rfc6902). `json-pointer`
provides each operation directly as a pure transformation of `Jsont.json`.

This example appends a task, replaces the status, removes a task, copies the
status, moves another task, and finally tests the result:

```ocaml
let document =
  Json_pointer.add (pointer "/tasks/-") document
    ~value:(Jsont.Json.string "ship")

let document =
  Json_pointer.replace (pointer "/status") document
    ~value:(Jsont.Json.string "done")

let document = Json_pointer.remove (pointer "/tasks/0") document
```

Run the [complete example](json_patch.ml) to see all six operations:

```console
$ cd example/4-json-patch
$ opam install --deps-only --yes .
$ dune exec --root . ./json_patch.exe
before: {"tasks":["draft","review"],"status":"open"}
after:  {"tasks":["ship"],"status":"done","previousStatus":"done","highlight":"review"}
status is done: true
```

The final `-` in `/tasks/-` means “after the last array element” for `add`.
Array indices insert rather than replace for that operation. `replace`,
`remove`, and the source of `move` or `copy` must already exist; all operations
that mutate the document raise `Jsont.Error` when their preconditions fail.
`test` instead returns `false` when its pointer does not resolve.

These functions implement individual RFC 6902 operations. An application that
accepts a JSON Patch document can decode its operation list and fold the
corresponding functions over its input document.

## Next steps

- [`5-jsont-codecs`](../5-jsont-codecs#readme) performs typed queries and edits.
- [`6-jmap`](../6-jmap#readme) adds JMAP's array wildcard during evaluation.

## Folders and files

- [`json_patch.ml`](json_patch.ml) is the complete program.
- [`dune`](dune) describes its executable.
- [`dune-project`](dune-project) and [`4-json-patch.opam`](4-json-patch.opam)
  make this a standalone project.

[Up to the tutorial index](../#readme)
