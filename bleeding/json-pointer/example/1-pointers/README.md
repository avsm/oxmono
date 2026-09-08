# `1-pointers`

A JSON Pointer is a sequence of string tokens. Its string form is empty for the
document root; otherwise, every token has a leading `/`. Within a token, `~`
is written as `~0` and `/` as `~1`.

The first example parses a pointer and builds the same value from unescaped
tokens:

```ocaml
let parsed = Json_pointer.of_string "/users/0/display~1name"

let built =
  Json_pointer.(root / "users" / "0" / "display/name")

let same = Json_pointer.equal parsed built
```

The library keeps tokens as strings. It does not guess whether `"0"` will be
an object member or an array index; evaluation settles that from the JSON value
later. Building with `Json_pointer.of_tokens` or the `/` operator also means
callers never have to escape tokens themselves.

Run the [complete example](pointers.ml):

```console
$ cd example/1-pointers
$ opam install --deps-only --yes .
$ dune exec --root . ./pointers.exe
parsed: /users/0/display~1name
tokens: ["users"; "0"; "display/name"]
built: /users/0/display~1name
same pointer: true
escaped tokens: /a~1b/m~0n
invalid input: Invalid JSON Pointer: must be empty or start with '/': users/0
```

`Json_pointer.of_string` raises `Jsont.Error` for malformed input.
`Json_pointer.of_string_result`, used by the last line of the example, is more
convenient at untrusted-input boundaries.

## Next steps

- [`2-evaluation`](../2-evaluation#readme) applies these tokens to a JSON value.
- [`3-uri-fragments`](../3-uri-fragments#readme) shows the second RFC 6901
  representation.

## Folders and files

- [`pointers.ml`](pointers.ml) is the complete program.
- [`dune`](dune) describes its executable.
- [`dune-project`](dune-project) and [`1-pointers.opam`](1-pointers.opam) make
  this a standalone project.

[Up to the tutorial index](../#readme)
