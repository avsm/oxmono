# `2-evaluation`

The [first example](../1-pointers#readme) built pointers. This one applies them
to a `Jsont.json` document.

Use `Json_pointer.get` when the location is required:

```ocaml
let grace =
  Json_pointer.get (Json_pointer.of_string "/users/1/name") document
```

It returns the JSON value or raises `Jsont.Error` with the failing location.
For optional data, `Json_pointer.find` returns `None` instead. A third form,
`Json_pointer.get_result`, keeps the detailed error in a result value.

Run the [complete example](evaluation.ml):

```console
$ cd example/2-evaluation
$ opam install --deps-only --yes .
$ dune exec --root . ./evaluation.exe
required value: "Grace"
/users/0/name -> "Ada"
/0 -> "zero is an object member"
/- -> "hyphen is too"
/users/9 -> not found
```

Notice that `0` selects an array element below `/users`, but selects the member
named `"0"` at the document root. Likewise, `-` is an ordinary object member.
It becomes special only as the final array token of an RFC 6902 `add` operation.

## Next steps

- [`3-uri-fragments`](../3-uri-fragments#readme) puts pointers into URIs.
- [`4-json-patch`](../4-json-patch#readme) uses pointers to transform documents.

## Folders and files

- [`evaluation.ml`](evaluation.ml) is the complete program.
- [`dune`](dune) describes its executable.
- [`dune-project`](dune-project) and [`2-evaluation.opam`](2-evaluation.opam)
  make this a standalone project.

[Up to the tutorial index](../#readme)
