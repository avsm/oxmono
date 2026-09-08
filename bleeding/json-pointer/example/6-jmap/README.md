# `6-jmap`

[RFC 8620 result references](https://www.rfc-editor.org/rfc/rfc8620#section-3.7)
extend pointer evaluation with one rule: when the current value is an array,
the `*` token applies the remaining pointer to every element. Array results are
flattened by one level.

The pointer type and parser do not change. Choose `Json_pointer.Jmap.get` only
at evaluation time:

```ocaml
let ids =
  Json_pointer.Jmap.get
    (Json_pointer.of_string "/list/*/id")
    response

let typed_ids =
  Jsont.Json.decode'
    (Json_pointer.Jmap.path_list
       (Json_pointer.of_string "/list/*/id")
       Jsont.string)
    response
  |> Result.get_ok
```

Run the [complete example](jmap.ml):

```console
$ cd example/6-jmap
$ opam install --deps-only --yes .
$ dune exec --root . ./jmap.exe
ids: ["a","b"]
flattened tags: ["x","y","z"]
typed ids: [a; b]
object wildcard: "literal member"
```

The final line is deliberate: `*` is special only while traversing an array.
On an object, it remains the ordinary member name `"*"`. Use the non-JMAP
functions everywhere that RFC 8620 wildcard semantics are not wanted.

## Next steps

- Return to the [tutorial index](../#readme) to revisit any stage.
- Continue with the [full odoc tutorial](../../doc/tutorial.mld) or the
  [`Json_pointer` interface](../../src/json_pointer.mli).

## Folders and files

- [`jmap.ml`](jmap.ml) is the complete program.
- [`dune`](dune) describes its executable.
- [`dune-project`](dune-project) and [`6-jmap.opam`](6-jmap.opam) make this a
  standalone project.

[Up to the tutorial index](../#readme)
