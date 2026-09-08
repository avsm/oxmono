# `5-jsont-codecs`

Direct evaluation returns `Jsont.json`. When the target has a known type,
compose the pointer with a Jsont codec instead:

```ocaml
let host =
  Jsont.Json.decode'
    (Json_pointer.path
       (Json_pointer.of_string "/server/host")
       Jsont.string)
    config
  |> Result.get_ok
```

`Json_pointer.path` reports both navigation and type errors through Jsont. Its
optional `absent` argument supplies a default only when the target is missing;
malformed indices and type mismatches remain errors.

Pointer update combinators are codecs too. The [complete example](jsont_codecs.ml)
uses `update_path` with a small integer codec, creates a missing timeout with
`set_path`, and removes a field with `delete_path`:

```console
$ cd example/5-jsont-codecs
$ opam install --deps-only --yes .
$ dune exec --root . ./jsont_codecs.exe
host: localhost
timeout: 30
updated: {"server":{"host":"localhost","port":8081,"timeout":30}}
```

The update codecs run with `Jsont.Json.recode'`, so they compose with ordinary
Jsont transformations. `set_path` replaces an existing value by default;
`allow_absent:true` permits creation of the final object member or array
position without inventing missing parent containers.

## Next steps

- [`6-jmap`](../6-jmap#readme) uses a typed pointer codec with wildcard results.
- The [full odoc tutorial](../../doc/tutorial.mld) covers every query and update
  variant.

## Folders and files

- [`jsont_codecs.ml`](jsont_codecs.ml) is the complete program.
- [`dune`](dune) describes its executable.
- [`dune-project`](dune-project) and [`5-jsont-codecs.opam`](5-jsont-codecs.opam)
  make this a standalone project.

[Up to the tutorial index](../#readme)
