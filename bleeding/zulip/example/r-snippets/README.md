# Work with saved snippets

This recipe creates a temporary saved snippet, edits only its content, finds it
in the account's saved snippets, prints it, and deletes it.

```sh
dune exec example/r-snippets/snippets.exe -- --profile tutorial
```

Expected output has the new server-assigned identifier followed by the title
and edited Markdown source:

```text
Saved snippet 42: OCaml API example
The temporary snippet now contains `edited Markdown`.
```

The identifier varies by server. The program deletes the temporary snippet
before it exits. It also attempts deletion if listing, decoding, or editing
fails after creation.

The create call sends the title and original Markdown as ordinary form values:

```ocaml
let saved_snippet_id =
  Saved_snippets.create client ~title ~content |> Error.or_raise
```

Editing accepts optional title and content fields. Supplying only `content`
leaves the title unchanged:

```ocaml
Saved_snippets.edit client ~saved_snippet_id ~content:edited_content ()
|> Error.or_raise
```

`Saved_snippets.list` returns typed values with accessors for the identifier,
title, original Markdown content, and creation timestamp. Endpoint failures are
returned as structured `Error.t` values. This command uses `Error.or_raise`
because it cannot produce a meaningful partial result.

[All examples and profile setup](../README.md) · [Source](snippets.ml) ·
[Saved snippets API](../../lib/zulip_eio/saved_snippets.mli) ·
[Build file](dune)
