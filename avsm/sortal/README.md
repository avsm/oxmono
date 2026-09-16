# Sortal

Sortal stores contact metadata as vCard 3.0 files. The OCaml library exposes
names, email addresses, accounts, affiliations, links, photos and feed
subscriptions. Bushel and Arod use the same `Sortal.Store` API.

The default store is `~/.local/share/sortal`, or `$XDG_DATA_HOME/sortal`:

```
store.json          local sync identity and storage format version
cards/<uid>.vcf     one contact per stable UID
*.png, *.jpg, ...   legacy or unreferenced photo assets
feeds/              existing feed caches and annotations
.git/               optional local version history
```

`UID` identifies a contact across stores. The store UUID stays in local
`store.json` and sync journals, never in the cards. `X-SORTAL-ID` carries the
handle. Renaming a handle keeps the UID and filename.
Embedded card photos are authoritative. Sortal materializes them beneath the
XDG cache directory when a caller needs a file path. Loose files in the data
directory are retained only when they are not referenced by a card.
Standard fields use compatible vCard properties. Additional metadata uses
individual properties, including `X-SORTAL-FIELD` for future fields. No whole
contact payload is embedded. Typed edits retain unknown fields, parameters
and properties. Stale writes and ambiguous edits are rejected.

```sh
dune exec -- sortal list
dune exec -- sortal show avsm
dune exec -- sortal stats
```

Use `--data-dir /absolute/store/path` on these commands to select another
store. Reading through `Sortal.Store.create` uses XDG configuration.
`Sortal.Store.create_at fs path` opens an explicit path without creating
other application directories.

CardDAV settings live in `~/.config/sortal/config.toml` under `[carddav]`.
`sortal init` creates the adjacent `carddav-password` file with mode `0600`.
Put the Fastmail app password there. CardDAV commands use these settings by
default, while command-line options override them.

```ocaml
let store = Sortal.Store.create env#fs "sortal" in
let contact = Sortal.Contact.make
    ~handle:"example" ~names:["Example Person"]
    ~emails:["person@example.org"] () in
Sortal.Store.save store contact
```

The one-off YAML migration is complete. Sortal no longer reads or writes YAML
contacts. Original source bytes and Git history remain in the migration backup.

CardDAV tools operate on this live native store. A recovery snapshot preserves
all card and asset bytes:

```sh
dune exec -- sortal carddav export \
  --source ~/.local/share/sortal --output /tmp/sortal-snapshot
dune exec -- sortal carddav verify \
  --bundle /tmp/sortal-snapshot --source ~/.local/share/sortal
```

See [storage and CardDAV workflows](spec/carddav-migration.md) for mappings,
dry runs and the current synchronization limits.

```sh
dune build @avsm/sortal/all
dune runtest avsm/sortal --force
```
