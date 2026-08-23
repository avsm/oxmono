# Vendoring tw

This is `tw` from https://github.com/samoht/tw at `43f7afe6`, the `main` branch
as of 2026-08-21. It is **not a release**. The newest tag is 1.0.0 and `main`
is 469 commits past it, so `CHANGES.md` and `dune-project` still say 1.0.0
while the code does not match the 1.0.0 tarball. Take the git revision above as
the version.

Tracking `main` rather than 1.0.0 is deliberate: `tw.opam` on `main` requires
`cascade >= 1.1.0`, which is what `bleeding/cascade` now holds, and the pair is
what upstream tests together.

`bushel.web` is the only consumer. It uses about 113 `Tw.` values and compiled
against this revision unchanged from the 1.0.0 API.

## What differs from upstream

Four hunks, none touching library code.

* `dune` and `lib/dune`: each `(mdx ...)` stanza gains `unix`. Declaring
  `libraries` explicitly loses the implicit dependency, and mdx will not link
  without it.
* Nine `examples/*/dune`: the rules that shell out to `npx tailwindcss` to
  compare against the reference compiler are `(enabled_if false)`. There is no
  npx or tailwindcss in this workspace. Upstream guards them with
  `%{bin-available:npx}`, which is true here for `npx` and then fails inside
  the rule.
* `tw.opam` is the file dune regenerates in this workspace, which orders
  `version:` after `opam-version:` and ends with a newline.

## Re-vendoring

    git clone https://github.com/samoht/tw
    rsync -a --exclude .git tw/ bleeding/tw/

Re-apply the four hunks above, then verify through the consumer, because
`@bleeding/tw/all` cannot be built in this switch: the mdx stanzas need
`mdx.top` and `ocaml-mdx`, which are not installed, and that failure predates
this copy.

    dune build @avsm/bushel/all
    dune build avsm/bushel/bin/main.exe

Then serve the knowledge base and fetch the stylesheet, which is what exercises
the whole tw to cascade path:

    ./_build/default/avsm/bushel/bin/main.exe serve -d ~/bushel/data -p 8092
    curl -s 'http://127.0.0.1:8092/tw.css?v=...' | wc -c

Expect roughly 26KB with six `@layer` rules and 77 `--tw-` custom properties.
The bytes are not stable across upstream revisions, since constant folding and
the default font stack both changed between 1.0.0 and this one. Compare the
structure, not a checksum.
