# Remaining OxCaml porting work

State as of 2026-08-21, branch `minus39`. Arod serves through proffer with
`listing`, `entry`, `entry_markdown`, `paper_bib` and `blogroll` running as
`@@ portable` handlers over an immutable `Arod.Ctx.t`. `Arod_env.t` holds 12
fields. Five closures remain, three of them liftable. Every claim below was
compiler-probed in the session that wrote this file. The per-field reasons in
`avsm/arod/lib_handlers/arod_render.mli` and `arod_env.mli` are probe
transcripts and are the authority if this file drifts.

## Liftable closures, in recommended order

### 1. `sitemap` (effort S)

`Sitemap.v`/`Sitemap.output` at `arod_render.ml:359-361`. The library is
already vendored at `vendor/sitemap` (dragged in verbatim by the xmlm
shadowing cascade) and its only dependencies, xmlm and ptime, are both
portable. Do an htmlit-style annotation pass: floating `@@ portable` on
`sitemap.mli`, kinds where capture needs them, a capture-shaped guard in
`avsm/arod/test/`, README hunk inventory. Follow `vendor/xmlm/README.md` as
the template. Lifts the `sitemap` field from `Arod_env.t`.

### 2. `feed` (effort M, gated on a probe)

`Arod.Feed.feed_string` at `arod_render.ml:339-344`. The chain is clear
except **jsonfeed** (vendored verbatim at `vendor/jsonfeed`, never annotated)
and its dependency **jsont** (+ bytesrw), which are opam libraries. Run the
half-day feasibility probe first: can jsont's codec surface be annotated
without a semantic fork? Look for the logs-shaped pattern (closures capturing
handles or formatters stored in records) before investing. If jsont is
htmlit-shaped, vendor and annotate jsont, then annotate `vendor/jsonfeed`,
then `Arod.Feed`. Note the two feed callers of `Bushel.Md.note_references`
(`arod_jsonfeed.ml`, `arod_md.ml:with_feed_references`) reach the author via
`author_exn` while the render path reads the ctx precompute. When feed goes
portable, move those callers to the precompute and retire the dual route
consciously (recorded concern in the walls-final round).

### 3. `pagination` (effort S-M, shares the jsont answer)

Three trivial `Ezjsonm.to_string` emitters at `arod_render.ml:408-485`.
Ezjsonm is ruled not worth vendoring (drags hex, jsonm, sexplib0, uutf).
Either ride the jsont work from item 2, or hand a small portable JSON-emit
helper into arod. If hand-rolled, differential-test the escaper against
Ezjsonm over adversarial strings. Hand-rolled JSON escaping is where the
survey found a real RFC 8259 bug in tessabot, so do not skip the comparison.
Lifting this does not free `search`, which also holds the domain-bound index
handle.

## The floor

After items 1-3, every remaining `Arod_env.t` closure is genuinely
domain-bound and stays: `search` (index handle), `report` (SQLite),
`read_image`/`read_paper` (confined Eio capabilities), `now` (clock),
`log_search` (log source). `ctx`, `config` and `cache` are data.

## Settled questions. Do not reopen without new facts.

- **logs**: structurally blocked. `format_reporter`'s closure captures
  `Format.formatter`s, which can never be portable, and 20+ in-tree callers
  install reporters. The env-closure pattern is the answer. The compiler
  error is reproducible in minutes if doubted.
- **opam uri in `arod_ctx.ml`**: permanent. `normalise_url`'s
  decode-then-re-encode semantics are load-bearing for persisted feed
  annotation keys (`test_feed_annotations` pins it). Startup-only, never on
  a render path.
- **Re**: never vendor for portability. Compiled `Re.re` values carry
  internally mutable DFA caches mutated on execution. The references
  precompute at `Arod.Ctx` build made it startup-only.
- **sqlite at render time**: declined by ruling. A db handle in a portable
  handler reintroduces the env-closure pattern. Precomputed immutable
  structures are the house answer for build-once data.

## Adjacent opportunities

- **Multi-domain serving**: the strategic payoff of all the portability
  work, and the original PROFFER.md goal. `Proffer_httpz.run` through
  `Domain_manager` plus the queue-based log bridge. The comment in
  `avsm/arod/lib/server/arod_server.ml` marks the spot. Everything the
  render path touches now crosses domains. Verify with the same
  live-differential methodology (`avsm/arod/test/render_capture.sh`).
- **sortal_web onto htmlit** (S-M, deletes ~662 lines):
  `avsm/sortal/lib/web/html.ml` and `pages.ml` hand-roll HTML in `Buffer`
  solely because htmlit was nonportable when they were written. It is
  portable now.
- **Off-path stragglers, low priority**: `Bushel.Md.extract_all_links` (the
  one nonportable whole-document conversion left, no render consumer),
  Astring's four uses in `bushel_sync`, `fmt` in arod's `lib_component` and
  `lib_search` (off the portable paths, the collapse compiled around them).

## Where the methodology lives

- Vendoring and annotation playbook: the READMEs under `vendor/base64`,
  `vendor/htmlit`, `vendor/ptime`, `vendor/xmlm`, `vendor/cmarkit`,
  `vendor/syndic`. Each carries a hunk inventory graded by provenance and a
  re-vendoring checklist.
- Behaviour oracles: `avsm/arod/test/test_md_golden.ml` (golden renders,
  never regenerate to make a test pass), `render_capture.sh` (full-site
  byte differential, 1588 routes, noise floor documented in its header),
  `link_predicate_diff.ml` (URL predicate corpus differential).
- Portability guards: `test_payload_kinds.ml`, `test_cmarkit_portable.ml`
  and siblings. Guards must capture module-level values inside `@ portable`
  closures. Parameter-shaped ascriptions prove nothing.
