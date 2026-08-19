Syndic
======

RSS and Atom feed parsing

This is Syndic 1.7.0, vendored from https://github.com/Cumulus/Syndic and
patched to parse feeds that real publishers emit. It is not patched for
OxCaml portability, and the section below records why.

Local patches
=============

Three hunks, all of them making the parser accept a document that the
specification forbids. Everything else in `lib/` is believed to be upstream
1.7.0. The three differ in how well that provenance is established, so each
one says below how it was determined.

* `syndic_atom.ml`, a missing entry `<updated>`. RFC 4287 requires exactly
  one, and upstream raises without it. The parser now stores the
  `dummy_updated` sentinel and `make_feed` replaces it with the entry's own
  `<published>`, or failing that with the feed-level `<updated>`. That value
  has either been read from the document or already raised by the time the
  fixup runs, so no timestamp is invented. This is the same shape as
  upstream's own `dummy_name` fixup for a missing author, which is upstream
  code and not a local patch.

  Provenance: certain. This hunk is commit `0888157ea`, which changes only
  this file and the test, so `git show 0888157ea` is the whole patch.

* `syndic_date.ml`, `of_rfc822` trims its argument before scanning it.
* `syndic_date.ml`, `of_rfc822` falls back to `Ptime.of_rfc3339` when every
  RFC 822 pattern fails, because many RSS feeds carry ISO 8601 dates.

  Provenance of those last two: inferred, not verified. Both arrived with the
  initial vendoring, commit `43aee663f`, which added the whole tree in one
  go, so git cannot separate them from upstream. Upstream 1.7.0 is not on this
  machine, and they were identified by diffing against the 1.8.0 sources in
  `~/.opam/5.2.0+ox/.opam-switch/sources/syndic.1.8.0`. Neither appears there.
  The inference is that upstream never had them, on the reasoning that 1.8.0
  is later than 1.7.0 and would not have dropped them silently. That is a
  judgement about upstream's behaviour rather than an observation, and the
  same 1.8.0 diff also carries genuine version drift, namely 1.8.0's
  `relaxed` parsing feature and its RFC 822 timezone rework in this very
  function. Treat the boundary between these two hunks and the surrounding
  1.7.0 code as approximate until it is checked against a real 1.7.0 file.

`avsm/sortal/test/test_feed.ml` holds the regression tests. Without the
`<updated>` hunk its excerpt raises `Syndic.Error.Error`. The two
`syndic_date.ml` hunks have no test of their own, which is a second reason to
re-derive them rather than reapply them from this list.

Portability status
==================

Nothing in `lib/` carries `@@ portable`, and nothing can until `uri`, `ptime`
and `xmlm` are portable. This was measured, not assumed. Every claim below is
a compiler verdict against this copy.

The published types are the harder half of the problem. `Uri.t` and `Ptime.t`
are abstract at the bare `value` kind in this switch, so neither crosses
portability. `Syndic.Atom.feed`, `entry`, `link`, `author`, `id`, `icon` and
`logo` are all built from those two types, so no feed and no entry can be
held by a portable closure however the interface is annotated. A kind
annotation cannot fix this from here, because the kind that has to change is
on an abstract type in another package.

The function bodies are the easier half and still do not clear. Annotating
`Syndic_atom.to_xml` names `Syndic_common.Util.add_node_option` as the
blocker. Annotating `Syndic_common` names `Syndic_xml.resolve`. Annotating
`Syndic_xml` names `Uri.resolve`, which is where the chain stops. On the date
side, annotating `Syndic_date` names `epoch`, which is `Ptime.epoch`, and the
rest of that module is `Ptime` under other names. `Xmlm.make_output` is
nonportable too, so `Syndic_atom.output` would fail on `xmlm` even with `uri`
and `ptime` solved.

The one blocker local to this copy is `month_to_int` in `syndic_date.ml`, a
module-level `Hashtbl` filled by a top-level `let ()`. That is the shape the
htmlit vendoring fixed by replacing a module-level `Set` with a match. Fixing
it here would gain nothing, because every value in `Syndic_date` reaches
`Ptime` anyway.

What would have to land first, in order:

1. `ptime` vendored and annotated, including a crossing kind on `Ptime.t`.
2. `xmlm` vendored and annotated, at least `make_output` and `to_xmlm`.
3. `uri` vendored and annotated, including a crossing kind on `Uri.t`.

`vendor/ocaml-uri` does not satisfy the third. It builds `uriz`, which is a
rewrite with a different interface rather than a patched `Uri`. Its functions
are already portable, so `fun s -> Uriz.to_string (Uriz.of_string_exn s)`
compiles at `@ portable`, but `Uriz.t` is abstract at the bare `value` kind
just as `Uri.t` is, so a URI still cannot be captured. Moving Syndic onto
`uriz` would be a semantic fork of the parser rather than an annotation pass,
and it would not by itself let a feed cross.

The callers are blocked independently of all this. `Arod.Feed.feed_string` is
nonportable, and so is `Arod.Feed.form_uri`, which is one call to
`Uri.of_string`. The Arod feed path also reaches `Arod.Ctx.author_exn` and
`Arod.Md.to_atom_html`, which
`.superpowers/sdd/vendoring-followups/item3-report.md` records as blocked on
`Ptime` and `Cmarkit`. Making Syndic portable would therefore not on its own
collapse the `feed` or `blogroll` closure in `Arod_env.t`.

Re-vendoring checklist
======================

1. Before overwriting anything, fetch the upstream 1.7.0 `syndic_date.ml` and
   diff this copy against it. That settles whether the trim and the
   `Ptime.of_rfc3339` fallback are local patches to carry forward or 1.7.0
   code to drop, which the inventory above infers but does not establish.
   Record what the diff says here, replacing the inference.
2. Copy `lib/` and `LICENSE` from the new release over this directory,
   keeping `dune`, `dune-project`, `syndic.opam` and this file.
3. Reapply the `<updated>` hunk, which `git show 0888157ea` gives exactly,
   and whichever of the two `syndic_date.ml` hunks step 1 confirmed.
4. Update the version in `syndic.opam`, in `dune-project` and in the first
   line of this file.
5. `dune build @avsm/sortal/all @avsm/sortal/runtest`, which reaches the
   `test_feed` suite that pins the `<updated>` hunk. Nothing pins the two
   date hunks, so a green build does not show that they survived.
6. `dune build @avsm/arod/all @avsm/bushel/all`.

Do not add `@@ portable` while re-vendoring. Read the section above first,
then check whether `uri`, `ptime` and `xmlm` have moved.

Build Requirements
==================

 * OCaml >= 4.01.0
 * Ptime >= 0.8.0
 * Xmlm >= 1.2.0
 * Uri >= 1.3.1

Documentation
=============

Upstream documentation is [here](http://cumulus.github.io/Syndic/).
