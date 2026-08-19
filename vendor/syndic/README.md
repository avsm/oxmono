Syndic
======

RSS and Atom feed parsing

This is Syndic 1.7.0, vendored from https://github.com/Cumulus/Syndic and
patched to parse feeds that real publishers emit. It has also been ported off
opam `uri` onto the vendored `uriz`, which is a breaking change to the public
interface. The sections below record both.

Local patches
=============

Five hunks. Three of them make the parser accept a document that the
specification forbids, the fourth is a portability prerequisite that changes
nothing a caller can see, and the fifth is the `uriz` port, which changes
every published type that carries a URI. Everything else in `lib/` is believed
to be upstream 1.7.0. The hunks differ in how well that provenance is
established, so each one says below how it was determined.

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

* `syndic_date.ml`, `month_to_int` is a match rather than a module-level
  `Hashtbl` filled by a top-level `let ()`. A portable function cannot read a
  hash table held at module level, and this lookup is on the path of
  `of_rfc822`. The twelve keys and values are the ones the table held, and
  since the lookup was `Hashtbl.find`, an unrecognised name raised
  `Not_found`. The last arm raises it too, so `of_rfc822`'s pattern chain
  still falls through to the next `sscanf` and then to the RFC 3339 fallback
  exactly as before. Nothing else changed, and nothing in the interface did.

  Provenance: certain. This hunk is the commit that replaced the table, which
  touches only this file, this list and the test that pins it.

* All of `lib/`, `uri` replaced by `uriz`. This is the large one. Every
  `Uri.t` in the six published interfaces is now a `Uriz.t`, so the change is
  visible to every caller. The mapping is mechanical for most of it and is
  set out in the next section.

  Provenance: certain. This hunk is the commit "Move syndic from uri to
  uriz", which touches this directory and every in-tree consumer at once,
  because `Syndic_xml.resolve` carries the type in its own signature and no
  partial state compiles.

The uriz port
=============

`uri` and `uriz` differ in one way that matters to a feed parser.
`Uri.of_string` is total: it coerces any string into a URI. `Uriz.of_string`
validates against RFC 3986 and returns `Null` for anything that is not a
reference. A feed carries whatever its publisher wrote, so a parser that
stopped there would be useless.

`Syndic_xml.uri_of_string` bridges the two, and every parse site in `lib/`
now calls it. It is total and it never raises. It tries three things in turn.

1. Parse the text as it stands. Almost everything real takes this path.
2. Percent-encode the bytes that no component of a URI reference admits, then
   parse again. That covers a space, a control character, a raw UTF-8 byte, a
   bracket outside the authority, a `%` that starts no triplet, and a second
   `#`. The scheme, the authority and the segment boundaries stay where they
   are, so an href that is merely unencoded comes back as the URI its
   publisher meant. The output of this pass matches what `Uri.of_string` used
   to produce for the same input.
3. Percent-encode the whole of the text down to the unreserved set. The
   result is one opaque path segment holding the original bytes. It is a
   valid relative reference and it compares equal to itself, which is what a
   feed reader needs of an `<id>`, but it no longer names anything. Only
   input that survives step 2 and is still not a reference reaches here, such
   as a non-numeric port or a first segment holding a colon.

The other calls map directly. `Uri.to_string` is `Uriz.to_string` and is now
free, because the canonical text is the representation.
`Uri.resolve "" base u` is `Uriz.resolve ~base u`, which is plain RFC 3986
section 5.2. `Uri.with_fragment u (Some d)` is
`Uriz.with_fragment u (This d)`, and `Uri.scheme` and `Uri.host` return
`or_null` rather than `option`. Two calls needed more than a rename.
`Uriz.make`, which the RSS2 `<cloud>` parser uses, raises `Invalid_argument`
where `Uri.make` coerced, so `Syndic_rss2.make_cloud` catches it and falls
back to `uri_of_string` on the assembled text. `Uriz.with_query` takes
encoded query text rather than an association list, so `Syndic_w3c.url`
joins its parameters itself, encoding each key and value with the
`` `Query_value `` component.

The published record fields keep `option` where they had it. `or_null`
appears only where a `uriz` signature forces it, and the conversion happens
at that call.

Nothing here is upstream. Re-vendoring overwrites all of it, so a new release
has to have this port re-applied or re-evaluated before it can build. The
tests named below pin the three tiers of `uri_of_string`, its totality over
every single byte, and that a malformed href in a real Atom document reaches
the entry rather than stopping the parse. Rendering the six feeds in `test/`
through `Syndic.Atom.output` produced byte-identical XML before and after the
port, so well-formed input is unaffected.

Tests
=====

`avsm/sortal/test/test_feed.ml` holds the regression tests. Without the
`<updated>` hunk its excerpt raises `Syndic.Error.Error`, and
`test_rfc822_month_names` walks the twelve month names and checks that an
unknown month is rejected. It cannot see `Not_found` itself, since every call
site catches every exception. The trim and the RFC 3339 fallback have no test
of their own, which is a second reason to re-derive them rather than reapply
them from this list.

Portability status
==================

Nothing in `lib/` carries `@@ portable` yet. The type-level blocker is gone
and the remaining work is the annotation pass itself plus `xmlm`. This was
measured, not assumed. Every claim below is a compiler verdict against this
copy.

The published types used to be the harder half of the problem, and are no
longer. `Uri.t` was abstract at the bare `value` kind in this switch, so
`Syndic.Atom.feed`, `entry`, `link`, `author`, `id`, `icon` and `logo` could
not cross portability however the interface was annotated, and a kind
annotation could not fix it from here because the kind that had to change was
on an abstract type in another package. Those types now hold `Uriz.t`, which
`vendor/ocaml-uri` gives the `immutable_data` kind, so a feed parsed once can
be captured by a portable closure. `Ptime.t` was the other half of this and
`vendor/ptime` settled it the same way.

The function bodies still do not clear, and the chain is shorter than it was.
Annotating `Syndic_atom.to_xml` names `Syndic_common.Util.add_node_option` as
the blocker. Annotating `Syndic_common` names `Syndic_xml.resolve`, which
used to stop at `Uri.resolve` and no longer does, since every `Uriz` export
is `portable`. On the date side, `Syndic_date` is `Ptime` under other names
and `vendor/ptime` has annotated that. `Xmlm.make_output` is nonportable, so
`Syndic_atom.output` would still fail on `xmlm`.

The blockers local to this copy are all in `syndic_date.ml`, and one of them
is gone. `month_to_int` was a module-level `Hashtbl` filled by a top-level
`let ()`, which is the shape the htmlit vendoring fixed by replacing a
module-level `Set` with a match, and it is a match here now. With it gone,
putting `@@ portable` at the head of `syndic_date.mli` no longer names
`of_rfc822`. It names `to_rfc822`, which calls `day_of_week`, and
`day_of_week` is a closure over a `wday` array built at module level.
`month_of_date` is the same shape over a `months` array. Both are at
`syndic_date.ml:139-167`. Neither array is a module-level binding of its own,
so a search for one will not find them: the array is bound in the `let` that
returns the function. Those two are what is left, and they are the shape
`vendor/ptime` fixed three times.

What would have to land first, in order:

1. The two remaining array closures in `syndic_date.ml` turned into matches.
2. `xmlm` annotated, at least `make_output` and `to_xmlm`. It is vendored as
   of `vendor/xmlm`, unpatched, so the annotation has somewhere to land.
3. The annotation pass over `lib/` itself.

The third step used to be blocked on vendoring and annotating `uri`, and that
is what the `uriz` port replaced. `vendor/ocaml-uri` builds `uriz`, a rewrite
with a different interface rather than a patched `Uri`, and moving Syndic
onto it was a semantic fork of the parser rather than an annotation pass.
That fork has now happened, and what it bought is recorded above.

The callers are blocked independently of all this. `Arod.Feed.feed_string` is
nonportable, and so is `Arod.Feed.form_uri`, which is one call to
`Uriz.of_string_exn`. The Arod feed path also reaches `Arod.Ctx.author_exn`
and `Arod.Md.to_atom_html`, which are blocked on `Cmarkit` and on Bushel,
whose own interface carries no annotations. Making Syndic portable would
therefore not on its own collapse the `feed` or `blogroll` closure in
`Arod_env.t`.

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
   the `month_to_int` match, and whichever of the two remaining
   `syndic_date.ml` hunks step 1 confirmed.
4. Re-apply the `uriz` port, or decide against it. The new `lib/` will use
   `uri` throughout and will not build against `lib/dune`, which names
   `uriz`. The mapping is in "The uriz port" above, and
   `git show` on the commit "Move syndic from uri to uriz" gives the whole
   of it, including the consumers that break with it. Deciding against it
   means reverting that commit across the tree, not just here, because the
   published types are part of it.
5. Update the version in `syndic.opam`, in `dune-project` and in the first
   line of this file.
6. `dune build @avsm/sortal/all @avsm/sortal/runtest`, which reaches the
   `test_feed` suite. That pins the `<updated>` hunk, the three tiers of
   `uri_of_string` and its totality, and `test_rfc822_month_names` in the
   same file pins all twelve arms of `month_to_int` and that an unknown month
   is rejected. Nothing pins the trim or the RFC 3339 fallback, so a green
   build does not show that those two survived.
7. `dune build @avsm/arod/all @avsm/bushel/all`.

Do not add `@@ portable` while re-vendoring. Read the section above first,
then check whether `uriz`, `ptime` and `xmlm` have moved.

Build Requirements
==================

 * OCaml >= 4.01.0
 * Ptime >= 0.8.0
 * Xmlm >= 1.2.0
 * Uriz, from `vendor/ocaml-uri`

Documentation
=============

Upstream documentation is [here](http://cumulus.github.io/Syndic/).
