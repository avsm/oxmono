Syndic
======

RSS and Atom feed parsing

This is Syndic 1.7.0, vendored from https://github.com/Cumulus/Syndic and
patched to parse feeds that real publishers emit. It has also been ported off
opam `uri` onto the vendored `uriz`, which is a breaking change to the public
interface, and annotated so that its whole interface is callable from a
`portable` context. The sections below record all three.

Local patches
=============

Six hunks. Three of them make the parser accept a document that the
specification forbids, the fourth is a portability prerequisite that changes
nothing a caller can see, the fifth is the `uriz` port, which changes every
published type that carries a URI, and the sixth is the portability
annotation. Everything else in `lib/` is believed to be upstream 1.7.0. The
hunks differ in how well that provenance is established, so each one says
below how it was determined.

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

* All of `lib/`, annotated for OxCaml portability. Nine interfaces gained a
  `@@ portable` modality, `Syndic_common.XML.generate_catcher` and
  `dummy_of_xml` gained modes so that the parsers built from them stay the
  module-level partial applications upstream wrote, and four module-level
  mutable tables became code. Five call sites moved. The detail is in "The
  portability annotation" below.

  Provenance: certain. This hunk is the commit "Annotate syndic for OxCaml
  portability", which touches this directory, `avsm/arod/test` and this file.

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
   input that survives step 2 and is still not a reference reaches here: a
   non-numeric port, a first segment holding a colon, or a bracket inside a
   registered name, as in `http://ex[a]mple.com/x`, where step 2 leaves the
   bracket alone because it is inside the authority. `Uri.of_string` produced
   escaped-bracket garbage for that last shape rather than an opaque segment,
   so the two disagree there, and neither result names the intended host.

`Uri.to_string` is `Uriz.to_string`, and that is not a free rename. Both
render a canonical form, but not the same one, so a URL that was already
valid can come out spelled differently. Measured on already-valid input, uri
re-encodes `+` and `;` in a query and `&`, `+` and `;` in a fragment, encodes
sub-delimiters in a host, and decodes pre-encoded reserved triplets such as
`%2B`, `%2F` and `%3A` in paths and queries. So `?q=ocaml+uri` renders as
`?q=ocaml%20uri` under uri and unchanged under uriz, `/wiki/C%2B%2B` becomes
`/wiki/C++` under uri, and `?to=https%3A%2F%2Fa.org%2Fb` becomes
`?to=https://a.org/b`. uriz is right in each case, and uri decoding `%2F`
changes which resource the URL names, but this is a production-visible byte
change in every feed this library emits from the swap commit onwards.
Anything that persists a rendered URL as a key has to canonicalise it rather
than compare raw text.

The rest map directly. `Uri.resolve "" base u` is `Uriz.resolve ~base u`,
which is plain RFC 3986 section 5.2. `Uri.with_fragment u (Some d)` is
`Uriz.with_fragment u (This d)`, and `Uri.scheme` and `Uri.host` return
`or_null` rather than `option`. Two calls needed more than a rename.
`Uriz.make`, which the RSS2 `<cloud>` parser uses, agrees with `Uri.make` on
every shape a `<cloud>` can produce and refuses only a negative port, which
the port attribute can carry, so `Syndic_rss2.make_cloud` catches that and
falls back to `uri_of_string` on the assembled text. `Uriz.with_query` takes
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
port, so nothing in that corpus falls in the `to_string` divergence class
above. That is evidence about those six feeds, not a general guarantee.

Callers that hold a rendered URL
================================

Two consequences of the section above reach out of this directory.

`Arod.Ctx` keys its feed annotation file on the rendered entry URL. Keys
written before the swap carry uri's spelling, so `Arod.Ctx.annotation_index`
re-keys the file through `Arod.Ctx.normalise_url` on load, and
`arod feed associate` writes that form. `avsm/arod/test/test_feed_annotations.ml`
pins both halves. Any other store keyed on a rendered URL needs the same
treatment.

`Arod.Feed.form_uri` uses the raising `Uriz.of_string_exn`, and it runs on
more than configuration. `Arod.Feed.atom_id` calls it on
`Bushel.Entry.site_url`, whose slug comes from a note's frontmatter or its
filename. That is deliberate. A slug is site-owner-authored internal content
and a malformed one should fail loudly rather than be coerced into a URL
nobody meant. The failure mode is worth knowing: `Arod.Feed.feed` re-raises,
so the whole feed route fails and the server logs it through `on_error` and
answers with an error rather than the feed, until the slug is corrected. That
is louder than a silently wrong `<id>`, but it takes out the route rather
than the one bad entry.

Tests
=====

`avsm/sortal/test/test_feed.ml` holds the regression tests. Without the
`<updated>` hunk its excerpt raises `Syndic.Error.Error`, and
`test_rfc822_month_names` walks the twelve month names and checks that an
unknown month is rejected. It cannot see `Not_found` itself, since every call
site catches every exception. The trim and the RFC 3339 fallback have no test
of their own, which is a second reason to re-derive them rather than reapply
them from this list.

`avsm/arod/test/test_syndic.ml` holds the portability guard. See "The
portability annotation" below for what it pins.

The portability annotation
==========================

Every one of the nine interfaces in `lib/` carries `@@ portable`, so a
`portable` closure can call `Syndic.Atom.parse`, `to_xml`, `output` and
`aggregate`, `Syndic.Rss1.parse`, `Syndic.Rss2.parse` and `to_atom`,
`Syndic.Opml1.parse`, `to_xml` and `output`, `Syndic.W3C.parse` and `url`,
`Syndic.Date` in full and `Syndic.XML` in full. A `Syndic.Atom.feed` built or
parsed once can be held at module level and read from a portable closure,
because every type reachable from it is data: `Uriz.t` and `Ptime.t` both have
the `immutable_data` kind, `Xmlm.pos` is a pair of `int`, and the rest is
strings, lists, options and variants of those. Nothing in the interface is
left nonportable, `Syndic_xml.input_of_channel` included. What a portable
closure still cannot do is read an `in_channel`, an `out_channel` or a
`Buffer.t` bound at module level and hand it to `parse` or `output`, but that
is stdlib's rule about a shared sink rather than anything this library
imposes, and passing one in as an argument is unaffected.

None of the published types needed a kind annotation. They are concrete
records and variants, so the compiler derives their kinds from their fields.

`avsm/arod/test/test_syndic.ml` is the guard, 12 checks. It lives outside
`vendor/` because the root `dune` declares `(vendored_dirs vendor)` and dune
skips aliases there, so a `runtest` alias under this directory would never
run. Strip the `@@ portable` from `syndic_atom.mli` and the guard stops
compiling, with `The value "Syndic.Atom.output" is "nonportable" but is
expected to be "portable"`.

What the implementations needed
-------------------------------

Nothing in the annotation changes what any function computes. Rendering the
six feeds in `test/` through `Syndic.Atom.output` gives XML byte-identical to
what the copy produced before this pass, 6165 lines, MD5
`db46ba2797ac8cd0cfcbabe289f0313f`, 0 parse failures.

**Modes on `generate_catcher` and `dummy_of_xml`, 2 declarations.** This is
the whole of it, and everything else below is a consequence.

Almost every parser in this library is a module-level partial application:

```ocaml
let entry_of_xml =
  let data_producer = [ ... ] in
  generate_catcher ~namespaces ~data_producer make_entry
```

That binds a closure into the module, and a module-level closure is
nonportable unless the function it came from says otherwise, which drags every
parser above it down to `parse`. `lib/` holds 83 calls to `generate_catcher`
and 53 partial applications of `dummy_of_xml`, counted with

    grep -c 'generate_catcher ' lib/syndic_{atom,rss1,rss2,opml1,w3c}.ml
    grep -h 'dummy_of_xml ~ctor' lib/*.ml | wc -l

and the fix is two declarations in `syndic_common.{ml,mli}` that leave all but
four of them exactly as upstream wrote them. Both halves of each declaration
are needed. In the implementation, the mode goes on parameters whose types are
**written out**, and on the returned closure:

```ocaml
let generate_catcher ?(namespaces : string list @ portable = [""])
    ?(attr_producer :
       (string * (xmlbase:Uriz.t option -> string -> 'a)) list @ portable = [])
    ?(data_producer :
       (string * (xmlbase:Uriz.t option -> node -> 'a)) list @ portable = [])
    ?(leaf_producer :
       (xmlbase:Uriz.t option -> Xmlm.pos -> string -> 'a) option @ portable)
    (maker @ portable) = ...
  let generate : _ @ portable = fun ~xmlbase ((pos, tag, datas) : node) -> ...
```

In the interface, the same modes on the parameters **and a mode on the
result**, which is what makes the partial application portable at a call site
in another compilation unit:

```ocaml
  val generate_catcher :
       ?namespaces:string list @ portable
    -> ...
    -> (pos:Xmlm.pos -> 'a list -> 'b) @ portable
    -> (xmlbase:Uriz.t option -> node -> 'b) @ portable
```

`dummy_of_xml` takes the same treatment on its one parameter and its result,
so a producer list holding `dummy_of_xml ~ctor:...` entries stays at module
level too.

Writing the types out is not decoration. An unannotated `?(attr_producer =
[])` with only `@ portable` on it still leaves `generate` reading it at
`contended`, and the build stops with `This value is "contended" ... expected
to be "uncontended"`. Four earlier shapes failed for that reason: a result
mode alone, `@ portable` on `maker` alone, `@ portable` on the untyped
optional parameters, and `@ portable` on the inner `catch_attr` /
`catch_datas` / `generate` bindings.

**Consequences elsewhere, 5 sites.** Everything else in `lib/` is untouched.
`syndic_rss1.ml` and `syndic_opml1.ml` are byte-identical to what they were
before this pass.

- `Syndic_atom.author_of_xml` and `contributor_of_xml` need parentheses.
  `generate_catcher` now delivers its parser in one application, so
  `generate_catcher ... maker ~xmlbase xml` is rejected with `This
  application is complete, but surplus arguments were provided afterwards`.
  Written as `(generate_catcher ... maker) ~xmlbase xml` it compiles. Only
  these two are affected, because only they apply the node in the same
  expression rather than storing the parser.
- `Syndic_w3c.errorlist_of_xml` and `warninglist_of_xml` need the same
  parentheses and, additionally, take the node as an argument. They fix
  `~xmlbase:None` at the definition, and `(generate_catcher ... maker)
  ~xmlbase:None` on its own is still a closure, which at module level is still
  nonportable.
- `Syndic_common.Util.datas_has_leaf` was `List.exists (function ...)`, a
  partial application of a function that says nothing about modes, so it is
  eta expanded.

**Module-level mutable tables, 4.** All four are memoised pure functions and
none is written after it is built, so turning each into code loses nothing.

| Where | Was | Is |
| --- | --- | --- |
| `syndic_date.ml` `month_of_date` | `[|Jan; ...; Dec|]` indexed by `i - 1` | a match on `i` |
| `syndic_date.ml` `day_of_week` | `[|Thu; ...; Wed|]` indexed by the day count mod 7 | a match on that index |
| `syndic_rss2.ml` `valid_local_part` | `Array.init 256 is_valid` | `is_valid_local_part` applied to the character |
| `syndic_rss2.ml` `valid_domain_part` | `Array.init 256 is_valid` | `is_valid_domain_part` applied to the character |

The two `syndic_date.ml` arrays were bound inside the `let` that returned the
function, not as module-level bindings of their own, which is why a search for
them found nothing. Both matches answer what indexing answered, over the whole
range the caller can produce:

| `month_of_date` index | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Array | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec |
| Match | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec |

| `day_of_week` index | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Array | Thu | Fri | Sat | Sun | Mon | Tue | Wed |
| Match | Thu | Fri | Sat | Sun | Mon | Tue | Wed |

`Ptime.to_date` returns a month in `1 .. 12` and the day count mod 7,
adjusted, is in `0 .. 6`, so neither match can fall through. The last arm of
each raises `Invalid_argument "index out of bounds"`, which is what indexing
the array raised, so even an impossible index behaves as it did.

The two `syndic_rss2.ml` arrays memoised a predicate over all 256 bytes and
were read as `valid_local_part.(Char.code c)`. The predicate is now applied to
`c` directly, which is the same answer for every character, since the array
entry at `Char.code c` was the predicate at `Char.unsafe_chr (Char.code c)`.
Upstream's domain-part predicate lists `'.'` twice. The second is dropped, and
the predicate is unchanged.

**Stdlib functors, 1.** `Syndic_atom.LinkSet` is `Set.MakePortable (LinkOrder)`
rather than `Set.Make`. The two differ only in that the second requires a
portable `compare` and delivers portable operations, and `LinkOrder.compare`
already was one. `LinkSet.empty` is a module-level value of an abstract type
with no kind, so a portable function cannot read it: the one use is now
`LinkSet.of_list []`, the same empty set built rather than read.

The callers
-----------

`Arod.Feed.feed_string` and the `feed` and `blogroll` closures in `Arod_env.t`
are still nonportable, and syndic is no longer why. `Arod.Feed.form_uri` is
one call to `Uriz.of_string_exn`, which is portable. What remains is
`Arod.Ctx.author_exn` and `Arod.Md.to_atom_html`, blocked on `Cmarkit` and on
Bushel, whose own interface carries no annotations.

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
5. Re-apply the portability annotation. Upstream carries none of it, so a
   straight copy loses every `@@ portable` and every reshaped body, and
   `test_syndic` stops compiling. "The portability annotation" above says what
   each class of change is and why, and `git show` on the commit "Annotate
   syndic for OxCaml portability" gives the whole of it. Do the work rather than
   replaying the patch, because a new release moves the parsers around.
6. Update the version in `syndic.opam`, in `dune-project` and in the first
   line of this file.
7. `dune build @avsm/sortal/all @avsm/sortal/runtest`, which reaches the
   `test_feed` suite. That pins the `<updated>` hunk, the three tiers of
   `uri_of_string` and its totality, and `test_rfc822_month_names` in the
   same file pins all twelve arms of `month_to_int` and that an unknown month
   is rejected. Nothing pins the trim or the RFC 3339 fallback, so a green
   build does not show that those two survived.
8. `dune build @avsm/arod/all @avsm/arod/runtest @avsm/bushel/all`.
   `test_syndic` under `avsm/arod/test` is what pins the annotation and the
   four replaced tables.

Check whether `uriz`, `ptime` and `xmlm` have moved before assuming the
annotation still lands where it did.

Build Requirements
==================

 * OCaml >= 4.01.0
 * Ptime >= 0.8.0
 * Xmlm >= 1.2.0
 * Uriz, from `vendor/ocaml-uri`

Documentation
=============

Upstream documentation is [here](http://cumulus.github.io/Syndic/).
