## xmlm - streaming XML codec for OCaml

This is xmlm 1.4.0, vendored from https://erratique.ch/software/xmlm and
patched so that its interface is callable from a `portable` context. The
patch is the twelve hunks listed below, all of them in `xmlm.ml` and
`xmlm.mli`. Everything else in those two files is the 1.4.0 release text.

The copy exists for two reasons.

`avsm/sortal/lib/feed` and `avsm/sortal/test` call `Xmlm` directly, so the
workspace has a first-party caller whose portability depends on this
interface, independently of anything else.

Syndic's interface is written in terms of `Xmlm.pos`, `Xmlm.input`,
`Xmlm.dest`, `Xmlm.tag`, `Xmlm.attribute` and `Xmlm.name`, so a portable
syndic needs a portable xmlm first. A vendored `public_name` is what lets an
annotation land here at all.

### What differs from the upstream distribution

* `dune` and `dune-project` are written for this workspace. Upstream builds
  with ocamlbuild and topkg.
* `xmlm.opam` is the upstream file with the `ocamlfind`, `ocamlbuild` and
  `topkg` build dependencies replaced by `dune`, and with a paragraph in the
  description recording why the copy exists.
* Upstream's `CHANGES.md`, `README.md`, `doc/` and `test/` are not vendored.
  The release this copy came from is named in `xmlm.opam` and here.
* The portability patch below.

### Local patches

Twelve hunks, all from the commit "Annotate xmlm for OxCaml portability".
Seven are annotations, four replace a module-level mutable table with code,
and one swaps a stdlib functor for its portable twin.

`xmlm.mli`, four hunks.

1. A floating `@@ portable` at the top of the signature. Every value the
   module publishes is portable, which is what lets a `portable` closure call
   `make_input`, `input`, `input_tree`, `input_doc_tree`, `peek`, `eoi`,
   `pos`, `make_output`, `output`, `output_depth`, `output_tree`,
   `output_doc_tree`, `error_message` and the five printers, and read `ns_xml`
   and `ns_xmlns`.
2. `module type String` gains `@@ portable` and its `type t` gains the kind
   `value mod portable contended`. The kind is load-bearing rather than
   decorative: the codec binds constants of this type at module level, so
   without it `error_message` alone fails to satisfy the annotation, on
   `String.empty`.
3. `module type Buffer` gains `@@ portable`.
4. `module type S` gains `@@ portable`, so the functorial interface publishes
   the same guarantee the default instantiation does.

`xmlm.ml`, eight hunks.

5. `module type String` gains `@@ portable` and the kind, mirroring hunk 2.
6. `module type Buffer` gains `@@ portable`, mirroring hunk 3.
7. `module type S` gains `@@ portable`, mirroring hunk 4.
8. `utf8_len`, the 256-entry array of UTF-8 byte lengths, becomes a match.
9. Its one call site, `utf8_len.(b0)` in `uchar_utf8`, becomes `utf8_len b0`.
10. `Hashtbl.Make` becomes `Hashtbl.MakePortable` for the namespace table
    functor `Ht`. The two differ only in that the second requires a portable
    `hash` and delivers portable operations, which is what a per-handle table
    built inside `make_input` needs.
11. `predefined_entities`, the five-entry `Ht.t` built at module level,
    becomes ten string constants and a comparison chain, `predefined_entity`.
12. Its one call site, `Ht.find predefined_entities ent`, becomes
    `predefined_entity ent`. The surrounding `try ... with Not_found` is
    unchanged, because the chain raises `Not_found` where `Ht.find` did.

Hunks 8 and 11 exist because a module-level array and a module-level hash
table are both mutable data. A function that reads one is nonportable, and
everything that calls that function is nonportable in turn, which in this
codec is every entry point. Neither table is ever written after it is built,
so nothing is lost by turning it into code.

**Equivalence of hunk 8.** The match answers the array's value for all 256
byte values.

| Byte range | Array | Match |
| --- | --- | --- |
| `0x00`-`0x7F` | 1 | 1 |
| `0x80`-`0xC1` | 0 | 0 |
| `0xC2`-`0xDF` | 2 | 2 |
| `0xE0`-`0xEF` | 3 | 3 |
| `0xF0`-`0xF4` | 4 | 4 |
| `0xF5`-`0xFF` | 0 | 0 |

The ranges were derived from the release's array literal, not from the UTF-8
specification, so the table above is a transcription and not a re-derivation.
The first arm of the match raises `Invalid_argument "index out of bounds"` for
an argument outside `0x00`-`0xFF`, which is what indexing the array raised. It
is reachable only from a `` `Fun `` source whose callback breaks its documented
contract by returning something that is not a byte.

**Equivalence of hunk 11.** `Ht` was built on `str_eq` as its equality, and
the chain compares with `str_eq` in the same order, so it finds what `Ht.find`
found and raises `Not_found` where `Ht.find` raised it.

| Entity | Expansion |
| --- | --- |
| `lt` | `<` |
| `gt` | `>` |
| `amp` | `&` |
| `apos` | `'` |
| `quot` | `"` |

### Where the portable boundary falls

Nowhere inside xmlm. All three arms of `source` and all three arms of `dest`
are usable from a `portable` closure, including `` `Channel `` and `` `Fun ``,
provided the closure makes the channel, the buffer or the callback itself.
That was measured, not assumed: a `portable` closure that opens a file, writes
a document to it through `` `Channel ``, reads it back through `` `Channel ``,
and separately round-trips a document through `` `Fun `` on both sides,
compiles and runs. Two of those four are checks in the guard test.

The one thing a `portable` closure cannot do is read a `Buffer.t`, an
`in_channel` or an `out_channel` that was bound at module level, because those
are mutable data and do not cross contention. That is stdlib's rule about
sharing a sink between domains, it is not something this interface imposes,
and it does not apply to a destination passed in as an argument. Syndic only
ever takes a `dest` as an argument, so it is unaffected.

`type input` and `type output` carry no kind. They are records of mutable
fields and closures, so at best they would be mutable data, and nothing in the
workspace holds one at module level or moves one between domains. Adding a
kind would constrain the implementation for no caller's benefit.

The functorial interface is narrowed in two separate ways, both of which a
prospective functor argument has to satisfy. The kind in hunks 2 and 5 says
`String.t` must cross portability and contention, so an argument whose strings
are a mutable rope no longer typechecks. The `@@ portable` in hunks 2, 3, 5
and 6 says something different and independent: every operation the argument
supplies must itself be portable, so an argument whose `append`, `iter`,
`add_uchar` or `contents` closes over module-level mutable state is refused
even if its `t` has the right kind. That is the price of `Make` producing a
portable module at all. The functorial interface is deprecated upstream and
this workspace only ever uses the default instantiation.

### Tests

`avsm/arod/test/test_xmlm.ml`, 14 checks, is the guard. It lives outside
`vendor/` because the root `dune` declares `(vendored_dirs vendor)` and dune
skips aliases there, so a `runtest` alias under this directory would never
run. It ascribes `@ portable` to each closure that touches `Xmlm`, so it fails
to compile if a re-vendor drops hunk 1: stripping the floating `@@ portable`
gives `The value "Xmlm.make_output" is "nonportable" but is expected to be
"portable"`. Stripping the kind in hunks 2 and 5 fails earlier still, inside
this directory, on `error_message` and `String.empty`.

It also pins the two replaced tables: one check per UTF-8 sequence length, two
malformed lead bytes, all five predefined entities, and an entity that falls
through to the `~entity` callback.

`avsm/sortal/test/test_feed.ml` and `avsm/sortal/test/test_feed_sync.ml`
drive this copy through syndic on real feed documents and are the behavioural
gate.

### The sitemap side effect

A vendored `public_name` shadows the installed package for the whole
workspace, and two libraries named `xmlm` cannot be linked into one
executable. Every workspace dependency of xmlm therefore has to be built from
source alongside it. Of the installed packages whose META requires xmlm,
`syndic` was already vendored and `sitemap` was not. `vendor/sitemap` is that
copy. It began as an unpatched v1.0 and now carries a one-hunk portability
annotation of its own, which this copy is what makes possible.

### Re-vendoring checklist

1. Copy `src/xmlm.ml` and `src/xmlm.mli` from the new release over this
   directory, keeping `dune`, `dune-project`, `xmlm.opam`, `LICENSE.md` and
   this file.
2. Re-apply the twelve hunks above. Upstream carries none of them, so a
   straight copy loses the whole patch and every portable caller stops
   compiling.
3. Update the version in `xmlm.opam` and in the first line of this file.
4. Rebuild every consumer, since this copy shadows the installed package for
   all of them: `dune build @avsm/arod/all @avsm/arod/runtest
   @avsm/sortal/all @avsm/sortal/runtest`. Arod reaches this copy through
   `vendor/sitemap`, sortal through `vendor/syndic` and through its own direct
   dependency. Do not add `@vendor/xmlm/all` or any other alias under
   `vendor/`. The root `dune` declares `(vendored_dirs vendor)`, so dune skips
   aliases there: such a build exits 0 having compiled nothing, which reads as
   a pass and is not one.
5. `test_xmlm` pins the annotations and both replaced tables, and the sortal
   feed suites pin the behaviour.
