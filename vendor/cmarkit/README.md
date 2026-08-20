## cmarkit - CommonMark parser and renderer for OCaml

This is cmarkit 0.3.0, vendored from https://erratique.ch/software/cmarkit and
patched for OxCaml portability.

The copy was taken from `~/.opam/5.2.0+ox/lib/cmarkit`, which the switch built
from `cmarkit.0.3.0+ox`. That package applies one patch to upstream, in
`tool/cmd_latex.ml`, and the tool is not vendored, so the sources landed here
matched both the installed copy and the upstream tarball, file by file under
`cmp`. Everything that has moved since is below.

### The structural hunks

These change what a module-level value is, not what any interface promises
about modes. The mode annotations are in the next section.

One rule explains most of it. **A module-level value of a stdlib container
cannot be read from a portable function, even when the container is immutable
and even when the functor is `MakePortable`.** `Map.S` and `Set.S` declare no
kind on `t`, so such a value is seen as contended. There are three ways out,
in increasing cost: call a constructor instead of reading a constant, replace
the container with code, or move to `iarray`. All three appear below.

* `cmarkit_data.ml`. The four derived tables, `whitespace_uset`,
  `punctuation_uset`, `case_fold_umap` and `html_entity_smap`, were
  module-level `Set.Make (Uchar)`, `Map.Make (Uchar)` and `Map.Make (String)`
  values, and every one of them is read on the parsing path. They are now
  `iarray`s sorted by key, from `Basement.Stdlib_iarray_labels`, that a binary
  search reads. Each is still *built* through the stdlib container upstream
  used and then frozen, which is what keeps upstream's duplicate handling, a
  later entry of the source array winning, and keeps the sort order out of
  this file's hands. The containers are local to the initialisation, which
  runs once. The generated arrays in `cmarkit_data_uchar.ml` and
  `cmarkit_data_html.ml` are untouched: they are read at initialisation only.

  Lookup stays logarithmic and is not slower. Two details earn that. Each
  table's last index is computed once rather than on every lookup, and the
  searches halve with `lsr` rather than `/`, which on a possibly negative
  operand compiles to sign correction. Written the obvious way the searches
  run about a third slower than the Set and Map they replace. Written this
  way, over
  66 million lookups, whitespace runs in 0.126 s against 0.165 s, punctuation
  in 0.222 s against 0.270 s and case folding in 0.265 s against 0.310 s. The
  entity table is unchanged within noise, and end to end rendering does not
  move.

* `cmarkit_base.ml`, `Meta_dict`. `Meta.t` has to cross portability and
  contention, because a client holds a `Meta.key` at module level and reads it
  from a portable function. The dictionary it was built on cannot: `Dict`'s
  key is a packed first-class module, whose kind is `value non_float`, and its
  container is a `Map.Make (Int)`. Neither crosses.

  `Dict` is not the place to fix that. It is also `Cmarkit_renderer`'s state
  dictionary, and all three renderers stash a `mutable` record in it, so an
  `immutable_data` bound on its bindings is rejected. `Meta` therefore has a
  dictionary of its own. `Meta_dict`'s key is the Stdlib's `Type.Id.t`, which
  declares `immutable_data`, and its container is an association list, since
  no stdlib container carries a kind.

  The association list is not a semantic change. This dictionary is reached
  only through `mem`, `add`, `tag`, `remove` and `find`, none of which
  observes order, `add` shadows an existing binding as `Map.add` did, and
  there is no iteration in the interface. It holds a couple of bindings.

* `cmarkit_base.ml`, `Dict`. Upstream shims `Type.Id` inside this module, for
  compilers before 5.1, by packing a first-class module. That shim is deleted
  and the Stdlib's `Type.Id` is used instead. The bodies of `key`,
  `provably_equal` and `uid` are the same words either way, so the module
  reads as it did, but the Stdlib's identifier declares `immutable_data` and
  the shim's packed module is `value non_float`. That kind is what lets a
  backend hold its `Cmarkit_renderer.Context.State` key at module level and
  read it from a portable rendering function, which both backends do.
  `Map.Make (Int)` becomes `Map.MakePortable (Int)` and `empty` becomes
  `empty ()`, since a module-level map value does not cross.

* `cmarkit_base.mli` and `cmarkit.mli`, `Meta.add` and `Meta.find`. Both gain
  an `('a : immutable_data)` bound, which the implementation forces. This is a
  real narrowing of the public interface: a metadata value whose type does not
  cross is now refused. It costs nothing in this tree. Nothing in `avsm/` or
  `bleeding/` calls `Meta.add`, and the thirteen calls to `Meta.tag` and
  `Meta.find` in `avsm/bushel/lib/bushel_md.ml` all carry `unit`.

* `cmarkit_base.mli`, the `Dict` doc comment. It said `Dict` is used by
  `Cmarkit.Meta`, which is no longer true. It names the renderer state
  instead.

* `cmarkit_base.ml`, the two HTML block start conditions.
  `html_start_cond_1_set` and `html_start_cond_6_set` were module-level
  `Set.Make (String)` values read by `html_block_start`, which runs on every
  line that opens with a tag. They are matches, `is_html_start_cond_1_tag` and
  `is_html_start_cond_6_tag`, and the same 4 and 62 names answer `true`. This
  is the shape htmlit's `void_els` took. `module String_set` had no other user
  in the file and is gone.

* `cmarkit.ml`, three functor applications. `Label.Map`, `Pos_set` and
  `Closer_index` become `Map.MakePortable` and `Set.MakePortable`. Their
  operations are read from parsing and rendering functions that the annotation
  slices must make portable, and the swap is invisible: `MakePortable` answers
  with `sig @@ portable include S ... end`, so `Label.Map` still satisfies the
  `Map.S with type key = string` its interface promises. `Closer` needed no
  change, its `Stdlib.compare` being portable already. This does not on its
  own let `Label.Map.empty` be read from a portable function. That call site
  changes when the function that reads it is annotated and the compiler names
  it.

* `cmarkit.ml`, `module String_map = Map.Make (String)`. Dead, with no use
  anywhere in the file. Deleted, so that no reader takes it for a functor the
  portability pass forgot.

* `cmarkit_html.ml`, `String_set`. `Set.MakePortable (String)`, for the same
  reason as the three above. Its members are the heading identifiers already
  handed out, held in the renderer's mutable state.

### The annotation hunks

Three interfaces carry a floating `@@ portable` at the head of the file, which
makes every value in them portable: `cmarkit_data.mli`, `cmarkit_base.mli` and
`cmarkit.mli`. Together they let a `@ portable` closure parse a document,
resolve its labels through a resolver of its own, and map and fold the tree.
The renderer and the two backends follow.

Four types carry a kind, and no more than four do. A kind is only worth adding
where a value of the type has to be held at module level and read from a
portable function, and most of cmarkit's types cannot carry one in any case:
`Inline.t`, `Block.t` and `Label.def` are extensible variants, whose kind is
`value`, and `Doc.t` reaches them.

* `Cmarkit.Meta.key` and `Cmarkit_base.Meta.key`, `immutable_data`. This is the
  one the campaign was gated on. A client mints a key once at module level and
  reads it from a portable function, and it has no choice about that: a key
  minted twice is two identities and `Meta.find` matches on identity, so
  moving the key inside its reader would silently return `None` for ever.
  `avsm/bushel/lib/bushel_md.ml` holds two.
* `Cmarkit.Meta.t` and `Cmarkit_base.Meta.t`, `immutable_data`, which the key's
  own use forces.
* `Cmarkit.Textloc.t` and `Cmarkit_base.Textloc.t`, `immutable_data`, which
  `Meta.t` contains.
* `Cmarkit_base.Dict.key`, `immutable_data`. This is the key of the renderer
  state dictionary, which both backends hold at module level. It costs
  nothing, being a `Type.Id.t` once the shim is gone. The annotation that
  reads it is in the renderer's own hunk.

Six `.empty` sites became calls, each named by the compiler the moment the
function that reads it was required to be portable:

| Site | Read | Now |
| --- | --- | --- |
| `cmarkit.ml:781`, `Block.defs` | `?(init = Label.Map.empty)` | `Label.Map.of_list []` |
| `cmarkit.ml:880`, `parser` | `?(defs = Label.Map.empty)` | `Label.Map.of_list []` |
| `cmarkit.ml:886`, `parser` | `cidx = Closer_index.empty` | `Closer_index.of_list []` |
| `cmarkit.ml:1110`, `rev_token_list_and_make_closer_index` | `loop Closer_index.empty` | `loop (Closer_index.of_list [])` |
| `cmarkit.ml:2961`, `Doc.make` | `?(defs = Label.Map.empty)` | `Label.Map.of_list []` |
| `cmarkit_renderer.ml:49`, `Context.make` | `Dict.empty` | `Dict.empty ()` |

Three constants became functions, which is a break in the published
interface. Each is a value whose type cannot cross, so there is no annotation
that would have kept it a constant.

* `Cmarkit.Doc.empty`, now `unit -> t`. `Doc.t` holds a `Label.Map.t` and
  reaches `Block.t`. `Cmarkit_renderer.Context.make` seeds a context with it
  and `Context.doc` clears the context back to it, and both must be portable.
  Nothing else in this tree reads it.
* `Cmarkit.Inline.empty` and `Cmarkit.Block.empty`, now `unit -> t`. Both are
  extensible variant values. `Block.Footnote.stub` and the two `Mapper`
  defaults read them, all inside `cmarkit.ml`. Nothing in this tree reads
  either.

### What was deliberately left alone

* `cmarkit_latex.ml` and `cmarkit_latex.mli`. The LaTeX renderer is on no
  in-tree path. Its `String_set` is untouched and its interface carries no
  annotation, so `Cmarkit_latex.of_doc` is not callable from a portable
  function. Annotating it would be the same work as the interfaces above and
  would buy nothing today.

* `cmarkit_commonmark.ml`'s `Char_set` and its five module-level escaping
  sets. They do need to change, and a predicate is the shape that works, which
  breaks the published type of `escaped_string` and `buffer_add_escaped_string`
  and removes the `module Char_set : Set.S` beside them. That break belongs
  with the annotation that forces it, in the backends' own hunk.

* `Cmarkit.Mapper.default`, `Mapper.delete` and `Folder.default`. These are
  module-level values of a polymorphic variant type, which crosses nothing, so
  a portable function cannot read them. The literals they stand for,
  `` `Default ``, `` `Map None `` and `` `Fold v ``, are what a portable
  mapper or folder writes instead, and `Mapper.ret` and `Folder.ret` are
  function calls and work. Making the three constants functions would break
  every existing mapper for a convenience that already has a one-word
  spelling.

### Behaviour identity

Three independent checks.

`avsm/arod/test/test_md_golden.ml` renders four documents through the three
markdown renderers and the sidenote collector and compares 34 results against
checked-in bytes. All 34 pass unchanged. It bites: a `Meta_dict.add` that drops
its binding, which is what a wrong dictionary would look like, fails it at
`links.article.html` byte 183.

`avsm/arod/test/test_cmarkit_tables.ml` is new here, and is the standing guard
on the tables. 6665 checks, all of them against the generated arrays the
tables are built from rather than against checked-in expectations, so new
Unicode data in a later release needs no edit. Every listed whitespace,
punctuation and case folding code point answers through its accessor and no
unlisted one does, all 2125 entity names resolve to their replacements while a
name with a character added or removed resolves only if it is itself a name,
and all 66 condition 1 and condition 6 tag names behave as their condition
says while 17 other names and the six unreachable numbered heading names do
not. Nine mutations were run against it and all nine fail it: a short last
index on each of the four tables, a lowercasing entity lookup, a case fold
search that never hits, a dropped condition 6 tag, an added one, and a dropped
condition 1 tag. Two more cover the two ends of the search window those nine
left untested, and both fail it too: an entity search that starts at index 1,
so the first entry is unreachable, and one that gives up on a one-element
window by testing `lo >= hi` for `lo > hi`.

A differential harness, built in a scratch directory, compiles one driver
against the pristine 0.3.0 sources and against this copy and diffs the two
transcripts. 5887 lines, 140189 bytes, identical. It covers every code point
from 0 to 0x10FFFF through the three Unicode accessors, all 2125 entity names,
38 near-miss entity probes, a digest over 232699 bytes of further near misses
built from every proper prefix of every name, every name with a character
appended, three case foldings of every name and every one and two character
ASCII string, all 66 condition 1 and condition 6 tag names in the five shapes
that reach `html_block_start` plus 19 names that must not match, and 15
documents rendered through strict HTML, extended HTML, CommonMark, LaTeX, a
CommonMark round trip and a text location fold.

Fourteen mutations were run against it. Twelve are caught: a reversed entity
comparison, a lowercasing entity lookup, a short last index on each of the
three sorted tables, shifted case fold keys, a case fold search that never
hits, a dropped whitespace code point, a dropped condition 1 tag, two dropped
condition 6 tags and an added one. The other two are not caught and should
not be, both being equivalent rewrites. One of them is `(lo + hi + 1) lsr 1`,
which is a correct binary search.

### What differs from the upstream distribution

* `cmarkit.opam` is the upstream file with the `ocamlfind`, `ocamlbuild` and
  `topkg` build dependencies replaced by `dune`, since the workspace builds
  this copy directly, with `basement` added, and with a paragraph in the
  description recording the patch. The `cmdliner` depopt and the `build:`
  stanza are dropped with the command line tool they served.
* `dune` and `dune-project` are written for this workspace rather than taken
  from upstream.
* Upstream builds with any OCaml from 4.14 and this copy needs an OxCaml
  compiler. `cmarkit.opam`'s `ocaml` bound is `>= 5.1.0`, which is the
  Stdlib's `Type.Id` and is necessary rather than sufficient. The mode and
  kind syntax in the interfaces and the `basement` dependency both ask for
  OxCaml on top of it.
* Upstream has no dependencies and this copy has one, `basement`, for the
  `iarray` operations `cmarkit_data.ml` now uses. There is no `Iarray` module
  in this switch's stdlib, and `basement` is where `Stdlib_iarray_labels`
  lives. It is oxcaml-native and ships with the switch. This is the first
  thing in `avsm/`, `bleeding/` or `vendor/` to use it.
* Upstream builds one library whose modules are all top level and publishes
  five of them, `Cmarkit`, `Cmarkit_renderer`, `Cmarkit_commonmark`,
  `Cmarkit_html` and `Cmarkit_latex`, through topkg's `~api` list. Dune cannot
  publish some modules of an unwrapped library and hide the rest, so all nine
  are visible here. That is a superset of the upstream interface and no
  in-tree module names collide with the four now exposed.
* `tool/`, upstream's `cmarkit` command line program, is not vendored.
  Nothing in the workspace runs it, and it is the only thing that wanted
  `cmdliner`.
* `test/`, `doc/`, `support/`, `B0.ml` and upstream's `CHANGES.md` and
  `README.md` are not vendored. The release this copy came from is named in
  `cmarkit.opam` and here.

### Why the copy exists

Nothing in the switch requires cmarkit. A sweep of every `META` under
`~/.opam/5.2.0+ox/lib` for a `cmarkit` requirement matched only cmarkit's own,
so shadowing the installed package with this one pulls no other package into
the workspace build. The four in-tree consumers, `avsm/arod/lib`,
`avsm/arod/bin`, `avsm/bushel/lib` and `avsm/bushel/lib_web`, name the library
`cmarkit` and link unchanged.

### Re-vendoring checklist

1. Copy the nine `.ml` files, the seven `.mli` files and `LICENSE.md` from the
   new release over this directory, keeping `dune`, `dune-project`,
   `cmarkit.opam` and this file.
2. Reapply the hunks above.
3. Update the version in `cmarkit.opam` and in the first line of this file.
4. Dune skips aliases under a vendored directory, so `dune runtest` does not
   reach anything here. Verify through the consumer aliases instead:

       dune build @avsm/arod/all @avsm/arod/runtest --force
       dune build @avsm/bushel/all @avsm/sortal/all @avsm/sortal/runtest

   Two tests there are the gate. `avsm/arod/test/test_md_golden.ml`
   byte-compares 34 rendered results against checked-in files, so a cmarkit
   change that alters one byte of output fails there. Never regenerate the
   goldens to make it pass. `avsm/arod/test/test_cmarkit_tables.ml` checks the
   four tables against the generated arrays they are built from, so it
   survives new Unicode data but not a mis-built table. Its two lists of tag
   names are the one thing in it that a release could legitimately move.
5. Rebuild the differential harness described above, since the two tests reach
   only what their corpus and their tag lists reach. Two scratch dune
   projects, one holding the new pristine sources and one holding this
   directory, each with the driver and `(flags (:standard -w -a))`, and the
   two transcripts diffed. The driver is not checked in, because it has to be
   written against whichever release is being vendored.
