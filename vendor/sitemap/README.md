## sitemap - XML sitemap generation for OCaml

This is sitemap v1.0, vendored from https://github.com/avsm/ocaml-sitemap and
patched so that its interface is callable from a `portable` context. The patch
is one hunk, listed below, in `sitemap.mli`. Everything else in `sitemap.ml`
and `sitemap.mli` is the v1.0 release text.

The copy exists for two reasons.

`vendor/xmlm` shadows the installed xmlm for the whole workspace, and two
libraries named `xmlm` cannot go into one executable, so every workspace
dependency of xmlm has to be built from source alongside it. sitemap is the
one such package that was not already vendored. That is why the copy is here
at all, and it can be dropped when `vendor/xmlm` is dropped, and not before.

`Arod_render.sitemap` renders `/sitemap.xml` and is `@@ portable`, so a
proffer handler calls it directly rather than through a closure in
`Arod_env.t`. That needs this interface annotated. A vendored `public_name` is
what lets an annotation land here at all.

### What differs from the upstream distribution

* `dune` and `dune-project` are written for this workspace. Upstream keeps the
  sources in `lib/` and generates its opam file from `dune-project`. This copy
  is flat and its opam file is checked in.
* `sitemap.opam` is the generated upstream file with `license: "ISC"` restored
  from the release's own opam metadata, the `ptime` and `odoc` `with-doc`
  dependencies dropped since no documentation is built here, `dune` raised to
  the workspace's 3.21, and a paragraph in the description recording why the
  copy exists.
* Upstream's `CHANGES.md`, `README.md`, `.github/` and `.gitignore` are not
  vendored. The release this copy came from is named in `sitemap.opam` and
  here.
* The portability patch below.

### Local patches

One hunk, from the commit "Annotate sitemap for OxCaml portability".

`sitemap.mli`, one hunk.

1. A floating `@@ portable` at the top of the signature, before the module's
   own documentation comment and therefore before every declaration it
   applies to. Every value the module publishes is portable, which is what
   lets a `portable` closure call `priority`, `changefreq_to_string`, `v`,
   `output`, `output_url`, `output_urlset` and `output_urlset_to_buffer`.

`sitemap.ml` is unpatched. Nothing in the implementation is module-level
mutable data: the four helpers `lastmod_to_string`, `priority_to_string`,
`tag` and the two output functions close over nothing, and there is no table
of the kind `vendor/xmlm` had to replace with code. The annotation compiled
on the first attempt.

No declaration needed a kind either. `type url` is a private record rather
than an abstract type, so the compiler reads `immutable_data` off its fields,
and `type priority` and `type lastmod` are a private `float` and a triple of
`int`. That is why a module-level `Sitemap.url list` can be read from inside a
`portable` closure, which the guard test does and which an abstract `url`
would have refused.

### Where the portable boundary falls

`output` is the only entry point a portable closure can call with no
preparation, and it is the only one arod uses. The other three take an
`Xmlm.output` or a `Buffer.t`, neither of which carries a kind, so a portable
closure reaches them only by making the output or the buffer itself. That is
stdlib's and xmlm's rule about sharing a sink between domains, not something
this interface imposes, and it does not apply to one passed in as an argument.
All three are exercised that way in the guard test.

### Tests

`avsm/arod/test/test_sitemap.ml`, 11 checks, is the guard. It lives outside
`vendor/` because the root `dune` declares `(vendored_dirs vendor)` and dune
skips aliases there, so a `runtest` alias under this directory would never
run. It binds a `Sitemap.url list` at module level and reads it inside a
`@ portable` closure, so it fails to compile if a re-vendor drops the hunk:
stripping the floating `@@ portable` gives `The value "Sitemap.output" is
"nonportable" but is expected to be "portable"`.

It also pins the document. Arod serves these bytes, so the checks cover the
element order within a `url`, an absent optional member, the seven change
frequencies, the clamping and one-decimal rounding of a priority, and the
2048-character refusal on a location.

`avsm/arod/test/render_capture.sh` fetches the real `/sitemap.xml` and is the
behavioural gate.

### Re-vendoring checklist

1. Copy `lib/sitemap.ml` and `lib/sitemap.mli` from the new release over this
   directory, keeping `dune`, `dune-project`, `sitemap.opam`, `LICENSE.md` and
   this file.
2. Re-apply the hunk above. Upstream carries none of it, so a straight copy
   loses the annotation and `Arod_render.sitemap` stops compiling.
3. Update the version in `sitemap.opam` and in the first line of this file.
4. `dune build @avsm/arod/all @avsm/arod/runtest`, which is where the
   consumers are. Do not add `@vendor/sitemap/all`. The root `dune` declares
   `(vendored_dirs vendor)`, so dune skips aliases there: such a build exits 0
   having compiled nothing, which reads as a pass and is not one.
5. `test_sitemap` pins the annotation and the document.
