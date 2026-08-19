## htmlit - HTML generation combinators for OCaml

This is htmlit 0.2.0, vendored from https://erratique.ch/software/htmlit and
patched for OxCaml portability.

The code patch touches `htmlit.ml` and `htmlit.mli` only:

* `El.add_child` decides whether an element is void by matching on its name,
  where upstream tested membership of a module-level `Set.Make (String)`. A
  set does not cross portability, so a module-level one cannot be read from a
  portable function, and `add_child` is on the path of every render. Matching
  needs no module-level value at all. The same fourteen names answer true, so
  `module String_set` and `let void_els` are both gone.
* `htmlit.mli` carries `@@ portable` at the head of `At`, `El` and `El.Low`,
  which annotates every value in the interface. Nothing needed a narrower
  annotation. `At.pp` is portable too, since only the global formatters of
  `Format` are nonportable and `pp` takes its own.
* `At.t` and `El.html` are given the `immutable_data` kind. Both are built
  from strings and lists, so a rendered fragment crosses portability and
  contention and can be held by a portable handler.

Rendering behaviour is unchanged, including escaping, class and style
merging, void elements, splice separators, the doctype and page generation.
This is checked by a differential run of every element and attribute
constructor against the pristine 0.2.0 sources.

What else differs from the upstream distribution:

* `htmlit.opam` is the upstream file with the `ocamlfind`, `ocamlbuild` and
  `topkg` build dependencies replaced by `dune`, `x-maintenance-intent`
  dropped, since the workspace builds this copy directly, and with a
  paragraph in the description recording the patch.
* `dune`, `dune-project` and `test/` are written for this workspace rather
  than taken from upstream.
* Upstream's `CHANGES.md`, `README.md` and doc pages are not vendored. The
  release this copy came from is named in `htmlit.opam` and here.

`test/` pins the void element table and the portability of the interface.
Dune skips aliases under a vendored directory, so `dune runtest` does not
reach it and it must be run by name:

    dune exec vendor/htmlit/test/test_htmlit.exe

Because that test is inert in an ordinary build, `avsm/arod/test/test_htmlit.ml`
holds a smaller copy of the same two guarantees, outside `vendor/` and so under
`dune runtest`. A re-vendor that drops the patch fails there.

### Re-vendoring checklist

1. Copy `htmlit.ml`, `htmlit.mli` and `LICENSE.md` from the new release over
   this directory, keeping `dune`, `dune-project`, `htmlit.opam`, this file
   and `test/`.
2. Reapply the three patch items above. The set is the only one that is more
   than an annotation.
3. Update the version in `htmlit.opam` and in the first line of this file.
4. `dune build @avsm/arod/all @avsm/arod/runtest`, which reaches both the
   portability ascriptions and the rendered page bodies pinned by
   `avsm/arod/test/test_routes.ml`.
5. `dune exec vendor/htmlit/test/test_htmlit.exe`.

One thing to know before writing a portable ascription against this library. A
partial application is nonportable even when the function and every applied
argument is portable, so `El.to_string ~doctype:false` is rejected where
`fun h -> El.to_string ~doctype:false h` is accepted. That is a property of
the mode system, not of this patch, and it looks exactly like a lost
annotation. Eta-expand before concluding the patch is gone.

The two test files divide the guarantee. The plain ascriptions pin the
`@@ portable` annotations. The two closures that capture a module-level
fragment and a module-level attribute pin the `immutable_data` kinds, which
nothing else here would notice the loss of, since a type used only as a
parameter or a result need not cross anything.
