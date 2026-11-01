The merlin queries, when the workspace has an ocamlmerlin to answer them. The
name asked about is a function, so the type reported is one merlin had to work
out rather than the type of a literal.

The queries whose answers name a place outside this workspace, which are
search and complete, are left to the unit tests. What they answer with is the
standard library of whichever compiler is installed, and the file and line of
a name in it move between releases.

  $ WS=$(mktemp -d /tmp/humpty-expect.XXXXXX)
  $ cd $WS
  $ cat > dune-project <<'EOF'
  > (lang dune 3.21)
  > EOF
  $ mkdir lib
  $ cat > lib/dune <<'EOF'
  > (library (name fix))
  > EOF
  $ cat > lib/fix.ml <<'EOF'
  > let f s = s ^ "!"
  > let g = f "hi"
  > EOF
  $ cat > lib/fix.mli <<'EOF'
  > val f : string -> string
  > val g : string
  > EOF

  $ humpty-cpu expect --dir "$WS" <<'EOF'
  > outline {"cap":"","path":"lib/fix.ml"}
  > type_at {"cap":"","path":"lib/fix.ml","line":2,"col":8}
  > locate {"cap":"","path":"lib/fix.ml","line":2,"col":8}
  > occurrences {"cap":"","path":"lib/fix.ml","line":1,"col":4}
  > errors {"cap":"","path":"lib/fix.ml"}
  > project {"module":"Fix"}
  > EOF
  okit: dune tools active, ocamlmerlin found
  > outline {"cap":"","path":"lib/fix.ml"}
  [outline: lib/fix.ml]
  [merlin: outline lib/fix.ml]
  [merlin: reply]
  lib/fix.mli is this module's interface, which states what it offers and nothing else. Outline that before reading on.

  Value f : string -> string
  Value g : string
  > type_at {"cap":"","path":"lib/fix.ml","line":2,"col":8}
  [type_at: lib/fix.ml:2:8]
  [merlin: type-enclosing lib/fix.ml]
  [merlin: reply]
  string -> string
  > locate {"cap":"","path":"lib/fix.ml","line":2,"col":8}
  [locate: lib/fix.ml:2:8]
  [merlin: locate lib/fix.ml]
  [merlin: reply]
  lib/fix.ml:1:4
       1  let f s = s ^ "!"
  > occurrences {"cap":"","path":"lib/fix.ml","line":1,"col":4}
  [occurrences: indexing]
  [dune: flush]
  [dune: reply]
  [dune: build (alias_rec ocaml-index)]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 0 diagnostics]
  [occurrences: lib/fix.ml:1:4]
  [merlin: occurrences lib/fix.ml]
  [merlin: reply]
  lib/fix.ml:1:4
  lib/fix.ml:2:8
  lib/fix.mli:1:4
  > errors {"cap":"","path":"lib/fix.ml"}
  [errors: lib/fix.ml]
  [merlin: errors lib/fix.ml]
  [merlin: reply]
  no errors or warnings
  > project {"module":"Fix"}
  [describe: running]
  [describe: 1 component]
  library fix (lib)
    Fix lib/fix.ml lib/fix.mli

  $ rm -rf $WS
