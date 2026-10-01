The tools an OCaml workspace gets, driven from a script rather than a model.
The workspace goes under /tmp, because a cram sandbox path is longer than a
unix socket address may be and dune's RPC socket lives under the workspace.

Each call names itself and its argument on the trace before it does anything.
The trace is okitd's, which runs the operations and streams what it is doing
while it runs them. project describes the workspace on every call, so its
describe is traced here rather than above the transcript.

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
  > let x = 1
  > EOF

An edit is a write of part of a file, so okitd answers it with the same build
and names it on the trace the same way. A passage that names no place changes
nothing and is not built, since a build reporting ok would read as the edit
having gone through.

  $ humpty-cpu expect --dir "$WS" <<'EOF'
  > # the tools a dune workspace gets
  > project {}
  > write {"cap":"","path":"lib/fix.ml","content":"let x = 1\nlet y = x + 1\n"}
  > edit {"cap":"","path":"lib/fix.ml","old":"x + 1","new":"x + 2"}
  > edit {"cap":"","path":"lib/fix.ml","old":"let z = 3","new":"let z = 4"}
  > build {}
  > EOF
  okit: dune tools active, ocamlmerlin found
  > project {}
  [describe: running]
  [describe: 1 component]
  root: $WS
  library fix (lib)
    Fix lib/fix.ml
  > write {"cap":"","path":"lib/fix.ml","content":"let x = 1\nlet y = x + 1\n"}
  [write: lib/fix.ml]
  [dune: flush]
  [dune: reply]
  [dune: build .]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 0 diagnostics]
  wrote lib/fix.ml
  build ok
  > edit {"cap":"","path":"lib/fix.ml","old":"x + 1","new":"x + 2"}
  [edit: lib/fix.ml]
  [dune: flush]
  [dune: reply]
  [dune: build .]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 0 diagnostics]
  edited lib/fix.ml
  build ok
  > edit {"cap":"","path":"lib/fix.ml","old":"let z = 3","new":"let z = 4"}
  "let z = 3" is not in lib/fix.ml. Copy the text to replace out of a read of the file, whitespace and all.
  > build {}
  [build: .]
  [dune: flush]
  [dune: reply]
  [dune: build .]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 0 diagnostics]
  build ok

  $ rm -rf $WS
