A write that introduces a type error answers with the diagnostic inline, so
the model sees what the build said about the file it just wrote.

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

  $ humpty-cpu expect --dir "$WS" <<'EOF'
  > write {"cap":"","path":"lib/fix.ml","content":"let x : int = \"no\"\n"}
  > build {}
  > EOF
  okit: dune tools active, ocamlmerlin found
  > write {"cap":"","path":"lib/fix.ml","content":"let x : int = \"no\"\n"}
  [write: lib/fix.ml]
  [dune: flush]
  [dune: reply]
  [dune: build .]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 1 diagnostic]
  wrote lib/fix.ml
  File "$WS/lib/fix.ml", line 1, characters 14-18:
  Error
  This constant has type string but an expression was expected of type
    int
  > build {}
  [build: .]
  [dune: flush]
  [dune: reply]
  [dune: build .]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 1 diagnostic]
  File "$WS/lib/fix.ml", line 1, characters 14-18:
  Error
  This constant has type string but an expression was expected of type
    int
  build failed

  $ rm -rf $WS
