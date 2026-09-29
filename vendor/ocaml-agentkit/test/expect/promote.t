A test whose recorded output no longer matches: the failure carries the diff
and the file to promote, promote accepts it, and the test then passes.

  $ WS=$(mktemp -d /tmp/humpty-expect.XXXXXX)
  $ cd $WS
  $ cat > dune-project <<'EOF'
  > (lang dune 3.21)
  > EOF
  $ mkdir t
  $ cat > t/dune <<'EOF'
  > (executable (name t))
  > (rule (with-stdout-to t.out (run ./t.exe)))
  > (rule (alias runtest) (action (diff t.expected t.out)))
  > EOF
  $ cat > t/t.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF
  $ cat > t/t.expected <<'EOF'
  > goodbye
  > EOF

  $ humpty-cpu expect --dir "$WS" <<'EOF'
  > test {}
  > promote {"path":"t/t.expected"}
  > test {}
  > EOF
  okit: dune tools active, ocamlmerlin found
  > test {}
  [test: running]
  [dune: flush]
  [dune: reply]
  [dune: runtest]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 1 diagnostic]
  File "$WS/t/t.expected", line 1, characters 0-0:
  Error
  diff --git a/_build/default/t/t.expected b/_build/default/t/t.out
  index dd7e1c6..ce01362 100644
  --- a/_build/default/t/t.expected
  +++ b/_build/default/t/t.out
  @@ -1 +1 @@
  -goodbye
  +hello
  wrote $WS/t/t.expected (run promote to accept)
  tests failed
  > promote {"path":"t/t.expected"}
  [promote: t/t.expected]
  [dune: promote t/t.expected]
  [dune: reply]
  promoted t/t.expected
  > test {}
  [test: running]
  [dune: flush]
  [dune: reply]
  [dune: runtest]
  [dune: reply]
  [dune: diagnostics]
  [dune: reply]
  [dune: 0 diagnostics]
  tests ok

  $ cat t/t.expected
  hello

  $ rm -rf $WS
