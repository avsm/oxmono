A script with a prompt loads a model, so the model is resolved before
anything is started. A model path that is not there is refused with no
okitd spawned and no dune server started, which the absence of trace and
status lines shows. A script with no prompt never resolves a model, so a
bad --model does not matter to it.

This transcript may run in the cram sandbox, whose path is longer than a
unix socket address may be, because neither command opens a dune socket: the
first fails before the tools are assembled, and the second has no
dune-project for a server to serve.

  $ humpty-cpu expect --dir . --model /nonexistent/model.gguf <<'EOF'
  > ? describe this repository
  > EOF
  humpty: model /nonexistent/model.gguf is not there. The list subcommand says which models this machine has and the download subcommand fetches one.
  [123]

  $ echo hello > note.txt
  $ humpty-cpu expect --dir . --model /nonexistent/model.gguf <<'EOF'
  > read {"cap":"","path":"note.txt"}
  > EOF
  okit: off (no dune-project)
  > read {"cap":"","path":"note.txt"}
  hello
