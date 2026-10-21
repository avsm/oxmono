A model is resolved and checked before the engine is created, since the engine
reports a model it cannot open and then exits the process.

  $ unset DS4_MODEL
  $ export XDG_DATA_HOME=$PWD/data
  $ mkdir -p data/ds4 work

A path that names nothing is refused with the path, and nothing is written on
standard output.

  $ ds4-agent-cpu agent -d work -m /nonexistent/path.gguf hello 2>err
  [123]
  $ cat err
  ds4-agent: model /nonexistent/path.gguf is not there. The list subcommand says which models this machine has and the download subcommand fetches one.

A directory is not a model.

  $ ds4-agent-cpu chat -m work hello
  ds4-agent: model work is not there. The list subcommand says which models this machine has and the download subcommand fetches one.
  [123]

A target that is not downloaded names the command that fetches it.

  $ ds4-agent-cpu agent -d work -m q2 hello
  ds4-agent: model 'q2' is not downloaded. Run 'ds4-agent-cpu download q2'.
  [123]

With no model named and none downloaded, the refusal says every way to supply
one.

  $ ds4-agent-cpu agent -d work hello
  ds4-agent: no model found. Pass --model, set DS4_MODEL, run 'ds4-agent-cpu download <target>', or put a .gguf in $TESTCASE_ROOT/data/ds4.
  [123]

The listing marks no target present.

  $ ds4-agent-cpu list | grep -F '  [*] '
  [1]
