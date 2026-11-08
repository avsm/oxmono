A failure of the store or the metadata is one line on stderr and exit 1.
Cmdliner quotes option names when it has no colour terminal, so the
messages below are the same whatever terminal runs the tests.

  $ export NO_COLOR=1

A directory that is not there has no root document.

  $ zarr tree ../../test/fixtures/nowhere.zarr
  zarr: store: zarr.json: not found
  [1]

Neither has a node that is not in the hierarchy.

  $ zarr info ../../test/fixtures/hierarchy.zarr /a/nope
  zarr: store: a/nope/zarr.json: not found
  [1]

An argument out of range is a usage error instead, which cmdliner
reports with the usage line and exit 124. Spell it with an equals sign,
since a bare -1 is an option name.

  $ zarr stats ../../test/fixtures/hierarchy.zarr --sample=-1
  Usage: zarr stats [--help] [--json] [--sample=N] [OPTION]… STORE [PATH]
  zarr: option '--sample': -1 is negative
  [124]

  $ zarr tree ../../test/fixtures/hierarchy.zarr --depth=-1
  Usage: zarr tree [--help] [--depth=N] [--json] [OPTION]… STORE [PATH]
  zarr: option '--depth': -1 is negative
  [124]

A count that is not a number at all is rejected the same way.

  $ zarr tree ../../test/fixtures/hierarchy.zarr --depth=two
  Usage: zarr tree [--help] [--depth=N] [--json] [OPTION]… STORE [PATH]
  zarr: option '--depth': "two" is not a whole number
  [124]
