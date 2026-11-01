The CLI explains its commands and rejects invalid queries without network I/O.

  $ ../bin/memento_cli.exe --version
  0.1.0

  $ ../bin/memento_cli.exe versions https://example.org/ --limit 0
  memento: limit must be between 1 and 10000
  [124]

  $ ../bin/memento_cli.exe urls https://example.org/ --timeout 0
  memento: timeout must be a finite positive number
  [124]

  $ ../bin/memento_cli.exe versions https://example.org/ --from 2024-01-01
  memento: from must contain one to fourteen timestamp digits
  [124]

  $ ../bin/memento_cli.exe versions ftp://example.org/
  memento: Expected an absolute HTTP(S) URL without credentials
  [124]
