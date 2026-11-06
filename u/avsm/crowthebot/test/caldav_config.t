  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe config caldav set --help=plain | head -1
  NAME
  $ python3 - <<'PY'
  > import os
  > assert not os.path.exists('config')
  > with open('app-password', 'w') as f:
  >     f.write('synthetic-caldav-password\n')
  > os.chmod('app-password', 0o600)
  > PY

Writes need the Sekrit master key and never create it.

  $ ../bin/main.exe config caldav set fastmail --profile crow-one --user owner@example.test --password-file app-password
  Master key is missing. Run sekrit init for a new store, or restore the original key for existing encrypted data.
  [1]
  $ test -e config/sekrit/master.key
  [1]
  $ sekrit init
  Encrypted secret store initialized.

  $ ../bin/main.exe config caldav set fastmail --profile crow-one --user owner@example.test --password-file app-password
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config caldav list --profile crow-one
  fastmail (selected)
  $ ../bin/main.exe config caldav list --profile different
  $ python3 - <<'PY'
  > import os, stat
  > path = 'config/crowthebot/secrets/crow-one/caldav.json'
  > assert stat.S_IMODE(os.stat(path).st_mode) == 0o600
  > with open(path, 'rb') as f:
  >     data = f.read()
  > assert data.startswith(b'SEKRIT\x00\x01') and b'synthetic-caldav-password' not in data
  > PY
  $ support/secret_fixture.exe get crow-one caldav fastmail | python3 -c '
  > import json, sys
  > assert json.load(sys.stdin) == dict(url="https://caldav.fastmail.com/", user="owner@example.test", password="synthetic-caldav-password", max_bytes=33554432)'
  $ ../bin/main.exe config caldav remove fastmail --profile crow-one
  Configuration updated. Restart Crow to apply it.
  $ ../bin/main.exe config caldav list --profile crow-one
