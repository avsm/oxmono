  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe config email-ro set --help=plain | head -1
  NAME
  $ ../bin/main.exe config email-rw set --help=plain | head -1
  NAME
  $ python3 - <<'PY'
  > import os
  > assert not os.path.exists('config')
  > for mode in ['ro', 'rw']:
  >     with open(mode + '-token', 'w') as f:
  >         f.write(mode + '-fixture-secret\n')
  >     os.chmod(mode + '-token', 0o600)
  > PY
  $ ../bin/main.exe config email-ro set fastmail --profile one --token-file ro-token
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config email-rw set fastmail --profile one --account-id a --token-file rw-token
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config email-ro list --profile one
  fastmail (selected)
  $ ../bin/main.exe config email-rw list --profile one
  fastmail (selected)
  $ ../bin/main.exe config email-ro list --profile two
  $ python3 - <<'PY'
  > import json, os, stat
  > for mode in ['ro', 'rw']:
  >     path = 'config/crowthebot/secrets/one/email-' + mode + '.json'
  >     assert stat.S_IMODE(os.stat(path).st_mode) == 0o600
  >     with open(path) as f:
  >         value = json.load(f)['entries'][0]['value']
  >     assert value == dict(url='https://api.fastmail.com/jmap/session', token=mode+'-fixture-secret', account='a' if mode=='rw' else None, max_bytes=33554432)
  > PY
  $ ../bin/main.exe config email-rw remove fastmail --profile one
  Configuration updated. Restart Crow to apply it.
  $ ../bin/main.exe config email-ro list --profile one
  fastmail (selected)
  $ ../bin/main.exe config email-rw list --profile one
