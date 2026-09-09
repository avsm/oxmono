  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe config owntracks add --help=plain | head -1
  NAME
  $ ../bin/main.exe config openrouter add --help=plain | head -1
  NAME
  $ python3 - <<'PY'
  > import os
  > assert not os.path.exists('config')
  > with open('key', 'w') as f:
  >     f.write('fixture-secret\n')
  > os.chmod('key', 0o600)
  > PY
  $ ../bin/main.exe config owntracks add home --profile one --url https://recorder.example --username operator --password-file key
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config owntracks add spare --profile one --url http://localhost:8083 --anonymous --allow-http
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config owntracks list --profile one
  home (selected)
  spare
  $ ../bin/main.exe config owntracks list --profile two
  $ ../bin/main.exe config owntracks rename home house --profile one
  Configuration renamed. Update tool links and restart Crow.
  $ ../bin/main.exe config owntracks list --profile one
  house (selected)
  spare
  $ ../bin/main.exe config owntracks set house --profile one --url https://new-recorder.example --username operator --password-file key
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config owntracks remove house --profile one
  Configuration updated. Restart Crow to apply it.
  $ ../bin/main.exe config owntracks list --profile one
  spare
  $ ../bin/main.exe config owntracks select spare --profile one
  Configuration updated. Restart Crow to apply it.
  $ ../bin/main.exe config openrouter add work --profile one --api-key-file key
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config openrouter list --profile one
  work (selected)
  $ ../bin/main.exe config owntracks list --profile one
  spare (selected)
  $ python3 - <<'PY'
  > import json, os, stat
  > for name in ['secrets', 'secrets/one', 'secrets/one/owntracks.json', 'secrets/one/openrouter.json']:
  >     print(oct(stat.S_IMODE(os.stat('config/crowthebot/' + name).st_mode))[2:])
  > with open('config/crowthebot/secrets/one/openrouter.json') as f:
  >     assert json.load(f)['entries'][0]['value']['api_key'] == 'fixture-secret'
  > assert not os.path.exists('data/matrix/profiles/one/crowthebot.sqlite3')
  > PY
  700
  700
  600
  600
  $ ../bin/main.exe config owntracks add bad --profile one --url https://recorder.example --username operator </dev/null
  Use a secret file outside an interactive terminal.
  [1]
  $ ../bin/main.exe config openrouter add '../escape' --profile one --api-key-file key
  Names must contain 1 to 64 ASCII letters, digits, - or _.
  [1]
  $ CROWTHEBOT_CONFIG_DIR="$PWD/data/matrix/profiles/one/workspace" ../bin/main.exe config owntracks list --profile one
  Secret configuration must be outside the profile data directory.
  [1]
  $ CROWTHEBOT_CONFIG_DIR="$PWD/data/matrix/profiles/two/workspace" ../bin/main.exe config owntracks list --profile one
  Secret configuration must be outside the profile data directory.
  [1]
  $ ../bin/main.exe config owntracks add home --profile older.profile --url https://recorder.example --anonymous
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config owntracks list --profile older.profile
  home (selected)
  $ python3 - <<'PY'
  > import fcntl, subprocess
  > with open('config/crowthebot/secrets/one/.lock', 'r+') as lock:
  >     fcntl.lockf(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
  >     result = subprocess.run(['../bin/main.exe', 'config', 'owntracks', 'list', '--profile', 'one'])
  >     assert result.returncode == 1
  > PY
  Secret configuration is in use. Retry after the other command finishes.
