  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe init --profile one --admin @admin:example.org --homeserver https://matrix.example.org | sed 's|Created .*|Created profile|'
  Created profile
  Edit crowthebot.json to choose a model, prompt or plugins.
  $ ../bin/main.exe init --profile two --admin @other:example.org --homeserver https://matrix.example.org | sed 's|Created .*|Created profile|'
  Created profile
  Edit crowthebot.json to choose a model, prompt or plugins.
  $ ../bin/main.exe people --profile one
  @admin:example.org: friend, allowed
  $ ../bin/main.exe people --profile two
  @other:example.org: friend, allowed
  $ ../bin/main.exe memory --profile one 'store Bring jasmine tea.' 2>/dev/null
  Stored fact #1.
  $ ../bin/main.exe memory --profile one 'search jasmine' 2>/dev/null | tail -1
  Bring jasmine tea.
  $ ../bin/main.exe memory --profile one 'get 1' 2>/dev/null | tail -1
  Bring jasmine tea.
  $ ../bin/main.exe memory --profile two list 2>/dev/null
  No matching facts.
  $ ../bin/main.exe memory --profile one 'erase 1' 2>/dev/null
  Erased fact #1.
  $ ../bin/main.exe memory --profile one 'search jasmine' 2>/dev/null
  No matching facts.
  $ python3 - <<'PY'
  > import sqlite3
  > with sqlite3.connect('data/matrix/profiles/one/crowthebot.sqlite3') as db:
  >     print(db.execute('PRAGMA user_version').fetchone()[0])
  >     print(db.execute('SELECT tool,status FROM tool_uses ORDER BY id').fetchall())
  >     print(db.execute('SELECT count(*) FROM facts').fetchone()[0])
  > PY
  8
  [('memory_store', 'ok'), ('memory_search', 'ok'), ('memory_get', 'ok'), ('memory_erase', 'ok'), ('memory_search', 'ok')]
  0
  $ ../bin/main.exe init --profile one --admin @other:example.org --homeserver https://matrix.example.org
  profile already initialized. Edit crowthebot.json to configure it
  [1]
  $ python3 - <<'PY'
  > import os, stat
  > for name in ['', '/crowthebot.json', '/crowthebot.sqlite3']:
  >     print(oct(stat.S_IMODE(os.stat('data/matrix/profiles/one' + name).st_mode))[2:])
  > PY
  700
  600
  600
  $ ../bin/main.exe run --profile one
  no saved Matrix session. Run crowthebot login first
  [1]
  $ python3 - <<'PY'
  > import subprocess
  > for flag in ['--verbose', '-v']:
  >     r = subprocess.run(['../bin/main.exe', 'run', '--profile', 'one', flag], capture_output=True, text=True)
  >     assert r.returncode == 1 and r.stdout == ''
  >     assert 'Starting Crow profile="one"' in r.stderr
  >     assert 'Connecting Matrix profile="one"' in r.stderr
  >     assert 'no saved Matrix session' in r.stderr
  > r = subprocess.run(['../bin/main.exe', 'probe', '--verbose', '--help=plain'], capture_output=True, text=True)
  > assert r.returncode == 0 and '--verbose' in r.stdout
  > print('Verbose startup logs go to stderr; run and probe accept the flag.')
  > PY
  Verbose startup logs go to stderr; run and probe accept the flag.
  $ ../bin/main.exe --help=plain | head -1
  NAME
  $ ../bin/main.exe verify --help=plain | head -1
  NAME
  $ ../bin/main.exe verify --profile one </dev/null
  verification requires an interactive terminal
  [1]
  $ python3 - <<'PY'
  > import fcntl, subprocess
  > with open('data/matrix/profiles/one/.crowthebot.lock', 'r+') as lock:
  >     fcntl.lockf(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
  >     result = subprocess.run(['../bin/main.exe', 'people', '--profile', 'one'])
  >     assert result.returncode == 1
  >     import json
  >     result = subprocess.run(['../bin/main.exe', 'inspect', '--profile', 'one'], capture_output=True, text=True)
  >     assert result.returncode == 0
  >     data = json.loads(result.stdout)
  >     assert data['items'] == [] and data['outstanding']['reminders'] == 0
  >     result = subprocess.run(['../bin/main.exe', 'inspect', '--profile', 'one', '--section', 'tools', '--limit', '2'], capture_output=True, text=True)
  >     data = json.loads(result.stdout)
  >     assert len(data['items']) == 2 and data['next_after'] == 2
  > PY
  this profile is already in use by crowthebot
  $ ../bin/main.exe init --profile ../escape --admin @admin:example.org --homeserver https://matrix.example.org 2>/dev/null
  [1]
  $ ../bin/main.exe login --profile one --username @admin:example.org
  use a separate Matrix account for the bot
  [1]
