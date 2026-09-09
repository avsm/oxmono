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
  $ ../bin/main.exe memory --profile one 'store Bring jasmine tea.'
  Stored fact #1.
  $ ../bin/main.exe memory --profile one 'search jasmine' | tail -1
  Bring jasmine tea.
  $ ../bin/main.exe memory --profile one 'get 1' | tail -1
  Bring jasmine tea.
  $ ../bin/main.exe memory --profile two list
  No matching facts.
  $ ../bin/main.exe memory --profile one 'erase 1'
  Erased fact #1.
  $ ../bin/main.exe memory --profile one 'search jasmine'
  No matching facts.
  $ python3 - <<'PY'
  > import sqlite3
  > with sqlite3.connect('data/matrix/profiles/one/crowthebot.sqlite3') as db:
  >     print(db.execute('PRAGMA user_version').fetchone()[0])
  >     print(db.execute('SELECT tool,status FROM tool_uses ORDER BY id').fetchall())
  >     print(db.execute('SELECT count(*) FROM facts').fetchone()[0])
  > PY
  5
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
  > PY
  this profile is already in use by crowthebot
  $ ../bin/main.exe init --profile ../escape --admin @admin:example.org --homeserver https://matrix.example.org 2>/dev/null
  [1]
  $ ../bin/main.exe login --profile one --username @admin:example.org
  use a separate Matrix account for the bot
  [1]
