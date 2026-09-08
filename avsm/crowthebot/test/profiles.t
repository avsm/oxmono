  $ export XDG_DATA_HOME="$PWD/data"
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
  $ ../bin/main.exe init --profile one --admin @other:example.org --homeserver https://matrix.example.org
  profile already initialized. Edit crowthebot.json to configure it
  [1]
  $ stat -c '%a' data/matrix/profiles/one data/matrix/profiles/one/crowthebot.json data/matrix/profiles/one/crowthebot.sqlite3
  700
  600
  600
  $ ../bin/main.exe run --profile one
  no rooms enabled. Run crowthebot join first
  [1]
  $ ../bin/main.exe --help=plain | head -1
  NAME
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
