  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe init --profile one --admin @admin:example.org --homeserver https://matrix.example.org > /dev/null
  $ python3 - <<'PY'
  > import subprocess
  > def memory(command):
  >     r = subprocess.run(['../bin/main.exe', 'memory', '--profile', 'one', command], capture_output=True, text=True)
  >     assert r.returncode == 0
  >     assert 'Tool started' in r.stderr and 'Tool finished' in r.stderr
  >     assert 'source="command"' in r.stderr and 'status=ok' in r.stderr
  >     assert 'PRIVATE_FACT_SENTINEL' not in r.stderr
  >     assert 'Tool started' not in r.stdout
  >     return r
  > r = memory('store PRIVATE_FACT_SENTINEL')
  > assert r.stdout == 'Stored fact #1.\n' and 'Memory stored fact_id=1' in r.stderr
  > r = memory('search PRIVATE_FACT_SENTINEL')
  > assert 'Memory searched results=1' in r.stderr
  > r = memory('get 1')
  > assert 'Memory retrieved fact_id=1 found=true' in r.stderr
  > r = memory('erase 1')
  > assert r.stdout == 'Erased fact #1.\n' and 'Memory erased fact_id=1 removed=true' in r.stderr
  > print('Tool logs are visible on stderr without verbose; command output stays on stdout.')
  > PY
  Tool logs are visible on stderr without verbose; command output stays on stdout.
