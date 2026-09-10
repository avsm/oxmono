  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe init --profile one --admin @admin:example.org --homeserver https://matrix.example.org > /dev/null

A named CalDAV check is independent of OpenRouter and a Matrix session.

  $ ../bin/main.exe probe --profile one --caldav missing --api-key-file absent
  CalDAV "missing": FAILED (configuration: Connection is not configured. Use config caldav set NAME.)
  One or more probe checks failed.
  [1]
  $ ../bin/main.exe probe --profile one --caldav missing --model-only
  Use either --caldav or --model-only.
  [1]

Exercise selection and failure aggregation without external network requests.

  $ python3 - <<'PY'
  > import json, os, subprocess
  > def probe(*args):
  >     return subprocess.run(['../bin/main.exe', 'probe', '--profile', 'one', '--api-key-file', 'absent', *args], capture_output=True, text=True, timeout=10)
  > r = probe()
  > assert r.returncode == 1 and 'Model: FAILED' in r.stdout
  > assert 'CalDAV: skipped (no configured connections).' in r.stdout
  > root = 'config/crowthebot/secrets/one'
  > os.makedirs(root, mode=0o700, exist_ok=True)
  > def config(tool, names):
  >     path = root + '/' + tool + '.json'
  >     with open(path, 'w') as f:
  >         json.dump(dict(version=1, selected=names[0], entries=[dict(name=n, value={'private-secret':'should-not-print'}) for n in names]), f)
  >     os.chmod(path, 0o600)
  > config('caldav', ['first', 'second'])
  > config('openrouter', ['invalid-model'])
  > r = probe()
  > assert r.returncode == 1 and 'Model: FAILED' in r.stdout
  > for name in ['first', 'second']:
  >     assert 'CalDAV "' + name + '": FAILED (configuration: Invalid tool configuration.)' in r.stdout
  > assert 'should-not-print' not in r.stdout + r.stderr
  > r = probe('--caldav', 'second', '--verbose')
  > assert r.returncode == 1 and 'CalDAV "second": FAILED' in r.stdout
  > assert 'first' not in r.stdout + r.stderr and 'Model:' not in r.stdout
  > assert 'CalDAV probe started connection="second"' in r.stderr
  > assert 'should-not-print' not in r.stdout + r.stderr
  > r = probe('--model-only')
  > assert r.returncode == 1 and 'Model: FAILED' in r.stdout
  > assert 'CalDAV' not in r.stdout + r.stderr
  > r = probe('--help=plain')
  > assert r.returncode == 0 and '--caldav' in r.stdout and '--model-only' in r.stdout
  > print('Probe selectors and aggregate failures work without Matrix or network access.')
  > PY
  Probe selectors and aggregate failures work without Matrix or network access.
