  $ export XDG_DATA_HOME="$PWD/data" XDG_CONFIG_HOME="$PWD/config" XDG_CACHE_HOME="$PWD/cache" XDG_STATE_HOME="$PWD/state"
  $ ../bin/main.exe config calendar set --help=plain | head -1
  NAME
  $ python3 - <<'PY'
  > import os
  > assert not os.path.exists('config')
  > with open('calendar-token', 'w') as f:
  >     f.write('calendar-read-only-fixture\n')
  > os.chmod('calendar-token', 0o600)
  > PY
  $ ../bin/main.exe config calendar set personal --profile one --url https://jmap.example/session --account-id calendar-a --token-file calendar-token
  Configuration saved. Restart Crow to apply it.
  $ ../bin/main.exe config calendar list --profile one
  personal (selected)
  $ ../bin/main.exe config calendar list --profile two
  $ python3 - <<'PY'
  > import json, os, stat
  > path = 'config/crowthebot/secrets/one/calendar.json'
  > assert stat.S_IMODE(os.stat(path).st_mode) == 0o600
  > with open(path) as f:
  >     settings = json.load(f)['entries'][0]['value']
  > assert settings == dict(url='https://jmap.example/session', token='calendar-read-only-fixture', account='calendar-a', max_bytes=33554432)
  > PY
  $ ../bin/main.exe config calendar remove personal --profile one
  Configuration updated. Restart Crow to apply it.

The interactive prompt trims boundary whitespace, rejects malformed pasted input
without displaying it, and restores terminal echo.

  $ python3 - <<'PY'
  > import json, os, pty, select, subprocess, termios, time
  > token = 'fmu1-synthetic-calendar-token'
  > def configure(value):
  >     master, slave = pty.openpty()
  >     before = termios.tcgetattr(slave)
  >     child = subprocess.Popen([
  >         '../bin/main.exe', 'config', 'calendar', 'set', 'pasted',
  >         '--profile', 'one', '--url', 'https://jmap.example/session'
  >     ], stdin=slave, stdout=slave, stderr=slave)
  >     output = b''
  >     try:
  >         deadline = time.monotonic() + 10
  >         while b'(token only): ' not in output:
  >             assert time.monotonic() < deadline, 'prompt timed out'
  >             if select.select([master], [], [], 0.1)[0]:
  >                 output += os.read(master, 8192)
  >         assert not termios.tcgetattr(slave)[3] & termios.ECHO
  >         os.write(master, value + b'\n')
  >         while child.poll() is None:
  >             assert time.monotonic() < deadline, 'command timed out'
  >             if select.select([master], [], [], 0.1)[0]:
  >                 output += os.read(master, 8192)
  >         while select.select([master], [], [], 0)[0]:
  >             output += os.read(master, 8192)
  >         assert token.encode() not in output, 'token was echoed'
  >         assert termios.tcgetattr(slave) == before, 'terminal was not restored'
  >         return child.returncode, output
  >     finally:
  >         if child.poll() is None:
  >             child.kill()
  >         child.wait()
  >         os.close(master)
  >         os.close(slave)
  > code, output = configure(b' \t' + token.encode() + b' \t')
  > assert code == 0, 'whitespace-wrapped token was rejected'
  > path = 'config/crowthebot/secrets/one/calendar.json'
  > with open(path) as f:
  >     assert json.load(f)['entries'][0]['value']['token'] == token
  > cases = [
  >     (b'Bearer ' + token.encode(), b'without the Bearer prefix'),
  >     (b'"' + token.encode() + b'"', b'without surrounding quotes'),
  >     (token.encode() + b'\xc2\xa0', b'internal whitespace or invisible characters'),
  >     (token.encode() + b' other', b'internal whitespace or invisible characters'),
  > ]
  > for value, expected in cases:
  >     code, output = configure(value)
  >     assert code == 1 and expected in output, 'wrong validation diagnostic'
  > with open(path) as f:
  >     assert json.load(f)['entries'][0]['value']['token'] == token
  > print('Hidden prompt, token normalization, redaction and terminal restoration passed.')
  > PY
  Hidden prompt, token normalization, redaction and terminal restoration passed.
