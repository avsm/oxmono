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
  > os.makedirs('config/owntracks', mode=0o700)
  > text = '[owntracks.recorder]\nurl="https://recorder.example"\nuser="operator"\npassword="recorder-only-secret"\n[[owntracks.devices]]\nid="phone"\nname="Anil Phone"\n[mqtt]\npassword="mqtt-only-secret"\n'
  > for path, value in [('config/owntracks/owntracks.toml', text), ('alternate.toml', text.replace('https://recorder.example', 'http://localhost:8083'))]:
  >     with open(path, 'w') as f:
  >         f.write(value)
  >     os.chmod(path, 0o600)
  > PY
  $ ../bin/main.exe config owntracks add home --profile one --user alice --device 'Anil Phone' </dev/null
  Configuration saved. Restart Crow to apply it.
  $ python3 - <<'PY'
  > import json, os
  > with open('config/crowthebot/secrets/one/owntracks.json') as f:
  >     config = json.load(f)['entries'][0]['value']
  > assert config == {'config_file': os.path.join(os.getcwd(), 'config/owntracks/owntracks.toml'), 'user': 'alice', 'device': 'phone', 'allow_http': False, 'lookback_days': 7}
  > PY
  $ ../bin/main.exe config owntracks add spare --profile one --owntracks-config alternate.toml --user bob --device other --allow-http
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
  $ ../bin/main.exe config owntracks set house --profile one --user alice --device phone
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
  > with open('config/crowthebot/secrets/one/owntracks.json') as f:
  >     data = f.read()
  > assert 'recorder-only-secret' not in data and 'mqtt-only-secret' not in data
  > assert 'password' not in data and 'username' not in data and 'url' not in data
  > assert not os.path.exists('data/matrix/profiles/one/crowthebot.sqlite3')
  > PY
  700
  700
  600
  600
  $ ../bin/main.exe config owntracks add bad --profile one --owntracks-config alternate.toml --user alice --device phone
  Endpoint must be HTTPS with a host and no credentials, query or fragment. Use --allow-http for a trusted HTTP endpoint.
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
  $ ../bin/main.exe config owntracks add home --profile older.profile --user alice --device phone
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
  $ chmod 644 config/owntracks/owntracks.toml
  $ ../bin/main.exe config owntracks set spare --profile one --user alice --device phone
  Cannot read OwnTracks config. Use an owned regular file with mode 0600, at most 1 MiB.
  [1]
  $ chmod 600 config/owntracks/owntracks.toml
  $ ln -s config/owntracks/owntracks.toml linked.toml
  $ ../bin/main.exe config owntracks set spare --profile one --owntracks-config linked.toml --user alice --device phone
  Cannot read OwnTracks config. Use an owned regular file with mode 0600, at most 1 MiB.
  [1]
  $ python3 - <<'PY'
  > import json
  > path = 'config/crowthebot/secrets/one/owntracks.json'
  > with open(path) as f:
  >     config = json.load(f)
  > config['entries'][0]['value'] = {'url': 'https://recorder.example', 'password': 'legacy-duplicated-secret'}
  > with open(path, 'w') as f:
  >     json.dump(config, f)
  > PY
  $ ../bin/main.exe config owntracks set spare --profile one --user alice --device 'Anil Phone'
  Configuration saved. Restart Crow to apply it.
  $ python3 - <<'PY'
  > with open('config/crowthebot/secrets/one/owntracks.json') as f:
  >     data = f.read()
  > assert 'legacy-duplicated-secret' not in data and 'password' not in data
  > PY
