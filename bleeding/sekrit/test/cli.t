  $ export XDG_CONFIG_HOME="$PWD/config" XDG_DATA_HOME="$PWD/data"
  $ ../bin/main.exe --help=plain | head -1
  NAME
  $ test ! -e config && test ! -e data
  $ ../bin/main.exe init
  Encrypted secret store initialized.
  $ python3 - <<'PY'
  > import os
  > with open('password', 'w') as f:
  >     f.write('fixture-private-value\n')
  > os.chmod('password', 0o600)
  > PY
  $ ../bin/main.exe put mail/password --file password
  Saved sekrit:mail/password
  $ ../bin/main.exe list
  mail/password
  $ ../bin/main.exe put mail/password --file password
  Error: Secret already exists; use replace explicitly
  [1]
  $ ../bin/main.exe export mail/password --file exported
  Private plaintext file created.
  $ python3 - <<'PY'
  > import os, stat
  > assert open('exported').read() == 'fixture-private-value'
  > assert stat.S_IMODE(os.stat('exported').st_mode) == 0o600
  > assert b'fixture-private-value' not in open('data/sekrit/vault.sekrit', 'rb').read()
  > assert len(open('config/sekrit/master.key', 'rb').read()) == 32
  > PY
  $ SEKRIT_STORE_DIR=relative SEKRIT_KEY_FILE=relative ../bin/main.exe list --store "$PWD/data/sekrit" --key-file "$PWD/config/sekrit/master.key"
  mail/password
  $ ../bin/main.exe export mail/password --file exported
  Error: Cannot access secret storage
  [1]
  $ ../bin/main.exe remove mail/password
  Secret removed.
  $ ../bin/main.exe list
  $ mv config/sekrit/master.key original-key
  $ ../bin/main.exe init
  Error: Master key is missing. Run sekrit init for a new store, or restore the original key for existing encrypted data.
  [1]
  $ test ! -e config/sekrit/master.key
