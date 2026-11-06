#!/bin/sh
# Build ox from a clean checkout using the pinned external repositories.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
state=${OX_BOOTSTRAP_STATE:-/tmp/oxmono-bootstrap}
mkdir -p "$state"
python3 - "$root/toolchain.lock.json" "$state" <<'PY'
import json, pathlib, subprocess, sys
lock=json.load(open(sys.argv[1])); state=pathlib.Path(sys.argv[2])
assert lock['compiler']=='oxcaml-compiler.5.4.0-ox9'
assert len(lock['repositories']) == 2
for name,entry in zip(['ox','default'],lock['repositories']):
    path=state/name
    if not path.exists():
        subprocess.run(['git','init','-q',str(path)],check=True)
    subprocess.run(['git','-C',str(path),'fetch','--depth=1',entry['url'],entry['revision']],check=True)
    subprocess.run(['git','-C',str(path),'checkout','--detach',entry['revision']],check=True)
PY
export OPAMROOT="${OX_BOOTSTRAP_OPAMROOT:-$state/opam}"
export OPAMYES=1
if [ ! -f "$OPAMROOT/config" ]; then
  opam init --bare --disable-sandboxing --no-setup default "$state/default"
fi
if ! opam repository list --all --short | grep -qx ox; then
  opam repository add ox "$state/ox" --all-switches
fi
if ! opam switch list --short | grep -qx oxmono; then
  opam switch create oxmono oxcaml-compiler.5.4.0-ox9 --repositories=ox,default
fi
cd "$root"
opam install --switch=oxmono ./oxmono.opam --deps-only
opam exec --switch=oxmono -- dune build --profile release wip/ox/bin/main.exe
