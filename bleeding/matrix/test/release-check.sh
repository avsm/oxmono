#!/usr/bin/env bash
# Release validation on a runner with OCaml dependencies, Cargo, Docker and Python 3.
# Uses fresh per-run homeservers; logs and the exact tested patch are retained.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
RUN_DIR="$(mktemp -d "${TMPDIR:-/tmp}/matrix-chat-release-check.XXXXXXXX")"
exec > >(tee "$RUN_DIR/check.log") 2>&1
printf 'Release evidence: %s\n' "$RUN_DIR"
git rev-parse HEAD > "$RUN_DIR/revision.txt"
git diff --binary HEAD > "$RUN_DIR/changes.patch"
git ls-files --others --exclude-standard > "$RUN_DIR/untracked.txt"
python3 - "$RUN_DIR/source.tar.gz" <<'PYTHON'
import os
import subprocess
import sys
import tarfile
paths = subprocess.check_output(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"]).split(b"\0")
with tarfile.open(sys.argv[1], "w:gz") as archive:
    for raw in sorted(set(paths)):
        path = os.fsdecode(raw)
        if path and path != "ocaml-httpz" and os.path.isfile(path) and not os.path.islink(path):
            archive.add(path, arcname="matrix-chat/" + path, recursive=False)
PYTHON
cargo build --locked --release --manifest-path test/vodozemac-oracle/Cargo.toml
export VODOZEMAC_ORACLE="$ROOT/test/vodozemac-oracle/target/release/vodozemac-oracle"
export MATRIX_REQUIRE_ORACLE=1
[ -x "$VODOZEMAC_ORACLE" ] || { echo 'Required oracle is absent'; exit 1; }
unset MATRIX_TEST_HOMESERVER MATRIX_REQUIRE_HOMESERVER MATRIX_TEST_PROFILE
# exec forces a real oracle run even when ordinary dune tests are cached.
dune exec --root "$ROOT" --only-packages matrix-chat -- test/test_olm.exe --color=never
dune build --root "$ROOT" --only-packages matrix-chat matrix-chat.install @lib/all @bin/all @example/all @test/runtest @doc
MATRIX_TEST_DUNE_BUILD_DIR="$ROOT/_build" test/integration/run-both.sh
printf 'Release interoperability checks passed. Logs: %s\n' "$RUN_DIR"
