#!/usr/bin/env bash
# Verify the checked-in OCaml export with an explicitly selected SDK checkout.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SDK="${1:?usage: run-rust-export.sh SDK_CHECKOUT [cargo options]}"
shift
SDK="$(cd "$SDK" && pwd)"
[ -f "$SDK/crates/matrix-sdk-crypto/Cargo.toml" ] || {
  echo 'matrix-sdk-crypto is missing from the SDK checkout' >&2
  exit 1
}
RUN="$(mktemp -d "${TMPDIR:-/tmp}/matrix-rust-export.XXXXXXXX")"
trap 'rm -rf -- "$RUN"' EXIT
mkdir "$RUN/src"
cp "$HERE/rust-export-verifier/src/main.rs" "$RUN/src/main.rs"
cp "$HERE/rust-export-verifier/Cargo.lock" "$RUN/Cargo.lock"
python3 - "$HERE/rust-export-verifier/Cargo.toml" "$RUN/Cargo.toml" "$SDK" <<'PY'
import json
import pathlib
import sys
source, target, sdk = sys.argv[1:]
manifest = pathlib.Path(source).read_text()
manifest = manifest.replace(
    '"../../../../matrix-rust-sdk/crates/matrix-sdk-crypto"',
    json.dumps(str(pathlib.Path(sdk) / "crates/matrix-sdk-crypto")),
)
pathlib.Path(target).write_text(manifest)
PY
git -C "$SDK" rev-parse HEAD
cargo run --locked --manifest-path "$RUN/Cargo.toml" \
  --target-dir "${MATRIX_RUST_EXPORT_TARGET:-${TMPDIR:-/tmp}/ocaml-matrix-rust-export-target}" \
  "$@" -- "$ROOT"
