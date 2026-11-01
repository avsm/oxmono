#!/usr/bin/env bash
# Shared executable entry point for standalone and OxMono Docker fixtures.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKSPACE="$(cd "$ROOT/../.." && pwd)"
if [ -f "$ROOT/OXMONO.md" ] && [ -f "$WORKSPACE/dune-project" ]; then
  exec dune exec --root "$WORKSPACE" --profile release-check \
    --build-dir "${MATRIX_TEST_DUNE_BUILD_DIR:-$WORKSPACE/_build}" \
    bleeding/matrix/test/integration/test_homeserver.exe -- test "$@" --color=never
else
  exec dune exec --root "$ROOT" --only-packages matrix-chat \
    --build-dir "${MATRIX_TEST_DUNE_BUILD_DIR:-_build}" \
    test/integration/test_homeserver.exe -- test "$@" --color=never
fi
