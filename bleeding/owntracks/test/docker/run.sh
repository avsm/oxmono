#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
opam exec --switch="${MQTTZ_SWITCH:-5.2.0+ox}" -- \
  dune build --profile release-check @bleeding/owntracks/all
exec bleeding/mqttz/test/docker/run.sh python3 bleeding/owntracks/test/docker/check.py
