#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
opam exec --switch="${MQTTZ_SWITCH:-5.2.0+ox}" -- \
  dune build --profile release-check @wip/owntracks/all
exec wip/mqttz/test/docker/run.sh python3 wip/owntracks/test/docker/check.py
