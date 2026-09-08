#!/usr/bin/env bash
#
# Run the full Synapse reference suite and the portable Dendrite core profile
# against fresh, per-run homeservers.  Every container and data directory is
# derived from the run id and is cleaned up in reverse start order.
#
#   test/integration/run-both.sh
#   MATRIX_TEST_RUN_ID=ci-42 test/integration/run-both.sh
#
# The Dendrite peeking capability probe is intentionally not part of this
# passing run.  Use dendrite.sh run-peeking separately when its expected
# M_GUEST_ACCESS_FORBIDDEN result is useful.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SYNAPSE_SCRIPT="$HERE/synapse.sh"
DENDRITE_SCRIPT="$HERE/dendrite.sh"

log() { printf '%s\n' "$*" >&2; }
die() { log "run-both.sh: $*"; exit 1; }

RUN_ID="${MATRIX_TEST_RUN_ID:-}"
if [ -z "$RUN_ID" ]; then
  RUN_ID="run-$(date +%s)-$$-${RANDOM}"
fi

case "$RUN_ID" in
  [ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789]*) ;;
  *) die "MATRIX_TEST_RUN_ID must start with an ASCII letter or digit" ;;
esac
case "$RUN_ID" in
  *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
    die "MATRIX_TEST_RUN_ID contains characters unsafe for Docker names"
    ;;
esac
if [ "${#RUN_ID}" -gt 48 ]; then
  die "MATRIX_TEST_RUN_ID is too long (maximum 48 characters)"
fi

SYNAPSE_CONTAINER="ocaml-matrix-run-${RUN_ID}-synapse"
DENDRITE_CONTAINER="ocaml-matrix-run-${RUN_ID}-dendrite"
SYNAPSE_PORT="${MATRIX_TEST_PORT:-8008}"
DENDRITE_PORT="${MATRIX_TEST_DENDRITE_PORT:-18008}"
RUN_ROOT="${MATRIX_TEST_RUN_DATA_ROOT:-${TMPDIR:-/tmp}}"
SYNAPSE_DATA="$RUN_ROOT/$SYNAPSE_CONTAINER"
DENDRITE_DATA="$RUN_ROOT/$DENDRITE_CONTAINER"
DUNE_BUILD_DIR="${MATRIX_TEST_DUNE_BUILD_DIR:-_build-run-${RUN_ID}}"

validate_port() {
  local name="$1" value="$2"
  case "$value" in
    ''|*[!0123456789]*) die "$name must be a decimal host port" ;;
  esac
  if [ "$value" -lt 1 ] || [ "$value" -gt 65535 ]; then
    die "$name must be between 1 and 65535"
  fi
}

validate_port MATRIX_TEST_PORT "$SYNAPSE_PORT"
validate_port MATRIX_TEST_DENDRITE_PORT "$DENDRITE_PORT"
[ "$SYNAPSE_PORT" != "$DENDRITE_PORT" ] \
  || die "Synapse and Dendrite host ports must be distinct"

case "$RUN_ROOT" in
  /*) ;;
  *) die "MATRIX_TEST_RUN_DATA_ROOT must be an absolute path" ;;
esac
[ ! -L "$RUN_ROOT" ] || die "MATRIX_TEST_RUN_DATA_ROOT must not be a symlink"

need_docker() {
  command -v docker >/dev/null 2>&1 || die "docker is not on PATH"
  docker info >/dev/null 2>&1 || die "the Docker daemon is not reachable"
}

container_exists() {
  docker inspect "$1" >/dev/null 2>&1
}

preflight() {
  need_docker
  local name
  for name in "$SYNAPSE_CONTAINER" "$DENDRITE_CONTAINER"; do
    if container_exists "$name"; then
      die "refusing to adopt pre-existing container: $name"
    fi
  done
  local data
  for data in "$SYNAPSE_DATA" "$DENDRITE_DATA"; do
    if [ -e "$data" ] || [ -L "$data" ]; then
      die "refusing to adopt pre-existing data directory: $data"
    fi
  done
}

owns_synapse=0
owns_dendrite=0

cleanup() {
  local status=$?
  local cleanup_failed=0
  trap - EXIT HUP INT TERM

  if [ "$owns_dendrite" -eq 1 ]; then
    log "run-both.sh: cleaning Dendrite $DENDRITE_CONTAINER"
    if env \
      MATRIX_TEST_DENDRITE_CONTAINER="$DENDRITE_CONTAINER" \
      MATRIX_TEST_DENDRITE_DATA="$DENDRITE_DATA" \
      MATRIX_TEST_DENDRITE_PORT="$DENDRITE_PORT" \
      "$DENDRITE_SCRIPT" down --purge; then
      :
    else
      cleanup_failed=1
      log "run-both.sh: Dendrite cleanup failed"
    fi
  fi

  if [ "$owns_synapse" -eq 1 ]; then
    log "run-both.sh: cleaning Synapse $SYNAPSE_CONTAINER"
    if env \
      MATRIX_TEST_SYNAPSE_CONTAINER="$SYNAPSE_CONTAINER" \
      MATRIX_TEST_SYNAPSE_DATA="$SYNAPSE_DATA" \
      MATRIX_TEST_PORT="$SYNAPSE_PORT" \
      "$SYNAPSE_SCRIPT" down --purge; then
      :
    else
      cleanup_failed=1
      log "run-both.sh: Synapse cleanup failed"
    fi
  fi

  if [ "$cleanup_failed" -eq 1 ] && [ "$status" -eq 0 ]; then
    status=1
  fi
  exit "$status"
}

start_synapse() {
  env \
    MATRIX_TEST_SYNAPSE_CONTAINER="$SYNAPSE_CONTAINER" \
    MATRIX_TEST_SYNAPSE_DATA="$SYNAPSE_DATA" \
    MATRIX_TEST_PORT="$SYNAPSE_PORT" \
    "$SYNAPSE_SCRIPT" up
}

start_dendrite() {
  env \
    MATRIX_TEST_DENDRITE_CONTAINER="$DENDRITE_CONTAINER" \
    MATRIX_TEST_DENDRITE_DATA="$DENDRITE_DATA" \
    MATRIX_TEST_DENDRITE_PORT="$DENDRITE_PORT" \
    "$DENDRITE_SCRIPT" up
}

run_synapse_suite() {
  log "run-both.sh: running the full Synapse reference suite"
  env MATRIX_TEST_PROFILE= MATRIX_REQUIRE_HOMESERVER=1 \
    MATRIX_TEST_HOMESERVER="http://127.0.0.1:$SYNAPSE_PORT" \
    dune exec --only-packages matrix-chat --build-dir "$DUNE_BUILD_DIR" \
    test/integration/test_homeserver.exe -- test --color=never
}

run_dendrite_core() {
  log "run-both.sh: running the Dendrite core profile"
  env \
    MATRIX_TEST_DENDRITE_CONTAINER="$DENDRITE_CONTAINER" \
    MATRIX_TEST_DENDRITE_DATA="$DENDRITE_DATA" \
    MATRIX_TEST_DENDRITE_PORT="$DENDRITE_PORT" \
    MATRIX_TEST_DUNE_BUILD_DIR="$DUNE_BUILD_DIR" \
    MATRIX_TEST_HOMESERVER="http://127.0.0.1:$DENDRITE_PORT" \
    "$DENDRITE_SCRIPT" run-core
}

cd "$ROOT"
preflight
# Collision checks complete before either cleanup flag is set.  From this
# point on the two derived names are exclusively ours for this run.
owns_synapse=1
owns_dendrite=1
trap cleanup EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

if start_synapse; then :; else exit $?; fi
if start_dendrite; then :; else exit $?; fi
if run_synapse_suite; then :; else exit $?; fi
if run_dendrite_core; then :; else exit $?; fi

log "run-both.sh: Synapse reference and Dendrite core runs passed"
