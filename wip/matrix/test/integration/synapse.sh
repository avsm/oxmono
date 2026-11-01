#!/usr/bin/env bash
#
# Start, stop and inspect the Synapse homeserver the integration tests run
# against.
#
#   test/integration/synapse.sh up      # start it, print the export line
#   eval "$(test/integration/synapse.sh url --export)"
#   dune build @integration
#   test/integration/synapse.sh down --purge
#
# The server is a throwaway: its config, including every secret, is committed
# in test/integration/synapse/homeserver.yaml.

set -euo pipefail

# The image is pinned so that what CI runs is what a developer ran. Bump it
# deliberately, and re-check the experimental flag names in homeserver.yaml
# when you do.
IMAGE="ghcr.io/element-hq/synapse:v1.159.0"

CONTAINER="${MATRIX_TEST_SYNAPSE_CONTAINER:-ocaml-matrix-synapse}"
DATA_DIR="${MATRIX_TEST_SYNAPSE_DATA:-${TMPDIR:-/tmp}/$CONTAINER}"
PORT="${MATRIX_TEST_PORT:-8008}"
SERVER_NAME="localhost"
HEALTH_TIMEOUT="${MATRIX_TEST_SYNAPSE_TIMEOUT:-120}"
SENTINEL_NAME=".ocaml-matrix-synapse-data"

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG="$HERE/synapse/homeserver.yaml"

homeserver_url() { echo "http://127.0.0.1:$PORT"; }

log() { printf '%s\n' "$*" >&2; }
die() { log "synapse.sh: $*"; exit 1; }

validate_container_name() {
  case "$CONTAINER" in
    ocaml-matrix-*) ;;
    *) die "MATRIX_TEST_SYNAPSE_CONTAINER must start with ocaml-matrix-" ;;
  esac
  case "$CONTAINER" in
    *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
      die "MATRIX_TEST_SYNAPSE_CONTAINER contains characters unsafe for Docker"
      ;;
  esac
}

validate_data_dir() {
  [ -n "$DATA_DIR" ] || die "the data directory is empty"
  case "$DATA_DIR" in
    /*) ;;
    *) die "MATRIX_TEST_SYNAPSE_DATA must be an absolute path" ;;
  esac
  local base
  base="$(basename -- "$DATA_DIR")"
  case "$base" in
    ocaml-matrix-*) ;;
    *) die "the data directory basename must start with ocaml-matrix-" ;;
  esac
  case "$base" in
    *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
      die "the data directory basename contains unsafe characters"
      ;;
  esac
  [ ! -L "$DATA_DIR" ] || die "the data directory must not be a symlink"
}

need_docker() {
  command -v docker >/dev/null 2>&1 || die "docker is not on PATH"
}

# Docker's "running", "exited" or the empty string for "no such container".
container_state() {
  docker inspect -f '{{.State.Status}}' "$CONTAINER" 2>/dev/null || true
}

pull_image() {
  if docker image inspect "$IMAGE" >/dev/null 2>&1; then
    return 0
  fi
  log "synapse.sh: pulling $IMAGE"
  if ! docker pull "$IMAGE"; then
    die "could not pull $IMAGE.
A stale ghcr.io entry in ~/.docker/config.json makes anonymous pulls fail with
'denied'; 'docker logout ghcr.io' fixes it."
  fi
}

# The image drops privileges with gosu when UID/GID are set (note: not
# SYNAPSE_UID/SYNAPSE_GID, whatever the upstream README once said — see
# /start.py's `if "UID" in environ`), so the signing key, the database and
# the media store end up owned by whoever ran this script rather than root.
docker_run_common=()
set_run_common() {
  docker_run_common=(
    -v "$DATA_DIR:/data"
    -e "UID=$(id -u)"
    -e "GID=$(id -g)"
    -e "SYNAPSE_SERVER_NAME=$SERVER_NAME"
    -e "SYNAPSE_REPORT_STATS=no"
  )
}

generate_once() {
  if [ -f "$DATA_DIR/$SERVER_NAME.signing.key" ]; then
    return 0
  fi
  log "synapse.sh: generating a signing key and log config in $DATA_DIR"
  docker run --rm "${docker_run_common[@]}" "$IMAGE" generate
}

wait_for_health() {
  local url deadline
  url="$(homeserver_url)/health"
  deadline=$(( $(date +%s) + HEALTH_TIMEOUT ))
  while [ "$(date +%s)" -lt "$deadline" ]; do
    if curl -fsS -o /dev/null --max-time 2 "$url" 2>/dev/null; then
      return 0
    fi
    if [ "$(container_state)" != "running" ]; then
      log "synapse.sh: the container stopped while starting up; last log lines:"
      docker logs --tail 40 "$CONTAINER" >&2 || true
      return 1
    fi
    sleep 1
  done
  log "synapse.sh: $url did not answer within ${HEALTH_TIMEOUT}s; last log lines:"
  docker logs --tail 40 "$CONTAINER" >&2 || true
  return 1
}

cmd_up() {
  validate_data_dir
  need_docker
  [ -f "$CONFIG" ] || die "missing $CONFIG"

  case "$(container_state)" in
    running)
      log "synapse.sh: $CONTAINER is already running"
      echo "export MATRIX_TEST_HOMESERVER=$(homeserver_url)"
      return 0
      ;;
    "") ;;
    *)
      log "synapse.sh: removing the stopped $CONTAINER"
      docker rm -f "$CONTAINER" >/dev/null
      ;;
  esac

  pull_image
  mkdir -p "$DATA_DIR"
  [ ! -L "$DATA_DIR/$SENTINEL_NAME" ] \
    || die "refusing a symlinked data-directory sentinel: $DATA_DIR/$SENTINEL_NAME"
  touch "$DATA_DIR/$SENTINEL_NAME"
  set_run_common
  generate_once

  # The committed config replaces whatever `generate` wrote. The signing key
  # and the log config it also wrote are kept.
  cp "$CONFIG" "$DATA_DIR/homeserver.yaml"

  log "synapse.sh: starting $CONTAINER on 127.0.0.1:$PORT (data in $DATA_DIR)"
  docker run -d --name "$CONTAINER" \
    -p "127.0.0.1:$PORT:8008" \
    "${docker_run_common[@]}" \
    "$IMAGE" >/dev/null

  if ! wait_for_health; then
    die "Synapse did not come up"
  fi

  log "synapse.sh: Synapse is up"
  echo "export MATRIX_TEST_HOMESERVER=$(homeserver_url)"
}

cmd_down() {
  local purge=0
  case "${1:-}" in
    --purge) purge=1 ;;
    "") ;;
    *) die "down accepts only --purge" ;;
  esac
  validate_data_dir
  if [ "$purge" -eq 1 ] && [ -d "$DATA_DIR" ]; then
    [ -f "$DATA_DIR/$SENTINEL_NAME" ] \
      && [ ! -L "$DATA_DIR/$SENTINEL_NAME" ] \
      || die "refusing to purge an unmarked data directory: $DATA_DIR"
  fi
  need_docker

  if [ -n "$(container_state)" ]; then
    log "synapse.sh: removing $CONTAINER"
    docker rm -f "$CONTAINER" >/dev/null
  else
    log "synapse.sh: $CONTAINER is not there"
  fi

  if [ "$purge" -eq 1 ]; then
    # The database and media store belong to the caller (see UID/GID above),
    # so this needs no privileges.
    log "synapse.sh: removing $DATA_DIR"
    rm -rf -- "$DATA_DIR"
  fi
}

cmd_logs() {
  need_docker
  docker logs "$@" "$CONTAINER"
}

cmd_url() {
  if [ "${1:-}" = "--export" ]; then
    echo "export MATRIX_TEST_HOMESERVER=$(homeserver_url)"
  else
    homeserver_url
  fi
}

cmd_status() {
  need_docker
  local state
  state="$(container_state)"
  echo "image:      $IMAGE"
  echo "container:  $CONTAINER (${state:-absent})"
  echo "data dir:   $DATA_DIR"
  echo "homeserver: $(homeserver_url)"
  if [ "$state" = "running" ] && curl -fsS -o /dev/null --max-time 2 "$(homeserver_url)/health"; then
    echo "health:     ok"
  else
    echo "health:     unreachable"
    return 1
  fi
}

usage() {
  cat >&2 <<'USAGE'
usage: synapse.sh <command>

  up               start Synapse (idempotent) and print the export line
  down [--purge]   stop and remove it; --purge also deletes the data dir
  logs [args...]   docker logs for the container (e.g. logs -f)
  url [--export]   print the homeserver URL, or the export line
  status           print the image, container, data dir and health

environment:
  MATRIX_TEST_PORT            host port to bind, default 8008
  MATRIX_TEST_SYNAPSE_CONTAINER
                              Docker name, default ocaml-matrix-synapse
  MATRIX_TEST_SYNAPSE_DATA    data directory, default $TMPDIR/<container-name>
  MATRIX_TEST_SYNAPSE_TIMEOUT seconds to wait for /health, default 120
USAGE
  exit 2
}

validate_container_name
case "${1:-}" in
  up) shift; cmd_up "$@" ;;
  down) shift; cmd_down "$@" ;;
  logs) shift; cmd_logs "$@" ;;
  url) shift; cmd_url "$@" ;;
  status) shift; cmd_status "$@" ;;
  *) usage ;;
esac
