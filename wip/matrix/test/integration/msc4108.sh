#!/usr/bin/env bash
#
# Run a small, isolated Synapse fixture for the unauthenticated MSC4108
# rendezvous endpoint.  It intentionally does not start Matrix Authentication
# Service and therefore does not exercise OAuth or authenticated login.
#
#   test/integration/msc4108.sh up
#   eval "$(test/integration/msc4108.sh url --export)"
#   test/integration/msc4108.sh status
#   test/integration/msc4108.sh down --purge
#
# The ordinary fixture is test/integration/synapse.sh and uses port 8008.  This
# one has its own container, data directory, and default host port (8009).

set -euo pipefail

IMAGE="ghcr.io/element-hq/synapse:v1.159.0"
CONTAINER="${MATRIX_TEST_MSC4108_CONTAINER:-ocaml-matrix-synapse-msc4108}"
DATA_DIR="${MATRIX_TEST_MSC4108_DATA:-${TMPDIR:-/tmp}/$CONTAINER}"
# The overlay contains this port in public_baseurl, so keep the fixture fixed
# at 8009.  A custom port would make rendezvous URLs advertise the wrong host.
PORT="8009"
HEALTH_TIMEOUT="${MATRIX_TEST_MSC4108_TIMEOUT:-120}"
SERVER_NAME="localhost"
SENTINEL_NAME=".ocaml-matrix-msc4108-data"

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_CONFIG="$HERE/synapse/homeserver.yaml"
OVERLAY_CONFIG="$HERE/msc4108.yaml"

homeserver_url() { echo "http://127.0.0.1:$PORT"; }
log() { printf '%s\n' "$*" >&2; }
die() { log "msc4108.sh: $*"; exit 1; }
need_docker() { command -v docker >/dev/null 2>&1 || die "docker is not on PATH"; }

validate_container_name() {
  case "$CONTAINER" in
    ocaml-matrix-*) ;;
    *) die "MATRIX_TEST_MSC4108_CONTAINER must start with ocaml-matrix-" ;;
  esac
  case "$CONTAINER" in
    *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
      die "MATRIX_TEST_MSC4108_CONTAINER contains characters unsafe for Docker"
      ;;
  esac
}

validate_data_dir() {
  [ -n "$DATA_DIR" ] || die "the data directory is empty"
  case "$DATA_DIR" in
    /*) ;;
    *) die "MATRIX_TEST_MSC4108_DATA must be an absolute path" ;;
  esac
  case "$(basename -- "$DATA_DIR")" in
    ocaml-matrix-*) ;;
    *) die "the data directory basename must start with ocaml-matrix-" ;;
  esac
  case "$(basename -- "$DATA_DIR")" in
    *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
      die "the data directory basename contains unsafe characters"
      ;;
  esac
  [ ! -L "$DATA_DIR" ] || die "the data directory must not be a symlink"
}

container_state() {
  docker inspect -f '{{.State.Status}}' "$CONTAINER" 2>/dev/null || true
}

pull_image() {
  if docker image inspect "$IMAGE" >/dev/null 2>&1; then return 0; fi
  log "msc4108.sh: pulling $IMAGE"
  docker pull "$IMAGE" || die "could not pull the pinned Synapse image"
}

# The image's start.py uses UID/GID to drop privileges.  Keeping these values
# equal to the invoking user makes the SQLite database, signing key, and media
# directory removable without a root cleanup helper.
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
  if [ -f "$DATA_DIR/$SERVER_NAME.signing.key" ] \
    && [ -f "$DATA_DIR/$SERVER_NAME.log.config" ]; then
    return 0
  fi
  log "msc4108.sh: generating signing key and log config in $DATA_DIR"
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
    if [ "$(container_state)" != running ]; then
      log "msc4108.sh: container stopped while starting; recent logs:"
      docker logs --tail 40 "$CONTAINER" >&2 || true
      return 1
    fi
    sleep 1
  done
  log "msc4108.sh: $url did not answer within ${HEALTH_TIMEOUT}s; recent logs:"
  docker logs --tail 40 "$CONTAINER" >&2 || true
  return 1
}

cmd_up() {
  need_docker
  validate_data_dir
  [ -f "$BASE_CONFIG" ] || die "missing $BASE_CONFIG"
  [ -f "$OVERLAY_CONFIG" ] || die "missing $OVERLAY_CONFIG"

  case "$(container_state)" in
    running)
      log "msc4108.sh: $CONTAINER is already running"
      echo "export MATRIX_TEST_RENDEZVOUS_HOMESERVER=$(homeserver_url)"
      return 0
      ;;
    "") ;;
    *)
      log "msc4108.sh: removing the stopped $CONTAINER"
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

  # Keep both config files read-only and separate from /data.  Synapse merges
  # them in order; the second -c is the MSC4108 test-only overlay.
  log "msc4108.sh: starting $CONTAINER on 127.0.0.1:$PORT (data in $DATA_DIR)"
  docker run -d --name "$CONTAINER" \
    -p "127.0.0.1:$PORT:8008" \
    "${docker_run_common[@]}" \
    -v "$BASE_CONFIG:/config/ocaml-matrix-homeserver.yaml:ro" \
    -v "$OVERLAY_CONFIG:/config/msc4108.yaml:ro" \
    "$IMAGE" run \
    -c /config/ocaml-matrix-homeserver.yaml \
    -c /config/msc4108.yaml >/dev/null

  if ! wait_for_health; then
    die "Synapse did not come up"
  fi
  log "msc4108.sh: Synapse MSC4108 fixture is up (unauthenticated rendezvous only)"
  echo "export MATRIX_TEST_RENDEZVOUS_HOMESERVER=$(homeserver_url)"
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
    log "msc4108.sh: removing $CONTAINER"
    docker rm -f "$CONTAINER" >/dev/null
  else
    log "msc4108.sh: $CONTAINER is not there"
  fi

  if [ "$purge" -eq 1 ]; then
    if [ -d "$DATA_DIR" ]; then
      log "msc4108.sh: removing $DATA_DIR"
      rm -rf -- "$DATA_DIR"
    fi
  fi
}

cmd_logs() {
  need_docker
  docker logs "$@" "$CONTAINER"
}

cmd_url() {
  if [ "${1:-}" = "--export" ]; then
    echo "export MATRIX_TEST_RENDEZVOUS_HOMESERVER=$(homeserver_url)"
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
  if [ "$state" = running ] \
    && curl -fsS -o /dev/null --max-time 2 "$(homeserver_url)/health"; then
    echo "health:     ok"
  else
    echo "health:     unreachable"
    return 1
  fi
}

usage() {
  cat >&2 <<'USAGE'
usage: msc4108.sh <command>

  up               start the isolated Synapse fixture and print the export line
  down [--purge]   stop/remove it; --purge also deletes its marked data dir
  logs [args...]   docker logs for the fixture container (e.g. logs -f)
  url [--export]   print the URL, or the shell export form
  status           print image, container, data dir, and health

This fixture validates only the unauthenticated MSC4108 rendezvous endpoint;
it deliberately does not start or test Matrix Authentication Service login.

environment:
  MATRIX_TEST_MSC4108_CONTAINER Docker name, default ocaml-matrix-synapse-msc4108
  MATRIX_TEST_MSC4108_DATA     data directory (default: $TMPDIR/<container-name>)
  MATRIX_TEST_MSC4108_TIMEOUT  health timeout in seconds (default: 120)
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
