#!/usr/bin/env bash
# Run the live integration harness against Dendrite, a second Matrix
# homeserver implementation. This deliberately keeps the full test command
# available: a missing/unsupported MSC is a reported test result, never a
# silently ignored failure. Use `run-core` for the small portable CS-API slice
# while bringing up a new Dendrite version.
#
#   test/integration/dendrite.sh up
#   eval "$(test/integration/dendrite.sh url --export)"
#   test/integration/dendrite.sh run-core
#   test/integration/dendrite.sh down --purge

set -euo pipefail

# Dendrite's v0.15.2 multi-architecture manifest digest (released 2025-08-15).
# The digest, rather than a floating tag, makes this test server reproducible.
IMAGE="docker.io/matrixdotorg/dendrite-monolith:v0.15.2@sha256:7dafe6edfc8cfab758a68a4cf20414df1ade4a36b45b1852554d81fb70b1272c"
CONTAINER="${MATRIX_TEST_DENDRITE_CONTAINER:-ocaml-matrix-dendrite}"
DATA_DIR="${MATRIX_TEST_DENDRITE_DATA:-${TMPDIR:-/tmp}/$CONTAINER}"
PORT="${MATRIX_TEST_DENDRITE_PORT:-18008}"
HEALTH_TIMEOUT="${MATRIX_TEST_DENDRITE_TIMEOUT:-120}"
SENTINEL_NAME=".ocaml-matrix-dendrite-data"

homeserver_url() { echo "http://127.0.0.1:$PORT"; }
log() { printf '%s\n' "$*" >&2; }
die() { log "dendrite.sh: $*"; exit 1; }
need_docker() { command -v docker >/dev/null 2>&1 || die "docker is not on PATH"; }

validate_container_name() {
  case "$CONTAINER" in
    ocaml-matrix-*) ;;
    *) die "MATRIX_TEST_DENDRITE_CONTAINER must start with ocaml-matrix-" ;;
  esac
  case "$CONTAINER" in
    *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
      die "MATRIX_TEST_DENDRITE_CONTAINER contains characters unsafe for Docker"
      ;;
  esac
}

validate_data_dir() {
  [ -n "$DATA_DIR" ] || die "the data directory is empty"
  case "$DATA_DIR" in
    /*) ;;
    *) die "MATRIX_TEST_DENDRITE_DATA must be an absolute path" ;;
  esac
  # Purge runs a root helper over this bind mount. Restrict the final component
  # as well as using a sentinel so an accidental broad path can never qualify.
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
  log "dendrite.sh: pulling $IMAGE"
  docker pull "$IMAGE" || die "could not pull the pinned Dendrite image"
}

generate_once() {
  validate_data_dir
  [ ! -L "$DATA_DIR/$SENTINEL_NAME" ] \
    || die "refusing a symlinked data-directory sentinel: $DATA_DIR/$SENTINEL_NAME"
  if [ -f "$DATA_DIR/dendrite.yaml" ] && [ -f "$DATA_DIR/matrix_key.pem" ]; then
    touch "$DATA_DIR/$SENTINEL_NAME"
    return 0
  fi
  log "dendrite.sh: generating keys and a SQLite monolith config in $DATA_DIR"
  mkdir -p "$DATA_DIR"
  touch "$DATA_DIR/$SENTINEL_NAME"
  docker run --rm --entrypoint=/usr/bin/generate-keys \
    -v "$DATA_DIR:/mnt" "$IMAGE" -private-key /mnt/matrix_key.pem
  # The generated config points all component databases at the requested SQLite
  # file and uses /var/dendrite for media, search, and JetStream state.
  docker run --rm --entrypoint=/bin/sh -v "$DATA_DIR:/mnt" "$IMAGE" \
    -c '/usr/bin/generate-config -dir /var/dendrite/ \
      -db file:///var/dendrite/dendrite.db -server localhost > /mnt/dendrite.yaml'

  # Registration is intentionally open only for this throwaway local server.
  # Disable rate limits to keep the existing random-user suite deterministic.
  sed -i \
    -e 's/^  registration_disabled: true$/  registration_disabled: false/' \
    -e 's/^    enabled: true$/    enabled: false/' \
    "$DATA_DIR/dendrite.yaml"
}

wait_for_health() {
  local url deadline
  # Dendrite does not expose Synapse's /health endpoint; versions is the
  # portable unauthenticated client-server readiness probe.
  url="$(homeserver_url)/_matrix/client/versions"
  deadline=$(( $(date +%s) + HEALTH_TIMEOUT ))
  while [ "$(date +%s)" -lt "$deadline" ]; do
    if curl -fsS -o /dev/null --max-time 2 "$url" 2>/dev/null; then return 0; fi
    if [ "$(container_state)" != running ]; then
      log "dendrite.sh: container stopped while starting; recent logs:"
      docker logs --tail 60 "$CONTAINER" >&2 || true
      return 1
    fi
    sleep 1
  done
  log "dendrite.sh: $url did not answer within ${HEALTH_TIMEOUT}s; recent logs:"
  docker logs --tail 60 "$CONTAINER" >&2 || true
  return 1
}

cmd_up() {
  need_docker
  case "$(container_state)" in
    running)
      log "dendrite.sh: $CONTAINER is already running"
      echo "export MATRIX_TEST_HOMESERVER=$(homeserver_url)"
      return 0
      ;;
    "") ;;
    *) docker rm -f "$CONTAINER" >/dev/null ;;
  esac
  pull_image
  generate_once
  log "dendrite.sh: starting $CONTAINER on 127.0.0.1:$PORT"
  docker run -d --name "$CONTAINER" \
    -p "127.0.0.1:$PORT:8008" \
    -v "$DATA_DIR:/etc/dendrite" \
    -v "$DATA_DIR:/var/dendrite" \
    "$IMAGE" -really-enable-open-registration >/dev/null
  wait_for_health || die "Dendrite did not come up"
  log "dendrite.sh: Dendrite is up"
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
  [ -n "$(container_state)" ] && docker rm -f "$CONTAINER" >/dev/null || true
  if [ "$purge" -eq 1 ]; then
    log "dendrite.sh: removing $DATA_DIR"
    # Dendrite writes its SQLite/media/JetStream files as root. Use the
    # already-pulled image as a tiny root cleanup helper, then remove the now
    # empty directory as the invoking user.
    if [ -d "$DATA_DIR" ]; then
      docker run --rm --user 0 --entrypoint=/bin/sh \
        -v "$DATA_DIR:/data" "$IMAGE" \
        -c 'find /data -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +'
      rmdir "$DATA_DIR" || die "could not remove $DATA_DIR after root cleanup"
    fi
  fi
}

cmd_run() {
  need_docker
  [ -n "${MATRIX_TEST_HOMESERVER:-}" ] || die "set MATRIX_TEST_HOMESERVER (run: eval \"\$(test/integration/dendrite.sh url --export)\")"
  # Keep the Alcotest subcommand first: its positional filter/test-case
  # arguments otherwise get parsed as global command arguments.
  dune exec --only-packages matrix-chat \
    --build-dir "${MATRIX_TEST_DUNE_BUILD_DIR:-_build}" \
    test/integration/test_homeserver.exe -- test "$@" --color=never
}

cmd_run_core() {
  # Use a named profile rather than positions in the full Synapse list. New
  # reference-only scenarios can then never silently retarget this smoke run.
  MATRIX_TEST_PROFILE=dendrite-core cmd_run rooms
}

cmd_run_peeking() {
  # This is an explicit capability probe, not part of the portable pass set.
  # The pinned Dendrite currently returns M_GUEST_ACCESS_FORBIDDEN and this
  # command deliberately preserves that non-zero test result.
  MATRIX_TEST_PROFILE=peeking cmd_run rooms
}

cmd_capabilities() {
  need_docker
  local versions
  versions="$(curl -fsS --max-time 5 "$(homeserver_url)/_matrix/client/versions")" \
    || die "Dendrite is not reachable at $(homeserver_url)"
  echo "advertised client-server versions: $versions"
  echo "capability skips (not counted as core passes):"
  if grep -q 'msc3575\|org.matrix.simplified_msc3575' <<<"$versions"; then
    echo "  sliding sync (MSC3575/MSC4186): advertised; run rooms case 6"
  else
    echo "  sliding sync (MSC3575/MSC4186): unavailable; rooms case 6 is a capability skip"
  fi
  echo "  preallocated media (MSC3916): run rooms case 5; failures identify server support gaps"
  echo "  thread receipts: run rooms case 3; failures identify server support gaps"
  echo "  legacy peeking: run-peeking; the pinned server currently rejects it"
}

cmd_status() {
  need_docker
  local state
  state="$(container_state)"
  echo "image:      $IMAGE"
  echo "container:  $CONTAINER (${state:-absent})"
  echo "data dir:   $DATA_DIR"
  echo "homeserver: $(homeserver_url)"
  if [ "$state" = running ] && curl -fsS -o /dev/null --max-time 2 "$(homeserver_url)/_matrix/client/versions"; then
    echo "health:     ok"
  else
    echo "health:     unreachable"
    return 1
  fi
}

usage() {
  cat >&2 <<'EOF'
usage: dendrite.sh <command>

  up                 start pinned Dendrite and print the export line
  down [--purge]     stop/remove it; --purge also deletes its data directory
  run [FILTER]       run the complete live integration executable
  run-core           run the portable room CS-API smoke cases only
  run-peeking        run the explicit legacy-peeking capability probe
  capabilities       report advertised features and explicit capability skips
  logs [args...]      show Dendrite container logs
  url [--export]      print the URL, or its shell export form
  status              print image/container/data/health status

environment:
  MATRIX_TEST_DENDRITE_CONTAINER Docker name, default ocaml-matrix-dendrite
  MATRIX_TEST_DENDRITE_DATA     data directory (default: $TMPDIR/<container-name>)
  MATRIX_TEST_DENDRITE_PORT     host port (default: 18008)
  MATRIX_TEST_DENDRITE_TIMEOUT  health timeout in seconds (default: 120)
  MATRIX_TEST_DUNE_BUILD_DIR    optional isolated Dune build directory
  MATRIX_TEST_PROFILE           selected internally by run-core
EOF
  exit 2
}

validate_container_name
case "${1:-}" in
  up) shift; cmd_up "$@" ;;
  down) shift; cmd_down "$@" ;;
  run) shift; cmd_run "$@" ;;
  run-core) shift; cmd_run_core "$@" ;;
  run-peeking) shift; cmd_run_peeking "$@" ;;
  capabilities) shift; cmd_capabilities "$@" ;;
  logs) shift; need_docker; docker logs "$@" "$CONTAINER" ;;
  url)
    shift
    if [ "${1:-}" = --export ]; then
      echo "export MATRIX_TEST_HOMESERVER=$(homeserver_url)"
    else
      homeserver_url
    fi
    ;;
  status) shift; cmd_status "$@" ;;
  *) usage ;;
esac
