#!/usr/bin/env bash
#
# Bring up the pinned local Zulip server used by the live integration suite.
#
#   test/integration/zulip.sh up
#   eval "$(test/integration/zulip.sh env)"
#   dune build @integration
#   test/integration/zulip.sh down --purge
#
# `run` creates a private project by default, so a failed or interrupted live
# test cannot affect an interactive `up` server.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$HERE/compose.yaml"
RUN_ID="${ZULIP_TEST_RUN_ID:-local}"
PROJECT="ocaml-zulip-$RUN_ID"
DATA_DIR="${ZULIP_TEST_DATA:-${TMPDIR:-/tmp}/ocaml-zulip-$RUN_ID}"
SECRETS_DIR="$DATA_DIR/secrets"
OUTPUT_DIR="$DATA_DIR/output"
FIXTURES="$OUTPUT_DIR/fixtures.json"
DOCKER_CONFIG="${ZULIP_TEST_DOCKER_CONFIG:-$DATA_DIR/docker-config}"
PORT="${ZULIP_TEST_PORT:-19999}"
TIMEOUT="${ZULIP_TEST_TIMEOUT:-300}"
SENTINEL=".ocaml-zulip-integration-data"

log() { printf '%s\n' "zulip.sh: $*" >&2; }
die() { log "$*"; exit 1; }
server_url() { printf 'http://127.0.0.1:%s' "$PORT"; }
compose() {
  docker --config "$DOCKER_CONFIG" compose --project-name "$PROJECT" --file "$COMPOSE_FILE" "$@"
}

validate_name() {
  case "$RUN_ID" in
    '' | *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
      die "ZULIP_TEST_RUN_ID contains characters unsafe for Docker"
      ;;
  esac
  case "$PROJECT" in ocaml-zulip-*) ;; *) die "internal project-name check failed" ;; esac
}

validate_data_dir() {
  [ -n "$DATA_DIR" ] || die "the data directory is empty"
  case "$DATA_DIR" in /*) ;; *) die "ZULIP_TEST_DATA must be an absolute path" ;; esac
  local base
  base="$(basename -- "$DATA_DIR")"
  case "$base" in ocaml-zulip-*) ;; *) die "data directory basename must start with ocaml-zulip-" ;; esac
  case "$base" in *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-]*)
    die "data directory basename contains unsafe characters"
    ;;
  esac
  [ ! -L "$DATA_DIR" ] || die "the data directory must not be a symlink"
}

canonical_data_dir() {
  realpath -e -- "$DATA_DIR"
}

marker_contents() {
  printf 'project=%s\ndata_dir=%s\n' "$PROJECT" "$(canonical_data_dir)"
}

verify_data_owner() {
  [ -d "$DATA_DIR" ] || die "the data directory does not exist: $DATA_DIR"
  [ ! -L "$DATA_DIR" ] || die "the data directory must not be a symlink"
  [ -f "$DATA_DIR/$SENTINEL" ] && [ ! -L "$DATA_DIR/$SENTINEL" ] \
    || die "refusing an unmarked data directory: $DATA_DIR"
  [ "$(cat "$DATA_DIR/$SENTINEL")" = "$(marker_contents)" ] \
    || die "data-directory ownership marker does not match $PROJECT"
}

verify_child_dir() {
  local child="$1"
  [ -d "$child" ] && [ ! -L "$child" ] \
    || die "expected a non-symlink directory at $child"
}

need_docker() {
  command -v docker >/dev/null 2>&1 || die "docker is not on PATH"
  docker info >/dev/null 2>&1 || die "Docker is unavailable; start Docker or grant this user access to its socket"
}

prepare_data() {
  validate_data_dir
  if [ -e "$DATA_DIR" ]; then
    verify_data_owner
  else
    mkdir -p "$DATA_DIR"
    [ ! -L "$DATA_DIR" ] || die "the data directory must not be a symlink"
    marker_contents >"$DATA_DIR/$SENTINEL"
  fi
  mkdir -p "$SECRETS_DIR" "$OUTPUT_DIR" "$DOCKER_CONFIG"
  verify_child_dir "$SECRETS_DIR"
  verify_child_dir "$OUTPUT_DIR"
  verify_child_dir "$DOCKER_CONFIG"
  chmod 700 "$DATA_DIR" "$SECRETS_DIR" "$DOCKER_CONFIG"
  # The server's unprivileged `zulip` user writes only the generated fixture.
  chmod 777 "$OUTPUT_DIR"
  local name
  for name in postgres_password memcached_password rabbitmq_password redis_password secret_key email_password; do
    [ ! -L "$SECRETS_DIR/$name" ] || die "refusing a symlinked secret file"
    if [ ! -s "$SECRETS_DIR/$name" ]; then
      umask 077
      head -c 32 /dev/urandom | base64 >"$SECRETS_DIR/$name"
    fi
  done
}

export_compose_environment() {
  export ZULIP_TEST_PORT="$PORT"
  export ZULIP_TEST_SECRETS_DIR="$SECRETS_DIR"
  export ZULIP_TEST_OUTPUT_DIR="$OUTPUT_DIR"
  export ZULIP_TEST_SOURCE_DIR="$HERE"
}

is_running() {
  [ "$(compose ps --status running --services 2>/dev/null | grep -cx zulip || true)" = 1 ]
}

wait_for_api() {
  local deadline=$(( $(date +%s) + TIMEOUT ))
  local url="$(server_url)/api/v1/server_settings"
  while [ "$(date +%s)" -lt "$deadline" ]; do
    if curl -fsS --max-time 3 "$url" >/dev/null 2>&1; then
      return 0
    fi
    if ! is_running; then
      log "Zulip stopped while starting; recent logs follow:"
      compose logs --tail 120 >&2 || true
      return 1
    fi
    sleep 2
  done
  log "$url did not become ready within ${TIMEOUT}s; recent logs follow:"
  compose logs --tail 120 >&2 || true
  return 1
}

seed() {
  rm -f -- "$FIXTURES"
  compose exec -T --user zulip -e "ZULIP_TEST_PORT=$PORT" zulip \
    /home/zulip/deployments/current/manage.py shell -c \
    "exec(compile(open('/ocaml-zulip-test/seed.py').read(), '/ocaml-zulip-test/seed.py', 'exec'))"
  [ -s "$FIXTURES" ] || die "fixture bootstrap completed without producing $FIXTURES"
}

print_env() {
  [ -s "$FIXTURES" ] || die "no fixture file exists; run '$0 up' first"
  printf 'export ZULIP_TEST_SERVER=%q\n' "$(server_url)"
  printf 'export ZULIP_TEST_FIXTURES=%q\n' "$FIXTURES"
  printf 'export ZULIP_TEST_RESTART_HELPER=%q\n' "$HERE/zulip.sh"
  printf 'export ZULIP_TEST_RUN_ID=%q\n' "$RUN_ID"
  printf 'export ZULIP_TEST_PORT=%q\n' "$PORT"
  printf 'export ZULIP_TEST_DATA=%q\n' "$DATA_DIR"
}

cmd_restart() {
  need_docker
  validate_data_dir
  verify_data_owner
  export_compose_environment
  is_running || die "no running test server to restart"
  compose restart -t 5 zulip
  wait_for_api || die "Zulip did not recover after restart"
}

cmd_up() {
  need_docker
  prepare_data
  export_compose_environment
  [ -f "$COMPOSE_FILE" ] || die "missing $COMPOSE_FILE"
  if ! is_running; then
    if [ ! -f "$DATA_DIR/.initialized" ]; then
      log "initializing Zulip's database for $PROJECT"
      compose run --rm zulip app:init
      : >"$DATA_DIR/.initialized"
    fi
    log "starting $PROJECT on $(server_url)"
    compose up -d
  fi
  wait_for_api || die "Zulip did not start"
  seed
  print_env
}

cmd_smoke() {
  cmd_up >/dev/null
  local credentials response
  credentials="$(python3 -c 'import json,sys; u=json.load(open(sys.argv[1]))["users"]["admin"]; print(u["email"]+":"+u["api_key"])' "$FIXTURES")"
  response="$(curl -fsS --max-time 15 --user "$credentials" "$(server_url)/api/v1/users/me")"
  case "$response" in
    *'"result":"success"'* | *'"result": "success"'*) log "authentication smoke test passed" ;;
    *) die "authentication smoke test returned an unexpected response: $response" ;;
  esac
}

cmd_down() {
  local purge=0
  case "${1:-}" in
    --purge) purge=1 ;;
    '') ;;
    *) die "down accepts only --purge" ;;
  esac
  validate_data_dir
  verify_data_owner
  need_docker
  export_compose_environment
  if [ "$purge" = 1 ]; then
    compose down --remove-orphans --volumes
  else
    compose down --remove-orphans
  fi
  if [ "$purge" = 1 ] && [ -d "$DATA_DIR" ]; then
    log "removing $DATA_DIR"
    rm -rf -- "$DATA_DIR"
  fi
}

cmd_status() {
  need_docker
  validate_data_dir
  verify_data_owner
  export_compose_environment
  printf 'project:    %s\n' "$PROJECT"
  printf 'server:     %s\n' "$(server_url)"
  printf 'fixtures:   %s\n' "$FIXTURES"
  printf 'data dir:   %s\n' "$DATA_DIR"
  compose ps
  if curl -fsS --max-time 3 "$(server_url)/api/v1/server_settings" >/dev/null 2>&1; then
    printf 'health:     ok\n'
  else
    printf 'health:     unreachable\n'
    return 1
  fi
}

cmd_logs() {
  need_docker
  validate_data_dir
  verify_data_owner
  export_compose_environment
  compose logs "$@"
}

cmd_run() {
  # A fresh default project lets `run` clean up all containers and volumes it
  # creates.  An explicit run ID makes it inspectable after a failure.
  if [ -z "${ZULIP_TEST_RUN_ID+x}" ]; then
    local port
    port="$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
    ZULIP_TEST_RUN_ID="run-$$-$RANDOM" ZULIP_TEST_PORT="$port" "$0" run "$@"
    return
  fi
  # These cannot be local variables: EXIT/INT/TERM traps execute after this
  # function has returned, when Bash has discarded local scope.
  RUN_CLEANUP=1
  if [ "${ZULIP_TEST_KEEP:-}" = 1 ]; then RUN_CLEANUP=0; fi
  RUN_DID_CLEANUP=0
  cleanup_run() {
    local status="${1:-0}"
    [ "$RUN_CLEANUP" = 1 ] || return 0
    [ "$RUN_DID_CLEANUP" = 0 ] || return 0
    RUN_DID_CLEANUP=1
    if [ "$status" -ne 0 ]; then
      local log_dir="${ZULIP_TEST_LOG_DIR:-${TMPDIR:-/tmp}/ocaml-zulip-logs}"
      local log_file="$log_dir/$PROJECT-$(date +%Y%m%dT%H%M%SZ).log"
      mkdir -p "$log_dir"
      export_compose_environment
      compose logs --no-color >"$log_file" 2>&1 || true
      log "failure logs preserved at $log_file"
    fi
    "$0" down --purge || true
  }
  on_exit() {
    local status=$?
    cleanup_run "$status"
    trap - EXIT
    exit "$status"
  }
  on_interrupt() {
    cleanup_run 130
    trap - EXIT INT TERM
    exit 130
  }
  on_terminate() {
    cleanup_run 143
    trap - EXIT INT TERM
    exit 143
  }
  trap on_exit EXIT
  trap on_interrupt INT
  trap on_terminate TERM
  cmd_smoke
  print_env
  export ZULIP_TEST_RESTART_HELPER="$HERE/zulip.sh"
  export ZULIP_TEST_RUN_ID="$RUN_ID" ZULIP_TEST_PORT="$PORT" ZULIP_TEST_DATA="$DATA_DIR"
  if [ "$#" -gt 0 ]; then
    ZULIP_TEST_SERVER="$(server_url)" ZULIP_TEST_FIXTURES="$FIXTURES" "$@"
  else
    ZULIP_TEST_SERVER="$(server_url)" ZULIP_TEST_FIXTURES="$FIXTURES" dune build @integration
  fi
}

usage() {
  cat >&2 <<'USAGE'
usage: test/integration/zulip.sh <command> [arguments]

  up                 initialize, start and seed a local Zulip server
  smoke              prove the seeded administrator can call /api/v1/users/me
  restart            restart this test project's server, retaining its data
  run [command...]   use an isolated server, smoke-test it, then run command
                     (default: dune build @integration); removes it afterwards
  down [--purge]     stop this project; --purge also removes its Docker volumes
                     and marked local fixture directory
  logs [args...]     pass arguments to `docker compose logs`
  status             show project state and HTTP health
  env                print shell exports for a running seeded server

environment:
  ZULIP_TEST_RUN_ID         safe project suffix; defaults to local
  ZULIP_TEST_PORT           loopback HTTP port; defaults to 19999
  ZULIP_TEST_DATA           absolute marked data directory under /tmp by default
  ZULIP_TEST_DOCKER_CONFIG  isolated Docker config used for anonymous GHCR pulls
  ZULIP_TEST_TIMEOUT        startup deadline in seconds; defaults to 300
  ZULIP_TEST_KEEP=1         leave an explicitly named `run` project running
USAGE
  exit 2
}

validate_name
case "${1:-}" in
  up) shift; [ "$#" = 0 ] || usage; cmd_up ;;
  smoke) shift; [ "$#" = 0 ] || usage; cmd_smoke ;;
  restart) shift; [ "$#" = 0 ] || usage; cmd_restart ;;
  run) shift; cmd_run "$@" ;;
  down) shift; cmd_down "$@" ;;
  logs) shift; cmd_logs "$@" ;;
  status) shift; [ "$#" = 0 ] || usage; cmd_status ;;
  env) shift; [ "$#" = 0 ] || usage; print_env ;;
  *) usage ;;
esac
