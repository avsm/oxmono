#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../../../.."
root=$PWD
harness="$root/bleeding/mqttz/test/docker"
export MQTTZ_FIXTURES
MQTTZ_FIXTURES=$(mktemp -d "${TMPDIR:-/tmp}/mqttz-harness.XXXXXX")
chmod 755 "$MQTTZ_FIXTURES"
project="mqttz-test-$$"
compose=(docker compose -p "$project" -f "$harness/compose.yaml")
cleanup() {
  status=$?
  if (( status != 0 )); then "${compose[@]}" logs --no-color >&2 || true; fi
  "${compose[@]}" down --volumes --remove-orphans >/dev/null 2>&1 || true
  rm -rf "$MQTTZ_FIXTURES"
  exit "$status"
}
trap cleanup EXIT

opam exec --switch="${MQTTZ_SWITCH:-5.2.0+ox}" -- \
  dune build --profile release-check bleeding/mqttz/test/integration.exe
cp "$harness/mosquitto.conf" "$MQTTZ_FIXTURES/"
openssl req -x509 -newkey rsa:2048 -nodes -days 1 \
  -subj /CN=localhost -addext 'subjectAltName=DNS:localhost,IP:127.0.0.1' \
  -keyout "$MQTTZ_FIXTURES/server.key" \
  -out "$MQTTZ_FIXTURES/server.crt" >/dev/null 2>&1
chmod 644 "$MQTTZ_FIXTURES/server.key"
docker run --rm --user "$(id -u):$(id -g)" --entrypoint mosquitto_passwd \
  -v "$MQTTZ_FIXTURES:/fixtures" \
  eclipse-mosquitto:2.0.22@sha256:212f89e1eaeb2c322d6441b64396e3346026674db8fa9c27beac293405c32b3c \
  -b -c /fixtures/passwords mqttz mqttz-test
chmod 644 "$MQTTZ_FIXTURES/passwords"
"${compose[@]}" up -d --wait --wait-timeout 30
export MQTTZ_PORT MQTTZ_AUTH_PORT MQTTZ_TLS_PORT MQTTZ_CA_FILE
MQTTZ_PORT=$("${compose[@]}" port broker 1883 | sed 's/.*://')
MQTTZ_AUTH_PORT=$("${compose[@]}" port broker 1884 | sed 's/.*://')
MQTTZ_TLS_PORT=$("${compose[@]}" port broker 8883 | sed 's/.*://')
MQTTZ_CA_FILE="$MQTTZ_FIXTURES/server.crt"

# Independent Mosquitto clients exercise both directions of interoperability.
"${compose[@]}" exec -T broker mosquitto_pub -V mqttv5 -q 2 -r \
  -t mqttz/oracle/in -m from-mosquitto
"$root/_build/default/bleeding/mqttz/test/integration.exe"
oracle=$("${compose[@]}" exec -T broker mosquitto_sub -V mqttv5 \
  -q 2 -t mqttz/oracle/out -C 1 -W 5)
test "$oracle" = from-mqttz
printf '%s\n' 'PASS independent mosquitto_pub/mosquitto_sub interoperability'
