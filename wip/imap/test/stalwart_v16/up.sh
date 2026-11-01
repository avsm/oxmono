#!/bin/sh
# Start a fresh Stalwart v0.16 IMAPS fixture. Eval the export output.
set -eu
NAME=${1:-imap-stalwart-v16-codex}
IMAGE=${IMAP_STALWART_V16_IMAGE:-stalwartlabs/stalwart@sha256:be215678796691bc39bdda918ecc50d14a9032a099a1d1950e51950aec7e2592}
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
case "$NAME" in
  ''|*[!a-zA-Z0-9_.-]*) echo 'invalid Docker container name' >&2; exit 2 ;;
esac
if docker inspect "$NAME" >/dev/null 2>&1; then
  echo "refusing to reuse existing container $NAME" >&2
  exit 2
fi
DIR=$(mktemp -d /tmp/imap-stalwart-v16-XXXXXXXX)
touch "$DIR/.oxmono-imap-stalwart-v16"
mkdir "$DIR/etc" "$DIR/data" "$DIR/log"
SUCCESS=0
cleanup() {
  if [ "$SUCCESS" -eq 0 ]; then "$HERE/down.sh" "$NAME" "$DIR"; fi
}
trap cleanup EXIT HUP INT TERM

start() {
  phase=$1
  shift
  docker run --rm -d --name "$NAME" \
    --label org.oxmono.imap-stalwart-v16=true \
    --label "org.oxmono.imap-stalwart-v16.dir=$DIR" \
    --user "$(id -u):$(id -g)" \
    -v "$DIR/etc:/etc/stalwart" \
    -v "$DIR/data:/var/lib/stalwart" \
    -v "$DIR/log:/var/log/stalwart" \
    "$@" "$IMAGE" >/dev/null
  if [ "$phase" = http ]; then
    mapped=$(docker port "$NAME" 8080/tcp)
  else
    mapped=$(docker port "$NAME" 993/tcp)
  fi
  case "$mapped" in
    127.0.0.1:*) PORT=${mapped##*:} ;;
    *) echo "fixture must publish loopback only: $mapped" >&2; exit 1 ;;
  esac
}

start http -e STALWART_RECOVERY_ADMIN=admin:imap-fixture-bootstrap \
  -p 127.0.0.1:0:8080
python3 "$HERE/provision.py" bootstrap "$PORT"
docker stop "$NAME" >/dev/null

start http -e STALWART_RECOVERY_MODE=1 \
  -e STALWART_RECOVERY_ADMIN=admin:imap-fixture-bootstrap \
  -p 127.0.0.1:0:8080
python3 "$HERE/provision.py" account "$PORT"
docker stop "$NAME" >/dev/null

start imaps -p 127.0.0.1:0:993
for attempt in $(seq 1 50); do
  if openssl s_client -connect "127.0.0.1:$PORT" -servername localhost \
      -showcerts </dev/null 2>/dev/null |
      openssl x509 -out "$DIR/cert.pem" 2>/dev/null; then
    break
  fi
  if [ "$attempt" -eq 50 ]; then
    echo 'IMAPS listener did not become ready' >&2
    exit 1
  fi
  sleep 0.2
done
SUCCESS=1
trap - EXIT HUP INT TERM
echo 'export IMAP_STALWART_HOST=localhost'
echo "export IMAP_STALWART_PORT=$PORT"
echo "export IMAP_STALWART_CA_CERT=$DIR/cert.pem"
echo 'export IMAP_STALWART_USER=imap-test-user@example.org'
echo 'export IMAP_STALWART_PASSWORD=imap-test-password'
echo "export IMAP_STALWART_V16_DIR=$DIR"
