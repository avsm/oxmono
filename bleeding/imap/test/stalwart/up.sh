#!/bin/sh
# Start an isolated Stalwart IMAP fixture. Eval the export output.
set -eu
NAME="${1:-imap-stalwart-codex}"
IMAGE="${IMAP_STALWART_IMAGE:-stalwartlabs/stalwart@sha256:dcf575db2d53d9ef86d6ced8abe4ba491984659a0f8862cc6079ee7b41c3c568}"
PORT="${IMAP_STALWART_PORT:-0}"
LABEL='org.oxmono.imap-stalwart'
case "$PORT" in
  ''|*[!0-9]*) echo 'IMAP_STALWART_PORT must be 0..65535' >&2; exit 2 ;;
esac
if [ "$PORT" -gt 65535 ]; then
  echo 'IMAP_STALWART_PORT must be 0..65535' >&2; exit 2
fi
if docker inspect "$NAME" >/dev/null 2>&1; then
  owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-stalwart" }}' "$NAME")
  if [ "$owner" != true ]; then
    echo "refusing to use unrelated container $NAME" >&2
    exit 2
  fi
  running=$(docker inspect --format '{{ .State.Running }}' "$NAME")
  if [ "$running" != true ]; then docker start "$NAME" >/dev/null; fi
else
  HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
  docker run -d --name "$NAME" --label "$LABEL=true" \
    --tmpfs /opt/stalwart/data:rw,size=256m \
    -v "$HERE/config.toml:/opt/stalwart/etc/config.toml:ro" \
    -p "127.0.0.1:$PORT:143" "$IMAGE" >/dev/null
fi
mapped=$(docker port "$NAME" 143/tcp)
case "$mapped" in
  127.0.0.1:*) PORT="${mapped##*:}" ;;
  *) echo "fixture must publish loopback only: $mapped" >&2; exit 2 ;;
esac
printf 'waiting for Stalwart %s' "$NAME" >&2
for _ in $(seq 1 40); do
  if python3 - "$PORT" <<'PY' >/dev/null 2>&1
import socket, sys
with socket.create_connection(('127.0.0.1', int(sys.argv[1])), timeout=1) as s:
    s.settimeout(1)
    greeting = s.makefile('rb').readline()
    if not greeting.startswith(b'* OK'):
        raise RuntimeError(greeting)
PY
  then
    echo ' ready' >&2
    echo 'export IMAP_STALWART_HOST=127.0.0.1'
    echo "export IMAP_STALWART_PORT=$PORT"
    echo 'export IMAP_STALWART_USER=imap-test-user'
    echo 'export IMAP_STALWART_PASSWORD=imap-test-password'
    exit 0
  fi
  printf '.' >&2
  sleep 1
done
echo ' timed out' >&2
docker logs "$NAME" 2>&1 | tail -40 >&2
exit 1
