#!/bin/sh
# Start (or reuse) the shared Cyrus IMAP/JMAP test oracle.
# Usage: eval "$(scripts/oracle-up.sh [name])"
set -eu

NAME="${1:-jmap-oracle}"
IMAGE="${JMAP_ORACLE_IMAGE:-ghcr.io/cyrusimap/cyrus-docker-test-server:bookworm}"
HTTP="${JMAP_ORACLE_HTTP_PORT:-18080}"
LMTP="${JMAP_ORACLE_LMTP_PORT:-18024}"
MGMT="${JMAP_ORACLE_MGMT_PORT:-18001}"
IMAP="${JMAP_ORACLE_IMAP_PORT:-18143}"
LABEL='org.oxmono.mail-oracle'

check_port() {
  case "$2" in
    ''|*[!0-9]*) echo "$1 must be a port number (0..65535)" >&2; exit 2 ;;
  esac
  if [ "$2" -gt 65535 ]; then
    echo "$1 must be a port number (0..65535)" >&2
    exit 2
  fi
}
check_port JMAP_ORACLE_HTTP_PORT "$HTTP"
check_port JMAP_ORACLE_LMTP_PORT "$LMTP"
check_port JMAP_ORACLE_MGMT_PORT "$MGMT"
check_port JMAP_ORACLE_IMAP_PORT "$IMAP"

if docker inspect "$NAME" >/dev/null 2>&1; then
  owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.mail-oracle" }}' "$NAME")
  if [ "$owner" != 'true' ]; then
    echo "refusing to use unrelated container $NAME" >&2
    exit 2
  fi
  running=$(docker inspect --format '{{ .State.Running }}' "$NAME")
  if [ "$running" != 'true' ]; then docker start "$NAME" >/dev/null; fi
else
  # Zero asks Docker to allocate a free loopback port. The actual ports are
  # read back below, so several uniquely named fixtures can run concurrently.
  # The image's unqualified upload limit is interpreted as KiB and can overflow
  # the advertised maxSizeUpload. Give the fixture a 50 MiB limit explicitly.
  docker run -d --name "$NAME" --label "$LABEL=true" \
    -p "127.0.0.1:$HTTP:8080" -p "127.0.0.1:$LMTP:8024" \
    -p "127.0.0.1:$MGMT:8001" -p "127.0.0.1:$IMAP:8143" "$IMAGE" \
    sh -c 'sed -i "s/^jmap_max_size_upload:.*/jmap_max_size_upload: 51200/" /srv/testserver/imapd.conf && exec /srv/testserver/start-server' >/dev/null
fi

mapped_port() {
  endpoint=$(docker port "$NAME" "$1/tcp") || {
    echo "$NAME does not publish container port $1" >&2
    exit 2
  }
  case "$endpoint" in
    127.0.0.1:*) printf '%s\n' "${endpoint##*:}" ;;
    *) echo "$NAME port $1 is not bound to 127.0.0.1: $endpoint" >&2; exit 2 ;;
  esac
}
HTTP=$(mapped_port 8080)
LMTP=$(mapped_port 8024)
IMAP=$(mapped_port 8143)

imap_ready() {
  python3 - "$IMAP" <<'PY'
import socket
import sys

with socket.create_connection(('127.0.0.1', int(sys.argv[1])), timeout=2) as s:
    s.settimeout(2)
    stream = s.makefile('rb')
    if not stream.readline().startswith(b'* OK'):
        raise RuntimeError('no IMAP OK greeting')
    s.sendall(b'a001 CAPABILITY\r\n')
    capabilities = []
    while True:
        line = stream.readline()
        if not line:
            raise RuntimeError('IMAP closed during CAPABILITY')
        if line.startswith(b'* CAPABILITY '):
            capabilities.append(line.upper())
        if line.startswith(b'a001 '):
            if not line.startswith(b'a001 OK') or not capabilities:
                raise RuntimeError('IMAP CAPABILITY failed')
            break
    s.sendall(b'a002 LOGIN user1 x\r\n')
    while True:
        line = stream.readline()
        if not line:
            raise RuntimeError('IMAP closed during LOGIN')
        if line.startswith(b'a002 '):
            if not line.startswith(b'a002 OK'):
                raise RuntimeError('IMAP LOGIN failed')
            break
PY
}

printf 'waiting for %s' "$NAME" >&2
for _ in $(seq 1 60); do
  if curl -fs -m 2 -u user1:x "http://127.0.0.1:$HTTP/jmap" >/dev/null 2>&1; then
    if [ "${JMAP_ORACLE_CHECK_IMAP:-0}" != 1 ] || imap_ready >/dev/null 2>&1; then
      echo ' ready' >&2
      echo "export JMAP_ORACLE_URL=http://127.0.0.1:$HTTP/.well-known/jmap"
      echo "export JMAP_ORACLE_LMTP=127.0.0.1:$LMTP"
      echo 'export IMAP_ORACLE_HOST=127.0.0.1'
      echo "export IMAP_ORACLE_PORT=$IMAP"
      echo 'export IMAP_ORACLE_TLS=plain-test'
      exit 0
    fi
  fi
  printf '.' >&2
  sleep 1
done
echo ' timed out' >&2
docker logs "$NAME" 2>&1 | tail -20 >&2
exit 1
