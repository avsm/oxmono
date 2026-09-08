#!/bin/sh
# Start the Cyrus IMAP JMAP test server used as a test oracle.
# Usage: scripts/oracle-up.sh [name]   then: eval "$(scripts/oracle-env.sh)"
set -e
NAME="${1:-jmap-oracle}"
IMAGE="${JMAP_ORACLE_IMAGE:-ghcr.io/cyrusimap/cyrus-docker-test-server:bookworm}"
HTTP="${JMAP_ORACLE_HTTP_PORT:-18080}"
LMTP="${JMAP_ORACLE_LMTP_PORT:-18024}"
MGMT="${JMAP_ORACLE_MGMT_PORT:-18001}"
docker rm -f "$NAME" >/dev/null 2>&1 || true
# The image's unqualified upload limit is interpreted as KiB and can overflow
# the advertised maxSizeUpload. Give the fixture a 50 MiB limit explicitly.
docker run -d --name "$NAME" -p "127.0.0.1:$HTTP:8080" -p "127.0.0.1:$LMTP:8024" -p "127.0.0.1:$MGMT:8001" "$IMAGE" \
  sh -c 'sed -i "s/^jmap_max_size_upload:.*/jmap_max_size_upload: 51200/" /srv/testserver/imapd.conf && exec /srv/testserver/start-server' >/dev/null
printf 'waiting for %s' "$NAME" >&2
for _ in $(seq 1 60); do
  if curl -fs -m 2 -u user1:x "http://localhost:$HTTP/jmap" >/dev/null 2>&1; then
    echo " ready" >&2
    echo "export JMAP_ORACLE_URL=http://localhost:$HTTP/.well-known/jmap"
    echo "export JMAP_ORACLE_LMTP=localhost:$LMTP"
    exit 0
  fi
  printf '.' >&2
  sleep 1
done
echo " timed out" >&2
docker logs "$NAME" 2>&1 | tail -20 >&2
exit 1
