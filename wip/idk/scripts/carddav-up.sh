#!/bin/sh
# Start the Radicale server the CardDAV oracle tests run against.
# Usage: scripts/carddav-up.sh [name]   then: eval "$(scripts/carddav-env.sh)"
set -e
NAME="${1:-carddav-oracle}"
IMAGE="${CARDDAV_ORACLE_IMAGE:-tomsquest/docker-radicale:latest}"
HTTP="${CARDDAV_ORACLE_HTTP_PORT:-15232}"
USER="${CARDDAV_ORACLE_USER:-alice}"
docker rm -f "$NAME" >/dev/null 2>&1 || true
# The image authenticates with "type = none", which accepts any password but
# still needs a user name, and stores collections under /data in the container.
docker run -d --name "$NAME" -p "127.0.0.1:$HTTP:5232" "$IMAGE" >/dev/null
printf 'waiting for %s' "$NAME" >&2
for _ in $(seq 1 60); do
  if curl -fs -m 2 -u "$USER:x" -X PROPFIND -H 'Depth: 0' "http://localhost:$HTTP/" >/dev/null 2>&1; then
    echo " ready" >&2
    echo "export CARDDAV_ORACLE_URL=http://localhost:$HTTP/"
    exit 0
  fi
  printf '.' >&2
  sleep 1
done
echo " timed out" >&2
docker logs "$NAME" 2>&1 | tail -20 >&2
exit 1
