#!/bin/sh
# Remove only a container created by oracle-up.sh.
set -eu
NAME="${1:-jmap-oracle}"
if ! docker inspect "$NAME" >/dev/null 2>&1; then
  echo "$NAME does not exist" >&2
  exit 1
fi
owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.mail-oracle" }}' "$NAME")
if [ "$owner" != 'true' ]; then
  echo "refusing to remove unrelated container $NAME" >&2
  exit 2
fi
docker rm -f "$NAME" >/dev/null
echo 'stopped'
