#!/bin/sh
set -eu
NAME="${1:-imap-stalwart-codex}"
if ! docker inspect "$NAME" >/dev/null 2>&1; then
  echo "$NAME does not exist" >&2
  exit 1
fi
owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-stalwart" }}' "$NAME")
if [ "$owner" != true ]; then
  echo "refusing to remove unrelated container $NAME" >&2
  exit 2
fi
docker rm -f "$NAME" >/dev/null
echo 'stopped'
