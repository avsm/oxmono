#!/bin/sh
set -eu
NAME="${1:-imap-dovecot-codex}"
if ! docker inspect "$NAME" >/dev/null 2>&1; then
  echo "$NAME does not exist" >&2
  exit 1
fi
owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-dovecot" }}' "$NAME")
if [ "$owner" != true ]; then
  echo "refusing to remove unrelated container $NAME" >&2
  exit 2
fi
cert_dir=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-dovecot-certdir" }}' "$NAME")
docker rm -f "$NAME" >/dev/null
case "$cert_dir" in
  /tmp/imap-dovecot-tls.*)
    if [ -d "$cert_dir" ] && [ ! -L "$cert_dir" ]; then
      rm -rf -- "$cert_dir"
    fi ;;
esac
echo 'stopped'
