#!/bin/sh
# Print the environment for a running shared Cyrus oracle.
set -eu
NAME="${1:-jmap-oracle}"
if ! docker info >/dev/null 2>&1; then
  echo 'cannot reach Docker daemon to inspect oracle ports' >&2
  exit 2
fi

if docker inspect "$NAME" >/dev/null 2>&1; then
  owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.mail-oracle" }}' "$NAME")
  if [ "$owner" != 'true' ]; then
    echo "refusing to use unrelated container $NAME" >&2
    exit 2
  fi
  mapped_port() {
    endpoint=$(docker port "$NAME" "$1/tcp") || exit 2
    case "$endpoint" in
      127.0.0.1:*) printf '%s\n' "${endpoint##*:}" ;;
      *) echo "$NAME port $1 is not bound to 127.0.0.1: $endpoint" >&2; exit 2 ;;
    esac
  }
  HTTP=$(mapped_port 8080)
  LMTP=$(mapped_port 8024)
  IMAP=$(mapped_port 8143)
else
  # Preserve the old behavior when the fixture has not yet been started.
  HTTP="${JMAP_ORACLE_HTTP_PORT:-18080}"
  LMTP="${JMAP_ORACLE_LMTP_PORT:-18024}"
  IMAP="${JMAP_ORACLE_IMAP_PORT:-18143}"
fi

echo "export JMAP_ORACLE_URL=http://127.0.0.1:$HTTP/.well-known/jmap"
echo "export JMAP_ORACLE_LMTP=127.0.0.1:$LMTP"
echo 'export IMAP_ORACLE_HOST=127.0.0.1'
echo "export IMAP_ORACLE_PORT=$IMAP"
echo 'export IMAP_ORACLE_TLS=plain-test'
