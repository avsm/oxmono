#!/bin/sh
# Start an isolated Dovecot CE 2.4.5 IMAP fixture. Eval its export output.
set -eu
NAME="${1:-imap-dovecot-codex}"
IMAGE="${IMAP_DOVECOT_IMAGE:-dovecot/dovecot@sha256:c807be4fb5a97d9c3a90770569d3a6c4cbdcb36742ad41f90409cbd929166553}"
PORT="${IMAP_DOVECOT_PORT:-0}"
TLS_PORT="${IMAP_DOVECOT_TLS_PORT:-0}"
LABEL='org.oxmono.imap-dovecot'
for value in "$PORT" "$TLS_PORT"; do
  case "$value" in
    ''|*[!0-9]*) echo 'Dovecot ports must be 0..65535' >&2; exit 2 ;;
  esac
  if [ "$value" -gt 65535 ]; then
    echo 'Dovecot ports must be 0..65535' >&2; exit 2
  fi
done
if docker inspect "$NAME" >/dev/null 2>&1; then
  owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-dovecot" }}' "$NAME")
  if [ "$owner" != true ]; then
    echo "refusing to use unrelated container $NAME" >&2
    exit 2
  fi
  running=$(docker inspect --format '{{ .State.Running }}' "$NAME")
  if [ "$running" != true ]; then docker start "$NAME" >/dev/null; fi
else
  HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
  CERT_DIR=$(mktemp -d /tmp/imap-dovecot-tls.XXXXXXXX)
  trap 'rm -rf -- "$CERT_DIR"' EXIT HUP INT TERM
  mkdir "$CERT_DIR/server"
  openssl req -x509 -newkey rsa:2048 -nodes -sha256 -days 2 \
    -subj '/CN=Oxmono IMAP fixture CA' \
    -addext 'basicConstraints=critical,CA:TRUE' \
    -keyout "$CERT_DIR/ca.key" -out "$CERT_DIR/ca.crt" >/dev/null 2>&1
  openssl req -newkey rsa:2048 -nodes -sha256 \
    -subj '/CN=127.0.0.1' \
    -keyout "$CERT_DIR/server/tls.key" -out "$CERT_DIR/server.csr" >/dev/null 2>&1
  cat > "$CERT_DIR/leaf.ext" <<'EOF'
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth
subjectAltName=IP:127.0.0.1
EOF
  openssl x509 -req -in "$CERT_DIR/server.csr" -sha256 -days 2 \
    -CA "$CERT_DIR/ca.crt" -CAkey "$CERT_DIR/ca.key" -CAcreateserial \
    -extfile "$CERT_DIR/leaf.ext" -out "$CERT_DIR/server/tls.crt" >/dev/null 2>&1
  openssl req -x509 -newkey rsa:2048 -nodes -sha256 -days 2 \
    -subj '/CN=Untrusted IMAP fixture CA' \
    -addext 'basicConstraints=critical,CA:TRUE' \
    -keyout "$CERT_DIR/wrong-ca.key" -out "$CERT_DIR/wrong-ca.crt" >/dev/null 2>&1
  # The image runs as UID 1000 and must read this disposable key.
  chmod 755 "$CERT_DIR" "$CERT_DIR/server"
  chmod 644 "$CERT_DIR/server/tls.key"
  set --
  if [ "${IMAP_DOVECOT_SHARED:-0}" = 1 ]; then
    mkdir "$CERT_DIR/mail"
    cat > "$CERT_DIR/shared.conf" <<EOF
mail_uid = $(id -u)
mail_gid = $(id -g)
mail_home = /srv/shared/%{user | lower}
EOF
    set -- --user 0:0 \
      -v "$CERT_DIR/mail:/srv/shared" \
      -v "$CERT_DIR/shared.conf:/etc/dovecot/conf.d/shared.conf:ro"
  fi
  docker run -d --name "$NAME" --label "$LABEL=true" "$@" \
    --label "org.oxmono.imap-dovecot-certdir=$CERT_DIR" \
    -e 'USER_PASSWORD={PLAIN}imap-test-password' \
    -v "$HERE/cram.conf:/etc/dovecot/conf.d/cram.conf:ro" \
    -v "$CERT_DIR/server:/etc/dovecot/ssl:ro" \
    -p "127.0.0.1:$PORT:31143" -p "127.0.0.1:$TLS_PORT:31993" \
    "$IMAGE" >/dev/null
  trap - EXIT HUP INT TERM
fi
mapped=$(docker port "$NAME" 31143/tcp)
case "$mapped" in
  127.0.0.1:*) PORT="${mapped##*:}" ;;
  *) echo "fixture must publish loopback only: $mapped" >&2; exit 2 ;;
esac
mapped_tls=$(docker port "$NAME" 31993/tcp)
case "$mapped_tls" in
  127.0.0.1:*) TLS_PORT="${mapped_tls##*:}" ;;
  *) echo "TLS fixture must publish loopback only: $mapped_tls" >&2; exit 2 ;;
esac
CERT_DIR=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-dovecot-certdir" }}' "$NAME")
if [ ! -f "$CERT_DIR/ca.crt" ] || [ ! -f "$CERT_DIR/wrong-ca.crt" ]; then
  echo 'fixture TLS certificates missing; remove the container and restart' >&2
  exit 2
fi
if [ "${IMAP_DOVECOT_SHARED:-0}" = 1 ] && [ ! -d "$CERT_DIR/mail" ]; then
  echo 'existing fixture is not shared; use a fresh fixture name' >&2
  exit 2
fi
printf 'waiting for Dovecot %s' "$NAME" >&2
for _ in $(seq 1 30); do
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
    if [ -d "$CERT_DIR/mail" ]; then
      shared_path=$(docker exec "$NAME" doveadm mailbox path -u imap-test-user INBOX 2>/dev/null) || {
        docker exec "$NAME" doveadm mailbox create -u imap-test-user INBOX >&2
        shared_path=$(docker exec "$NAME" doveadm mailbox path -u imap-test-user INBOX)
      }
      case "$shared_path" in
        /srv/shared/*) echo "export IMAP_DOVECOT_SHARED_MAILDIR=$CERT_DIR/mail/${shared_path#/srv/shared/}" ;;
        *) echo "unexpected shared Maildir path: $shared_path" >&2; exit 2 ;;
      esac
    fi
    echo 'export IMAP_DOVECOT_HOST=127.0.0.1'
    echo "export IMAP_DOVECOT_PORT=$PORT"
    echo "export IMAP_DOVECOT_TLS_PORT=$TLS_PORT"
    echo "export IMAP_DOVECOT_CA_CERT=$CERT_DIR/ca.crt"
    echo "export IMAP_DOVECOT_WRONG_CA_CERT=$CERT_DIR/wrong-ca.crt"
    echo 'export IMAP_DOVECOT_USER=imap-test-user'
    echo 'export IMAP_DOVECOT_PASSWORD=imap-test-password'
    exit 0
  fi
  printf '.' >&2
  sleep 1
done
echo ' timed out' >&2
docker logs "$NAME" 2>&1 | tail -30 >&2
exit 1
