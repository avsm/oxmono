#!/bin/sh
# Build and run every example in examples/ against a JMAP server, reporting
# pass/fail per example.
#
# Usage:
#   scripts/oracle-up.sh                     # start the Cyrus oracle
#   scripts/run-examples.sh [--profile NAME] [--allow-insecure] [--seed [N]] [example ...]
#
# With no example names, every tutorial step examples/<n>-<word>/ is run in
# order. Each step exits non-zero on an unexpected response, so this doubles as
# a coverage check of the client against a real server.
#
# Configuration (defaults suit the oracle from scripts/oracle-up.sh):
#   JMAP_SESSION_URL  session or well-known URL (default: $JMAP_ORACLE_URL, or
#                     http://localhost:18080/.well-known/jmap)
#   JMAP_API_KEY      "user:password" for basic auth, or a bearer token
#   JMAP_AUTH         basic | bearer (default: basic)
#   JMAP_PROFILE      saved profile (disables the default URL/key/auth above)
#   JMAP_ORACLE_LMTP  host:port used by --seed to inject test messages
#
# Against Fastmail:
#   JMAP_SESSION_URL=https://api.fastmail.com/.well-known/jmap \
#   JMAP_API_KEY=<token> JMAP_AUTH=bearer scripts/run-examples.sh
set -u

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root" || exit 1

seed=0
examples=""
allow_insecure=""
while [ $# -gt 0 ]; do
  case "$1" in
    --profile)
      [ $# -ge 2 ] && [ -n "$2" ] || { echo "--profile needs a name" >&2; exit 2; }
      JMAP_PROFILE=$2
      export JMAP_PROFILE
      shift
      ;;
    --allow-insecure) allow_insecure="--allow-insecure" ;;
    --seed)
      seed=2
      case "${2:-}" in [0-9]*) seed=$2; shift ;; esac
      ;;
    -h|--help) sed -n '2,/^set -u/{ /^set -u/d; p; }' "$0"; exit 0 ;;
    -*) echo "unknown option: $1" >&2; exit 2 ;;
    *) examples="$examples $1" ;;
  esac
  shift
done

if [ -z "${JMAP_PROFILE:-}" ]; then
  : "${JMAP_SESSION_URL:=${JMAP_ORACLE_URL:-http://localhost:18080/.well-known/jmap}}"
  : "${JMAP_API_KEY:=user1:x}"
  : "${JMAP_AUTH:=basic}"
  export JMAP_SESSION_URL JMAP_API_KEY JMAP_AUTH
fi
: "${JMAP_ORACLE_LMTP:=localhost:18024}"
: "${JMAP_ORACLE_USER:=user1}"
: "${JMAP_ORACLE_DOMAIN:=example.com}"

# The bundled oracle is deliberately cleartext and bound to loopback. Keep the
# client's insecure exception explicit, and never infer it for a remote host.
case "${JMAP_SESSION_URL:-}" in
  http://localhost|http://localhost/*|http://localhost:*|\
  http://127.0.0.1|http://127.0.0.1/*|http://127.0.0.1:*|\
  http://\[::1\]|http://\[::1\]/*|http://\[::1\]:*)
    allow_insecure="--allow-insecure"
    ;;
esac

# Some examples only have something to show when the account has mail. Seed a
# few messages over LMTP first if asked (oracle only).
seed_mail() {
  n=$1
  host=${JMAP_ORACLE_LMTP%:*}
  port=${JMAP_ORACLE_LMTP##*:}
  command -v python3 >/dev/null 2>&1 || {
    echo "--seed needs python3" >&2; return 1; }
  python3 - "$host" "$port" "$JMAP_ORACLE_USER@$JMAP_ORACLE_DOMAIN" "$n" <<'PY'
import socket, sys, time, email.utils
host, port, rcpt, n = sys.argv[1], int(sys.argv[2]), sys.argv[3], int(sys.argv[4])
def chat(f, s, sock, expect):
    if s is not None:
        sock.sendall((s + "\r\n").encode())
    while True:
        line = f.readline().decode()
        if not line:
            raise SystemExit("LMTP connection closed")
        if line[3:4] != "-":
            break
    if not line.startswith(expect):
        raise SystemExit("LMTP: expected %s, got %r" % (expect, line))
for i in range(n):
    subject = "run-examples oracle seed %d/%d" % (i + 1, n)
    body = ("This is seed message %d for the ocaml-jmap examples. It mentions "
            "the word oracle so the search example has something to find.\r\n" % (i + 1))
    msg = "\r\n".join([
        "From: Alice <alice@example.org>",
        "To: <%s>" % rcpt,
        "Subject: %s" % subject,
        "Date: %s" % email.utils.formatdate(localtime=False),
        "Message-ID: <seed-%d-%d@example.org>" % (int(time.time()), i),
        "MIME-Version: 1.0",
        "Content-Type: text/plain; charset=utf-8",
        "", body])
    with socket.create_connection((host, port), timeout=180) as sock:
        f = sock.makefile("rb")
        chat(f, None, sock, "220")
        chat(f, "LHLO localhost", sock, "250")
        chat(f, "MAIL FROM:<alice@example.org>", sock, "250")
        chat(f, "RCPT TO:<%s>" % rcpt, sock, "250")
        chat(f, "DATA", sock, "354")
        sock.sendall(msg.encode())
        chat(f, ".", sock, "250")
        sock.sendall(b"QUIT\r\n")
print("seeded %d message(s) to %s" % (n, rcpt))
PY
}

# Every example must be runnable with no arguments against a configured
# server; add a case here only if one genuinely needs more.
example_args() {
  case "$1" in
    f-push) echo "--poll 2" ;;
    o-watch) echo "--timeout 5" ;;
    *) echo "" ;;
  esac
}

# The one .ml of examples/<n>-<word>/, which is <word>.ml.
example_source() {
  set -- examples/"$1"/*.ml
  [ $# -eq 1 ] && [ -f "$1" ] && echo "$1"
}

echo "Building examples..."
dune build examples/ || exit 1

if [ "$seed" -gt 0 ]; then
  seed_mail "$seed" || exit 1
  # Give the server a moment to index the new mail.
  sleep 2
fi

# The tutorial steps (<n>-<word>) in their numbered order.
if [ -z "$examples" ]; then
  for d in examples/[0-9a-z]-*/; do
    name=$(basename "$d")
    case " $examples " in *" $name "*) continue ;; esac
    [ -n "$(example_source "$name")" ] && examples="$examples $name"
  done
fi

log=$(mktemp -d)
pass=0
fail=0
failed=""
if [ -n "${JMAP_PROFILE:-}" ]; then
  echo "Profile: $JMAP_PROFILE (explicit connection settings still take precedence)"
else
  echo "Server: $JMAP_SESSION_URL (auth $JMAP_AUTH)"
fi
echo
for name in $examples; do
  src=$(example_source "$name")
  if [ -z "$src" ]; then
    printf '%-16s SKIP  (no single .ml in examples/%s/)\n' "$name" "$name"
    continue
  fi
  exe="${src%.ml}.exe"
  insecure_arg=$allow_insecure
  case "$name" in 0-profiles) insecure_arg="" ;; esac
  start=$(date +%s)
  # shellcheck disable=SC2046
  if dune exec --no-build -- "$exe" $insecure_arg $(example_args "$name") >"$log/$name.out" 2>&1; then
    printf '%-16s PASS  (%ss, %s lines)\n' "$name" "$(( $(date +%s) - start ))" \
      "$(wc -l <"$log/$name.out" | tr -d ' ')"
    pass=$((pass + 1))
  else
    status=$?
    printf '%-16s FAIL  (exit %s)\n' "$name" "$status"
    sed 's/^/                 | /' "$log/$name.out" | tail -20
    fail=$((fail + 1))
    failed="$failed $name"
  fi
done

echo
echo "$pass passed, $fail failed. Output in $log"
[ "$fail" -eq 0 ] || { echo "failed:$failed"; exit 1; }
