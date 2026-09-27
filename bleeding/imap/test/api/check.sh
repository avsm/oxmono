#!/usr/bin/env bash
set -euo pipefail
compiler=$1
interface=$2
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
cp "$interface" "$scratch/imap_eio.cmi"
cat > "$scratch/check.ml" <<'ML'
let connect = Imap_eio.Client.connect
let with_mailbox = Imap_eio.Client.with_mailbox
let search = Imap_eio.Selected.uid_search
let endpoint = Imap_eio.Transport.v
let credentials = Imap_eio.Auth.password
ML
"$compiler" -I "$scratch" -c "$scratch/check.ml"
for hidden in \
  Selected.create Selected.invalidate \
  Transport.of_flow Transport.connect Transport.upgrade Transport.compress_deflate \
  Auth.resolve_password Auth.resolve_token Auth.plain_response \
  Auth.cram_md5_response Auth.oauthbearer_response; do
  printf 'let forbidden = Imap_eio.%s\n' "$hidden" > "$scratch/check.ml"
  if "$compiler" -I "$scratch" -c "$scratch/check.ml" > "$scratch/error" 2>&1; then
    printf 'Public interface exposes %s\n' "$hidden" >&2
    exit 1
  fi
  if ! grep -F "Unbound value" "$scratch/error" > /dev/null; then
    cat "$scratch/error" >&2
    exit 1
  fi
done
