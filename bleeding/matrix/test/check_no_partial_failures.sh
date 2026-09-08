#!/bin/sh
set -eu

root=${1:-.}

if rg -n \
  --glob '*.ml' \
  --glob '*.mli' \
  'assert[[:space:]]+false|\bfailwith\b' \
  "$root/lib"
then
  printf '%s\n' \
    'Library code must return a typed error or use a total representation;' \
    'assert false and failwith are not allowed under lib/.' >&2
  exit 1
fi
