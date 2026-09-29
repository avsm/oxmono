#!/usr/bin/env bash
# Callbacks that Sqlite3 stores in a handle must be portable. Each case
# below registers a closure over a module-level ref, or a mutable
# accumulator, and must fail to compile with a portability error. The
# control case must compile, so a failure is due to the ref.
set -euo pipefail
compiler=$1
interface=$2
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
cp "$interface" "$scratch/sqlite3.cmi"
prelude='let db = Sqlite3.db_open ":memory:"
let r = ref 0
let null _ = Sqlite3.Data.NULL'
compile() {
  printf '%s\nlet () = %s\n' "$prelude" "$1" > "$scratch/check.ml"
  "$compiler" -I "$scratch" -c "$scratch/check.ml" > "$scratch/error" 2>&1
}
compile 'Sqlite3.create_fun1 db "f" (fun x -> x)' || {
  cat "$scratch/error" >&2
  exit 1
}
reject() {
  if compile "$2"; then
    printf 'Sqlite3 accepts a nonportable %s\n' "$1" >&2
    exit 1
  fi
  if ! grep -F "$3" "$scratch/error" > /dev/null; then
    cat "$scratch/error" >&2
    exit 1
  fi
}
reject 'create_fun1 closure' \
  'Sqlite3.create_fun1 db "f" (fun x -> incr r; x)' \
  'which is expected to be "portable"'
reject 'aggregate inverse' \
  'Sqlite3.Aggregate.create_fun1 db "g" ~init:0 ~step:(fun a _ -> a)
     ~inverse:(fun a _ -> incr r; a) ~final:null' \
  'which is expected to be "portable"'
reject 'aggregate accumulator' \
  'Sqlite3.Aggregate.create_fun0 db "g" ~init:r ~step:(fun a -> a)
     ~final:null' \
  'value mod portable contended'
reject 'collation' \
  'Sqlite3.create_collation db "c" (fun a b -> incr r; compare a b)' \
  'which is expected to be "portable"'
