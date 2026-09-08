#!/bin/sh
set -eu

root=${1:-.}
client="$root/lib/matrix_client"
violations=""

# Route expansion is the only place where endpoint path arguments may be
# percent-encoded.  The five uses below are deliberately not endpoint paths:
# they encode Matrix.to permalink fragments and the permalink's [via] query.
# Keep this allowlist source-based rather than file-based, so a new use in
# room.ml cannot silently become permitted.
allowed_room_lines='"via=" ^ Uriz.pct_encode ~component:`Query_value server)
^ Uriz.pct_encode ~component:`Segment (Id.Room_alias.to_string alias)
^ Uriz.pct_encode ~component:`Segment (Id.Room_id.to_string t.room_id)
(Uriz.pct_encode ~component:`Segment (Id.Room_id.to_string t.room_id))
(Uriz.pct_encode ~component:`Segment (Id.Event_id.to_string event_id))'

matches=$(rg -n --glob '*.ml' --glob '*.mli' 'Uriz\.pct_encode' "$client" || true)

while IFS=: read -r file line source; do
  [ -n "$file" ] || continue

  case "$file" in
    "$client/route.ml")
      continue
      ;;
  esac

  normalized=$(printf '%s' "$source" \
    | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')
  allowed=false
  if [ "$file" = "$client/room.ml" ]; then
    if printf '%s\n' "$allowed_room_lines" | grep -F -x -q "$normalized"; then
      allowed=true
    fi
  fi

  if [ "$allowed" != true ]; then
    violations="${violations}${file}:${line}:${source}\n"
  fi
done <<EOF
$matches
EOF

if [ -n "$violations" ]; then
  printf '%b' "$violations" >&2
  printf '%s\n' \
    'Matrix endpoint paths must use the central Route helper;' \
    'only the five explicit Matrix.to/permalink encodes in room.ml are allowed.' >&2
  exit 1
fi
