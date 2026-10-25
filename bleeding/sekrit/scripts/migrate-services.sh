#!/bin/sh
# Migrate this user's service credentials. Preview unless --apply is supplied.
set -eu
umask 077
export PYTHONDONTWRITEBYTECODE=1
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if ! command -v uv >/dev/null 2>&1; then
    echo 'This script requires uv. Install it, then rerun this command.' >&2
    exit 1
fi
exec uv run --no-project --script "$script_dir/migrate_services.py" "$@"
