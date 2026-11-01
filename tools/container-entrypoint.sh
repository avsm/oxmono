#!/bin/sh
set -eu
if [ "$#" -eq 0 ]; then
  printf 'Available commands:\n'
  ls /opt/oxmono/bin
  exit 0
fi
exec "$@"
