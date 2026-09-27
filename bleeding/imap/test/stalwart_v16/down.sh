#!/bin/sh
set -eu
NAME=${1:-imap-stalwart-v16-codex}
DIR=${2:-}
if docker inspect "$NAME" >/dev/null 2>&1; then
  owner=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-stalwart-v16" }}' "$NAME")
  if [ "$owner" != true ]; then
    echo "refusing to stop unrelated container $NAME" >&2
    exit 2
  fi
  labeled_dir=$(docker inspect --format '{{ index .Config.Labels "org.oxmono.imap-stalwart-v16.dir" }}' "$NAME")
  if [ -n "$DIR" ] && [ "$DIR" != "$labeled_dir" ]; then
    echo 'fixture directory does not match container label' >&2
    exit 2
  fi
  DIR=$labeled_dir
  docker stop "$NAME" >/dev/null
fi
case "$DIR" in
  /tmp/imap-stalwart-v16-*) ;;
  '') exit 0 ;;
  *) echo "refusing to remove unexpected fixture path $DIR" >&2; exit 2 ;;
esac
if [ -f "$DIR/.oxmono-imap-stalwart-v16" ]; then
  rm -rf -- "$DIR"
else
  echo "refusing to remove unmarked fixture path $DIR" >&2
  exit 2
fi
