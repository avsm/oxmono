#!/bin/sh
# Fail if the vendored Public Suffix List is older than 90 days.
#
# Reads the "// VERSION: YYYY-MM-DD_HH-MM-SS_UTC" line embedded in
# data/public_suffix_list.dat and compares its date against today. Run from
# anywhere; the script locates the data file relative to itself.
#
# Usage: httpz/pubsuffix/check_psl_freshness.sh

set -eu

max_age_days=90
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
dat="$here/data/public_suffix_list.dat"

if [ ! -f "$dat" ]; then
  echo "check_psl_freshness: $dat not found" >&2
  exit 2
fi

version_line=$(grep -m1 '^// VERSION: ' "$dat" || true)
if [ -z "$version_line" ]; then
  echo "check_psl_freshness: no VERSION line in $dat" >&2
  exit 2
fi

version_date=$(printf '%s' "$version_line" | sed -n 's/^\/\/ VERSION: \([0-9-]*\)_.*/\1/p')
if [ -z "$version_date" ]; then
  echo "check_psl_freshness: could not parse a date from: $version_line" >&2
  exit 2
fi

version_epoch=$(date -u -d "$version_date" +%s 2>/dev/null \
  || date -u -j -f "%Y-%m-%d" "$version_date" +%s 2>/dev/null)
if [ -z "$version_epoch" ]; then
  echo "check_psl_freshness: could not parse date '$version_date' with either GNU or BSD date" >&2
  exit 2
fi

now_epoch=$(date -u +%s)
age_days=$(( (now_epoch - version_epoch) / 86400 ))

if [ "$age_days" -gt "$max_age_days" ]; then
  echo "check_psl_freshness: vendored PSL is $age_days days old (VERSION $version_date), exceeds ${max_age_days}-day limit" >&2
  echo "Run: curl -fsSL -o $dat https://publicsuffix.org/list/public_suffix_list.dat && dune build" >&2
  exit 1
fi

echo "check_psl_freshness: vendored PSL is $age_days days old (VERSION $version_date), within ${max_age_days}-day limit"
