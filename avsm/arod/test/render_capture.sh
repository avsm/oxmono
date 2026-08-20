#!/bin/bash
#---------------------------------------------------------------------------
# Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
# SPDX-License-Identifier: ISC
#---------------------------------------------------------------------------
#
# The render differential.
#
# This is a tool, not a test. It serves a real bushel data directory and
# fetches every route the site answers, so it needs a corpus that is not in
# this repository and a free TCP port. Neither belongs under [dune runtest].
# Run it by hand, once before a change and once after, and compare:
#
#   avsm/arod/test/render_capture.sh /tmp/before
#   ... make the change ...
#   avsm/arod/test/render_capture.sh /tmp/after
#   diff -rq /tmp/before/r /tmp/after/r
#
# Run it from the root of the workspace. It serves under AROD_CONFIG, which
# defaults to ~/.config/arod/config.toml, with the listening port replaced by
# PORT, which defaults to 8099, so it does not collide with a server already
# running. The configuration is copied rather than written from scratch,
# because the site name, description, feed titles and well-known keys all
# reach the rendered pages: a capture taken under a different configuration
# differs from one taken under the real one on every HTML page, which would
# drown the difference the tool exists to find.
#
# What it captures, over the data directory as of August 2026:
#
#   8 listing pages as HTML             /, network, notes, links,
#                                       papers, ideas, projects, videos
#   8 listing pages as markdown         the same eight
#   8 pagination API responses          one per collection
#   6 documents                         sitemap.xml, news.xml,
#                                       notes/feed.json, perma/atom.xml,
#                                       perma/feed.json,
#                                       network/blogroll.opml
#   779 entry URLs, twice               every /papers, /notes, /ideas,
#                                       /projects and /videos path the
#                                       sitemap names, as HTML and as
#                                       markdown
#
# That is 8 + 8 + 8 + 6 + 779 * 2 = 1588 files. The entry URLs come from the
# served sitemap rather than from a list kept here, so the set grows with the
# data. A curl that fails writes a line to a [failures] file, which is absent
# when every route answered, and the run exits non-zero when it is not, so a
# script comparing two captures cannot mistake a partial one for a clean run.
#
# [pipefail] is set for the same reason. The entry URLs come out of a pipeline
# over the served sitemap, and without it a sitemap that failed to fetch or
# came back empty would leave the pipeline reporting [sort -u]'s success, the
# entry loop would run over nothing, and the run would exit 0 having captured
# about thirty files instead of 1588. Under [pipefail] the [grep] finds no
# [loc] element, exits non-zero and stops the run, which is the right answer:
# no entries means no valid capture.
#
# The noise floor is one file. Capturing the same binary twice and diffing
# reports [blogroll.opml] and nothing else, because the OPML head stamps the
# clock. Read a one-file difference as no difference, and check that the one
# file is that one. Every other route is byte-deterministic across runs.
#
# What it proved, over the three commits that froze the link graph into
# Bushel.Entry.t, precomputed note references at context build and collapsed
# the three render closures into portable handlers: 1587 of 1588 files
# byte-identical at each of the three commits, the exception being the
# blogroll clock stamp. The link graph freeze changes what iterates the
# external link list, so the links listing, the network page and the
# pagination API are the routes that would have shown an ordering change.
# They did not.

set -euo pipefail

OUT=${1:?usage: render_capture.sh OUTDIR}
CONFIG=${AROD_CONFIG:-$HOME/.config/arod/config.toml}
PORT=${PORT:-8099}
B=http://127.0.0.1:$PORT

test -f "$CONFIG" || { echo "no configuration at $CONFIG"; exit 1; }

rm -rf "$OUT"
mkdir -p "$OUT/r"

# [port] appears once, under [server], so replacing every line that starts
# with it rewrites the one that matters and nothing else.
sed "s/^port = .*/port = $PORT/" "$CONFIG" > "$OUT/config.toml"

dune build avsm/arod/bin/main.exe
./_build/default/avsm/arod/bin/main.exe serve -c "$OUT/config.toml" \
  > "$OUT/server.log" 2>&1 &
PID=$!
trap 'kill $PID 2>/dev/null || true' EXIT

for _ in $(seq 1 180); do
  if curl -sf -o /dev/null "$B/robots.txt" 2>/dev/null; then break; fi
  sleep 1
done

fetch () { # $1 path, $2 filename
  curl -sS "$B$1" -o "$OUT/r/$2" || echo "FAIL $1" >> "$OUT/failures"
}

for p in "" network notes links papers ideas projects videos; do
  fetch "/$p" "list-${p:-index}.html"
done
for m in index notes papers ideas projects videos links network; do
  fetch "/$m.md" "list-$m.md"
done
for c in entries links network notes papers ideas projects videos; do
  fetch "/api/entries?collection=$c&offset=0&limit=25" "pag-$c.json"
done

fetch /sitemap.xml sitemap.xml
fetch /news.xml news.xml
fetch /notes/feed.json feed.json
fetch /perma/atom.xml perma-atom.xml
fetch /perma/feed.json perma-feed.json
fetch /network/blogroll.opml blogroll.opml

grep -o '<loc>[^<]*</loc>' "$OUT/r/sitemap.xml" \
  | sed 's|<loc>||; s|</loc>||; s|https://anil.recoil.org||' \
  | grep -E '^/(papers|notes|ideas|projects|videos)/' \
  | sort -u > "$OUT/entry-urls.txt"

while read -r u; do
  k=$(echo "$u" | tr '/' '_')
  fetch "$u" "e$k.html"
  fetch "$u.md" "e$k.md"
done < "$OUT/entry-urls.txt"

kill $PID 2>/dev/null || true
wait $PID 2>/dev/null || true

echo "entries=$(wc -l < "$OUT/entry-urls.txt") files=$(ls "$OUT/r" | wc -l)"
test ! -e "$OUT/failures" || {
  echo "some routes failed:"
  cat "$OUT/failures"
  exit 1
}
