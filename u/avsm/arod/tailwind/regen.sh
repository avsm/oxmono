#!/bin/sh
# Regenerates ../assets/tw.css from the OCaml sources. Run after adding or
# removing utility classes in the markup, then commit the result.
set -e
cd "$(dirname "$0")"
npm install --no-audit --no-fund
npx tailwindcss -c tailwind.config.js -i input.css -o ../assets/tw.css --minify
