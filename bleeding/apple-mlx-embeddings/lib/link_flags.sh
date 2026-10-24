#!/bin/sh
set -eu
if [ "$(uname -s)" = Darwin ] && [ "$(uname -m)" = arm64 ]; then
  sdk=$(xcrun --sdk macosx --show-sdk-path)
  swiftc=$(xcrun --find swiftc)
  toolchain=$(dirname "$(dirname "$swiftc")")
  printf '(-lc++ -framework Accelerate -framework Foundation -framework Metal -isysroot "%s" "-L%s/usr/lib/swift" "-L%s/lib/swift/macosx")\n' "$sdk" "$sdk" "$toolchain"
else
  printf '()\n'
fi
