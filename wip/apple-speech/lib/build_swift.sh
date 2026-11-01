#!/bin/sh
set -eu

source_file=$1
output_file=$2
sdk_path=$(xcrun --sdk macosx --show-sdk-path)
swiftc_path=$(xcrun --find swiftc)

if ! test -d "$sdk_path/System/Library/Frameworks/Speech.framework"; then
  echo "Speech.framework is missing from the selected macOS SDK" >&2
  exit 1
fi

standard_interface=$(find "$sdk_path/usr/lib/swift/Swift.swiftmodule" \
  -name '*-apple-macos.swiftinterface' -print | head -n 1)
interface_version=$(sed -n 's@^// swift-compiler-version: @@p' "$standard_interface" | head -n 1)
cache_dir=$(mktemp -d "${TMPDIR:-/tmp}/apple-speech-swift.XXXXXX")
trap 'rm -rf "$cache_dir"' EXIT HUP INT TERM

arch=$(uname -m)
case "$arch" in
  arm64 | x86_64) ;;
  *)
    echo "unsupported macOS architecture: $arch" >&2
    exit 1
    ;;
esac

CLANG_MODULE_CACHE_PATH="$cache_dir/clang" \
SWIFT_MODULE_CACHE_PATH="$cache_dir/swift" \
"$swiftc_path" \
  -interface-compiler-version "$interface_version" \
  -parse-as-library \
  -warnings-as-errors \
  -sdk "$sdk_path" \
  -target "$arch-apple-macosx26.0" \
  -emit-library \
  -static \
  -module-name AppleSpeechBridge \
  "$source_file" \
  -o "$output_file"
