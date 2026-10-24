#!/bin/bash
set -euo pipefail

source_package=$(cd "$1" && pwd)
script_directory=$(cd "$(dirname "$0")" && pwd)
output_directory=$(cd "$(dirname "$2")" && pwd)
output_archive="$output_directory/$(basename "$2")"
scratch=${APPLE_MLX_SWIFT_BUILD_DIR:-"$output_directory/.apple-mlx-swift"}
mkdir -p "$scratch"
scratch=$(cd "$scratch" && pwd)

if [ "$(uname -s)" != Darwin ] || [ "$(uname -m)" != arm64 ]; then
  echo "apple-mlx-embeddings requires an Apple silicon Mac" >&2
  exit 1
fi
if [ ! -f "$source_package/Package.resolved" ]; then
  echo "apple-mlx-embeddings requires its pinned Package.resolved" >&2
  exit 1
fi
if ! mkdir "$scratch/build.lock" 2>/dev/null; then
  echo "apple-mlx-embeddings build already running in $scratch" >&2
  exit 1
fi
trap 'rmdir "$scratch/build.lock"' EXIT

mkdir -p "$scratch/package" "$scratch/assets" "$scratch/cache" \
  "$scratch/config" "$scratch/security" "$scratch/modules"
# SwiftPM may update package metadata. Keep its writable package in the scratch
# directory, including when Dune passes a read-only source tree.
rsync -a --delete --exclude .build/ "$source_package/" "$scratch/package/"

wheel_url=https://files.pythonhosted.org/packages/dd/cd/4e50bf325100e7165e13d025f264362bf0009196269f9eaf87f2c6e738a2/mlx_metal-0.32.2-py3-none-macosx_26_0_arm64.whl
wheel_hash=e6abeac9ac5265830c9c1541b6f96e9be37a85c2446763a46ad466c63a3837ab
shader_hash=dc59d1cceb1a5c7e578232e6e41e28e2c73c9463ac6dbc3886c3ee17ffc270ed
wheel="$scratch/assets/mlx-metal-0.32.2.whl"
shader="$scratch/assets/mlx.metallib"

has_hash() {
  [ -f "$1" ] && [ "$(shasum -a 256 "$1" | awk '{print $1}')" = "$2" ]
}

if ! has_hash "$wheel" "$wheel_hash"; then
  curl --fail --location --retry 3 --proto '=https' --proto-redir '=https' \
    "$wheel_url" --output "$wheel.download"
  if ! has_hash "$wheel.download" "$wheel_hash"; then
    echo "MLX shader wheel SHA256 mismatch" >&2
    exit 1
  fi
  mv "$wheel.download" "$wheel"
fi
if ! has_hash "$shader" "$shader_hash"; then
  unzip -p "$wheel" mlx/lib/mlx.metallib > "$shader.extract"
  if ! has_hash "$shader.extract" "$shader_hash"; then
    echo "MLX Metal library SHA256 mismatch" >&2
    exit 1
  fi
  mv "$shader.extract" "$shader"
fi

sdk_path=$(xcrun --sdk macosx --show-sdk-path)
standard_interface=$(find "$sdk_path/usr/lib/swift/Swift.swiftmodule" \
  -name '*-apple-macos.swiftinterface' -print | head -n 1)
interface_version=$(sed -n 's@^// swift-compiler-version: @@p' \
  "$standard_interface" | head -n 1)
export CLANG_MODULE_CACHE_PATH="$scratch/modules/clang"
export SWIFT_MODULE_CACHE_PATH="$scratch/modules/swift"

# Swift Build tries to compile the upstream Metal resources. The native engine
# builds the C++ JIT support without requiring the absent Metal compiler.
swift package --package-path "$scratch/package" \
  --scratch-path "$scratch/build" --cache-path "$scratch/cache" \
  --config-path "$scratch/config" --security-path "$scratch/security" \
  --force-resolved-versions --skip-update resolve

# Append the embedding adapter inside the pinned upstream file so it can share
# the private vision tower without copying or reimplementing the architecture.
vlm_checkout="$scratch/build/checkouts/mlx-swift-lm"
vlm_source=Libraries/MLXVLM/Models/Qwen3VL.swift
git -C "$vlm_checkout" show "HEAD:$vlm_source" > "$scratch/assets/Qwen3VL.base"
if ! has_hash "$scratch/assets/Qwen3VL.base" \
  406caff3f084e23a36f01872a62b7a43ba5dfd8c9d7193c61845911d70704056; then
  echo "Qwen3VL source differs from the reviewed embedding adapter" >&2
  exit 1
fi
cat "$scratch/assets/Qwen3VL.base" \
  "$script_directory/Qwen3VLEmbeddings.swift.inc" \
  > "$scratch/assets/Qwen3VL.swift"
if ! cmp -s "$scratch/assets/Qwen3VL.swift" "$vlm_checkout/$vlm_source"; then
  chmod u+w "$vlm_checkout/$vlm_source"
  cp "$scratch/assets/Qwen3VL.swift" "$vlm_checkout/$vlm_source"
fi

swift_args=(
  --package-path "$scratch/package"
  --scratch-path "$scratch/build"
  --cache-path "$scratch/cache"
  --config-path "$scratch/config"
  --security-path "$scratch/security"
  --build-system native
  --configuration release
  --triple arm64-apple-macosx26.0
  --force-resolved-versions
  --skip-update
  -Xswiftc -interface-compiler-version -Xswiftc "$interface_version"
)
swift build "${swift_args[@]}" --product AppleMLXEmbeddingsBridge
cmp "$source_package/Package.resolved" "$scratch/package/Package.resolved"
core_commit=$(git -C "$scratch/build/checkouts/mlx-swift/Source/Cmlx/mlx" \
  rev-parse HEAD)
if [ "$core_commit" != 1f8e74e3f12f31365464a6867c6579f0e9b29d85 ]; then
  echo "MLX core differs from the pinned shader release" >&2
  exit 1
fi
binary_directory=$(swift build "${swift_args[@]}" --show-bin-path)
swift_archive="$binary_directory/libAppleMLXEmbeddingsBridge.a"
test -f "$swift_archive"

# Assembly keeps the shader binary verbatim without generating a large C array.
cat > "$scratch/assets/metallib.s" <<'ASM'
.section __TEXT,__const
.p2align 4
.globl _ame_metallib_start
.private_extern _ame_metallib_start
_ame_metallib_start:
.incbin "mlx.metallib"
.globl _ame_metallib_end
.private_extern _ame_metallib_end
_ame_metallib_end:
ASM
cat > "$scratch/assets/metallib.c" <<'C'
#include <stddef.h>
#include <stdint.h>
extern const unsigned char ame_metallib_start[];
extern const unsigned char ame_metallib_end[];
const void *ame_metallib_data(void) { return ame_metallib_start; }
size_t ame_metallib_size(void) {
  return (size_t)((uintptr_t)ame_metallib_end - (uintptr_t)ame_metallib_start);
}
C
(
  cd "$scratch/assets"
  xcrun clang -target arm64-apple-macosx26.0 -isysroot "$sdk_path" \
    -c metallib.s -o metallib_data.o
  xcrun clang -target arm64-apple-macosx26.0 -isysroot "$sdk_path" \
    -Wall -Wextra -Werror -c metallib.c -o metallib_api.o
)
xcrun libtool -static -o "$output_archive.tmp" "$swift_archive" \
  "$scratch/assets/metallib_data.o" "$scratch/assets/metallib_api.o"
mv "$output_archive.tmp" "$output_archive"
