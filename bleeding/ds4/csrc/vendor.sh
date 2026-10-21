#!/bin/sh
# Re-vendor the DS4 engine sources in this directory from antirez/ds4.
#
#   ./csrc/vendor.sh              # vendor the pinned revision in csrc/DS4_VERSION
#   DS4_REF=main ./csrc/vendor.sh # vendor upstream main and re-pin to it
#
# Everything in csrc/ except ds4_stubs.c, this script, logging-patch.pl and
# DS4_VERSION is a verbatim copy of upstream, with one local patch on top:
# engine diagnostics are routed through an installable sink instead of going
# straight to stderr (see logging-patch.pl).  Keeping that patch in a script
# rather than as edits-in-place is what makes an upstream bump reviewable: the
# diff after a bump is upstream's changes, not ours mixed in with them.
#
# ds4_stubs.c is ours, not upstream's, and is never touched here.

set -eu

REPO=${DS4_REPO:-https://github.com/antirez/ds4}
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

if [ -n "${DS4_REF:-}" ]; then
    REF=$DS4_REF
elif [ -r "$HERE/DS4_VERSION" ]; then
    REF=$(sed -n 's/^revision: *//p' "$HERE/DS4_VERSION")
else
    echo "no DS4_REF given and no csrc/DS4_VERSION to read" >&2
    exit 1
fi

# Sources copied verbatim from upstream.  ds4.c pulls in ds4_tp.h,
# ds4_layer_pack.h and ds4_gpu_mgpu.h, so those travel with it, and
# ds4_cuda.cu pulls in ds4_iq2_tables_cuda.inc and ds4_qwen4_cuda.cuh, which
# pulls in ds4_qwen4_vision.h.  Upstream's ROCm backend is
# not vendored.
FILES="ds4.c ds4.h \
       ds4_gpu.h ds4_gpu_mgpu.h \
       ds4_metal.m \
       ds4_cuda.cu ds4_iq2_tables_cuda.inc ds4_glm53_vision_gpu.cuh \
       ds4_deepseek4_vision_gpu.cuh ds4_deepseek41_cuda.cuh \
       ds4_qwen4_cuda.cuh ds4_qwen4_vision.h \
       ds4_deepseek41_gpu.h ds4_gpu_tp.h ds4_linux_memory.h \
       ds4_engram.c ds4_engram.h \
       ds4_tool_text.h ds4_qwen4_unicode.inc \
       ds4_image.c ds4_image.h \
       ds4_ssd.c ds4_ssd.h \
       ds4_distributed.c ds4_distributed.h \
       ds4_tp.c ds4_tp.h \
       ds4_layer_pack.c ds4_layer_pack.h \
       ds4_streaming_hotlist.inc ds4_streaming_hotlist_glm52.inc \
       third_party/iris/jpeg.h third_party/iris/png.h"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

echo "fetching $REPO at $REF"
git clone --quiet "$REPO" "$TMP/ds4"
git -C "$TMP/ds4" checkout --quiet "$REF"
SHA=$(git -C "$TMP/ds4" rev-parse HEAD)
DATE=$(git -C "$TMP/ds4" log -1 --format=%ad --date=short)

# CUDA's quantised matrix kernels are separate compilation units.
FILES="$FILES $(git -C "$TMP/ds4" ls-files 'cuda/mmq/*')"

mkdir -p "$HERE/third_party/iris"
for f in $FILES; do
    [ -r "$TMP/ds4/$f" ] || { echo "upstream is missing $f" >&2; exit 1; }
    mkdir -p "$HERE/$(dirname "$f")"
    cp "$TMP/ds4/$f" "$HERE/$f"
done

rm -rf "$HERE/metal"
cp -R "$TMP/ds4/metal" "$HERE/metal"

echo "applying local patches"
perl "$HERE/logging-patch.pl" "$HERE"

cat > "$HERE/DS4_VERSION" <<EOF
# Upstream revision vendored into this directory by vendor.sh.
repository: $REPO
revision: $SHA
date: $DATE
EOF

echo "vendored $SHA ($DATE)"
