# bloscz — Blosc1 for OxCaml

Bindings to the system [C-Blosc1](https://www.blosc.org/) (1.21 or later).
Blosc splits a buffer into blocks, optionally shuffles the bytes or bits of
each block so that like-valued bytes of an element sit together, and hands
each block to an inner compressor such as zstd or lz4. It is the compressor
the Zarr `blosc` codec names.

Only the context interface is bound. `blosc_compress_ctx` and
`blosc_decompress_ctx` take no global lock, need no `blosc_init` and keep no
state between calls, so a call is a pure function of its arguments and safe
from any domain.

- Buffers are `Base_bigstring.t`, so the data is off heap and the stubs read
  it without copying. The caller sizes the destination and `Bloscz.compress`
  and `Bloscz.decompress` write into it.
- The runtime lock is released around calls that move more than 64 KiB, which
  lets other domains run during a large compression.
- `Bloscz.compressors ()` reports the inner compressors this build of the C
  library was compiled with. Distributions differ, so check for the one you
  need rather than assuming it.

## The C library

No opam package provides C-Blosc, so it must be installed already, with its
headers. Debian and Ubuntu call the packages `libblosc1` and `libblosc-dev`,
Homebrew and Nix call them `c-blosc`. The build links `-lblosc` and needs
`blosc.h` on the include path.

Version 1.21 is the minimum: `blosc_cbuffer_validate`, which is how
`Bloscz.validate` checks that a frame from a stranger is safe to decompress,
arrived in it. An older header fails the build with a message saying so.

C-Blosc2 installs a different header and a different library and is not what
this binds.

## Build

    dune build @bleeding/bloscz/all @bleeding/bloscz/runtest
