The build embeds the Metal shader library from the official
`mlx-metal` 0.32.2 macOS 26 arm64 wheel. The matching MLX core in
`mlx-swift` 0.32.3 is tag `v0.32.2`, commit
`1f8e74e3f12f31365464a6867c6579f0e9b29d85`.

Source: [official PyPI release](https://pypi.org/project/mlx-metal/0.32.2/).
The pinned wheel URL is in `build_swift.sh`. Its SHA256 is
`e6abeac9ac5265830c9c1541b6f96e9be37a85c2446763a46ad466c63a3837ab`.
The extracted `mlx/lib/mlx.metallib` has SHA256
`dc59d1cceb1a5c7e578232e6e41e28e2c73c9463ac6dbc3886c3ee17ffc270ed`.
The wheel and shader are verified before use. Only the shader is linked.
No Python interpreter or Python package is installed or used at runtime.

The shader requires macOS 26 and occupies 182,351,120 bytes before executable
compression. The archive exports `ame_metallib_data` and `ame_metallib_size`.
The bridge writes these bytes to its private cache and selects that file through
`MLX.GPU.metallib` before GPU initialization. Shader discovery does not depend on
the executable location or SwiftPM resource bundles.

When upgrading MLX, check the vendored core commit, select its matching official
wheel, update both hashes, and test inference from a moved executable after
removing its build directory. Preserve the upstream MIT license below.

MIT License

Copyright © 2023 Apple Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
