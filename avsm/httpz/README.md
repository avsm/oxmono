# httpz - Zero-Allocation HTTP/1.1 Parser for OxCaml

A high-performance HTTP/1.1 parser and serializer achieving zero heap
allocations using OxCaml's unboxed types (`int16#`, `int64#`, `char#`) and local allocations.

Will soon have io_uring on Linux.

## Features

- **Zero heap allocations**: Parser results are stack-allocated using OxCaml unboxed records and local lists
- **Unboxed integers throughout**: Uses `int16#` for offsets/counts and `int64#` for content lengths - no boxing overhead
- **Direct bigstring I/O**: Read and write directly to/from bigarray buffers
- **HTTP/1.1 support**: Methods, headers, chunked transfer encoding, keep-alive, range requests, ETags
- **Async file server included**: Production-ready static file server

## Architecture

httpz achieves zero-allocation parsing through:

1. **Unboxed records** (`#{...}`): Request, span, and header types are stack-allocated
2. **Unboxed primitives**: `int16#` for buffer offsets, `int64#` for content lengths, `char#` for byte comparisons
3. **Local lists** (`@ local`): Header list grows on the stack, not heap
4. **Span-based parsing**: Strings are referenced by offset+length into the input buffer
5. **Pre-allocated buffers**: 32KB read buffer reused across requests

## Performance

Benchmarks comparing httpz (OxCaml) vs httpe (Eio-based parser):

| Request Size | httpz (ns/op) | httpe (ns/op) | Speedup | Allocation Reduction |
|--------------|---------------|---------------|---------|---------------------|
| Small (35B)  | 154           | 159           | 1.03x   | 45x fewer words     |
| Medium (439B)| 1,150         | 1,218         | 1.06x   | 399x fewer words    |
| Large (1155B)| 2,762         | 2,912         | 1.05x   | 823x fewer words    |

**Throughput**: 6.5M requests/sec

### Detailed Timings

| Operation | Time | Heap Allocations |
|-----------|------|------------------|
| Parse minimal request (35B) | 300ns | 0 words |
| Parse simple request (4 headers) | 925ns | 0 words |
| Parse browser request (10 headers) | 3.3μs | 0 words |
| Parse 50 headers | 11.2μs | 0 words |
| Write status line | 76ns | 0 words |
| Write full response headers | 454ns | 0 words |

*True zero-allocation parsing - all values are stack-allocated via unboxed records and threaded position.*

## Installation

Requires OxCaml compiler from https://oxcaml.org/

## Static File Server

An Async-based static file server is included:

```bash
# Serve current directory on port 8080
dune exec bin/httpz_server.exe

# Serve specific directory on custom port
dune exec bin/httpz_server.exe -- -d /var/www -p 3000

# Get help
dune exec bin/httpz_server.exe -- -help
```

Features:
- Async concurrent connection handling (up to 10,000 connections)
- Zero-copy bigstring I/O
- MIME type detection
- Directory traversal protection
- Automatic `index.html` for directories

## Running Benchmarks

```bash
# Comparative benchmark (httpz vs httpe)
dune exec bench/bench_compare.exe

# Detailed httpz benchmarks with core_bench
dune exec bench/bench_httpz.exe -- -quota 2
```

## License

ISC
