# Conpool Test Suite

This directory contains tests for the Conpool connection pooling library.

## Test Files

### Unit Tests

- **test_simple.ml**: Basic connection pooling test with a simple echo server
- **test_localhost.ml**: Stress test with 16 concurrent endpoints and 50k requests

### Integration Tests

- **httpbin.t**: Cram test suite for HTTP client functionality against httpbin

## Running Tests

### Run all tests

```bash
dune test
```

### Run specific tests

```bash
# Run unit tests
dune exec test/test_simple.exe
dune exec test/test_localhost.exe

# Run cram tests
dune test test/httpbin.t
```

## HTTP Integration Tests (httpbin.t)

The `httpbin.t` file contains comprehensive tests for HTTP client functionality.

### Prerequisites

1. **curl** must be installed (used by cram tests)
2. **httpbin server** must be running (default: `http://localhost:8088`)

### Quick Start

Start a httpbin server on port 8088 (choose one method):

```bash
# Docker (recommended)
docker run -p 8088:80 kennethreitz/httpbin

# Go httpbin
go-httpbin -port 8088

# Python httpbin
gunicorn httpbin:app -b 127.0.0.1:8088
```

Verify httpbin is running:

```bash
curl http://localhost:8088/status/200
```

### Running httpbin Tests

With default URL (http://localhost:8088):

```bash
dune test test/httpbin.t
```

With custom httpbin URL:

```bash
HTTPBIN_URL=http://localhost:9000 dune test test/httpbin.t
HTTPBIN_URL=http://httpbin.example.com dune test test/httpbin.t
```

### Test Coverage

The httpbin.t cram test validates:

- ✅ **Basic HTTP**: GET, POST, PUT, DELETE requests
- ✅ **Headers**: Custom headers, User-Agent, Accept-Encoding
- ✅ **Cookies**: Setting, reading, deleting cookies with cookie jar
- ✅ **Authentication**: Basic auth, Bearer tokens
- ✅ **Redirects**: Following redirects, redirect chains
- ✅ **Response Codes**: 200, 404, 500, etc.
- ✅ **Content Types**: JSON, HTML, XML, images
- ✅ **Compression**: gzip, deflate
- ✅ **Uploads**: Form data, JSON, file uploads
- ✅ **Streaming**: Delayed responses, chunked data
- ✅ **Range Requests**: Partial content
- ✅ **Error Handling**: Timeouts, connection errors

### XDG Isolation

The cram test creates isolated XDG directories within the test workspace to avoid
polluting user configuration:

- `test-xdg/config` - XDG_CONFIG_HOME
- `test-xdg/cache` - XDG_CACHE_HOME
- `test-xdg/data` - XDG_DATA_HOME

Cookie jars and other test artifacts are stored here and cleaned up after tests.

### Environment Variables

The httpbin.t test respects these environment variables:

- `HTTPBIN_URL` - Base URL for httpbin server (default: http://localhost:8088)
- `XDG_CONFIG_HOME`, `XDG_CACHE_HOME`, `XDG_DATA_HOME` - Set within test for isolation

Example usage:

```bash
# Test against remote httpbin
HTTPBIN_URL=https://httpbin.org dune test test/httpbin.t

# Test against local instance on different port
HTTPBIN_URL=http://127.0.0.1:9999 dune test test/httpbin.t
```

## Continuous Integration

### GitHub Actions Example

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      httpbin:
        image: kennethreitz/httpbin
        ports:
          - 8088:80

    steps:
      - uses: actions/checkout@v2

      - name: Set up OCaml
        uses: ocaml/setup-ocaml@v2
        with:
          ocaml-compiler: 5.1.x

      - name: Install dependencies
        run: opam install . --deps-only --with-test

      - name: Build
        run: opam exec -- dune build

      - name: Run tests
        run: opam exec -- dune test
```

## Debugging Tests

### Enable debug logging

```bash
# For OCaml tests
OCAMLRUNPARAM=b dune exec test/test_simple.exe

# View verbose cram output
dune test test/httpbin.t --verbose
```

### Update cram test expectations

If the httpbin responses change or you modify the test:

```bash
dune promote test/httpbin.t
```

### Run single test section

You can extract sections of httpbin.t and run them manually:

```bash
# Example: test just the cookie handling
grep -A 20 "Cookie Handling" test/httpbin.t | bash
```

## Adding New Tests

### Adding OCaml Unit Tests

1. Create `test_myfeature.ml`
2. Create empty `test_myfeature.mli`
3. Add to `test/dune`:
   ```scheme
   (executable
    (name test_myfeature)
    (libraries conpool eio_main logs))
   ```

### Adding Cram Tests

1. Create `mytest.t` file with cram syntax
2. Add sections with `$` command prefix
3. Dune automatically discovers `.t` files
4. Run with `dune test test/mytest.t`

### Cram Test Format

```
Test Description
================

  $ command_to_run
  expected output line 1
  expected output line 2

  $ another_command
  more expected output
```

Use `(glob)` for wildcards, `(?)` for optional lines, `(re)` for regex.
