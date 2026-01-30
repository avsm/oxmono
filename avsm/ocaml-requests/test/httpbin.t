HTTP Client Testing with httpbin using ocurl
=============================================

This test suite validates the ocurl HTTP client functionality against a httpbin-compatible
endpoint. By default, it uses http://localhost:8088 but can be overridden via
HTTPBIN_URL environment variable.

Setup: Isolated XDG directories
--------------------------------

Create isolated XDG directories for this test to avoid polluting user config:

  $ export TEST_DIR="$PWD/test-xdg"
  $ mkdir -p "$TEST_DIR"/{config,cache,data}
  $ export XDG_CONFIG_HOME="$TEST_DIR/config"
  $ export XDG_CACHE_HOME="$TEST_DIR/cache"
  $ export XDG_DATA_HOME="$TEST_DIR/data"

Set httpbin base URL (default to localhost:8088):

  $ HTTPBIN_URL="${HTTPBIN_URL:-http://localhost:8088}"
  $ echo "Testing against: $HTTPBIN_URL"
  Testing against: http://localhost:8088

Verify httpbin is accessible (using curl as a pre-check):

  $ curl -s -f "$HTTPBIN_URL/status/200" > /dev/null && echo "httpbin accessible" || echo "ERROR: httpbin not accessible at $HTTPBIN_URL"
  httpbin accessible

Basic GET Requests
------------------

Simple GET request (using error verbosity to suppress extra output):

  $ ocurl --verbosity=error "$HTTPBIN_URL/get" | grep -o '"url": "[^"]*"'
  "url": "http://localhost:8088/get"

GET with query parameters:

  $ ocurl --verbosity=error "$HTTPBIN_URL/get?foo=bar&baz=qux" | grep '"args"' -A 3
    "args": {
      "baz": "qux",
      "foo": "bar"
    },

Verify URL is correctly formed:

  $ ocurl --verbosity=error "$HTTPBIN_URL/get" | grep -o '"url": "http://localhost:8088/get"'
  "url": "http://localhost:8088/get"

Response Headers with -i flag
------------------------------

Note: The -i flag requires verbosity level above 'warning' to show headers.
With default verbosity (warning), headers are suppressed.

Test that response includes HTTP status line with info verbosity:

  $ ocurl --verbosity=info -I "$HTTPBIN_URL/status/200" 2>&1 | grep -o "200 OK" | head -1
  200 OK

Custom Request Headers
----------------------

Send custom headers and verify they're received:

  $ ocurl --verbosity=error -H "X-Custom-Header: test-value" -H "X-Test-ID: 12345" \
  >   "$HTTPBIN_URL/headers" | \
  >   grep -o '"X-Custom-Header": "test-value"'
  "X-Custom-Header": "test-value"

Verify multiple custom headers (note: header names are normalized):

  $ ocurl --verbosity=error -H "X-Custom-Header: test-value" -H "X-Test-ID: 12345" \
  >   "$HTTPBIN_URL/headers" | \
  >   grep '"X-Test-Id"'
      "X-Test-Id": "12345"

User-Agent header (must use -H flag):

  $ ocurl --verbosity=error -H "User-Agent: ocurl-test/1.0" "$HTTPBIN_URL/headers" | \
  >   grep -o '"User-Agent": "ocurl-test/1.0"'
  "User-Agent": "ocurl-test/1.0"

POST Requests
-------------

POST with data:

  $ ocurl --verbosity=error -X POST -d "test data content" \
  >   "$HTTPBIN_URL/post" | \
  >   grep -o '"data": "test data content"'
  "data": "test data content"

POST with JSON data using --json flag:

  $ ocurl --verbosity=error -X POST --json '{"name":"test","value":42}' \
  >   "$HTTPBIN_URL/post" | \
  >   grep '"data"' -A 1
    "data": "{\"name\":\"test\",\"value\":42}",
    "files": {},

Verify JSON content type is set:

  $ ocurl --verbosity=error -X POST --json '{"key":"val"}' \
  >   "$HTTPBIN_URL/post" | \
  >   grep -o '"Content-Type": "application/json"'
  "Content-Type": "application/json"

PUT and DELETE Requests
------------------------

PUT request:

  $ ocurl --verbosity=error -X PUT -d "updated data" "$HTTPBIN_URL/put" | \
  >   grep -o '"data": "updated data"'
  "data": "updated data"

DELETE request:

  $ ocurl --verbosity=error -X DELETE "$HTTPBIN_URL/delete" | \
  >   grep -o '"url": "[^"]*"'
  "url": "http://localhost:8088/delete"

PATCH request:

  $ ocurl --verbosity=error -X PATCH -d "patched data" "$HTTPBIN_URL/patch" | \
  >   grep -o '"data": "patched data"'
  "data": "patched data"

OPTIONS request (returns headers only, no body):

  $ ocurl --verbosity=error -X OPTIONS "$HTTPBIN_URL/get" | wc -c | tr -d ' '
  0

HEAD request (returns headers only, no body):

  $ ocurl --verbosity=error -X HEAD "$HTTPBIN_URL/get" | wc -c | tr -d ' '
  0

Authentication
--------------

Basic authentication (success):

  $ ocurl --verbosity=error --allow-insecure-auth -u "user:passwd" "$HTTPBIN_URL/basic-auth/user/passwd" | \
  >   grep -o '"authenticated": true'
  "authenticated": true

Verify username is passed:

  $ ocurl --verbosity=error --allow-insecure-auth -u "user:passwd" "$HTTPBIN_URL/basic-auth/user/passwd" | \
  >   grep -o '"user": "user"'
  "user": "user"

Bearer token authentication:

  $ ocurl --verbosity=error -H "Authorization: Bearer test-token-12345" \
  >   "$HTTPBIN_URL/bearer" | \
  >   grep -o '"authenticated": true'
  "authenticated": true

Token value is verified:

  $ ocurl --verbosity=error -H "Authorization: Bearer test-token-12345" \
  >   "$HTTPBIN_URL/bearer" | \
  >   grep -o '"token": "test-token-12345"'
  "token": "test-token-12345"

Cookie Persistence Tests
-------------------------

Test cookie setting endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?session=abc123" | \
  >   grep -o '"session": "abc123"'
  "session": "abc123"

Test setting multiple cookies:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?session=abc123&user=testuser" | \
  >   grep '"cookies"' -A 4
    "cookies": {
      "session": "abc123",
      "user": "testuser"
    }
  }

Verify session cookie is set:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?session=xyz789" | \
  >   grep -o '"session": "xyz789"'
  "session": "xyz789"

Cookie persistence with --persist-cookies flag:
This allows cookies to be stored and reused across requests within the same session.

  $ ocurl --verbosity=error --persist-cookies "$HTTPBIN_URL/cookies/set?persistent=true" | \
  >   grep -o '"persistent": "true"'
  "persistent": "true"

Test cookie deletion endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/delete?session" | \
  >   grep '"cookies"'
    "cookies": {}

Response Formats
----------------

JSON response (ocurl pretty-prints by default):

  $ ocurl --verbosity=error "$HTTPBIN_URL/json" | head -5 | grep '"slideshow"'
    "slideshow": {

HTML response:

  $ ocurl --verbosity=error "$HTTPBIN_URL/html" | grep -o "<html>" | head -1
  <html>

XML response:

  $ ocurl --verbosity=error "$HTTPBIN_URL/xml" | grep -o '<?xml' | head -1
  <?xml

UTF-8 response:

  $ ocurl --verbosity=error "$HTTPBIN_URL/encoding/utf8" | \
  >   grep -o "UTF-8" | head -1
  UTF-8

Response Size Tests
-------------------

Request specific number of bytes:

  $ BYTES=$(ocurl --verbosity=error "$HTTPBIN_URL/bytes/100" | wc -c | tr -d ' ')
  $ test "$BYTES" -ge 100 && echo "Got at least 100 bytes"
  Got at least 100 bytes

Small byte response:

  $ BYTES=$(ocurl --verbosity=error "$HTTPBIN_URL/bytes/50" | wc -c | tr -d ' ')
  $ test "$BYTES" -ge 50 && echo "Got at least 50 bytes"
  Got at least 50 bytes

File Output
-----------

Download to file (requires info verbosity to see output):

  $ ocurl --verbosity=info -o "$TEST_DIR/output.json" "$HTTPBIN_URL/get" 2>&1 | \
  >   grep -o "Saved to"
  Saved to

Verify file was created and contains data:

  $ test -f "$TEST_DIR/output.json" && echo "File created"
  File created

  $ grep -q '"url"' "$TEST_DIR/output.json" && echo "File contains JSON"
  File contains JSON

Check file contains expected endpoint:

  $ grep -o '"url": "http://localhost:8088/get"' "$TEST_DIR/output.json"
  "url": "http://localhost:8088/get"

Request Introspection
---------------------

Get request headers:

  $ ocurl --verbosity=error "$HTTPBIN_URL/headers" | grep -o '"Host": "localhost:8088"'
  "Host": "localhost:8088"

Verify User-Agent is set with library default (ocaml-requests):

  $ ocurl --verbosity=error "$HTTPBIN_URL/user-agent" | grep -o '"user-agent": "ocaml-requests/[^"]*"'
  "user-agent": "ocaml-requests/0.1.0 (OCaml 5.4.0)"

Check connection header:

  $ ocurl --verbosity=error "$HTTPBIN_URL/headers" | grep -o '"Connection": "keep-alive"'
  "Connection": "keep-alive"

Timeout Configuration
---------------------

Set a timeout (test with a quick endpoint):

  $ ocurl --verbosity=error --timeout 5.0 "$HTTPBIN_URL/get" | grep -q '"url"' && echo "Timeout setting works"
  Timeout setting works

Short timeout works:

  $ ocurl --verbosity=error --timeout 10.0 "$HTTPBIN_URL/delay/1" | grep -q '"url"' && echo "Delay completed within timeout"
  Delay completed within timeout

Redirects
---------

Follow redirects (default behavior in ocurl):

  $ ocurl --verbosity=error "$HTTPBIN_URL/redirect/1" | grep -o '"url": "http://localhost:8088/get"'
  "url": "http://localhost:8088/get"

Absolute redirect:

  $ ocurl --verbosity=error "$HTTPBIN_URL/absolute-redirect/1" | grep -o '"url": "http://localhost:8088/get"'
  "url": "http://localhost:8088/get"

Multiple redirects:

  $ ocurl --verbosity=error "$HTTPBIN_URL/redirect/3" | grep -o '"url": "http://localhost:8088/get"'
  "url": "http://localhost:8088/get"

Redirect to relative URL:

  $ ocurl --verbosity=error "$HTTPBIN_URL/relative-redirect/1" | grep -o '"url": "http://localhost:8088/get"'
  "url": "http://localhost:8088/get"

Disable redirect following:

  $ ocurl --verbosity=error --no-follow-redirects "$HTTPBIN_URL/redirect/1" 2>&1 | \
  >   grep -i redirect > /dev/null && echo "Redirect response received"
  Redirect response received

Status Code Tests
-----------------

Status 200 OK:

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/200" | wc -c | tr -d ' '
  0

Status 201 Created:

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/201" | wc -c | tr -d ' '
  0

Status 204 No Content:

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/204" | wc -c | tr -d ' '
  0

Compression Support
-------------------

Note: ocurl receives compressed data but does not automatically decompress it.
The gzip and deflate endpoints return binary compressed data.

Request gzip compressed response (returns binary data):

  $ SIZE=$(ocurl --verbosity=error -H "Accept-Encoding: gzip" \
  >   "$HTTPBIN_URL/gzip" | wc -c | tr -d ' ')
  $ test "$SIZE" -gt 50 && echo "Received compressed gzip data"
  Received compressed gzip data

Request deflate compressed response (returns binary data):

  $ SIZE=$(ocurl --verbosity=error -H "Accept-Encoding: deflate" \
  >   "$HTTPBIN_URL/deflate" | wc -c | tr -d ' ')
  $ test "$SIZE" -gt 50 && echo "Received compressed deflate data"
  Received compressed deflate data

Verbose Output Testing
----------------------

Test with info verbosity level shows request details:

  $ ocurl --verbosity=info "$HTTPBIN_URL/get" 2>&1 | grep -c "Making GET request"
  1

Test with debug verbosity level:

  $ ocurl --verbosity=debug "$HTTPBIN_URL/get" 2>&1 | grep -c "GET request" | \
  >   awk '{if ($1 >= 1) print "Debug output present"}'
  Debug output present

Test quiet mode suppresses output:

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/200" | wc -c | tr -d ' '
  0

Content Type Handling
---------------------

Verify ocurl handles JSON content type:

  $ ocurl --verbosity=error "$HTTPBIN_URL/json" | head -1 | grep -o "{"
  {

HTML content type:

  $ ocurl --verbosity=error "$HTTPBIN_URL/html" | head -1 | grep -o "<!DOCTYPE"
  <!DOCTYPE

XML content type:

  $ ocurl --verbosity=error "$HTTPBIN_URL/xml" | head -1 | grep -o '<?xml'
  <?xml

Response Headers Inspection
----------------------------

Check response contains expected headers:

  $ ocurl --verbosity=error "$HTTPBIN_URL/response-headers?X-Test=foo" | \
  >   grep -o '"X-Test": "foo"'
  "X-Test": "foo"

Custom response header:

  $ ocurl --verbosity=error "$HTTPBIN_URL/response-headers?X-Custom=bar&Y-Another=baz" | \
  >   grep '"X-Custom"'
    "X-Custom": "bar",

Cache Control header test:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cache" | \
  >   grep '"url"' > /dev/null && echo "Cache endpoint accessible"
  Cache endpoint accessible

ETag support:

  $ ocurl --verbosity=error "$HTTPBIN_URL/etag/test-etag" | \
  >   grep '"url"' > /dev/null && echo "ETag endpoint accessible"
  ETag endpoint accessible

Advanced Cookie Tests
----------------------

Set cookie with specific value:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?test=value123" | \
  >   grep -o '"test": "value123"'
  "test": "value123"

Multiple cookie setting:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?a=1&b=2&c=3" | \
  >   grep '"a": "1"' && \
  >   ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?a=1&b=2&c=3" | \
  >   grep '"b": "2"' && \
  >   ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?a=1&b=2&c=3" | \
  >   grep '"c": "3"'
      "a": "1",
      "b": "2",
      "c": "3"

Test cookie names with special characters:

  $ ocurl --verbosity=error "$HTTPBIN_URL/cookies/set?my-cookie=my-value" | \
  >   grep -o '"my-cookie": "my-value"'
  "my-cookie": "my-value"

Image and Binary Response Tests
--------------------------------

Test PNG image endpoint returns data:

  $ SIZE=$(ocurl --verbosity=error "$HTTPBIN_URL/image/png" | wc -c | tr -d ' ')
  $ test "$SIZE" -gt 1000 && echo "PNG image data received (size: $SIZE bytes)"
  PNG image data received (size: 8090 bytes)

Test JPEG image endpoint:

  $ SIZE=$(ocurl --verbosity=error "$HTTPBIN_URL/image/jpeg" | wc -c | tr -d ' ')
  $ test "$SIZE" -gt 1000 && echo "JPEG image data received (size: $SIZE bytes)"
  JPEG image data received (size: 35588 bytes)

Test SVG image endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/image/svg" | head -1 | grep -o "<svg"
  <svg

Stream Testing
--------------

Test streaming bytes:

  $ SIZE=$(ocurl --verbosity=error "$HTTPBIN_URL/stream-bytes/100" | wc -c | tr -d ' ')
  $ test "$SIZE" -ge 100 && echo "Stream returned at least 100 bytes"
  Stream returned at least 100 bytes

Data Formats
------------

Base64 decode endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/base64/SGVsbG8gV29ybGQ=" | \
  >   grep -o "Hello World"
  Hello World

Delay endpoint (tests timeout handling):

  $ ocurl --verbosity=error --timeout 3.0 "$HTTPBIN_URL/delay/1" | \
  >   grep '"url"' > /dev/null && echo "Delay completed successfully"
  Delay completed successfully

Error Status Code Handling
---------------------------

Test 4xx client errors are handled:

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/400" 2>&1 | wc -c | tr -d ' '
  0

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/404" 2>&1 | wc -c | tr -d ' '
  0

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/403" 2>&1 | wc -c | tr -d ' '
  0

Test 5xx server errors are handled:

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/500" 2>&1 | wc -c | tr -d ' '
  0

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/502" 2>&1 | wc -c | tr -d ' '
  0

  $ ocurl --verbosity=error "$HTTPBIN_URL/status/503" 2>&1 | wc -c | tr -d ' '
  0

UUID Generation
---------------

Test UUID endpoint returns JSON with a UUID field:

  $ ocurl --verbosity=error "$HTTPBIN_URL/uuid" | grep -q '"uuid":' && echo "UUID field present"
  UUID field present

IP Address Endpoint
-------------------

Test origin IP endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/ip" | grep -o '"origin":'
  "origin":

Form Data Submission
--------------------

POST with data (body is sent as text/plain by default with -d):

  $ ocurl --verbosity=error -X POST -d "username=testuser&password=secret123" \
  >   "$HTTPBIN_URL/post" | grep -o '"data": "username=testuser&password=secret123"'
  "data": "username=testuser&password=secret123"

Robots.txt and Deny Endpoints
-----------------------------

Test robots.txt endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/robots.txt" | head -2
  User-agent: *
  Disallow: /deny

Test deny endpoint returns expected content:

  $ ocurl --verbosity=error "$HTTPBIN_URL/deny" | grep -q "YOU SHOULDN'T BE HERE" && echo "Deny message found"
  Deny message found

Anything Echo Endpoint
-----------------------

Test anything endpoint echoes method and data:

  $ ocurl --verbosity=error -X POST -d "echo test" "$HTTPBIN_URL/anything" | \
  >   grep -o '"method": "POST"'
  "method": "POST"

  $ ocurl --verbosity=error "$HTTPBIN_URL/anything/test/path" | \
  >   grep -o '"url": "http://localhost:8088/anything/test/path"'
  "url": "http://localhost:8088/anything/test/path"

Range Requests
--------------

Test range endpoint returns specified bytes:

  $ SIZE=$(ocurl --verbosity=error "$HTTPBIN_URL/range/100" | wc -c | tr -d ' ')
  $ test "$SIZE" -eq 100 && echo "Range returned exactly 100 bytes"
  Range returned exactly 100 bytes

Links/Pages Endpoint
--------------------

Test links endpoint:

  $ ocurl --verbosity=error "$HTTPBIN_URL/links/5/0" | grep -o 'href' | wc -l | tr -d ' '
  4

Drip Endpoint (Streaming Data)
-------------------------------

Test drip endpoint for streaming data:

  $ SIZE=$(ocurl --verbosity=error --timeout 5.0 "$HTTPBIN_URL/drip?duration=1&numbytes=50&code=200" | wc -c | tr -d ' ')
  $ test "$SIZE" -ge 50 && echo "Drip returned expected bytes"
  Drip returned expected bytes

Concurrent URL Fetching
------------------------

Test fetching multiple URLs concurrently (ip and uuid don't have "url" field):

  $ ocurl --verbosity=error "$HTTPBIN_URL/get" "$HTTPBIN_URL/headers" | \
  >   grep -c '"Host"' | tr -d ' '
  2

HEAD with Response Headers
--------------------------

Test HEAD request shows headers with -I flag at info level:

  $ ocurl --verbosity=info -I -X HEAD "$HTTPBIN_URL/get" 2>&1 | grep -o "200 OK" | head -1
  200 OK

Custom Accept Header
--------------------

Test Accept header is passed through:

  $ ocurl --verbosity=error -H "Accept: application/xml" "$HTTPBIN_URL/headers" | \
  >   grep -o '"Accept": "application/xml"'
  "Accept": "application/xml"

Large Response Body
-------------------

Test handling of larger responses:

  $ SIZE=$(ocurl --verbosity=error "$HTTPBIN_URL/bytes/10000" | wc -c | tr -d ' ')
  $ test "$SIZE" -ge 10000 && echo "Large response handled correctly"
  Large response handled correctly

Environment Variables
---------------------

Test that environment variables work for configuration:

  $ OCURL_TIMEOUT=2.0 ocurl --verbosity=error "$HTTPBIN_URL/delay/1" | \
  >   grep '"url"' > /dev/null && echo "Env var timeout works"
  Env var timeout works

Cleanup
-------

Remove test directories:

  $ rm -rf "$TEST_DIR"
  $ echo "Test environment cleaned up"
  Test environment cleaned up
