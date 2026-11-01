JSON Pointer Parsing Tests (RFC 6901)

Root pointer (empty string):
  $ ./test_pointer.exe parse ""
  OK: []

RFC 6901 Section 5 examples:
  $ ./test_pointer.exe parse "/foo"
  OK: ["foo"]
  $ ./test_pointer.exe parse "/foo/0"
  OK: ["foo"; "0"]
  $ ./test_pointer.exe parse "/"
  OK: [""]
  $ ./test_pointer.exe parse "/a~1b"
  OK: ["a/b"]
  $ ./test_pointer.exe parse "/c%d"
  OK: ["c%d"]
  $ ./test_pointer.exe parse "/e^f"
  OK: ["e^f"]
  $ ./test_pointer.exe parse "/g|h"
  OK: ["g|h"]
  $ ./test_pointer.exe parse '/i\j'
  OK: ["i\\j"]
  $ ./test_pointer.exe parse '/k"l'
  OK: ["k\"l"]
  $ ./test_pointer.exe parse "/ "
  OK: [" "]
  $ ./test_pointer.exe parse "/m~0n"
  OK: ["m~n"]

Numeric tokens (interpreted as indices only when traversing arrays):
  $ ./test_pointer.exe parse "/0"
  OK: ["0"]
  $ ./test_pointer.exe parse "/1"
  OK: ["1"]
  $ ./test_pointer.exe parse "/10"
  OK: ["10"]
  $ ./test_pointer.exe parse "/123"
  OK: ["123"]

Hyphen tokens (the final token appends only in JSON Patch [add] on arrays):
  $ ./test_pointer.exe parse "/-"
  OK: ["-"]
  $ ./test_pointer.exe parse "/foo/-"
  OK: ["foo"; "-"]

Multiple levels:
  $ ./test_pointer.exe parse "/a/b/c"
  OK: ["a"; "b"; "c"]
  $ ./test_pointer.exe parse "/0/1/2"
  OK: ["0"; "1"; "2"]
  $ ./test_pointer.exe parse "/foo/0/bar/1"
  OK: ["foo"; "0"; "bar"; "1"]

Escape sequences:
  $ ./test_pointer.exe parse "/~0"
  OK: ["~"]
  $ ./test_pointer.exe parse "/~1"
  OK: ["/"]
  $ ./test_pointer.exe parse "/~0~1"
  OK: ["~/"]
  $ ./test_pointer.exe parse "/~1~0"
  OK: ["/~"]
  $ ./test_pointer.exe parse "/~01"
  OK: ["~1"]

Empty member names:
  $ ./test_pointer.exe parse "//"
  OK: [""; ""]
  $ ./test_pointer.exe parse "///"
  OK: [""; ""; ""]
  $ ./test_pointer.exe parse "/foo/"
  OK: ["foo"; ""]

Invalid: must start with /
  $ ./test_pointer.exe parse "foo"
  ERROR: Invalid JSON Pointer: must be empty or start with '/': foo
  $ ./test_pointer.exe parse "a/b"
  ERROR: Invalid JSON Pointer: must be empty or start with '/': a/b

Invalid: incomplete escape
  $ ./test_pointer.exe parse "/~"
  ERROR: Invalid JSON Pointer: incomplete escape sequence at end
  $ ./test_pointer.exe parse "/foo~"
  ERROR: Invalid JSON Pointer: incomplete escape sequence at end

Invalid: bad escape sequence
  $ ./test_pointer.exe parse "/~2"
  ERROR: Invalid JSON Pointer: invalid escape sequence ~2
  $ ./test_pointer.exe parse "/~a"
  ERROR: Invalid JSON Pointer: invalid escape sequence ~a

Leading zeros are valid tokens (but invalid as array indices at evaluation time):
  $ ./test_pointer.exe parse "/00"
  OK: ["00"]
  $ ./test_pointer.exe parse "/01"
  OK: ["01"]
  $ ./test_pointer.exe parse "/007"
  OK: ["007"]

RFC 6901 Section 4: ~01 decodes to ~1, not / (order matters):
  $ ./test_pointer.exe parse "/~01"
  OK: ["~1"]
  $ ./test_pointer.exe parse "/~10"
  OK: ["/0"]

Unicode characters (RFC 6901 specifies JSON Pointer is Unicode):
  $ ./test_pointer.exe parse "/café"
  OK: ["café"]
  $ ./test_pointer.exe parse "/日本語"
  OK: ["日本語"]
  $ ./test_pointer.exe parse "/emoji🎉"
  OK: ["emoji🎉"]

Numeric and signed tokens remain strings:
  $ ./test_pointer.exe parse "/0"
  OK: ["0"]
  $ ./test_pointer.exe parse "/-1"
  OK: ["-1"]
  $ ./test_pointer.exe parse "/+1"
  OK: ["+1"]

Very deep paths:
  $ ./test_pointer.exe parse "/a/b/c/d/e/f/g/h/i/j"
  OK: ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]

Complex escape sequences:
  $ ./test_pointer.exe parse "/~0~0"
  OK: ["~~"]
  $ ./test_pointer.exe parse "/~1~1"
  OK: ["//"]
  $ ./test_pointer.exe parse "/a~0b~1c~0d~1e"
  OK: ["a~b/c~d/e"]

Invalid: tilde at end of path:
  $ ./test_pointer.exe parse "/foo/bar~"
  ERROR: Invalid JSON Pointer: incomplete escape sequence at end
