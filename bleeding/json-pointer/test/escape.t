Token Escaping Tests

Escape tilde:
  $ ./test_pointer.exe escape "~"
  ~0
  $ ./test_pointer.exe escape "~0"
  ~00
  $ ./test_pointer.exe escape "~1"
  ~01

Escape slash:
  $ ./test_pointer.exe escape "/"
  ~1
  $ ./test_pointer.exe escape "a/b"
  a~1b
  $ ./test_pointer.exe escape "/foo/bar"
  ~1foo~1bar

Combined:
  $ ./test_pointer.exe escape "~/"
  ~0~1
  $ ./test_pointer.exe escape "/~"
  ~1~0
  $ ./test_pointer.exe escape "a~b/c"
  a~0b~1c

No escaping needed:
  $ ./test_pointer.exe escape "foo"
  foo
  $ ./test_pointer.exe escape "hello world"
  hello world

Unescape:
  $ ./test_pointer.exe unescape "~0"
  OK: ~
  $ ./test_pointer.exe unescape "~1"
  OK: /
  $ ./test_pointer.exe unescape "~0~1"
  OK: ~/
  $ ./test_pointer.exe unescape "~1~0"
  OK: /~
  $ ./test_pointer.exe unescape "a~0b~1c"
  OK: a~b/c
  $ ./test_pointer.exe unescape "foo"
  OK: foo

Unescape errors:
  $ ./test_pointer.exe unescape "~"
  ERROR: Invalid JSON Pointer: incomplete escape sequence at end
  $ ./test_pointer.exe unescape "~2"
  ERROR: Invalid JSON Pointer: invalid escape sequence ~2
  $ ./test_pointer.exe unescape "foo~"
  ERROR: Invalid JSON Pointer: incomplete escape sequence at end
