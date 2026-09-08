JSON Pointer Evaluation Tests (RFC 6901 Section 5)

Using the RFC 6901 example document:
  $ cat data/rfc6901_example.json
  {
    "foo": ["bar", "baz"],
    "": 0,
    "a/b": 1,
    "c%d": 2,
    "e^f": 3,
    "g|h": 4,
    "i\\j": 5,
    "k\"l": 6,
    " ": 7,
    "m~n": 8
  }

Root pointer returns whole document:
  $ ./test_pointer.exe eval data/rfc6901_example.json ""
  OK: {"foo":["bar","baz"],"":0,"a/b":1,"c%d":2,"e^f":3,"g|h":4,"i\\j":5,"k\"l":6," ":7,"m~n":8}

RFC 6901 examples:
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo"
  OK: ["bar","baz"]
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/0"
  OK: "bar"
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/1"
  OK: "baz"
  $ ./test_pointer.exe eval data/rfc6901_example.json "/"
  OK: 0
  $ ./test_pointer.exe eval data/rfc6901_example.json "/a~1b"
  OK: 1
  $ ./test_pointer.exe eval data/rfc6901_example.json "/c%d"
  OK: 2
  $ ./test_pointer.exe eval data/rfc6901_example.json "/e^f"
  OK: 3
  $ ./test_pointer.exe eval data/rfc6901_example.json "/g|h"
  OK: 4
  $ ./test_pointer.exe eval data/rfc6901_example.json '/i\j'
  OK: 5
  $ ./test_pointer.exe eval data/rfc6901_example.json '/k"l'
  OK: 6
  $ ./test_pointer.exe eval data/rfc6901_example.json "/ "
  OK: 7
  $ ./test_pointer.exe eval data/rfc6901_example.json "/m~0n"
  OK: 8

Error: nonexistent member:
  $ ./test_pointer.exe eval data/rfc6901_example.json "/nonexistent"
  ERROR: JSON Pointer: member 'nonexistent' not found
  File "-":

Error: a duplicate referenced member is undefined by RFC 6901:
  $ ./test_pointer.exe duplicate get
  ERROR: JSON Pointer: member 'duplicate' is not unique

Error: index out of bounds:
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/2"
  ERROR: JSON Pointer: index 2 out of bounds (array has 2 elements)
  File "-":
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/99"
  ERROR: JSON Pointer: index 99 out of bounds (array has 2 elements)
  File "-":

Error: invalid array index (not a valid integer):
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/bar"
  ERROR: JSON Pointer: invalid array index 'bar'
  File "-":
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/999999999999999999999999999999999999"
  ERROR: JSON Pointer: array index is too large: 999999999999999999999999999999999999
  File "-":

Error: end marker not allowed in get:
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/-"
  ERROR: JSON Pointer: '-' refers to a nonexistent array element
  File "-":

Error: navigating through primitive (string):
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/0/0"
  ERROR: JSON Pointer: cannot index into string with '0'
  File "-":
  $ ./test_pointer.exe eval data/rfc6901_example.json "/foo/0/bar"
  ERROR: JSON Pointer: cannot index into string with 'bar'
  File "-":

Nested evaluation with deep nesting:
  $ cat data/nested.json
  {
    "a": {
      "b": {
        "c": {
          "d": "deep value"
        }
      }
    },
    "arr": [[1, 2], [3, 4]],
    "mixed": { "list": [{"x": 1}, {"y": 2}] }
  }
  $ ./test_pointer.exe eval data/nested.json "/a/b/c/d"
  OK: "deep value"
  $ ./test_pointer.exe eval data/nested.json "/arr/0/1"
  OK: 2
  $ ./test_pointer.exe eval data/nested.json "/arr/1/0"
  OK: 3
  $ ./test_pointer.exe eval data/nested.json "/mixed/list/0/x"
  OK: 1
  $ ./test_pointer.exe eval data/nested.json "/mixed/list/1/y"
  OK: 2

Null value handling:
  $ cat data/nulls.json
  {
    "null": null,
    "nested": { "null": null }
  }
  $ ./test_pointer.exe eval data/nulls.json "/null"
  OK: null
  $ ./test_pointer.exe eval data/nulls.json "/nested/null"
  OK: null

Boolean values:
  $ cat data/booleans.json
  {
    "true": true,
    "false": false
  }
  $ ./test_pointer.exe eval data/booleans.json "/true"
  OK: true
  $ ./test_pointer.exe eval data/booleans.json "/false"
  OK: false

Empty key access:
  $ ./test_pointer.exe eval data/rfc6901_example.json "/"
  OK: 0

Unicode member access:
  $ cat data/unicode.json
  {
    "café": "coffee",
    "日本語": "japanese",
    "🎉": "party"
  }
  $ ./test_pointer.exe eval data/unicode.json "/café"
  OK: "coffee"
  $ ./test_pointer.exe eval data/unicode.json "/日本語"
  OK: "japanese"
  $ ./test_pointer.exe eval data/unicode.json "/🎉"
  OK: "party"
